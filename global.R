library(httr)
library(jsonlite)
library(fcaR)

# Recupero los nombres de los archivos disponibles
returnNames <- function(){
  url <- "https://api.github.com/repos/fcatools/contexts/contents/contexts"
  response <- GET(url)
  if (status_code(response) == 200) {
    content_text <- content(response, as = "text", encoding = "UTF-8")
    content_json <- fromJSON(content_text)
    # Me quedo solo con los que tienen la extension .cxt
    files <- content_json$name[grepl("\\.cxt$", content_json$name)]
    return(files)
  }
  else{
    return("Error. Connection failed.")
  }
}

# Devuelve lista con las opciones disponibles
selectOptions <- function(){
  files <- returnNames()
  titles <- unlist(lapply(content_json$name, function(x) { yaml::read_yaml("https://fcarepository.org/contexts.yaml")[[x]]$title }))
  #languages <- unlist(lapply(content_json$name, function(x) { yaml::read_yaml("https://fcarepository.org/contexts.yaml")[[x]]$language }))
  return(list(files, titles))
  }

# Devuelve el contexto seleccionado por el usuario
returnFCFromRepo <- function(option){
  URL <- glue::glue("https://github.com/fcatools/contexts/raw/main/contexts/{option}")

  file <- tempfile(fileext = ".cxt")

  err <- try(
    download.file(URL,
                  destfile = file,
                  quiet = TRUE
    )
  )

  print(option)

  if (inherits(err, "try-error")) {
    stop("Download error.")
  } else {
    print("Found context.")
    print(option)

    meta <- yaml::read_yaml("https://fcarepository.org/contexts.yaml")[[option]]

    #print(meta)
    if (!is.null(meta)) {
      description <- glue::glue("- {cli::style_underline('Title')}: {meta$title}\n- {cli::style_underline('Description')}: {stringr::str_to_sentence(meta$description)}\n- {cli::style_underline('Source')}: {meta$source}\n\n",
                                .trim = FALSE
      ) |>
        cat()
    } else {
      description <- ""
    }
  }

  print(FormalContext$new(file)) # Esto devuelve el contexto requerido, y en description está la descripción.

  #unlink(file) # Esto borra el archivo temporal que hemos creado
}

parse_latex_table <- function(latex_text) {
  # Extract only lines with data (ignore caption, label, etc.)
  lines <- unlist(strsplit(latex_text, "\n"))
  table_lines <- lines[grepl("&", lines) & grepl("\\\\", lines)]

  # Remove LaTeX syntax
  table_lines <- gsub("\\\\(hline|end\\{tabular\\}|end\\{table\\})", "", table_lines)
  table_lines <- trimws(gsub("\\$|\\\\times", "×", table_lines))

  # Determinar el número máximo de columnas
  num_columns <- max(sapply(table_lines, function(line) length(unlist(strsplit(line, "&")))))

  # Split by &
  table_matrix <- do.call(rbind, lapply(table_lines, function(line) {
    cells <- unlist(strsplit(line, "&"))
    cells <- gsub("\\\\\\\\", "", cells)  # remove \\ at the end
    cells <- trimws(cells)
    # Rellenar con celdas vacías si faltan columnas
    length(cells) <- num_columns
    cells[is.na(cells)] <- ""
    return(cells)
  }))

  # Remove "×" from the start and end of each row
  table_matrix <- apply(table_matrix, 2, function(col) {
    gsub("^×|×$", "", col)
  })

  # First row is header
  colnames(table_matrix) <- c("-", table_matrix[1, -1])
  table_matrix <- table_matrix[-1, , drop = FALSE]

  # Convert to data.frame
  df <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  return(df)
}

parse_latex_concepts <- function(latex_text) {
  # Dividir en líneas
  lines <- unlist(strsplit(latex_text, "\n"))

  # Filtrar las líneas que contienen los conceptos
  concept_lines <- grep("^\\d+:.*\\\\\\\\$", lines, value = TRUE)

  # Limpiar y extraer las partes entre llaves con regex
  extract_concepts <- function(line) {
    # Eliminar código LaTeX innecesario
    line <- gsub("\\\\mathrm\\{([^}]*)\\}", "\\1", line)
    line <- gsub("\\\\ensuremath\\{?\\\\varnothing\\}?", "{}", line)
    line <- gsub("\\\\left|\\\\right|\\\\\\(|\\\\\\)|\\$|\\\\,", "", line)
    line <- gsub("\\\\_", "_", line)

    # Extraer las partes dentro de los conjuntos con regex
    matches <- regmatches(line, gregexpr("\\{[^}]*\\}", line))[[1]]

    # Casos: ambos conjuntos (lhs y rhs) o solo rhs (cuando lhs está vacío)
    lhs <- if (length(matches) >= 1) gsub("^\\{|\\}$", "", matches[1]) else ""
    rhs <- if (length(matches) >= 2) gsub("^\\{|\\}$", "", matches[2]) else ""

    return(c(lhs = lhs, rhs = rhs))
  }

  # Aplicar extracción
  concept_pairs <- t(sapply(concept_lines, extract_concepts))

  # Convertir a data.frame
  df <- as.data.frame(concept_pairs, stringsAsFactors = FALSE)

  df$lhs <- gsub("\\\\$", "", df$lhs)
  df$rhs <- gsub("\\\\$", "", df$rhs)
  return(df)
}

showPlot <- function(fc){
  dt <- parse_latex_concepts(fc$concepts$to_latex())

  concept_order <- Matrix::t(fcaR:::.subset(fc$concepts$extents()))

  M <- concept_order |>
    fcaR:::.reduce_transitivity()
  g <- igraph::graph_from_adjacency_matrix(
    M
  )

  V(g)$lhs <- dt$lhs
  V(g)$rhs <- dt$rhs


  # idea: le pongo el texto de latex y que lo imprima como tal

  vis_data <- visNetwork::toVisNetworkData(g)
  vis_data$nodes <- vis_data$nodes |>
    dplyr::mutate(title = glue::glue(
      "Concept {label}" # yo quiero que el titulo sea -> fca()$concepts[V(g)]
    ))


  visNetwork::visNetwork(
    nodes = vis_data$nodes,
    edges = vis_data$edges
  ) |>
    visNetwork::visIgraphLayout(
      layout = "layout_with_sugiyama"
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        algorithm = "hierarchical",
        labelOnly = FALSE
      ),
      nodesIdSelection = TRUE
    ) |>
    visNetwork::visEdges(
      arrows = list("to" = FALSE),
      smooth = TRUE
    ) |>
    visNetwork::visNodes(
      fixed = TRUE
    ) |>
    visNetwork::visLayout(randomSeed = 130301)

  }


