igraph_a_visNetwork <- function(grafo) {
  # Convertir a formato visNetwork
  visIgraph(grafo) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visPhysics(solver = "forceAtlas2Based")
}

server <- function(input, output, session) {

  # Evento para ir a get started
  observeEvent(input$btn_start, {
    updateTabItems(session, "tabs", "get_started")
  })

  # Fragmentos de código
  output$code_fc <- renderText({
    'fc <- FormalContext$new(data)\nprint(fc)'
  })

  output$code_concepts <- renderText({
    'fc$find_concepts()\nfc$concepts'
  })

  output$code_implications <- renderText({
    'fc$find_implications()\nfc$implications'
  })

  # Para cargar dataset
  dataset <- import_file_server("file_dataset")

  # Condicion para mostrar plot
  output$datasetNull <- reactive({ is.null(dataset$data()) })
  outputOptions(output, "datasetNull", suspendWhenHidden = FALSE)

  # Funcion para mostrar contexto formal
  getFC <- reactive({
    # Creo mi formal context
    if (is.null(dataset$data())) {
      return(NULL)
    }
    fc <- FormalContext$new(dataset$data())
    return(fc)
  })

  # Si no se han cargado datos, se muestra esta imagen
  output$img_no_data <- renderImage({
    list(
        src = "www/oops.png",
        contentType = "image/png",
        alt = "No has cargado datos todavía",
        class = "img_no_data"
      )
  }, deleteFile = FALSE)


  # FC
  observeEvent(input$btn_fc, {
    showModal(modalDialog(
      title = "Formal context",
      verbatimTextOutput("fc_text"),
      easyClose = TRUE
    ))
  })

   output$fc_text <- renderText({
     fc <- getFC()
     paste(capture.output(fc), collapse = "\n")
   })

   # Concepts
   observeEvent(input$btn_concepts, {
     showModal(modalDialog(
       title = "Get concepts",
       verbatimTextOutput("concepts_text"),
       easyClose = TRUE
     ))
   })

   output$concepts_text <- renderText({
        fc <- getFC()
        fc$find_concepts()
        paste(capture.output(fc$concepts), collapse = "\n")
   })

  # Implications
   observeEvent(input$btn_implications, {
     showModal(modalDialog(
       title = "Get implications",
       verbatimTextOutput("implications_text"),
       easyClose = TRUE
     ))
   })

   output$implications_text <- renderText({
     fc <- getFC()
     fc$find_implications()
     paste(capture.output(fc$implications), collapse = "\n")
   })

  # # Plot
  #  plotAux <- reactive({
  #    # Creo mi formal context
  #    fc <- getFC()
  #
  #    fc$find_concepts()
  #    fc$find_implications()
  #
  #    return(fc$concepts$plot())
  #  })
  #
  #
  #  output$graphFormalConcept <- renderVisNetwork({
  #    grafo <- plotAux()
  #
  #    # Convertir a visNetwork
  #    vis_grafo <- igraph_a_visNetwork(grafo)
  #
  #    vis_grafo
  #  })


   plotAux <- reactive({
         # Creo mi formal context
         fc <- getFC()

         fc$find_concepts()
         fc$find_implications()

         g <- fc$concepts$plot()

         return(g)

   })

   output$graphFormalConcept <- renderVisNetwork({

     g <- plotAux()

     nodes_df <- data.frame(
       id = V(g)$name,
       label = V(g)$name,
       title = V(g)$name
     )

     edges_df <- data.frame(
       from = as_edgelist(g)[,1],
       to = as_edgelist(g)[,2]
     )

     visNetwork(nodes = nodes_df, edges = edges_df) %>%
       visNodes(
         shape = "circle",
         font = list(size = 15)
       ) %>%
       visEdges(
         arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))
       ) %>%
       visOptions(
         highlightNearest = list(enabled = TRUE, degree = 1),
         selectedBy = list(variable = "label", multiple = TRUE)
       ) %>%
       visPhysics(solver = "forceAtlas2Based") %>%
       visInteraction(
         navigationButtons = TRUE,
         keyboard = TRUE,
         dragNodes = TRUE
       ) %>%
       visLayout(randomSeed = 123)
   })

}
