library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
library(fcaR)
library(shinyjs)
#devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")
source("uiHome.R")
source("uiUploadData.R")
source("uiBasicOperations.R")

server <- function(input, output, session) {

  # Evento para ir al primer paso
  observeEvent(input$btn_start, {
    updateTabItems(session, "tabs", "upload_data")
  })

  observeEvent(input$btnGoBasicOperations, {
    updateTabItems(session, "tabs", "basic_operations")
  })

  observeEvent(input$btnGoUploadData, {
    updateTabItems(session, "tabs", "upload_data")
  })

  observeEvent(input$btnGoImplications, {
    updateTabItems(session, "tabs", "ui_implications")
  })


  # Cargo el dataset
  data <- reactive({

    # Primero miro si han seleccionado algun dataset de los proporcionados
    if(input$selectDataset == 'v'){
      req(input$selectDataset)
      enable("btnGoBasicOperations")
      read.csv("data/games.csv", row.names=1)
    }
    else if(input$selectDataset == 'g'){
      req(input$selectDataset)
      enable("btnGoBasicOperations")
      read.csv("data/ganter.csv", sep=";", row.names=1)
    }
    else if(input$selectDataset == 'p'){
      req(input$selectDataset)
      enable("btnGoBasicOperations")
        read.delim("data/planets.txt", row.names = 1)
    }
    else{ # sino, miro el archivo que se ha cargado
      req(input$file)
      enable("btnGoBasicOperations")
      ext <- tools::file_ext(input$file$datapath)
      if (ext == "csv"){
        if(input$delim == ""){
          read.csv(input$file$datapath, row.names = 1, skip = input$skip)
        }
        else{
          read.csv(input$file$datapath, row.names = 1, sep = input$delim, skip = input$skip)
        }
      }
      else if(ext == "txt"){
          read.delim(input$file$datapath, row.names = 1, skip = input$skip)
      }
    }
  })


  # Compruebo si hay datos cargados y los visualizo
  output$contents <- renderUI({
    if(!(is.null(input$fileRds))){
      enable("btnGoBasicOperations")
      tags$div(
        style = "text-align: center;",
        tags$br(),
        tags$h4("Formal Context successfully imported!", class = "roboto-mono"),
        tags$img(
          src = "success2.gif",
          width = "400vh",
          height = "auto"
        ),
        tags$h5("Nothing to show here. Go to next page", class = "roboto-mono")
      )
    }
    else if (is.null(input$file)  && input$selectDataset == "") {
      tags$div(
        style = "text-align: center;",
        tags$br(),
        tags$img(
          src = "oops.png",
          width = "350vh",
          height = "auto"
        )      )
    } else {
      req(data())
      datatable(head(data(), 10), options = list(scrollX = TRUE))
    }
  })


  # Creo contexto formal
  fca <- reactive({
    if (is.null(input$fileRds)){
      FormalContext$new(data())
    }
    else{
      req(input$fileRds)
      FormalContext$new(input$fileRds$datapath)
      # cambiar imagen a una que ponga algo del estilo: nothing to show here
    }
  })


  # Visualizo contexto formal
  output$formalContext <- renderPrint({
    paste(capture.output(print(fca())))
    })

  # Visualizo implicaciones
  output$fcImplications <- renderText({
    fca()$find_implications()
    paste(capture.output(print(fca()$implications)), collapse = "\n")
  })

  # Aplicar reglas
  observeEvent(input$btnApplyRules, {
    fca()$implications$apply_rules(rules = c(input$selectRulesImplications))
    output$fcImplications <- renderText({
      paste(capture.output(print(fca()$implications)), collapse = "\n")
    })
  })

  # Limpiar y ver implicaciones sin reglas
  observeEvent(input$btnClearRules, {
    fca()$find_implications()
    output$fcImplications <- renderText({
      paste(capture.output(print(fca()$implications)), collapse = "\n")
    })
    })

   # Generar botones objetos y atributos
     btn_objects <- reactive ({ fca()$objects })
     btn_attributes <- reactive({ fca()$attributes })

     output$btn_objects <- renderUI({
       req(btn_objects())

       checkboxGroupInput("objects", "",
                          choiceNames = btn_objects(),
                          choiceValues = btn_objects(),
                          selected = 0
       )
     })

  output$btn_attributes <- renderUI({
    req(btn_attributes())

    checkboxGroupInput("attributes", "",
                       choiceNames = btn_attributes(),
                       choiceValues = btn_attributes()
    )
  })

  output$btn_attributes2 <- renderUI({
    req(btn_attributes())

    checkboxGroupInput("attributes2", "",
                         choiceNames = btn_attributes(),
                         choiceValues = btn_attributes()
    )
  })

    output$intent <- renderText({

      if(is.null(input$objects)){
        return("Choose objects to see attributes in common")
      }

      set_objects <- Set$new(fca()$objects)
      sapply(input$objects, function(x){
        do.call(set_objects$assign, setNames(list(1), x))
      })
      result <- fca()$intent(set_objects)
      paste(capture.output(print(result)))
    })

    output$extent <- renderText({

      if(is.null(input$attributes)){
        return("Choose attributes to see objects that have them")
      }

      set_attributes <- Set$new(fca()$attributes)
      sapply(input$attributes, function(x){
        do.call(set_attributes$assign, setNames(list(1), x))
      })
      result <- fca()$extent(set_attributes)
      paste(capture.output(print(result)))
    })

    output$closure <- renderText({

      if(is.null(input$attributes2)){
        return("Choose attributes to see closure")
      }

      set_attributes <- Set$new(fca()$attributes)
      sapply(input$attributes2, function(x){
        do.call(set_attributes$assign, setNames(list(1), x))
      })
      result <- fca()$closure(set_attributes)
      paste(capture.output(print(result)))
    })


    # Modal para ver la tabla latex del contexto formla
    observeEvent(input$createLatex, {
      showModal(modalDialog(
        title = "Latex table",
        p("Copy the following text and paste it in your .tex file"),
        verbatimTextOutput("latexTable"),
        footer = tagList(
          actionButton("closeModalFC", "Close", class = "btn-secondary")),
        size = "m",
        easyClose = TRUE
      ))
    })

    output$latexTable <- renderText({
      paste(capture.output(print(fca()$to_latex())), collapse = "\n")
    })

    # Close modal button
    observeEvent(input$closeModalFC, {
      removeModal()
    })

    # Descarga
    output$downloadRds <- downloadHandler(
      filename = function() {
        "fc.rds"
      },
      content = function(file) {
        # Guardo en local
        fca()$save(filename = "./fc.rds")
        # Luego lo copio para descarga
        file.copy("./fc.rds", file)
      }
    )

    # Modal para ver la tabla latex de implicaciones
    observeEvent(input$createLatexImplications, {
      showModal(modalDialog(
        title = "Latex table for implications",
        p("Copy the following text and paste it in your .tex file"),
        verbatimTextOutput("latexTableImplications"),
        footer = tagList(
          actionButton("closeModalImplications", "Close", class = "btn-secondary")),
        size = "m",
        easyClose = TRUE
      ))
    })

    output$latexTableImplications <- renderText({
      paste(capture.output(print(fca()$implications$to_latex())), collapse = "\n")
    })

    # Close modal button
    observeEvent(input$closeModalImplications, {
      removeModal()
    })

    # Actualizo el select con el numero de reglas generadas
    observe({
      fca()$find_implications()
      num <- fca()$implications$cardinality()

      options <- paste("Rule", 1:num)
      names(options) <- paste("Rule", 1:num)

      updateSelectInput(
        session,
        "selectRulesFromImplications",
        choices = options
      )

      # Aplico el operador holds_in
      output$holdsIn <- renderText({

        if(is.null(input$selectRulesFromImplications)){
          return("Select rules")
        }

        indexes <- as.numeric(gsub("\\D", "", input$selectRulesFromImplications))
        imp <- fca()$implications[indexes]
        holds <- imp %holds_in% fca()

        paste(holds)
      })

    })

    # fcText
    output$fcText <- renderText({ "fc <- FormalContext$new(dataset) \nprint(fc)" })

}
