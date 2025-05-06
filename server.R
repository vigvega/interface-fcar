library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")

server <- function(input, output, session) {

  # Evento para ir a get started
  observeEvent(input$btn_start, {
    updateTabItems(session, "tabs", "get_started")
  })

  data <- reactive({

    # Primero miro si han seleccionado algun dataset de los proporcionados
    if(input$selectDataset == 'v'){
      req(input$selectDataset)
      read.csv("data/games.csv", row.names=1)
    }
    else if(input$selectDataset == 'g'){
      req(input$selectDataset)
      read.csv("data/ganter.csv", sep=";", row.names=1)
    }
    else if(input$selectDataset == 'p'){
      req(input$selectDataset)
      read.delim("data/planets.txt", row.names = 1)
    }
    else{ # sino, miro el archivo que se ha cargado
      req(input$file)
      ext <- tools::file_ext(input$file$datapath)
      if (ext == "csv"){
        if(input$delim == ""){
          read.csv(input$file$datapath, row.names = 1)
        }
        else{
          read.csv(input$file$datapath, row.names = 1, sep = input$delim)
        }
      }
      else if(ext == "txt"){
          read.delim(input$file$datapath, row.names = 1)
      }
    }
  })

  fca <- reactive({
    FormalContext$new(data())
  })

    # Visualizo datos
  output$contents <- renderDT({
    head(data(), 5)
  })



  # # Generar botones objetos y atributos
     btn_objects <- reactive ({ fca()$objects }) # esto seria un user input
     btn_attributes <- reactive({ fca()$attributes })
  #
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
        return("Choose attributes to see objects that has them")
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

}
