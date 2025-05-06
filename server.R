library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")

games <- read.csv("~/Desktop/TFG/games.csv", row.names=1)
fca <- FormalContext$new(games)
set_objects <- Set$new(fca$objects)

server <- function(input, output, session) {

  # Evento para ir a get started
  observeEvent(input$btn_start, {
    updateTabItems(session, "tabs", "get_started")
  })

    # Para cargar dataset
    dataset <- import_file_server("file_dataset")

  # Generar botones objetos y atributos
    btn_objects <- fca$objects # esto seria un user input
    btn_attributes <- fca$attributes

    output$btn_objects <- renderUI({
      req(btn_objects)

      checkboxGroupInput("objects", "",
                         choiceNames = btn_objects,
                         choiceValues = btn_objects,
                         selected = 0
      )
    })

    output$btn_attributes <- renderUI({
      req(btn_attributes)

      checkboxGroupInput("attributes", "",
                         choiceNames = btn_attributes,
                         choiceValues = btn_attributes
      )
    })

    output$btn_attributes2 <- renderUI({
      req(btn_attributes)

      checkboxGroupInput("attributes2", "",
                           choiceNames = btn_attributes,
                           choiceValues = btn_attributes
      )
    })

    output$intent <- renderText({

      if(is.null(input$objects)){
        return("Choose objects to see attributes in common")
      }

      set_objects <- Set$new(fca$objects)
      sapply(input$objects, function(x){
        do.call(set_objects$assign, setNames(list(1), x))
      })
      result <- fca$intent(set_objects)
      paste(capture.output(print(result)))
    })

    output$extent <- renderText({

      if(is.null(input$attributes)){
        return("Choose attributes to see objects in common")
      }

      set_attributes <- Set$new(fca$attributes)
      sapply(input$attributes, function(x){
        do.call(set_attributes$assign, setNames(list(1), x))
      })
      result <- fca$extent(set_attributes)
      paste(capture.output(print(result)))
    })

    output$closure <- renderText({

      if(is.null(input$attributes2)){
        return("Choose attributes to see objects in common")
      }

      set_attributes2 <- Set$new(fca$attributes)
      sapply(input$attributes2, function(x){
        do.call(set_attributes2$assign, setNames(list(1), x))
      })
      result <- fca$closure(set_attributes2)
      paste(capture.output(print(result)))
    })


}
