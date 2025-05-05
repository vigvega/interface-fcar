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

      list(
      checkboxGroupInput("objects", "Objects:",
                         choiceNames = btn_objects,
                         choiceValues = btn_objects,
                         selected = 0
      ),
      textOutput("att_in_common")
      )
    })

    output$btn_attributes <- renderUI({
      req(btn_attributes)

      checkboxGroupInput("attributes", "Attributes:",
                         choiceNames = btn_attributes,
                         choiceValues = btn_attributes
      )
    })

    output$att_in_common <- renderText({

      set_objects <- Set$new(fca$objects)
      sapply(input$objects, function(x){
        name <- x[1]
        set_objects$assign(name = 1)
        print(name)
        result <- fca$intent(set_objects)
        #captured_output <- capture.output(print(result))
        #paste(captured_output)
        print(set_objects)
      })



    })


    # output$att_in_common <- renderText({
    #
    #   if (is.null(input$objects)) {
    #     return("Select desired objects.")
    #   }
    #
    #   set_objects <- Set$new(fca$objects)
    #   cont <- 0
    #    sapply(fca$objects, function(obj){
    #      indices <- obj %in% input$objects
    #      print(input$objects[3])
    #
    #      # sapply a todos los input_objects -> al final pongo  set_objects <- Set$new(fca$objects) ?
    #      #if (obj %in% input$objects){
    #        #set_objects$assign(obj = 1)
    #        #print(obj %in% input$objects)
    #      #}
    #
    #   #     cont <- cont + 1
    #   #     if(cont == length(fca$objects)){
    #   #       result <- fca$intent(set_objects)
    #   #
    #   #       cont <- 0
    #   #     }
    #    }
    #   )
    #   })


    # Evento para btn_objects
    # Se muestran los atributos en común para todos los que sean pulsados
    # observe({
    #   lapply(seq_along(btn_objects), function(i) {
    #     observeEvent(input[[paste0("btn_obj_", i)]], {
    #       output$clicked_button <- renderText({
    #         name <- btn_objects[i]
    #         set_objects$assign(name = 1)
    #         result <- fca$intent(set_objects)
    #
    #         # Captura la impresión de consola como texto
    #         captured_output <- capture.output(print(result))
    #         paste(captured_output)
    #       })
    #     })
    #   })
    # })

}
