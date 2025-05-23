library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
library(shinyalert)
library(shinyWidgets)
library(fcaR)
library(shinyjs)
#devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")

source("uiHome.R")
source("uiUploadData.R")
source("uiBasicOperations.R")
source("uiImplications.R")
source("uiConcepts.R")

server <- function(input, output, session) {

  # Eventos para navegar entre pestañas
  observeEvent(input$btn_start, {
    updateTabItems(session, "tabs", "upload_data")
  })

  observeEvent(input$btnGoBasicOperations, {
    updateTabItems(session, "tabs", "basic_operations")
  })

  observeEvent(input$btnGoUploadData, {
    updateTabItems(session, "tabs", "upload_data")
  })

  observeEvent(input$btnGoConcepts, {
    updateTabItems(session, "tabs", "ui_concepts")
  })

  observeEvent(input$btnGoBack, {
    updateTabItems(session, "tabs", "basic_operations")
  })

  observeEvent(input$btnGoImplications, {
    updateTabItems(session, "tabs", "ui_implications")
  })

  observeEvent(input$btnGoBackConcep, {
    updateTabItems(session, "tabs", "ui_concepts")
  })


  #######################
  ### TAB UPLOAD DATA ###
  #######################

  fca <- reactive({
    if(!(is.null(input$file))){
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if(ext != "csv" && ext != "rds" && ext != "cxt"){
        shinyalert(
          title = "Warning",
          text = "Unsupported file extension. Please upload a .csv, .rds or .cxt",
          type = "warning"
        )

        FormalContext$new(planets) # para que no explote simplemente

      }
      else{
        FormalContext$new(input$file$datapath)
    }
    }
     else if(input$selectDataset != ""){
       returnFCFromRepo(input$selectDataset)
     }
      else{
      FormalContext$new(planets) # para que no explote simplemente
      }
  })


  # Establecer conexion con el repo
  observeEvent(input$connectRepo, {
    Sys.sleep(5)  # Simular una tarea lenta

    options <- selectOptions()[[1]]

    if(options[1] == "Error. Connection failed."){
      output$connectionFailed <- renderText({ "Something went wrong. Please, try again" })
    }
    else{
      print(selectOptions()[[2]])
    names(options) <- selectOptions()[[2]]
    #print(names(options))
    updateSelectInput(
      session,
      "selectDataset",
      choices = options
    )
    }
  })

  output$contents <- renderUI({
    # no hay datos
    if(is.null(input$file) && input$selectDataset == ""){
      disable("btnGoBasicOperations")
           tags$div(
             style = "text-align: center;",
             tags$br(),
             tags$img(
               src = "oops.png",
               width = "350vh",
               height = "auto"
             )
           )
    }
    else{
      if(!(is.null(input$file))){
        ext <- tolower(tools::file_ext(input$file$name))
        # se ha subido fichero invalido
        if(ext != "csv" && ext != "rds" && ext != "cxt"){
          tags$div(
            style = "text-align: center;",
            tags$br(),
            tags$img(
              src = "oops.png",
              width = "350vh",
              height = "auto"
            )
          )
        }
        else{ # se ha subido fichero valido
          enable("btnGoBasicOperations")
          tableOutput("tableData")
        }
      }
      else{ # otros casos (select)
        enable("btnGoBasicOperations")
        tableOutput("tableData")
      }
    }
  })

  output$tableData <- renderTable({
    #print(fca())
    parse_latex_table(fca()$to_latex())
  })


  ############################
  ### TAB BASIC OPERATIONS ###
  ############################


  # # Visualizo contexto formal
   output$formalContext <- renderText({
     paste(capture.output(print(fca())), collapse = "\n")
   })

  # Clarify
  observeEvent(input$clarify, {
    fca()$clarify()
    output$formalContext <- renderText({
      paste(capture.output(print(fca())), collapse = "\n")
    })
  })

  # Reduce
  observeEvent(input$reduce, {
    fca()$reduce()
    output$formalContext <- renderText({
      paste(capture.output(print(fca())), collapse = "\n")
    })
  })

  # multiInput -> Para seleccionar objetos o atributos y calcular su intent, extent o closure
  observe({
  updateMultiInput(
    session = session,
    inputId = "intentOptions",
    choices = fca()$objects
  )
  })

  observe({
    updateMultiInput(
      session = session,
      inputId = "extentOptions",
      choices = fca()$attributes
    )
  })

  observe({
    updateMultiInput(
      session = session,
      inputId = "closureOptions",
      choices = fca()$attributes
    )
  })

  # Para mostrar los resultados de las operaciones anteriores

    output$intentResult <- renderText({

      if(is.null(input$intentOptions)){
        return("Nothing selected")
      }

      set_objects <- Set$new(fca()$objects)
      sapply(input$intentOptions, function(x){
        do.call(set_objects$assign, setNames(list(1), x))
      })
      result <- fca()$intent(set_objects)
      paste(capture.output(print(result)))
    })

    output$extentResult <- renderText({

      if(is.null(input$extentOptions)){
        return("Nothing selected")
      }

      set_attributes <- Set$new(fca()$attributes)
      sapply(input$extentOptions, function(x){
        do.call(set_attributes$assign, setNames(list(1), x))
      })
      result <- fca()$extent(set_attributes)
      paste(capture.output(print(result)))
    })

    output$closureResult <- renderText({

      if(is.null(input$closureOptions)){
        return("Nothing selected")
      }

      set_attributes <- Set$new(fca()$attributes)
      sapply(input$closureOptions, function(x){
        do.call(set_attributes$assign, setNames(list(1), x))
      })
      result <- fca()$closure(set_attributes)
      paste(capture.output(print(result)))
    })

    # # Modal para ver la tabla latex del contexto formal
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

    #   # Close modal button
       observeEvent(input$closeModalFC, {
         removeModal()
       })

    #   # Descarga
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

       ########################
       ### TAB IMPLICATIONS ###
       ########################

  # Aviso de que se van a calcular las implicaciones
       observeEvent(input$tabs, {
         #fca()$find_implications() # borrar

         if (input$tabs == "ui_implications" && fca()$implications$cardinality() == 0) {
           showModal(modalDialog(
             title = "Warning!",
             "Do you want to compute the set of implications? This may take a while.",
             easyClose = TRUE,
             footer = tagList(
               input_task_button("getImplications", "Compute set of implications"),
               actionButton("goBack", "Go back")
             )
           ))
           reactiveVal(FALSE)
         }
         else{
           output$fcImplications <- renderText({
             paste(capture.output(print(fca()$implications)), collapse = "\n")
           })
         }
       })

       # Calculo y muestro implicaciones
       observeEvent(input$getImplications, {
         Sys.sleep(3)  # Simular una tarea lenta
         fca()$find_implications()
         output$fcImplications <- renderText({
           paste(capture.output(print(fca()$implications)), collapse = "\n")
         })
         removeModal()
       })

       # O vuelvo atras
       observeEvent(input$goBack, {
         updateTabItems(session, "tabs", "basic_operations")
         removeModal()
       })


      # Filtros
      observe({
        updateMultiInput(
          session = session,
          inputId = "selectLHS",
        choices = fca()$attributes
        )
      })

      observe({
        updateMultiInput(
          session = session,
          inputId = "selectRHS",
          choices = fca()$attributes
        )
      })

      observe({
        updateMultiInput(
          session = session,
          inputId = "selectNotLHS",
          choices = fca()$attributes
        )
      })

      observe({
        updateMultiInput(
          session = session,
          inputId = "selectNotRHS",
          choices = fca()$attributes
        )
      })


      # Aplicar filtros
      observeEvent(input$btnApplyFilters, {

        imp <- fca()$implications
        if(input$support > 0){
          indx <- which(fca()$implications$support() > input$support)
          imp <- fca()$implications[indx]
        }

        # Si han escogido algun atributo
        filteredImplications <- imp$filter(
          lhs = c(input$selectLHS),
          not_lhs = c(input$selectNotLHS),
          rhs=c(input$selectRHS),
          not_rhs=c(input$selectNotRHS)
        )

        output$fcImplications <- renderText({
          paste(capture.output(print(filteredImplications)), collapse = "\n")
        })
      })


      # Limpiar y ver implicaciones sin filtros
      observeEvent(input$btnClearFilters, {
        #Sys.sleep(3)
        fca()$find_implications()
        output$fcImplications <- renderText({
          paste(capture.output(print(fca()$implications)), collapse = "\n")
        })
        })

      # Simplify
      observeEvent(input$simplifyImplications, {
        Sys.sleep(3)
        fca()$implications$apply_rules(rules = c("simplify"))
        output$fcImplications <- renderText({
          paste(capture.output(print(fca()$implications)), collapse = "\n")
        })
      })

      # Aplicar reglas
      observeEvent(input$selectRulesImplications, {
        fca()$implications$apply_rules(rules = c(input$selectRulesImplications))
        output$fcImplications <- renderText({
          paste(capture.output(print(fca()$implications)), collapse = "\n")
        })
      })

      # Closure
      observe({
        updateMultiInput(
          session = session,
          inputId = "selectClosure",
          choices = fca()$attributes
        )
      })

      output$implicationsClosure <- renderText({

        if(is.null(input$selectClosure)){
          return("Nothing selected")
        }

        set_attributes <- Set$new(fca()$attributes)
        sapply(input$selectClosure, function(x){
          do.call(set_attributes$assign, setNames(list(1), x))
        })
        result <- fca()$implications$closure(set_attributes)
        paste(capture.output(print(result)))
      })

      output$downloadRdsImp <- downloadHandler(
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

      #   # Modal para ver la tabla latex de implicaciones
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


      ####################
      ### TAB CONCEPTS ###
      ####################

         # Aviso de que se van a calcular los conceptos
         observeEvent(input$tabs, {
           #fca()$find_implications() # borrar
           print("Entro")
           print(fca()$concepts)
           if (input$tabs == "ui_concepts" && fca()$concepts$is_empty()) {

             showModal(modalDialog(
               title = "Warning!",
               "Do you want to compute the set of concepts? This may take a while.",
               easyClose = TRUE,
               footer = tagList(
                 input_task_button("getConcepts", "Compute set of concepts"),
                 actionButton("goBackConc", "Go back")
               )
             ))
             reactiveVal(FALSE)
           }
           else{
             output$plot <- renderVisNetwork({
               showPlot(fca())
             })

             output$conceptSelected <- renderText({
               index <- as.numeric(input$plot_selected)
               print(index)
               paste(capture.output(print(fca()$concepts[index])), collapse = "\n")
             })

             output$lowerNeighbours <- renderText({
               index <- as.numeric(input$plot_selected)
               lower <- fca()$concepts$lower_neighbours(fca()$concepts[index])
               paste(capture.output(print(lower)), collapse = "\n")
             })

             output$upperNeighbours <- renderText({
               index <- as.numeric(input$plot_selected)
               lower <- fca()$concepts$upper_neighbours(fca()$concepts[index])
               paste(capture.output(print(lower)), collapse = "\n")
             })
           }
         })

          # Calculo y muestro conceptos
          observeEvent(input$getConcepts, {
            Sys.sleep(3)  # Simular una tarea lenta
            fca()$find_concepts()

            # PLOT
            output$plot <- renderVisNetwork({
              showPlot(fca())
            })

            output$conceptSelected <- renderText({
              index <- as.numeric(input$plot_selected)
              print(index)
              paste(capture.output(print(fca()$concepts[index])), collapse = "\n")
            })

            output$lowerNeighbours <- renderText({
              index <- as.numeric(input$plot_selected)
              lower <- fca()$concepts$lower_neighbours(fca()$concepts[index])
              paste(capture.output(print(lower)), collapse = "\n")
            })

            output$upperNeighbours <- renderText({
              index <- as.numeric(input$plot_selected)
              lower <- fca()$concepts$upper_neighbours(fca()$concepts[index])
              paste(capture.output(print(lower)), collapse = "\n")
            })



            removeModal()
          })

          # O vuelvo atras
          observeEvent(input$goBackConc, {
            updateTabItems(session, "tabs", "ui_implications")
            removeModal()
          })

   # Modal para ver la tabla latex de conceptos
    observeEvent(input$createLatexConcepts, {
      showModal(modalDialog(
        title = "Latex table for concepts",
        p("Copy the following text and paste it in your .tex file"),
        verbatimTextOutput("latexTableConcepts"),
        footer = tagList(
          actionButton("closeModalConcepts", "Close", class = "btn-secondary")),
        size = "m",
        easyClose = TRUE
      ))
    })

     output$latexTableConcepts <- renderText({
      paste(capture.output(print(fca()$concepts$to_latex())), collapse = "\n")
    })

        # Close modal button
    observeEvent(input$closeModalConcepts, {
      removeModal()
    })

    # Descarga
    output$downloadRdsConp <- downloadHandler(
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


}
