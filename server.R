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

  # Variables reactivas
  rv <- reactiveValues()
  history <- reactiveVal(data.frame(objects = character(), attributes = character(), stringsAsFactors = FALSE))


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

    names(options) <- selectOptions()[[2]]

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
          d <- parse_latex_table(fca()$to_latex())
          DT::renderDT(d)
        }
      }
      else{ # otros casos (select)
        enable("btnGoBasicOperations")
        d <- parse_latex_table(fca()$to_latex())
        DT::renderDT(d)
      }
    }
  })

  output$tableData <- renderTable({
    parse_latex_table(fca()$to_latex())
  })


  ############################
  ### TAB BASIC OPERATIONS ###
  ############################

  output$fcBasicOperations <- renderUI({
        tableOutput("tableData")
  })


  # # Visualizo contexto formal
   output$formalContext <- renderText({
     paste(capture.output(print(fca())), collapse = "\n")
   })

  # Clarify
  observeEvent(input$clarify, {
    fca()$clarify()

    output$fcBasicOperations <- renderTable({
      parse_latex_table(fca()$to_latex())
    })
  })

  # Reduce
  observeEvent(input$reduce, {
    fca()$reduce()

    output$fcBasicOperations <- renderTable({
      parse_latex_table(fca()$to_latex())
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

    output$intentResult <- renderUI({

      if(is.null(input$intentOptions)){
        return("Nothing selected")
      }

      set_objects <- Set$new(fca()$objects)
      sapply(input$intentOptions, function(x){
        do.call(set_objects$assign, setNames(list(1), x))
      })
      result <- fca()$intent(set_objects)
      text <- capture.output(print(result))
      HTML(paste(text, collapse = "<br>"))
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
      text <- capture.output(print(result))
      HTML(paste(text, collapse = "<br>"))
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
      text <- capture.output(print(result))
      HTML(paste(text, collapse = "<br>"))
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
        fca()$to_latex()
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
           output$fcImplications <- renderTable({
             parse_latex_implications(fca()$implications$to_latex())
           })
         }
       })

       # Calculo y muestro implicaciones
       observeEvent(input$getImplications, {
         Sys.sleep(3)  # Simular una tarea lenta
         fca()$find_implications()
         output$fcImplications <- renderTable({
           parse_latex_implications(fca()$implications$to_latex())
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

        output$fcImplications <- renderTable({
          parse_latex_implications(filteredImplications$to_latex())
        })
      })


      # Limpiar y ver implicaciones sin filtros
      observeEvent(input$btnClearFilters, {
        #Sys.sleep(3)
        fca()$find_implications()
        output$fcImplications <- renderTable({
          parse_latex_implications(fca()$implications$to_latex())
        })
        })

      # Simplify
      observeEvent(input$simplifyImplications, {
        Sys.sleep(3)
        fca()$implications$apply_rules(rules = c("simplify"))
        output$fcImplications <- renderTable({
          parse_latex_implications(fca()$implications$to_latex())
        })
      })

      # Aplicar reglas
      observeEvent(input$selectRulesImplications, {
        fca()$implications$apply_rules(rules = c(input$selectRulesImplications))
        output$fcImplications <- renderTable({
          parse_latex_implications(fca()$implications$to_latex())
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



      output$implicationsClosure <- renderTable({
        set_attributes <- Set$new(fca()$attributes)
        sapply(input$selectClosure, function(x){
          do.call(set_attributes$assign, setNames(list(1), x))
        })
        result <- fca()$implications$closure(set_attributes, reduce = TRUE)$implications$to_latex()

        print("ENTRO")

        parse_latex_implications(result)
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
           #fca()$find_concepts() # borrar
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

             output$allConcepts <- renderTable({
               parse_latex_concepts(fca()$concepts$to_latex())
             })

             output$conceptSelected <- renderUI({

               if (is.null(input$plot_selected) || input$plot_selected == "") {
                 return(HTML("<p>Nothing selected</p>"))
               }
               index <- as.numeric(input$plot_selected)
               renderTable({
                 parse_latex_concepts(fca()$concepts[index]$to_latex())
               })
             })

             # output$conceptSelected <- renderText({
             #   if(input$plot_selected==""){
             #     return("Nothing selected")
             #   }
             #
             #   index <- as.numeric(input$plot_selected)
             #   text <- capture.output(print(fca()$concepts[index]))
             #   HTML(paste(text, collapse = "<br>"))
             #   #paste(capture.output(print(fca()$concepts[index])), collapse = "\n")
             # })

             output$upperNeighbours <- renderUI({

               if (is.null(input$plot_selected) || input$plot_selected == "") {
                 return(HTML("<p>Nothing selected</p>"))
               }
               index <- as.numeric(input$plot_selected)
               upper <- fca()$concepts$upper_neighbours(fca()$concepts[index])
               renderTable({
                 parse_latex_concepts(upper$to_latex())
               })
             })

             output$lowerNeighbours <- renderUI({

               if (is.null(input$plot_selected) || input$plot_selected == "") {
                 return(HTML("<p>Nothing selected</p>"))
               }
               index <- as.numeric(input$plot_selected)
               lower <- fca()$concepts$lower_neighbours(fca()$concepts[index])
               renderTable({
                 parse_latex_concepts(lower$to_latex())
               })
             })


             # output$lowerNeighbours <- renderText({
             #   if(input$plot_selected==""){
             #     return("Nothing selected")
             #   }
             #
             #   index <- as.numeric(input$plot_selected)
             #   lower <- fca()$concepts$lower_neighbours(fca()$concepts[index])
             #   paste(capture.output(print(lower)), collapse = "\n")
             # })

             # output$upperNeighbours <- renderText({
             #   if(input$plot_selected==""){
             #     return("Nothing selected")
             #   }
             #
             #   index <- as.numeric(input$plot_selected)
             #   lower <- fca()$concepts$upper_neighbours(fca()$concepts[index])
             #   paste(capture.output(print(lower)), collapse = "\n")
             # })

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

            output$allConcepts <- renderTable({
              parse_latex_concepts(fca()$concepts$to_latex())
            })

            output$conceptSelected <- renderUI({

              if (is.null(input$plot_selected) || input$plot_selected == "") {
                return(HTML("<p>Nothing selected</p>"))
              }
              index <- as.numeric(input$plot_selected)
              renderTable({
                parse_latex_concepts(fca()$concepts[index]$to_latex())
              })
            })


            output$upperNeighbours <- renderUI({

              if (is.null(input$plot_selected) || input$plot_selected == "") {
                return(HTML("<p>Nothing selected</p>"))
              }
              index <- as.numeric(input$plot_selected)
              upper <- fca()$concepts$upper_neighbours(fca()$concepts[index])
              renderTable({
                parse_latex_concepts(upper$to_latex())
              })
            })

            output$lowerNeighbours <- renderUI({

              if (is.null(input$plot_selected) || input$plot_selected == "") {
                return(HTML("<p>Nothing selected</p>"))
              }
              index <- as.numeric(input$plot_selected)
              lower <- fca()$concepts$lower_neighbours(fca()$concepts[index])
              renderTable({
                parse_latex_concepts(lower$to_latex())
              })
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
      fca()$concepts$to_latex()
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

    # Selección de atributos iniciales
    observe({
      disable("btnBeginExploration")
      disable("btnNextStep")
      updateSelectInput(
        session = session,
        inputId = "initAtt",
        choices = fca()$attributes
      )
    })

    # Selección de atributos objetivo
    observe({
      disable("btnBeginExploration")
      disable("btnNextStep")
      updateSelectInput(
        session = session,
        inputId = "targetAtt",
        choices = fca()$attributes
      )
    })

    output$plot2 <- renderVisNetwork({
      g <- getGraph(fca()$concepts)
      showPlot2(g)
    })

    # Mostrar target concept
    observeEvent({input$initAtt
      input$targetAtt}, {
      # Nodo inicial
      index_init <- getOneConcept(fca(), input$initAtt)
      print("Init")
      print(index_init)
      # Nodo target
      list <- c(input$initAtt, input$targetAtt)
      #print(list)
      index_target <- getOneConcept(fca(), list)

      nodes <- getGraph(fca()$concepts)$nodes

      # Cambio su color
      nodes$color <- ifelse((nodes$id == index_init | nodes$id == index_target), "blue", "#97C2FC")


      visNetworkProxy("plot2") %>%
        visUpdateNodes(nodes = nodes)

      enable("btnBeginExploration")


    })

    output$allConcepts <- renderTable({
      parse_latex_concepts(rv$sublattice$to_latex())
    })

    output$initConcept <- renderTable({
      index_init <- getOneConcept(fca(), input$initAtt)
      d <- fca()$concepts[index_init]$to_latex()
      dt <- parse_latex_concepts(d)
      dt[2:3]
    })

    output$targetConcept <- renderTable({
      list <- c(input$initAtt, input$targetAtt)
      index_target <- getOneConcept(fca(), list)
      d <- fca()$concepts[index_target]$to_latex()
      dt <- parse_latex_concepts(d)
      dt[2:3]
    })


    observeEvent(input$btnBeginExploration, {
      # Construyo el subretículo
      show("btnNextStep")
      list <- c(input$initAtt, input$targetAtt)
      init <- getOneConcept(fca(), input$initAtt)
      target <- getOneConcept(fca(), list)


      print("INIIIIIT")
      print(fca()$concepts[init])
      print("TARGGGGEEEEtttt")
      print(fca()$concepts[target])

      rv$sublattice <- getSublattice2(fca()$concepts, init, target)
      print("SUBLATTICEEEE")
      print(rv$sublattice)

      # Compruebo si ha sido posible crearlo
      if(is.null(rv$sublattice)){
        shinyalert("Oops!", "Those attributes are not reachable. Please, choose others.", type = "error")
      }
      else{
        if(rv$sublattice$size() == 2){
          hide("btnNextStep")

          nueva_fila <- parse_latex_concepts(rv$sublattice[rv$sublattice$size()]$to_latex())
          nueva_fila <- data.frame(
            attributes = nueva_fila[2],
            objects = nueva_fila[3],
            stringsAsFactors = FALSE
          )


          history_actual <- history()
          history(rbind(history_actual, nueva_fila))

        }

        # Actualizo lo de todos los conceptos
        output$allConcepts <- renderTable({
          parse_latex_concepts(rv$sublattice$to_latex())
        })
      g <- getGraph(rv$sublattice)

      output$plot2 <- renderVisNetwork({
        showPlot2(g)

      })

      # Cambio de color del primero y el ultimo, y los que se pueden seleccionar
      g$nodes$color <- character(length(g$nodes$id))  # Inicializar el vector de colores

      for (i in seq_along(g$nodes$id)) {
        if (g$nodes$id[i] == 1 || g$nodes$id[i] == rv$sublattice$size()) {
          g$nodes$color[i] <- "blue"
        }
        else if(g$nodes$id[i] %in% indexLowerNeighbours(rv$sublattice, rv$sublattice[1])){
          g$nodes$color[i] <- "orange"
        }
        else {
          g$nodes$color[i] <- "#97C2FC"
        }
      }


      visNetworkProxy("plot2") %>%
        visUpdateNodes(nodes = g$nodes)

      # Muestro el siguiente paso
      output$nextStepConcepts <- renderTable({
        d <- rv$sublattice$lower_neighbours(rv$sublattice[1])$to_latex()
        dt <- parse_latex_concepts(d)
        dt[2:3]
      })


      nueva_fila <- parse_latex_concepts(rv$sublattice[1]$to_latex())
      nueva_fila <- data.frame(
        attributes = nueva_fila[2],
        objects = nueva_fila[3],
        stringsAsFactors = FALSE
      )

      history_actual <- history()
      history(rbind(history_actual, nueva_fila))

      }

    })

    # Btn clear -> borra el progreso
    observeEvent(input$btnClearExploration, {
      output$plot2 <- renderVisNetwork({
        showPlot2(getGraph(fca()$concepts))
      })

      output$allConcepts <- renderTable({
        parse_latex_concepts(fca()$concepts$to_latex())
      })

      history(data.frame(objects = character(), attributes = character(), stringsAsFactors = FALSE))

    })

    # Habilito el boton de siguiente paso si se pulsa algo en un nodo correcto
    observeEvent(input$plot2_selected, {
      if(input$plot2_selected=="")
        disable("btnNextStep")
      else{
        # AQUI TENGO QUE VER SI ES UN VECINO INFERIOR
        if(!(is.null(rv$sublattice)) && input$plot2_selected %in% indexLowerNeighbours(rv$sublattice, rv$sublattice[1]))
          enable("btnNextStep")
        else{
          disable("btnNextStep")
        }
      }
    })

    # Btn next step -> enable si se pulsa un nodo correcto
    observeEvent(input$btnNextStep, {

        chosenConcept <- as.numeric(input$plot2_selected)
        new <- rv$sublattice[chosenConcept]


        rv$sublattice <- getSublattice2(rv$sublattice, chosenConcept, rv$sublattice$size())
        g <- getGraph(rv$sublattice)
        output$plot2 <- renderVisNetwork({
          showPlot2(g)
        })

        # Cambio de color del primero y el ultimo, y los que se pueden seleccionar
        g$nodes$color <- character(length(g$nodes$id))  # Inicializar el vector de colores

                for (i in seq_along(g$nodes$id)) {
          if (g$nodes$id[i] == 1 || g$nodes$id[i] == rv$sublattice$size()) {
            g$nodes$color[i] <- "blue"
          }
          else if(g$nodes$id[i] %in% indexLowerNeighbours(rv$sublattice, rv$sublattice[1])){
            g$nodes$color[i] <- "orange"
          }
          else {
            g$nodes$color[i] <- "#97C2FC"
          }
        }


        visNetworkProxy("plot2") %>%
          visUpdateNodes(nodes = g$nodes)


        # Muestro el siguiente paso
        output$nextStepConcepts <- renderTable({
          d <- rv$sublattice$lower_neighbours(rv$sublattice[1])$to_latex()
          dt <- parse_latex_concepts(d)
          dt[2:3]
        })

        # Actualizo lo de todos los conceptos
        output$allConcepts <- renderTable({
          parse_latex_concepts(rv$sublattice$to_latex())
        })

        nueva_fila <- parse_latex_concepts(rv$sublattice[1]$to_latex())
        nueva_fila <- data.frame(
          attributes = nueva_fila[2],
          objects = nueva_fila[3],
          stringsAsFactors = FALSE
        )

        history_actual <- history()
        history(rbind(history_actual, nueva_fila))

      # Si he llegado al final, se deja de pulsar
         if(rv$sublattice$size() == 2){
           hide("btnNextStep")

           nueva_fila <- parse_latex_concepts(rv$sublattice[rv$sublattice$size()]$to_latex())
           nueva_fila <- data.frame(
             attributes = nueva_fila[2],
             objects = nueva_fila[3],
             stringsAsFactors = FALSE
           )


           history_actual <- history()
           history(rbind(history_actual, nueva_fila))

         }
    })

    output$history <- renderTable({
      history()
    })

}
