library(shinydashboard)
library(bslib)
library(readxl)
source("ui.R")
devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")

server <- function(input, output) {

  dataset <- reactive({ # Funcion obtener dataset

    # Compruebo si ha seleccionado alguno de los que hay ya cargados
    if (is.null(input$dataset2)){
      return(get(input$dataset1, "package:datasets"))
    }
    else{ # o si ha introducido uno propio
      req(input$dataset2)
      ext <- tools::file_ext(input$dataset2$name)

      if (ext == "csv") { # csv por ; o ,
        dataset2 <- read.csv(input$dataset2$datapath, sep=";")

        if(ncol(dataset2) == 1) {
          dataset2 <- read.csv(input$dataset2$datapath)
        }
        row.names(dataset2) <- dataset2[[1]]
        dataset2 <- dataset2[-1]
      }
      else if (ext %in% c("xls", "xlsx")) {
        dataset2 <- read_excel(input$dataset2$datapath)
      }
      else if (ext == "txt" || ext == "tsv") {
        dataset2 <- read.delim(input$dataset2$datapath)
      }

      return(dataset2)
    }
  }) # Fin


  plotAux <- reactive({
    # Creo mi formal context
    fc <- FormalContext$new(dataset())
    print(fc)

    # Conceptos
    fc$find_concepts()
    fc$find_implications()

    return(fc$concepts$plot())
  })


  output$table <- DT::renderDataTable({
    DT::datatable(
      dataset(),
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        paging = TRUE
      )
    )
  })

  output$graphFormalConcept <- renderPlot({
    plotAux()
  }, height = 520)

}
