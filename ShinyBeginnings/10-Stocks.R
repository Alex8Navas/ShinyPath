library(shiny)
library(quantmod)
source("data/helpers2.R")

# ----- Interfaz -----
interfazStocks <- fluidPage(
  
  
  titlePanel(h1(strong("Yahoo Finance. Estudio de Cotización en Bolsa"))),
  
  sidebarLayout(
    sidebarPanel(
      
      # ----- Cuadro de Ayuda -----
      helpText("Selecciona un índice de cotización. 
               La información se recogerá de Yahoo Finance."), # Google dejó de proveer datos en marzo de 2018.
      
      # ----- Siglas del valor de Cotización -----
      textInput("siglas", "Siglas", "GOOG"),
      
      # ----- Rango de Fechas-----
      dateRangeInput("fechas", 
                     "Rango de fechas",
                     format = "dd/mm/yyyy", 
                     separator = "hasta",
                     start = "2020-01-01", 
                     end = as.character(Sys.Date())),
      
      # ---- CheckBox para pasar a Escala Logarítmica ----
      checkboxInput("log", "Eje Y (Escala Logarítmica)", 
                    value = FALSE),
      
      # ---- CheckBox para ajustar la inflación ----
      checkboxInput("ajuste", 
                    "Ajusta los precios a la inflación", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# ----- Servidor ----

serverStocks <- function(input, output) {
  
  stockValues <- reactive({getSymbols(input$siglas, src = "yahoo",
                              from = input$fechas[1],
                              to = input$fechas[2],
                              auto.assign = FALSE)
  })
  
  stockValuesAdjusted <- reactive({
    
    if(!input$ajuste){
      
      return(stockValues())
      
    } else{
      
      adjust(stockValues())
      
    }
    
  })
  
  output$plot <- renderPlot({
    
    chartSeries(stockValuesAdjusted(), name = "Cotización del Valor", 
                theme = chartTheme("black"),
                type = "auto", log.scale = input$log,
                clev = 1) 
  # clev: brillo en la gráfica (0 es lo más oscuro)
  # type = c("auto", "candlesticks", "matchsticks", "bars","line")
  # TA: llamadas por separado a las gráficas. 
  })
  
}

# ----- Lanzamiento -----
shinyApp(ui = interfazStocks, server = serverStocks)
