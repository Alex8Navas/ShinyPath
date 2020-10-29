library(shiny)
library(viridis)

# ----- Interfaz -----

interfaz <- fluidPage(
  
  titlePanel(h2(strong("Las Pestañas 2 (Tabsets)"), align = "center")),
  
  sidebarLayout(
    
    position = "left",
    
    sidebarPanel(
      
      # Selección del número de observaciones: 
      sliderInput("observatione", "Introduce el número de observaciones:",
                  min = 10, max = 10000, value = 1000)
      
    ),
    
    mainPanel(
      
      # Panel de Pestañas: 
      tabsetPanel(
        
        tabPanel("Normal", 
                 tabsetPanel(
                   tabPanel("Gráfico", plotOutput("normalgraphic")),
                   tabPanel("Resumen Estadístico", verbatimTextOutput("normalsumario")),
                   tabPanel("Tabla",tableOutput("normaldataframe"))
                 )),
        tabPanel("Uniforme", 
                 tabsetPanel(
                   tabPanel("Gráfico", plotOutput("unifgraphic")),
                   tabPanel("Resumen Estadístico", verbatimTextOutput("unifsumario")),
                   tabPanel("Tabla",tableOutput("unifdataframe"))
                 )
                 ),
    
        tabPanel("Exponencial", 
                 tabsetPanel(
                   tabPanel("Gráfico", plotOutput("expgraphic")),
                   tabPanel("Resumen Estadístico", verbatimTextOutput("expsumario")),
                   tabPanel("Tabla",tableOutput("expdataframe"))
                 )),
                 
        tabPanel("Logarítmica",
                 tabsetPanel(
                   tabPanel("Gráfico", plotOutput("loggraphic")),
                   tabPanel("Resumen Estadístico", verbatimTextOutput("logsumario")),
                   tabPanel("Tabla",tableOutput("logdataframe"))
                 )),
        tabPanel("Gamma",
                 tabsetPanel(
                   tabPanel("Gráfico", plotOutput("gammagraphic")),
                   tabPanel("Resumen Estadístico", verbatimTextOutput("gammasumario")),
                   tabPanel("Tabla",tableOutput("gammadataframe"))
                 ))
        
      ),
      
    )
    
  )
  
)




# ----- Servidor -----

servidor <- function(input, output){
  
  distributione <- reactive({
    
    # Marco de datos con las distribuciones a modo de variable reactiva: 
    
    data.frame(
      
           normal = rnorm(input$observatione),
           uniform = runif(input$observatione),
           exp = rexp(input$observatione),
           log = rlnorm(input$observatione),
           gamma = rgamma(input$observatione, shape = 1))
    
  })
  
  # Gráficas de cada una de las funciones: 
  
  output$normalgraphic <- renderPlot({
    
    hist(distributione()$normal, col = inferno(10), 
         main = "Histograma de una Normal", xlab = "",
         border = "white")
    
  })
  
  output$unifgraphic <- renderPlot({
    
    hist(distributione()$uniform, col = viridis(10),
         main = "Histograma de una Uniforme", xlab = "",
         border = "white")
    
  })
  
  output$expgraphic <- renderPlot({
    
    hist(distributione()$exp, col = magma(10),
         main = "Histograma de una Exponencial", xlab = "",
         border = "white")
    
  })
  
  output$loggraphic <- renderPlot({
    
    hist(distributione()$log, col = cividis(10), 
         main = "Histograma de una Logarítmica", xlab = "",
         border = "white")
    
  })
  
  output$gammagraphic <- renderPlot({
    
    hist(distributione()$gamma, col = inferno(10),
         main = "Histograma de una Gamma", xlab = "",
         border = "white")
    
  })
  
  # Resumen estadístico de cada una de las funciones: 
  
  output$normalsumario <-  renderPrint({
    
    summary(distributione()$normal)
    
  })
  
  output$unifsumario <-  renderPrint({
    
    summary(distributione()$uniform)
    
  })
  
  output$expsumario <-  renderPrint({
    
    summary(distributione()$exp)
    
  })
  
  output$logsumario <-  renderPrint({
    
    summary(distributione()$log)
    
  })
  
  output$gammasumario <-  renderPrint({
    
    summary(distributione()$gamma)
    
  })

  
  # Tablas de cada una de las funciones: 
  
  output$normaldataframe <- renderTable({
    
    distributione()$normal
    
  })
  
  output$unifdataframe <- renderTable({
    
    distributione()$uniform
    
  })
  
  output$expdataframe <- renderTable({
    
    distributione()$exp
    
  })
  
  output$logdataframe <- renderTable({
    
    distributione()$log
    
  })
  
  output$gammadataframe <- renderTable({
    
    distributione()$gamma
    
  })
  

  
}



# ----- Lanzamiento de la App -----

shinyApp(ui = interfaz, server = servidor)