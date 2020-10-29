library(shiny)
library(viridis)

# ----- Interfaz -----

interfaz <- fluidPage(
  
  titlePanel(h2(strong("Las Pestañas (Tabsets)"), align = "center")),
  
  sidebarLayout(
    
    position = "left",
    
    sidebarPanel(
      
      # Elección de la distribución: 
      radioButtons("distributione", "Introduce el tipo de distribución:",
                   choices = c("Normal" = "normal",
                               "Uniforme" = "uniform",
                               "Exponencial" = "exp",
                               "Logarítmica" = "log",
                               "Gamma" = "gamma"
                   )),
      # Selección del número de observaciones: 
      sliderInput("observatione", "Introduce el número de observaciones:",
                  min = 10, max = 1000, value = 500)
      
    ),
    
    mainPanel(
      
      # Panel de pestañas: 
      tabsetPanel(
        
        # Primera pestaña: 
        tabPanel("Gráfico",
                 
                 plotOutput("hitokram")
          
        ),
        
        # Segunda pestaña: 
        tabPanel("Resumen Estadístico", verbatimTextOutput("zumario")),
        
        
        # Tercera pestaña: 
        tabPanel("Tabla de Valores",
                 
                 tableOutput("tawla")
                 
        )
        
        
      )
      
    )
    
  )
  
)




# ----- Servidor -----

servidor <- function(input, output){
  
  distributione <- reactive({
    
    switch(input$distributione,
           normal = rnorm(input$observatione),
           uniform = runif(input$observatione),
           exp = rexp(input$observatione),
           log = rlnorm(input$observatione),
           gamma = rgamma(input$observatione, shape = 1))
    
  })
  
  output$hitokram <- renderPlot({
    
    hist(distributione(), col = inferno(10), main = "Histograma de una Normal", xlab = "")
    
  })
  
  output$zumario <-  renderText({
    
    summary(distributione())
    
  })
  
  output$tawla <- renderTable({
    
    distributione()
    
  })
  
}



# ----- Lanzamiento de la App -----

shinyApp(ui = interfaz, server = servidor)