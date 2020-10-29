library(shiny)
library(viridis)

# ----- Interfaz -----

interfaz <- fluidPage(
  
  sidebarLayout(
    
    position = "left",
    
    sidebarPanel(
      
      sliderInput("observatione", "Introduce el número de observaciones:",
                  min = 10, max = 1000, value = 500)
      
    ),
    
    mainPanel(
      
      plotOutput("hitokram")
      
    )
    
  )
  
)




# ----- Servidor -----

servidor <- function(input, output){
  
  output$hitokram <- renderPlot({
    
    hist(rnorm(input$observatione), col = viridis(10), main = "Histograma de una Normal", xlab = "")
    
  })
  
}



# ----- Lanzamiento de la App -----

shinyApp(ui = interfaz, server = servidor)
