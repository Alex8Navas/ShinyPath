library(shiny)
library(ggplot2)

# ----- Interfaz Gráfica -----

InterfazEx2 <- fluidPage(
  
  titlePanel(" "),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4(strong()),
      
    ),
    
    
    mainPanel(
      
      h4(strong()),
      
    )
    
    
  )
  
)


# ----- Servidor -----


ServidorEx2 <- function(input, output){
  
}


# ----- Lanzamiento -----


shinyApp(ui = InterfazEx2, server = ServidorEx2)