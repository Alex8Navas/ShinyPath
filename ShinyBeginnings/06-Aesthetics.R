library(shiny)
library(ggplot2)

# Modificaciones estéticas para las apps de Shiny: texto e imágenes. 


# ----- Interfaz Gráfica -----

InterfazEx2 <- fluidPage(
  
  titlePanel(h1(strong("Hey"), align = "center")),
  
  sidebarLayout(
    
    position = "right",
    
    sidebarPanel(
      
      h4(strong()),
      width = 5,
      
    ),
    
    
    mainPanel(
      
      # strong(): para negrita. 
      h4(strong("New App")),
      # p(): nuevo párrafo. 
      p("Esta aplicación sirve para...", style = "font-family : 'times'; font-si16pt", ),
      # em() para cursiva. 
      p(em("Alejandro Navas González", style = "color: steelblue;)"), align = "center"),
      # img(): para imagen. Recuerda modificar los permisos en la carpeta www. 
      img(src= "zerg.jpg", width = 400, height = 250)
    )
    
    
  )
  
)


# ----- Servidor -----


ServidorEx2 <- function(input, output){
  
}


# ----- Lanzamiento -----


shinyApp(ui = InterfazEx2, server = ServidorEx2)
  
