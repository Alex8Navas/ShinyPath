library(shiny)


# ----- Interfaz -----

interfaz <- fluidPage(
  
  
  titlePanel(h2(strong("Menú de Navegación"), align = "center")),
  
  navbarPage("La barra de navegación de la nave",
  
    tabPanel("First Component", h3(strong("Hey, you. Yes, you...."))),
    tabPanel("Second Component", h3(strong("Hey, you. No, you...."))),
    tabPanel("Third Component", h3(strong("Hey, you..."))),
    navbarMenu("Sala de Máquinas",
      
      tabPanel("Fourth Component", h3(strong("Haito"))),
      tabPanel("Fifth Component", h3(strong("Larga vida a Marco Aurelio"))),
      tabPanel("Sixth Component", h3(strong("Dévora, ¿pero qué has hecho?")))
      
      
    )
  )
)

# ----- Servidor ----

servidor <- function(input, output){
  
  
}

# ----- Lanzamiento de la App -----

shinyApp(ui = interfaz, server = servidor)