# Interfaces Dinámicas 

library(shiny)



# ----- Interfaz -----

interfaz <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput(inputId = "Nummer", label = "Selecciona un Número:", 
                   min = 0, max = 250, value = 75),
      
      textInput(inputId = "Text", "Introduce el Texto que quieras Descomponer:"),

      
      uiOutput(outputId = "UIDinamo"),
      
      uiOutput(outputId = "UIDinamo2")
      
      
    ),
    
    
    mainPanel()
    
  )
)

# ----- Servidor -----

servidor <- function(input, output){
  
  output$UIDinamo <- renderUI({
    
    selectInput(inputId = "Kombo1", "Selecciona una de las siguientes opciones:",
                choices = c(1:input$Nummer), selected = 2)
    
  })
  
  output$UIDinamo2 <- renderUI({
    
    words <- unlist(strsplit(input$Text, " "))
    
    selectInput(inputId = "Kombo2", "Observa cada una de las palabras:",
                choices = words, selected = words[1])
    
  })
  
}

# ----- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)