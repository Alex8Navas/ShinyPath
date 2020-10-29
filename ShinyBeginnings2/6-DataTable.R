# DataTable

library(shiny)
library(DT)

# ----- Interfaz ----- 

interfaz <- basicPage(
  
  h4(strong("Marco de Datos sobre Modelos de Coche")),
  
  dataTableOutput("mytab")
)

# ----- Servidor -----

servidor <- function(input, output){
  
  output$mytab <- renderDataTable(
    
    mtcars
    
  )
}

# ----- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)