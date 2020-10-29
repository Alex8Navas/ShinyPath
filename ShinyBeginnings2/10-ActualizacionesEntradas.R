# Actualizaciones de Valores de Entrada (Inputs)

library(shiny)

# ----- Interfaz -----

interfaz <- fluidPage(
  
  numericInput(inputId = "Nummer", label = "Selecciona un rango:",
               value = 5, min = 0, max = 100),
  
  selectInput(inputId = "kombinieren", 
              label = "Selecciona un Número:",
              choices = c(1:10), selected = 8),
  
  actionButton(inputId = "Knospe", label = "Procesar", 
               icon = icon("globe",lib = "glyphicon"))
  
  # Para ver todos los iconos: https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
  
)

# ----- Servidor -----

# Se añade el parámetro session para poder utilizar en el servidor la información de la sesión de la app. 
servidor <- function(input, output, session){
  
  observeEvent(
    input$Knospe,{
      
      # Actualizar el cuadro de selección. 
      updateSelectInput(session = session,
                        inputId = "kombinieren",
                        choices = c(1:input$Nummer),
                        selected = max(c(1:input$Nummer)))
      
      # Actualizar el botón de acción. 
      updateActionButton(session = session,
                         inputId = "Knospe",
                         label = "Guardar", # Cambiar la etiqueta del botón.
                         icon = icon("knight", lib = "glyphicon")) # Cambiar el emoticono del botón. 
      
      # Más opciones de actualización: 
      # updateCheckboxGroupInput()
      # updateCheckboxInput()
      # updateDateInput()
      # updateDateRangeInput()
      # updateNavbarPage()
      # updateNumericInput()
      # updateRadioButtons()
      # updateSliderInput()
      # updateTextAreaInput()
      # updateTextInput()
      
      
    }
  )
  
}

# ----- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)