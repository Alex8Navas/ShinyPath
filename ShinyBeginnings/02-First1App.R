library(shiny)
library(viridis)
# Guardar en formato UTF-8

# ----- Preprocesado de los Datos -----

mpgData <- mtcars
# Especificar las etiquetas de la variable am
mpgData$am <- factor(mpgData$am, labels = c("Automático", "Manual"))


# ----- Interfaz Gráfica -----
interfaz <- pageWithSidebar(
  
  # Título de la aplicación
  headerPanel = "Millas por Galón",
  
  # Sidebar Panel
  sidebarPanel(
    
    # Selector de la variable:
    selectInput("varSelected", "Variable a Seleccionar:",
                choices = c ("Cilindros" = "cyl",
                             "Transmisión" = "am",
                             "Motores" = "gear")),
  
  # Checkbox para valores atípicos:
  checkboxInput("outliers", "Mostrar los Valores Atípicos", TRUE)),
  
  # Main Panel:
  mainPanel(
    
    # Salida del texto de la variable (nombre del output del servidor).
    h3(textOutput("muestra")),
    
    # Gráfico de salida (nombre del output del servidor).
    plotOutput("mpgGraphic")
  )
  
)

# ----- Servidor -----
servidor <-  function(input, output){
  
  # Variable reactiva que guarde la selección de variable realizada. 
  formTexto <- reactive(
    {
      paste("mpg ~", input$varSelected)
    }
  )
  
  # Muestra el valor de la variable reactiva. 
  output$muestra <- renderText({
    
    formTexto() # Se pone paréntesis para indicar que la variable es reactiva. 
    
  })
  
  # Gáfico a mostrar. Excluir valores atípicos según checkbox. 
  
  output$mpgGraphic <- renderPlot(
    
    boxplot(as.formula(formTexto()),
            data = mpgData,
            outline = input$outliers,
            col = viridis(3),
            pch = 21)
    
  )
  
}

# Lanzamiento de la aplicación. 
shinyApp(ui = interfaz, server = servidor)

