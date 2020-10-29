# DataTable 2

library(shiny)
library(ggplot2)
library(DT)

# ----- Interfaz ----- 

interfaz <- fluidPage(
  
  title = h4(strong("Marcos de Datos")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(
        
        # Es una condición de JS. Por eso se pone la triple comilla, que mide que haya igualdad en valor y en modo (float32 por ejemplo). 
        condition = "input.dataset === 'diamonds'",
        checkboxGroupInput("variableSS", "¿Qué columnas del marco de datos quieres mostrar?",
                           choices = names(diamonds), 
                           selected = names(diamonds)
                           )
      ),
      
      conditionalPanel(
        
        condition = "input.dataset === 'mtcars'",
        helpText("Pulsa sobre el encabezado de la columna para ordenarla.")
        
        
      ),
      
      conditionalPanel(
        
        condition = "input.dataset === 'iris'",
        helpText("Muestra cinco sujetos por defecto.")
        
        
      ),
      
      conditionalPanel(
        
        condition = "input.dataset === 'faithful'",
        verbatimTextOutput("selecteD")
        
        
      )
      
    ), 
  
  mainPanel(
    
    tabsetPanel(
      
      id = "dataset",
      
      tabPanel("diamonds", dataTableOutput("tablaeins")),
      tabPanel("mtcars", dataTableOutput("tablazwei")),
      tabPanel("iris", dataTableOutput("tabladrei")),
      tabPanel("faithful", dataTableOutput("tablavier"))
      
    )
    
  )
  )
)

# ----- Servidor -----

servidor <- function(input, output){
  
  output$tablaeins <- renderDataTable({
   
    datatable(diamonds[, input$variableSS, drop = FALSE]) # Con drop = FALSE se evita tener una tabla vacía.
    
  })
  
  output$tablazwei <- renderDataTable({
   
    datatable(mtcars, options = list(orderClasses = TRUE)) # Con orderClasses = TRUE se permite ordenar las clases.
    
  })
  
  output$tabladrei <- renderDataTable({
    
    datatable(iris, options = list(lengthMenu = c(15, 30, 45), pageLength = 15))
    # Con lengthMenu se permite editar el número de sujetos de la tabla. Ponemos tres posibilidades y por defecto 15 con pageLength.
    
  })
  
  # En lugar de usar la función datatable se puede trabajar tras de la llave: 
  output$tablavier <- renderDataTable({
    
    faithful
  },
  
  escape = FALSE, # sirve para evitar caracteres especiales. 
  selection = list(mode = "multiple", selected = c(6,8,9)), 
  # single: sólo permite seleccionar una fila./multiple: para elegir varios sujetos. 
  # selected: sujetos seleccionados de inicio. 
  options = list(searching = FALSE, # Con esto quitas el buscador de la tabla.
                 displayStart = 1,
                 lengthMenu = c(10,20,30,40,50,60,70,80),
                 pageLength = 20, # Filas mostradas por defecto. 
                 dom = "tiplf" 
                 # Dom sirve para configurar el modo de visualización. 
                 # Con t sólo deja visible la tabla. 
                 # Con i muestras la info de la tabla en la parte inferior. 
                 # Con p muestra además la paginación.
                 # Con l se permite el input de control de entrada de filas por tabla.
                 # Con f se da a entrada al filtro.
                 # Todas las letras anteriores se pueden combinar en el número y orden que se quiera para la configuración.
                 # scrollY = "500px" # Permite crear una barra de scroll. 
                 )
  
  )
  
  output$selecteD <- renderText({
    
    paste("Sujeto seleccionado: ", input$tablavier_rows_selected, "\n",
          "- Erupciones: ", faithful$eruptions[input$tablavier_rows_selected], "\n",
          "- Tiempo de Espera: ", faithful$waiting[input$tablavier_rows_selected], "\n",
          collapse = "") # Mostrar los sujetos seleccionados.
    # collapse es como el sep de Python. 
  })
  
}

# ----- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)