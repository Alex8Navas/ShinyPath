library(shiny)
library(ggplot2)

# ----- Interfaz Gráfica -----

InterfazEx2 <- fluidPage(
  
  titlePanel(h1(strong("Widgets"), align = "center")),
  
  # Fila o estructura de maquetación: 
  fluidRow(
    
    # Hay 12 de ancho max. 
    column(3, h3(strong("Primer Segmento"), align = "center"),
           # Botón de acción: tiene un ID y luego un nombre a mostrar en pantalla. 
           actionButton("accion", "¡Acción!"),
           br(),
           br(),
           # En el caso de submit no es necesario un ID. 
           submitButton("GO", icon = icon("calendar")),
           br(),
           # Hay varios tipos de icono para el botón submit.
           submitButton("GO", icon = icon("refresh"))),
    
    column(3, h3(strong("Segundo Segmento"), align = "center"),
           checkboxInput("checkbox", "Option A", value = TRUE),
           checkboxInput("checkbox", "Option B", value = FALSE),
           checkboxInput("checkbox", "Option C", value = FALSE)),
    
    column(3, h3(strong("Tercer Segmento"), align = "center"),
           checkboxGroupInput("checkgroup", label = h4(strong("Checkgroup")), 
                              choices = list(
                                
                                "Option I" = 1,
                                "Option II" = 2,
                                "Option III" = 3,
                                "Option IV" = 4,
                                "Option V" = 5
                                
                              ),
                              # Selección por defecto (puedes especiicar más de uno):
                              selected = 1,
                              # Inline lo pone todo en la misma línea:
                              inline = FALSE)),
    
    column(3, h3(strong("Cuarto Segmento"), align = "center"),
           dateInput("fecha", h4(strong("Fecha")), 
                     value = "10/10/2020"))
    
  ), 
  
  fluidRow(
    
    column(3, h3(strong("Rango de Fechas"), align = "center"),
           dateRangeInput("fechas",
                          h4(strong("Rango de Fechas")),
                          # lenguaje del calendario: 
                          language = "es",
                          # Separador (por defecto es "to")
                          separator = "a",
                          # Formato: con guiones, con puntos o con shlash. 
                          format = "dd/mm/yyyy",
                          # Hay otras opciones como start o end. 
                          start = "2020/01/01",
                          end = "2024/12/30"
                          )),
    
    column(3, h3(strong("Ficheros"), align = "center"),
           fileInput("fichero", h4(strong("Selecciona un fichero:")),
                     # Seleccionar formatos que acepta la función. 
                     # accept = "especifica los formatos",
                     # Aceptar múltiples ficheros
                     multiple = TRUE,
                     # Cambiar la etiqueta del botón (por defecto Browse)
                     buttonLabel = "Indaga", 
                     # Mensaje tras la carga del fichero
                     placeholder = "Fichero Hallado, Hermano"
                     )),
    
    column(3, h3(strong("Help"), align = "center"),
           helpText("El cuadro de Ayuda no se considera como widget.",
                    "Pero permite manejar el texto de manera sencilla.")),
    
    column(3, h3(strong("Entrada Numérica"), align = "center"),
            numericInput(inputId = "numero",
                         h4(strong("Introduce un Número")),
                         min = 60,
                         max = 80,
                         step = 2,
                         value = 70
                          ))
    
  ),
  
  fluidRow(
    
    column(3, h3(strong("Radio"), align = "center"),
           radioButtons("radio", h4(strong("Botón de Radio")),
                        # No permite la selección múltiple. 
                        choices = list("Opción A" = 1,
                                       "Opción B" = 2,
                                       "Opción C" = 3,
                                       "Opción D" = 4,
                                       "Opción E" = 5),
                        selected = 1
                        )
           
           ),
    
    column(3, h3(strong("Selección"), align = "center"),
           selectInput("seleccion", h4(strong("Selecciona una opción")),
                       choices = list("Opción A" = 1,
                                        "Opción B" = 2,
                                        "Opción C" = 3,
                                        "Opción D" = 4,
                                        "Opción E" = 5),
                       selected = 1
                       )
           ),
    
    column(3, h3(strong("Sliders"), align = "center"),
           
           sliderInput("slider1", h4(strong("Slider I")),
                       min = 0,
                       max = 150,
                       value = 75
                       ),
           sliderInput("slider2", h4(strong("Slider II")),
                       min = 0,
                       max = 150,
                       value = c(25,75),
                       round = TRUE, # redondear decimales
                       step = 5
           )
           
           ),
    
    column(3, h3(strong("Entrada de Texto"), align = "center"),
           
           textInput("texto",h4(strong("Entrada de Texto")),
                     # mensaje dentro de la casilla de fondo: 
                     placeholder = "Introduzca su nombre:")
           # Para un valor por defecto de la casilla
           # value = "name"
           
           )
    
  ),
  
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