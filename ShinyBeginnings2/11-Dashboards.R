# Dashboards: su uso permite una apariencia más profesional. 

library(shiny)
library(shinydashboard)
library(viridis)
library(ggplot2)
library(RColorBrewer)



# ----- Interfaz -----

interfaz <- dashboardPage(
  
  dashboardHeader(title = "Moria", titleWidth = 230, disable = FALSE),
  
  dashboardSidebar(width = 230, disable = FALSE, collapsed = FALSE,
                   
                   # Se introducen elementos de Shiny.
                   sliderInput(inputId = "slidid",
                               label = "Número de Barras:",
                               min = 1,
                               max = 100,
                               value = 30),
                   hr(), # Fina barra horizontal entre los sliders. 
                   sliderInput(inputId = "rangeid",
                               label = "Número de sujetos:",
                               min = 1,
                               max = dim(faithful)[1],
                               value = c(1,dim(faithful)[1]))
                   
                   ), 
  # Con collapse se permite que al iniciar la app aparezca colapsada y se pueda desplegar pulsando un botón. 
  
  dashboardBody(
    
    
    fluidRow(
      
      # ---- Primera Fila: Gráficos -----
      # ?box
      box(title = h3(strong("Inputs"), align = "center"),
          status = "danger", # Pinta una línea en el borde superior de la caja (hay cinco colores)
          background = "navy", # Colorea el fondo. No permite cualquier color, sino los definidos para la función, que son de bootstrap
          width = 4, # Para que todas las cajas quepan en una misma línea (12). Si no se van situando de modo automático abajo. 
          # height = 20 # Ajustas las dimensiones de la caja. Es recomendable para maquetar. 
          
          # Se introducen elementos de Shiny
          plotOutput(outputId = "scatterid")
          
          ),
    
      box(title = h3(strong("Outputs"), align = "center"),
          status = "success",
          # background = "green",
          solidHeader = TRUE, # Deja sin colorear el background, pero sí colorea el header y los bordes.
          width = 4,
          
          plotOutput(outputId = "histogramid")
          
          ),
      box(title = h3(strong("Anderen"), align = "center"),
          status = "warning",
          # background = "teal",
          solidHeader = FALSE, 
          width = 4,
          
          helpText("Ésta es una aplicación para el aprendizaje de Dashboards. Mira el código
                   si estás interesado en conocer su funcionamiento. Muchas gracias por la visita.")
      )
      
    ),
    
    # ----- Segunda Fila: Cajas de Valores -----
    fluidRow(
      
      valueBoxOutput(outputId = "sujetosid", width = 4),
      
      valueBoxOutput(outputId =  "maxwait", width = 4),
      
      valueBoxOutput(outputId = "maxer", width = 4)
      
      
    )
    
  )
  
)


# ----- Servidor -----

servidor <- function(input, output){
  
  output$histogramid <- renderPlot({
    
    wait <- faithful$waiting
    valores <- seq(min(wait), 
                   max(wait), 
                   length.out = input$slidid + 1)
    
    ggplot(faithful, aes(x = waiting)) + geom_histogram(fill = inferno(input$slidid),
                                                         color = "white", breaks = valores) +
      labs(title = "**Erupciones en Moria**",
           x = "**Tiempo de espera entre Erupciones (min)**",
           y = " ",
           caption = "**Alejandro Navas González**") +
      theme_light() +
      guides(fill=FALSE, col = FALSE) +
      theme(plot.title = ggtext::element_markdown(hjust = 0.5, size = 16),
            axis.title.x = ggtext::element_markdown(size = 14),
            plot.caption = ggtext::element_markdown(size = 12))
    
  })
  
  
  output$scatterid <- renderPlot({
    
    palette36 <- colorRampPalette(palette("Polychrome 36"))(input$rangeid[2])
    subfaith <- faithful[c(input$rangeid[1]:input$rangeid[2]), ]
    ggplot(subfaith, aes(x = eruptions, y = waiting)) + geom_point(aes(col = palette36), size = 2) +
      geom_smooth(col = "purple", fill = "#C0E6EC") + 
      labs(title = "**Erupciones en Moria**",
           x = "**Número de Erupciones**",
           y = "**Tiempo de espera entre Erupciones (min)**",
           caption = "**Alejandro Navas González**") +
      theme_light() +
      guides(fill=FALSE, col = FALSE) +
      theme(plot.title = ggtext::element_markdown(hjust = 0.5, size = 16),
            axis.title.x = ggtext::element_markdown(size = 14),
            axis.title.y = ggtext::element_markdown(size = 14),
            plot.caption = ggtext::element_markdown(size = 12))
    
  })
  
  # ----- Cajas de Valores -----
  
  output$sujetosid <- renderValueBox(
     
    
    valueBox(value = dim(faithful)[1],
             subtitle = "Volcanes", 
             icon = icon("cd", lib = "glyphicon"),
             color = "purple")
    
  )
  
  output$maxwait <- renderValueBox(
    
    
    valueBox(value = max(faithful$waiting),
             subtitle = "Tiempo de Espera Máximo", 
             icon = icon("grain", lib = "glyphicon"),
             color = "olive")
    
  )
  
  output$maxer <- renderValueBox(
    
    
    valueBox(value = max(faithful$eruptions),
             subtitle = "Número máximo de erupciones", 
             icon = icon("fire", lib = "glyphicon"),
             color = "orange")
    
  )

  
}

# ----- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)