library(shiny)
library(ggplot2)

# runExample("01_Hello")


# ----- Interfaz de Usuario -----


interfazEx1 <- fluidPage(
  
  # Título
  titlePanel(h2(strong("Las Minas de Moria"))),
  
  # Paneles.
  sidebarLayout(
    
    # Panel de la izda.
    sidebarPanel(
      
      # Título del panel.
      h4(strong("Pregunta a los Abismos")),
    
      # Ancho del panel. 
      width = 2,
      
      # Botón de deslizamiento. 
      sliderInput(
        
        inputId = "Valores",
        label = "Valor a Seleccionar",
        min = 1,
        max = 80,
        value = 20
      )
      
    ), 
    
    
    # Panel principal. 
    mainPanel(
      
      # Título del panel:
      h4(strong("La respuesta de Moria")),
      
      # Gráfica a mostrar: 
      plotOutput(outputId = "graphic")
      
    ) 
    
  )
  
)



# ----- Servidor -----


servidorEx1 <- function(input, output){
  
  output$graphic <- renderPlot(
    {
      
      # browser()  para ver la evolución de la App con la consola. 
      valores <- seq(min(faithful$waiting), 
                     max(faithful$waiting), 
                     length.out = input$Valores + 1)
      
      ggplot(faithful, aes(waiting)) + geom_histogram(fill = viridis(input$Valores), color = "white", breaks = valores) +
        labs(title = "**Erupciones en Moria**",
             x = "**Tiempo de espera entre Erupciones (min)**",
             y = " ",
             caption = "**Alejandro Navas González**") +
        theme_light() +
        guides(fill=FALSE, col = FALSE) +
        theme(plot.title = ggtext::element_markdown(hjust = 0.5, size = 16),
              axis.title.x = ggtext::element_markdown(size = 14),
              plot.caption = ggtext::element_markdown(size = 12))
      
    }
  )
  
}



# ----- Lanzamiento -----

shinyApp(ui = interfazEx1, server = servidorEx1)


