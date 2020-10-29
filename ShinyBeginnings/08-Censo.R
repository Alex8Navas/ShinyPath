library(shiny)
library(tidyverse)
library(maps)
library(mapproj)
library(ggplot2)
source("data/helpers.R")


states <- readRDS("data/counties.rds")
head(states)
colnames(states) <- c("name", "total.pop", "Human", "Elf", "Orc", "Wizard")
head(states)

# ----- Interfaz GrÃ¡fica -----

InterfazEx2 <- fluidPage(
  
  titlePanel(h1(strong("Censo de la Población de Papúa Nueva Guinea"), align = "center")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ----- Cuadro de Ayuda -----
      helpText("Se crea un mapa demográfico del censo de 
               la población de Papúa Nueva Guinea de 2015"),
      
      
      # ----- Selección de Raza -----
      selectInput("variableraza", h4(strong("Elige la variable a analizar")),
                  choices = list("Porcentaje de Humanos",
                                 "Porcentaje de Elfos",
                                 "Porcentaje de Enanos",
                                 "Porcentaje de Magos"),
                  selected = "Porcentaje de Humanos"
                  
                  ),
      
      # ----- Slider -----
      sliderInput("sliderPercent", label = h4(strong("Porcentaje hallado:")),
                  min = 0,
                  max = 100,
                  value = c(0,100)
                  ),
      
      
      
      h4(strong()),
      
    ),
    
    
    mainPanel(
      
      h4(strong()),
      
      # Alineación de una imagen en shiny: 
      HTML('<center><img src="zerg.jpg" width="400" height = 250,></center>'),
      
      # ----- Output de Salida de Texto y Mapa-----
      textOutput("textvar"),
      textOutput("rangovar"),
      plotOutput("mapaRazas")
      
    )
    
    
  )
  
)


# ----- Servidor -----


ServidorEx2 <- function(input, output){
  
  # Renderizar la variable textvar:
  output$textvar <- renderText(
    
    paste("Se ha seleccionado: ", input$variableraza))
  
  # Renderizar la variable texto que nos habla del rango: 
  output$rangovar <- renderText(
    
    paste("El porcentaje hallado pertenece al intervalo: ", input$sliderPercent[1], "-", input$sliderPercent[2]) 
    # Si pones el rango sin especeficar [x] se repetirá el paste con [1] y con [2]. 
    
    )
  
  # Renderizar el mapa:
  output$mapaRazas <- renderPlot({
    
    data <- switch(input$variableraza,
                   "Porcentaje de Humanos" = states$Human,
                   "Porcentaje de Elfos" = states$Elf,
                   "Porcentaje de Enanos" = states$Orc,
                   "Porcentaje de Magos" = states$Wizard
                   
                   )
    colores<- switch(input$variableraza,
                   "Porcentaje de Humanos" = "steelblue",
                   "Porcentaje de Elfos" = "darkgreen",
                   "Porcentaje de Enanos" = "darkred",
                   "Porcentaje de Magos" = "purple4"
                   
                  )
    leyenda<- switch(input$variableraza,
                     "Porcentaje de Humanos" = "Porcentaje de Humanos",
                     "Porcentaje de Elfos" = "Porcentaje de Elfos",
                     "Porcentaje de Enanos" = "Porcentaje de Orcos",
                     "Porcentaje de Magos" = "Porcentaje de Magos"
                     
                     )
    
    min = input$sliderPercent[1]
    
    max = input$sliderPercent[2]
    
    percent_map(var = data,
                color = colores,
                legend.title = leyenda, 
                min = min, 
                max = max)
    
  })
  
  
}



# ----- Lanzamiento -----


shinyApp(ui = InterfazEx2, server = ServidorEx2)