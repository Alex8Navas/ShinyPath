library(shiny)
library(datasets) # Marco de datos WorldPhones
library(ggplot2)
library(reshape2)
library(dplyr)

# ----- Zona General -----

WorldPhones <- WorldPhones
colnames(WorldPhones) <- c("Noramérica","Europa","Asia","Sudamérica","Oceanía","África","Mesoamérica")
WorldPhones2 <- melt(WorldPhones, id.vars =  "Anno")
colnames(WorldPhones2) <- c("Anno", "Zone", "Calls")
WorldPhones2$Anno <- as.character(WorldPhones2$Anno)



# ----- Interfaz -----

interfaz <- fluidPage(    
  
  titlePanel(h3(strong("Llamadas por Zona"))),
  
  sidebarLayout(      
    
    sidebarPanel(
      
      selectInput("regionID", "Selecciona una Región:", 
                  choices = colnames(WorldPhones)),
      
      hr(),
      
      helpText("Datos de AT&T (1961). The World's Telephones.")
      
    ),
    
    mainPanel(
      
      plotOutput("phonePlotID")  
      
    )
    
  )
)


# ----- Servidor -----

 servidor <- function(input, output, session) {
  
  output$phonePlotID <- renderPlot({
    
    WorldPhones2 %>% filter(Zone == input$regionID) %>% ggplot(aes(x = Anno, y = Calls, fill = Anno)) +
      geom_bar(stat='identity') +
      labs(title = "Número de LLamadas por Zona",
           subtitle = "AT&T (1961) The World's Telephones.\nThe regions are: North America, Europe, Asia, South America, Oceania, Africa, Central America.\nThe years are: 1951, 1956, 1957, 1958, 1959, 1960, 1961.",
           x = "Año del Resgistro",
           y = "LLamadas",
           caption = "Alejandro Navas González") +
      geom_text(aes(label = Calls), vjust = 1.6, color = "black", size = 5) + 
      theme_light() + guides(fill = FALSE) + 
      theme(plot.title = element_text(size = 20, face = "bold",
                                      hjust = 0,
                                      margin = margin(t = 0, b = 0)),
            plot.caption = element_text(size = 14, hjust = 1, 
                                        margin = margin(t = 15),
                                        face = "italic"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(angle = 20, size = 12),
            axis.text.x = element_text(angle = 0, size = 12))

  })
}


# ---- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)
