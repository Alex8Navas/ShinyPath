library(shiny)


# ----- Interfaz -----

vars <- setdiff(names(iris), "Species")

interfaz <- pageWithSidebar(
    
    headerPanel("Agrupamientos K-Means con el Marco Iris"),
    sidebarPanel(
        
        selectInput("xcol", "Variable X", vars),
        
        selectInput("ycol", "Variable Y", vars, selected = vars[[2]]),
        
        numericInput("clusters", "Número de Grupos", 5, min = 1, max = 10)
        
    ),
    
    mainPanel(
        
        plotOutput('plot1')
        
    )
)

# ----- Servidor -----

servidor <- function(input, output, session) {
    selectedData <- reactive({
        
        iris[, c(input$xcol, input$ycol)]
        
    })
    
    clusters <- reactive({
        
        kmeans(selectedData(), input$clusters)
        
    })
    
    output$plot1 <- renderPlot({
        
        palette(c("#800000", "#000080", "#800080", "#F0CB15",
                  "#BD0953", "#91BD09", "#0B548A", "#E36A00",
                  "#1F8786"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3, main = "K-Means Clustering", 
             sub = "Marco de datos Iris")
        
        points(clusters()$centers, pch = 8, cex = 2, lwd = 2)
        
    })
    
}

# ----- Lanzamiento -----
shinyApp(ui = interfaz, server = servidor)
