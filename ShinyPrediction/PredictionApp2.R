# Aplicación para la predicción de Números

library(shiny)
library(shinydashboard)
library(png)
library(caret)
library(e1071)
library(randomForest)
library(neuralnet)

# ----- Parte General -----

# Se define una variable lado (Der Seite) para definir los límites de los gráficos.
# Se mide en número de píxeles, esto es, se tienen 25 píxeles. 

seite <- 28 
# Hay que cambiar el valor del lado a 28 porque son las dimensiones del marco de entrenamiento dado para los modelos. 

# Se define el grosor del lápiz para dibujar en el gráfico. 

spessore <- 5

# Se definen las dimensiones de la matriz.
# Son 10 puntos por cada píxel (cuadradito de la matriz) y 1 espacio para el margen. 
ancho <- seite  * 10 + 1
alto <- seite * 10 + 1

# Se carga el fichero con los nombres de las columnas:
namesF <- as.vector(unlist(read.csv("data/nombres.csv")))

# Se carga el preprocesado de train. Se usa la función nearZeroVar 
# para eliminar las varaibles de varianza cercana a cero. 
# En este archivo nvz se tienen las variables a eliminar. 
nvz <- as.vector(unlist(read.csv("data/nvz.columnas.csv")))

# Se cargan los modelos de la carpeta data.
load(file = "data/1-NaiveBayes.rda") # modelo.NaiveBayes
load(file = "data/2-PCA.rda") # train.preproc
load(file = "data/3-RandomForest.rda") # modelo.RandomForest
load(file = "data/4-RedesNeuronales.rda") # modelo.RedesNeuronales
load(file = "data/5-SVM.rda") # modelo.SVM

# Precisiones de los modelos.
AccNaiveB <- 0.5347
AccRandomF <- 0.9446
AccSupportVM <- 0.9721
AccArtificialNN <- 0.8976


# ----- Funciones de la Parte General -----


# Actualización de la imagen tras dibujar sobre el cuadrante. 
# Función para procesar los valores x e y que se dibujan en el plot. 
procesaValores <- function(y, x, mat){
  
  # Interpolación de los valores x e y. 
  y <- interpolaV(y, repeticiones = 3)
  x <- interpolaV(x, repeticiones = 3)
  
  # Es necesario invertir el sentido de las filas porque la imagen dibujada se halla invertida.
  # Es decir, al llevar lo que se dibuja a la matriz el cero en la matriz empieza por arriba,
  # no por abajo como en el plot.
  filas <- abs(round(as.vector(y), 0)-seite*10)
  columnas <- round(as.vector(x), 0)
  
  # Se leen para constituir la matriz. 
  for (i in 1:length(filas)){
    
    # Se busca que no haya NAs, algo que sucederá si escribimos varias líneas rápidamente. 
    if(!is.na(filas[i])){
      
      # Se mira que los valores se encuentren dentro del marco. 
      if (filas[i]-spessore > 0 & filas[i] + spessore < seite * 10 + 1
          & columnas[i]-spessore > 0 & columnas[i] + spessore < seite * 10 + 1){
        
        mat[seq(filas[i] - spessore, filas[i]+ spessore),
            seq(columnas[i] - spessore, columnas[i]+ spessore)] <- 0
        
      } else{
        
        # En caso de que lo anterior no se cumpla, salta de línea. 
        next()
        
      }
      
    }
    
  }
  
  return(mat)
  
}

# Función para interpolar los valores de los píxeles. 
# Se pone el parámetro repeticiones porque con una interpolación no suele ser suficiente. 
interpolaV <- function(vector, repeticiones = 1){
  
  # Se define un iterador. 
  itera <- 1
  
  while(itera <= repeticiones){
    
    neuesVector <- vector[1]
    precedente <- vector[1]
    
    for(i in 2:length(vector)){
      
      # Si hay NAs en el valor del vector en la posición i no hay interpolación. 
      if(is.na(vector[i])){
        
        neuesVector <- c(neuesVector, vector[i])
        precedente <- vector[i]
        
      } else {
        
        # Si hay NAs en el valor del vector precedente (i-1) no hay interpolación.
        if(is.na(precedente)){
          
          neuesVector <- c(neuesVector, vector[i])
          precedente <- vector[i]
          
        } else {
          
          # Interpolación: y0 + (y-y0)/2 
          neuesVal <- precedente + (vector[i] - precedente)/2 
          neuesVector <- c(neuesVector, neuesVal, vector[i])
          precedente <- vector[i]
          
        }
        
      }
      
    }
    
    itera = itera + 1
    vector <- neuesVector
    
  }
  
  return(vector)
  
}


# Función para transformar la matriz larga a una matriz que puedan procesar los modelos.
# Escala de 0 a 256 para cada píxel donde cero es nada coloreado y 256 es plenamente coloreado
# Es decir, cuando dibujes, los píxeles tomarán un valor en ese rango en función de si pasas o no por ellos. 
matrixConversion <- function(matr){
  
  imagen <- c()
  
  for(fila in 1:seite){
    for(columna in 1:seite){
      
      # El x10 es porque es el número de píxeles que se ha definido arriba (ancho y largo). 
      minRow <- (fila - 1) * 10 + 1
      maxRow <- fila * 10
      minCol <- (columna - 1) * 10 + 1
      maxCol <- columna * 10
      
      # Se define el valor del píxel en la escala de la matriz introducida.
      mrpix <- sum(matr[seq(minRow, maxRow), seq(minCol, maxCol)])
      
      # Al dibujar queremos la inversa, es decir, si se ha dibujado que muestre un negro.
      # Esto es, si son unos que muestre ceros, y si son ceros que el pixel muestre unos.
      # Por ello se resta cien (10x10) que es el caso máximo (todo unos en la matriz 10x10)
      # Y se pone en valor absoluto para que sirva también para el caso opuesto (todo ceros).
      mrpix <- abs(mrpix - 100)
      
      # Además, la escala con la que se trabaja es de 0 a 256, luego es necesaria una conversión. 
      # Se quiere además que lo que se dé como parámetro al modelo sea un número entero, luego
      # se utiliza round() con cero decimales. 
      mrpix <- round(x = (mrpix*256)/100, digits = 0)
      
      # Se añade al vector imagen cada píxel creado (en este caso, son 784 (consulta el marco de datos nombres.csv)).
      imagen <- c(imagen, mrpix)
      
    }
  }
  
  imagen <- as.data.frame(t(imagen))
  names(imagen) <- namesF
  return(imagen)
  
}

# ----- Interfaz -----

interfaz <- dashboardPage(skin = "purple",
                          
                          dashboardHeader(
                            
                            title = strong("El Oráculo"), 
                            titleWidth = 350
                            
                          ),
                          
                          
                          dashboardSidebar(width = 350, 
                                           
                                           fluidRow(
                                             
                                             column(10, offset = 1,
                                                    
                                                    # Se crea el marco donde se dibujarán los números.
                                                    plotOutput(outputId = "paint",
                                                               width = "100%", 
                                                               height = "300px", 
                                                               click = "handlerOne", # Qué sucede al clicar se controla con un handler. 
                                                               hover = hoverOpts(id = "hoverx", 
                                                                                 delay = 500,
                                                                                 delayType = "throttle",
                                                                                 nullOutside = TRUE)), 
                                                    # Hover sirve para controlar qué sucede al pasar el ratón por la pantalla. 
                                                    # ?hoverOpts
                                                    
                                                    
                                                    span(actionButton(inputId = "Erase", label = "Erase", icon = icon("unchecked", lib = "glyphicon")),
                                                         style = "position:relative;right:-14em;"
                                                         ),
                                                    
                                                    # Barra horizontal para la separación del marco de dibujo de la grilla con los píxeles. 
                                                    hr(),
                                                    
                                                    # Se define el array donde se mostrará aquello que se dibuja. 
                                                    imageOutput("matX")
                                                    
                                             )
                                           )
                                           
                          ),             
                          
                          
                          dashboardBody(
                            
                            # Color de los iconos (controlado por el parámetro class). 
                            tags$style(".bit {color:#EEEEEE; font-size: 60px; top: -20px;}"),
                            
                            
                            fluidRow(
                              
                              box(title = strong("Oracle"), status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
                                  
                                  fluidRow(
                                    
                                    column(6, valueBoxOutput(outputId = "NaiveB", width = 12)),
                                    
                                    column(6, valueBoxOutput(outputId = "RandomF", width = 12))
                                    
                                  ),
                                  
                                  fluidRow(
                                    
                                    column(6, valueBoxOutput(outputId = "SupportVM", width = 12)),
                                    
                                    column(6, valueBoxOutput(outputId = "ArtificialNN", width = 12))
                                    
                                  )
                                  
                                  
                              ),
                              
                              box(title = strong("Accuracy"), status = "danger", solidHeader = TRUE, width = 6, collapsible = TRUE,
                                  
                                  
                                  fluidRow(
                                    
                                    column(6, valueBoxOutput(outputId = "AccuracyNaiveB", width = 12)),
                                    
                                    column(6, valueBoxOutput(outputId = "AccuracyRandomF", width = 12))
                                    
                                  ),
                                  
                                  fluidRow(
                                    
                                    column(6, valueBoxOutput(outputId = "AccuracySupportVM", width = 12)),
                                    
                                    column(6, valueBoxOutput(outputId = "AccuracyArtificialNN", width = 12))
                                    
                                  )
                                  
                              )
                              
                              
                            ),
                            
                            
                            fluidRow(
                              
                              column(6, offset = 3,
                                     
                                     
                                     box(title = strong("Prophecy"), status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                         
                                         
                                         fluidRow(
                                           
                                           column(6,valueBoxOutput(outputId = "TotalOracle", width = 12)),
                                           
                                           column(6,valueBoxOutput(outputId = "TotalAcc", width = 12))
                                           
                                           
                                         )
                                         
                                         
                                         )
                                     
                                     )
                              
                            )
                            
                            

                            
                            
                          )
                          
)


# ----- Servidor -----

servidor <- function(input, output, session){
  
  # ----- Variables ----
  
  # Definir un valor reactivo dentro del servidor. 
  
  malen <- reactiveVal(value = FALSE)
  
  # Definir una variable reactiva con dos valores reactivos..
  
  valor <- reactiveValues(x = NULL, y = NULL)
  
  
  # Se crea la matriz de unos con las dimensiones definidas. 
  
  mat <- matrix(1, ncol = ancho, nrow = alto)
  
  
  # Se guarda la matriz como variable reactiva.
  # Se añade al valor reactivo la imagen en píxeles. 
  
  matreactive <- reactiveValues(mat = mat, neuesimg = NULL)
  
  # Se crea una variable con valores reactivos para la precisión. 
  oracleTotal <- reactiveValues(predNaiveB = NA,
                                predRandomF = NA,
                                predSupportVM = NA,
                                predArtificialNN = NA,
                                bestoAcc = NA)
  
  
  # ---- Funciones -----
  
  observeEvent(input$handlerOne,
               
               {
                 # Guardas el valor de malen en una variable.
                 # Así, cada vez que cliques tendrá distinto valor 
                 # la variable y permitirá usarla a modo de interruptor. 
                 pinta <- malen()
                 
                 # Cambias el valor de malen en virtud de la variable. 
                 malen(!pinta)
                 
                 if(!malen()){
                   
                   valor$x <- c(valor$x, NA)
                   valor$y <- c(valor$y, NA)
                   
                   # Se llama a la función procesarValores para que guarde lo dibujado en la matriz.
                   matreactive$mat <- procesaValores(valor$y, valor$x, matreactive$mat)
                   
                   # Se lleva a cabo la transformación de la matriz reactiva. 
                   matreactive$neuesimg <- matrixConversion(matreactive$mat)
                   
                 }
               }
               
  )
  
  observeEvent(input$hoverx,
               
               {
                 if(malen()){
                   
                   valor$x <- c(valor$x, input$hoverx$x)
                   valor$y <- c(valor$y, input$hoverx$y)
                   
                 }
                 
               })
  
  output$paint <- renderPlot({
    
    plot(valor$x, valor$y, # Valores
         xlim = c(0, seite*10 + 1), ylim = c(0, seite*10 + 1), # Límites de la escala
         xlab = "", ylab = "", main = "", # Etiquetas
         xaxt = "n", yaxt = "n", # Mostrar los ejes ("n" para no hacerlo)
         type = "l", lwd = spessore) 
    
  })
  
  output$matX <- renderImage({
    
    # Se define una variable que contenga la matriz reactiva. 
    mat <- matreactive$mat
    
    # Se quiere que cada píxel tenga 10 elementos de la matriz (cuadrados 10x10). 
    # Así se seleccionan las filas de la matriz en las que se desean cambiar los valores.
    # Se asigna el valor 0.75 para estas filas. 
    # Se hace lo mismo para las columnas. Se cambia alto por ancho para entender esto conceptualmente,
    # si bien en esta ocasión no sería necesario porque es un cuadrado. 
    # Se pone en esta posición para que dibuje el grid sobre el número transcrito.  
    
    mat[seq_len(ceiling(alto/10))*10-9, ] <- 0.75
    mat[,seq_len(ceiling(ancho/10))*10-9] <- 0.75
    
    # Se crea el array a partir de la matriz reactiva. 
    arrayX <- array(data = c(mat, mat, mat),
                    dim = c(ancho, alto, 3))
    
    # Se almacena en un fichero temporal. 
    temporalf <- tempfile(fileext = ".png")
    
    # Se guarda como una imagen png en el fichero temporal. 
    writePNG(image = arrayX, target = temporalf)
    
    # Se transforma el fichero en una imagen que mostrar en la interfaz.
    imagenX <- list(
      src = temporalf, # la fuente es el fichero temporal. 
      contentType = "image/png", # el tipo es una imagen en formato png.  
      width = ancho, 
      height = alto,
      alt = "Alejandro Navas González"
    )
    
    
  })
  
  output$NaiveB <- renderValueBox({
    
    if(is.null(matreactive$neuesimg)){
      
      pred <- "NA"
      
    } else {
      
      # Se realiza la predicción. 
      pred <- predict(object = modelo.NaiveBayes, newdata = matreactive$neuesimg)
      
      # Se transforma desde factor a numérico (pasa primero por caracter para evitar modificaciones en los valores).
      pred <- as.numeric(as.character(pred))
      
    }
    
    # Se asigna la predicción calculada a la variable reactiva con las predicciones. 
    oracleTotal$predNaiveB <- pred
    
    valueBox(value = pred, subtitle = h2(strong("Naive Bayes")),
             icon = icon("grain", lib = "glyphicon"), color = "olive")
    
  })
  
  output$RandomF <- renderValueBox({
    
    if(is.null(matreactive$neuesimg)){
      
      pred <- "NA"
      
    } else {
      
      # Se hace el filtrado de las columnas del archivo nvz.
      imagenNVZ <- matreactive$neuesimg[,-nvz]
      
      # Se aplica un análisis de componentes principales.
      imagenPCA <- predict(object = train.preproc, newdata = imagenNVZ)
      
      # Se realiza la predicción. 
      pred <- predict(object = modelo.RandomForest, newdata = imagenPCA)
      
      # Se transforma desde factor a numérico (pasa primero por caracter para evitar modificaciones en los valores).
      pred <- as.numeric(as.character(pred))
      
    }
    
    # Se asigna la predicción calculada a la variable reactiva con las predicciones. 
    oracleTotal$predRandomF <- pred
    
    valueBox(value = pred, subtitle = h2(strong("Random Forest")),
             icon = icon("leaf", lib = "glyphicon"), color = "yellow")
    
  })
  
  output$SupportVM <- renderValueBox({
    
    if(is.null(matreactive$neuesimg)){
      
      pred <- "NA"
      
    } else {
      
      # Se hace el filtrado de las columnas del archivo nvz.
      imagenNVZ <- matreactive$neuesimg[,-nvz]
      
      # Se aplica un análisis de componentes principales.
      imagenPCA <- predict(object = train.preproc, newdata = imagenNVZ)
      
      # Se realiza la predicción. 
      pred <- predict(object = modelo.SVM, newdata = imagenPCA)
      
      # Se transforma desde factor a numérico (pasa primero por caracter para evitar modificaciones en los valores).
      pred <- as.numeric(as.character(pred))
      
    }
    
    # Se asigna la predicción calculada a la variable reactiva con las predicciones. 
    oracleTotal$predSupportVM <- pred
    
    valueBox(value = pred, subtitle = h2(strong("Support Vector Machine")),
             icon = icon("knight", lib = "glyphicon"), color = "red")
    
  })
  
  output$ArtificialNN <- renderValueBox({
    
    if(is.null(matreactive$neuesimg)){
      
      pred <- "NA"
      
    } else {
      
      # Se hace el filtrado de las columnas del archivo nvz.
      imagenNVZ <- matreactive$neuesimg[,-nvz]
      
      # Se aplica un análisis de componentes principales.
      imagenPCA <- predict(object = train.preproc, newdata = imagenNVZ)
      
      # Se realiza la predicción. Para las redes neuronales se utiliza compute, una función de neuralnet.
      pred <- compute(x = modelo.RedesNeuronales, covariate = imagenPCA)
      
      # Esto devuelve una matriz de todas las posibilidades. Se elige la de mayor probabilidad. 
      pred <- max.col(pred$net.result)
      
      # Se transforma desde factor a numérico (pasa primero por caracter para evitar modificaciones en los valores).
      pred <- as.numeric(as.character(pred))
      
    }
    
    # Se asigna la predicción calculada a la variable reactiva con las predicciones. 
    oracleTotal$predArtificialNN <- pred
    
    valueBox(value = pred, subtitle = h2(strong("Artificial Neural Networks")),
             icon = icon("globe", lib = "glyphicon"), color = "fuchsia")
    
  })
  
  output$AccuracyNaiveB <- renderValueBox({
    
    AccNaiveBPercent <- paste(round(AccNaiveB*100, digits = 2), "%")
    
    valueBox(value = AccNaiveBPercent, subtitle = h2(strong("Naive Bayes")),
             icon = icon("screenshot", lib = "glyphicon"), color = "olive")
    
  })
  
  output$AccuracyRandomF <- renderValueBox({
    
    AccRandomFPercent <- paste(round(AccRandomF*100, digits = 2), "%")
    
    valueBox(value = AccRandomFPercent, subtitle = h2(strong("Random Forest")),
             icon = icon("screenshot", lib = "glyphicon"), color = "yellow")
    
  })
  
  output$AccuracySupportVM <- renderValueBox({
    
    AccSupportVMPercent <- paste(round(AccSupportVM*100, digits = 2), "%")
    
    valueBox(value = AccSupportVMPercent, subtitle = h2(strong("Support Vector Machine")),
             icon = icon("screenshot", lib = "glyphicon"), color = "red")
    
  })
  
  output$AccuracyArtificialNN <- renderValueBox({
    
    AccArtificialNNPercent <- paste(round(AccArtificialNN*100, digits = 2), "%")
    
    valueBox(value = AccArtificialNNPercent, subtitle = h2(strong("Artificial Neural Networks")),
             icon = icon("screenshot", lib = "glyphicon"), color = "fuchsia")
    
  })
  
  
  output$TotalOracle <- renderValueBox({
    
    # Vector con las predicciones calculadas. 
    predalles <- c(oracleTotal$predNaiveB, oracleTotal$predRandomF, oracleTotal$predSupportVM, oracleTotal$predArtificialNN)
    
    # Vector con las precisiones de cada modelo. 
    accalles <- c(AccNaiveB, AccRandomF, AccSupportVM, AccArtificialNN)
    
    # Marco de datos con las predicciones y sus respectivas precisiones. 
    marcoPA <- data.frame(predalles, accalles)
    
    # Tablas con la suma de las predicciones de los algoritmos que han realizado la misma predicción. 
    tablaPA <- aggregate(accalles ~ predalles, data = marcoPA, FUN = sum)
    
    # Se obtiene la mejor de las predicciones.
    bestoPred <- tablaPA[tablaPA$accalles == max(tablaPA$accalles), ]$predalles
    
    # Se define la precisión ponderada.
    oracleTotal$bestoAcc <- tablaPA[tablaPA$accalles == max(tablaPA$accalles), ]$accalles /sum(tablaPA$accalles)
    
    valueBox(value = bestoPred, subtitle = h2(strong("Mejor Predicción")),
             icon = icon("bitcoin", lib = "glyphicon", class = "bit"), color = "black")
    
  })
  
  output$TotalAcc <- renderValueBox({
    
    
    valueBox(value = paste(round(x = oracleTotal$bestoAcc * 100, digits = 3), "%"), subtitle = h2(strong("Precisión Ponderada")),
             icon = icon("piggy-bank", lib = "glyphicon", class = "bit"), color = "black")
    
  })
  
  
  # Botón de Acción para Limpiar: Se inicializarán todas las variables. 
  observeEvent(input$Erase, {
    
    valor$x <- NULL
    valor$y <- NULL
    matreactive$mat <- matrix(1, ncol = ancho, nrow = alto) # La matriz del inicio. 
    matreactive$neuesimg <- NULL
    
  })
  
}


# ----- Lanzamiento -----

shinyApp(ui = interfaz, server = servidor)

