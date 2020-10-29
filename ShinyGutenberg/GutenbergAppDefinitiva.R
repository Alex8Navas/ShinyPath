library(shiny)
library(shinydashboard)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(DT)



# ----- ZONA GENERAL -----


# ----- Archivos de la Zona General ----

# Abrir el csv desde Files y dar a save with encoding -> UTF-8 para que los acentos no produzcan caracteres extraños. 
listaGutenberg <- read.csv("data/Gutenbergv2.csv", stringsAsFactors = F)
# listaGutenberg$index <- NULL
books <- listaGutenberg$name

# Aparecen palabras que son frecuentes pero no aportan información real por su uso generalizado. 
# Estas palabras en castellano se han guardado en el siguiente fichero:
freqwords <- read.table("data/StopWordsCastellano.txt", encoding = "UTF-8")

# Se ha cargado como un marco de datos, luego se pasa a vector de caracteres. 
freqwords <- as.character(freqwords$V1)



# ----- Funciones de la Zona General -----

bookReading <- function(book){
  
  # Se introduce la lista de los nombres de los libros aquí como valor reactivo para que acepte nuevos libros:
  books <- v$listaGutenberg$name
  
  if(!(book %in% books)){
    
    stop("El libro que tratas de introducir no se halla en la base de datos") # Esto sirve para esquivar software malicioso.
    
  } else {
    
    
    bookText <- readLines(sprintf("./books/%s.txt", book), encoding = "UTF-8")
    
    # Hay que llamar al valor reactivo de listaGutenberg para observar cambios en las tablas tras cambiar de libro. 
    beginning <- v$listaGutenberg[v$listaGutenberg$name == book, ]$beginning  
    
    ending <- v$listaGutenberg[v$listaGutenberg$name == book, ]$ending
    
    resultado <- list(bookText, beginning, ending)
    
    return(resultado)
    
  }
}

bookProcessing <- function(bookText){
  
  description <- bookText
  
  description2 <- gsub("[[:punct:]]", " ", description) # Cambia los signos de puntuación por espacios en blanco. 
  
  description3 <- gsub("[0-9]", " ", description2) # Eliminación de números.
  
  description4 <- trimws(description3) # Eliminación de espacios en los extremos.
  
  # Lista cuyos elementos son los vectores de las palabras por línea. 
  wordlist <- strsplit(description4, " ") # El espacio en blanco como elemento de separación para la construcción de vectores.
  
  # Se deshace la lista. Quedan elementos en el vector que son espacios en blanco (porque se repitieran en el texto original)
  words <- unlist(wordlist)
  
  words <- words[words != ""] # Eliminación de los elementos que sean espacios en blanco.
  
  words <- tolower(words) # Se pasa todo a min?sculas para facilitar el conteo de palabras. 
  
}

wordsMatrix <- function(book){
  
  parameters <- bookReading(book)
  
  bookTextComplete <- parameters[[1]]
  
  begins <- parameters[[2]]
  
  ends <- parameters[[3]]
  
  bookTextObject <- bookTextComplete[c(seq(begins + 1, ends - 1))]
  
  bookCorpus <- bookProcessing(bookTextObject)
  
  bookCorpusClean <- bookCorpus[!(bookCorpus %in% freqwords)] # Filtro para evitar palabras frecuentes en castellano. 
  
  wordsDF <- as.data.frame(table(bookCorpusClean), stringsAsFactors = FALSE)
  
  wordsDF <- wordsDF[order(wordsDF$Freq, decreasing = TRUE), ]
  
  totalwords <- length(bookCorpus) # Número total de palabras. 
  
  totaluniquewords <- length(unique(bookCorpus)) # Número total de palabras únicas.
  
  specialwords <- dim(wordsDF)[1] # Número de filas del marco de datos tras el filtro de palabras genéricas. 
  # Es el número de palabras no frecuentes en castellano. 
  
  result <- list(
    
    totalwords <- totalwords,
    
    totaluniquewords <- totaluniquewords,
    
    specialwords <- specialwords, 
    
    wordsDF <- wordsDF,
    
    textComplete <- bookTextComplete,
    
    begins <- begins,
    
    ends <- ends
  )
  
  return(result)
  
}

# Función para establecer el inicio y el final de un nuevo libro. 
uroboro <- function(texto){
  
  firstLine <- 0 
  
  lastline <- 0
  
  # Bucle para hallar d?nde comienza y d?nde acaba el texto para evitar palabras en ingl?s. 
  for(i in 1:length(texto)){
    
    if(substr(texto[i], 1, 12) == "*** START OF"){
      
      firstLine <- i
      
    }
    
    if(substr(texto[i], 1, 10) == "*** END OF"){
      
      lastline <- i
      
      break()
      
    }
  }
  
  linesX <- list(firstLine, lastline)
  
  return(linesX)
  
}

handhaben <- function(ficheroElegido){
  
  # Si no hay fichero la app da warnings de conexión porque no hay nada, luego hay que evitar esto
  # más que por funcionalidad, por el hecho de que queda poco profesional. 
  
  if(is.null(ficheroElegido)){
    
    # Devuelve ceros para número total de líneas, inicio y final del libro, 
    # y espacio vacío para el texto. 
    return(list(0,0,0,""))
    
  }
  
  # Se lee el fichero escogido. Es necesario pasar el path para que no caiga la App. 
  text <- readLines(ficheroElegido$datapath, encoding = "UTF-8")
  
  # Número de líneas del texto: 
  lines <- length(text)
  
  # Se encuentran el inicio y el final del libro del fichero con la función uroboro. 
  anfSch <- uroboro(text)
  
  # Inicio del libro.
  anfangNB <- anfSch[[1]]
  
  # Final del libro. 
  schliesslichNB <- anfSch[[2]]
  
  # Variable con la lista de todos los elementos creados. 
  allesNeuesBuch <- list(lines, anfangNB, schliesslichNB, text)
  
  return(allesNeuesBuch)
}

# Valores Reactivos: se crea una aplicación que almacene valores reactivos. 
# Este valor reactivo es común para todo el servidor, esto es, si se conectan varias personas
# a tu app se pueden generar conflictos entre usuarios. 

v <-  reactiveValues(
  
  pagSchliesslich = 5,
  pagAnfang = 5,
  pagNew = 5,
  # Se añade listaGutenberg para que al cambiar de libro también lo haga el inicio y final de las tablas 
  # creadas en la segunda pestaña al pulsar el botón cambiar. 
  listaGutenberg = listaGutenberg
  
)

# ----- FIN DE LA ZONA GENERAL -----


# ----- Interfaz -----

interfaz <- dashboardPage(
  
  
  # ----- Encabezado del Dashboard -----  
  dashboardHeader(title =  h3(strong("Proyecto Gutenberg"), align = "center"),
                  titleWidth = 300
                  ),
  
  # ----- Panel del Dashboard -----  
  dashboardSidebar(
    
    width = 300, 
    
    # Crear un menú
    sidebarMenu(id = "mySideMenu",
      
      # TabName es el identificador de cada elemento del menú.
      # Con selected especificas que ese ítem del menú sea el que aparezca al iniciar la app. 
      menuItem("Estudio", tabName = "EstudioID", icon = icon("grain", lib = "glyphicon"), selected = TRUE),
      
      menuItem("Lectura", tabName = "LecturaID", icon = icon("cd", lib = "glyphicon")),
      
      menuItem("Biblioteca", tabName = "BibliotecaID", icon = icon("cloud", lib = "glyphicon"))
      
    ),
    
    hr(), 
    
    # Botón para seleccionar dentro de la base de libros. 
    selectInput("selektione", "Seleccione un Gutenberg", 
                choices = books, selected = "Apocalipsis"),
    
    # Botón para cambiar de libro: 
    actionButton("ilcambiare", "Cambiar"),
    
    # Barra horizontal para separar en el panel. 
    hr(), 
    
    # Selección de la frecuencia de las palabras: 
    sliderInput("frekquence", "Frecuencia Mínima:",
                min = 1, max = 100, value = 11),
    
    # Selección del número de palabras. 
    sliderInput("maxWords", "Máximo Número de Palabras:",
                min = 1, max = 300, value = 42),
    
    # Otra barra horizontal.
    hr(),
    
    # Selección de la paleta de colores.
    radioButtons("farben", "Seleccione una paleta de colores:", 
                 choices = list("Rojos & Azules" = "RdYlBu",
                                "Amarillos & Azules" = "YlGnBu",
                                "Morados & Rojos" = "PuRd",
                                "Morados & Verdes" = "PuBuGn",
                                "Azules & Morados" = "BuPu",
                                "Rosas & Verdes" = "PiYG",
                                "Paired" = "Paired",
                                "Darks" = "Dark2"), 
                 selected = "YlGnBu")
  ),
  
  # ----- Cuerpo del Dashboard ----- 
  dashboardBody(
    
    
    
    fluidPage(
      
      # Para reducir el tamaño de los iconos de las cajas de valores:
      # https://stackoverflow.com/questions/46513757/make-shiny-icon-smaller-in-valuebox 
      # tags$head(tags$style( HTML(".fa { font-size: 5px; }"))), 
      # El comando anterior sería para todo elemento .fa, pero se puede crear una clase sólo para iconos. 
      tags$style(HTML(".small_icon_test { font-size: 60px; }")),
      
      
      # Pestañas: se pone tabItems para ligar cada elemento a los ítems del menú creado. 
      tabItems(
                  
                  # Primera pestaña: 
                  tabItem(tabName = "EstudioID",
                           
                           fluidRow(
                             
                             
                             valueBoxOutput(width = 4, outputId = "totalwordsID"), 
                             
                             valueBoxOutput(width = 4, outputId = "totaluniquewordsID"), 
                             
                             valueBoxOutput(width = 4, outputId = "specialwordsID") 
                             
                           ),
                           
                           br(),
                           
                           fluidRow(
                             
                             h1(strong("Nube de Palabras"), align = "Center"),
                             br(),
                             column(12, align="center",
                                    
                                    plotOutput("nube", width = 1000, height = 700)), 
                             
                           )
                  ),
                  
                  # Segunda Pestaña. Sobre la primera y última l?neas recogidas para la nube de palabras. 
                  tabItem(tabName = "LecturaID",
                           
                           fluidRow(
                             
                             # ValueBox también se puede poner dentro de una columna (ocupará todo su ancho, 12)
                             column(5, valueBoxOutput("erste", width = 12)),
                             
                             column(5, valueBoxOutput("letze", width = 12)),
                             
                             column(2, actionButton("modifk", h4(strong("Modificar"))))
                             
                           ),
                           
                           fluidRow(
                             
                             tabsetPanel(type = "tabs",
                                         
                                         # Comienzo de la tabla: 
                                         tabPanel("Anfang",
                                                  
                                                  fluidRow(
                                                    
                                                    column(4, h4(strong("Inicio del Libro"), align = "center")),
                                                    column(1, offset = 5, actionButton("pageAnfUp", "Asciende")),
                                                    column(1, actionButton("pageAnfDown", "Desciende"))
                                                    
                                                    
                                                  ),
                                                  
                                                  fluidRow(
                                                    
                                                    column(12, dataTableOutput("erstetable"))
                                                  )
                                         ),
                                         
                                         # Final de la tabla: 
                                         tabPanel("Schliesslich",
                                                  
                                                  fluidRow(
                                                    
                                                    column(4, h4(strong("Final del Libro"), align = "center")),
                                                    column(1, offset = 5, actionButton("pageSSUp", "Asciende")),
                                                    column(1, actionButton("pageSSDown", "Desciende"))
                                                    
                                                  ),
                                                  
                                                  fluidRow(
                                                    
                                                    column(12, dataTableOutput("letzetable"))
                                                    
                                                  )
                                                  
                                         )
                             )
                           )
                  ),
                  
                  tabItem(tabName = "BibliotecaID",
                           
                           
                           fluidRow(
                             
                             # Seleccionar un fichero desde local. 
                             column(4, fileInput(inputId = "fichero", 
                                                 label = "Selecciona un libro:",
                                                 buttonLabel = "Explorar el Archivo",
                                                 placeholder = "Selecciona un libro del Archivo")),
                             
                             # Cuadro de texto para escribir el título del fichero. 
                             column(4, textInput(inputId = "titleID", label = "Introduce el Título:")),
                             
                             # Interfaz dinámica que recoge lo escrito en titleID. 
                             column(4, uiOutput(outputId = "Dinamo"))
                             
                           ),
                           
                           fluidRow(
                             
                             # Información en pantalla para el futuro usuario. 
                             column(4, 
                                    
                                    h5(p("Buenas, transeúnte. Escoge un fichero, escribe un título y selecciona el código.
                                         Pulsa 'Procesar fichero' y, al aparecer la confirmación de que se ha procesado,
                                         ejecuta la opción 'Guardar fichero' y así ya lo tendrás en tu biblioteca.", tags$br(),tags$br(),
                                         "Gracias por la visita a Proyecto Gutenberg. Suerte y próspera vida."))),
                             
                             # Botón para procesar el texto. 
                             column(1, offset = 4, actionButton("processID", "Procesar fichero", 
                                                    icon = icon("qrcode", lib = "glyphicon"))), 
                             
                             # Botón de guardar.
                             column(1, offset = 1, actionButton("saveID", "Guardar fichero",
                                                                icon = icon("cd", lib = "glyphicon")))
                             
                           ),
                           
                           br(),
                           br(),
                           
                           fluidRow(
                             
                             column(6, h3(strong("Características del Fichero"), align = "center"),
                                    br(), 
                                    h5(strong("Título del Nuevo Libro:")), textOutput("neuesBuchTitle"), 
                                    h5(strong("Código del Nuevo Libro:")), textOutput("neuesBuchCode"),
                                    hr(),
                                    valueBoxOutput("neuesBuchLines", width = 12),
                                    valueBoxOutput("neuesBuchBeginning", width = 12),
                                    valueBoxOutput("neuesBuchEnding", width = 12)
                             ),
                             
                             column(6, 
                                    
                                    fluidRow(
                                      
                                      column(8, h3(strong("Muestra del Libro"), align = "left")),
                                      column(1, offset = 1, actionButton("pageNewUp", "", 
                                                                         icon = icon("arrow-up", lib = "glyphicon"))),
                                      column(1, actionButton("pageNewDown", "",
                                                             icon = icon("arrow-down", lib = "glyphicon")))
                                      
                                    ),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      
                                      textOutput("neuesBuchText")
                                      
                                    )
                             )
                           )
                  )
      )
      
      
    )
    
  )
  
)



# ----- Servidor -----

servidore <- function(input, output, session){
  
  # ----- Pestaña Estudio -----
  
  # Variable reactiva con el libro leído. Contiene una lista con 4 elementos, que son:
  # 1. Total de palabras.
  # 2. Total de palabras ?nicas. 
  # 3. Total de palabras especiales (tras el filtro de palabras de uso com?n).
  # 4. El marco de datos con las palabras y su conteo. 
  bookKorp <- reactive({
    
    input$ilcambiare
    
    isolate({
      
      withProgress({
        
        setProgress(message = "Procesando el Corpus del Libro ... ... ...")
        
        wordsMatrix(input$selektione)
        
      })
      
    })
  })
  
  # Se crea una variable reactiva por cada elemento de la lista de bookKorp():
  wordsMarkus <- reactive({
    
    bookKorp()[[4]]
    
  })
  
  totalWorte <- reactive({
    
    bookKorp()[[1]]
    
  })
  
  totaluniqueWorte <- reactive({
    
    bookKorp()[[2]]
    
  })
  
  specialWorte <- reactive({
    
    bookKorp()[[3]]
    
  })
  
  vollstandig <- reactive({
    
    bookKorp()[[5]]
    
  })
  
  anfang <- reactive({
    
    bookKorp()[[6]]
    
  })
  
  schliesslich <- reactive({
    
    bookKorp()[[7]]
    
  })
  
  # Se realiza la nube de palabras: 
  output$nube <- renderPlot({
    
    buch <- wordsMarkus()
    
    wordcloud(words = buch$bookCorpus, freq = buch$Freq,
              min.freq = input$frekquence, 
              max.words = input$maxWords,
              colors = colorRampPalette(brewer.pal(8, input$farben))(42),)
    
  })
  
  output$totalwordsID <- renderValueBox({
    
    valueBox(value = totalWorte(), 
             subtitle = h4(strong("Total de Palabras")),
             icon = icon("globe", lib = "glyphicon", class = "small_icon_test"), 
             color = "aqua"
             )
    
    
    })
  
  output$totaluniquewordsID <- renderValueBox({
    
    valueBox(value = totaluniqueWorte(), 
             subtitle = h4(strong("Total de Palabras Únicas")), 
             icon = icon("cog", lib = "glyphicon", class = "small_icon_test"),
             color = "light-blue"
             
             )

    
    })
  
  output$specialwordsID <- renderValueBox({
    
    valueBox(value =  specialWorte(), 
             subtitle = h4(strong("Total de Palabras Especiales")),
             icon = icon("king", lib = "glyphicon", class = "small_icon_test"),
             color = "purple"
             )
   
    
    })
  
  # ----- Pestaña Modificación -----
  
  output$erste <- renderValueBox({
    
    valueBox(value = if(is.null(input$erstetable_rows_selected)){
      
      anfang()
      
    } else{
      
      input$erstetable_rows_selected
    },
    subtitle = h4(strong("Comienzo del Libro")), 
    color = "olive", 
    icon = icon("apple", lib = "glyphicon", class = "small_icon_test"))
    
    
  })
  
  output$letze <- renderValueBox({
    
    valueBox(value = if(is.null(input$letzetable_rows_selected)){
      
      schliesslich()
      
    } else{
      
      input$letzetable_rows_selected
    },
    subtitle =  h4(strong("Final del Libro")),
    color = "maroon",
    icon = icon("hourglass", lib = "glyphicon", class = "small_icon_test"))
    
    
  })
  
  
  # Al utilizar variables reactivas hay que recurrir a la función datatable y poner las opciones dentro de las llaves,
  # no fuera de éstas como se hizo en 7-DataTable2. 
  output$erstetable <- renderDataTable({
    
    datatable(as.data.frame(vollstandig(), stringsAsFactor = FALSE), 
              escape = FALSE,# sirve para evitar caracteres especiales.
              selection = list(mode = "single", selected = anfang()),  # Selecciona la primera línea del comienzo el texto. 
              options = list(searching = TRUE, # Con esto quitas (FALSE) o dejas (TRUE) el buscador de la tabla.
                             displayStart = anfang() - v$pagAnfang, # El comienzo lo marca el inicio del libro. Se pone un poco antes para dar más claridad.  
                             lengthMenu = c(10,20,30,40,50,60,70,80,90,100),
                             pageLength = 15, # Filas mostradas por defecto. 
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
    
  })
  
  # Función para crear un cuadro de mando para subir un número determiado de líneas al inicio del texto.  
  observeEvent(
    
    input$pageAnfUp, {
      
      v$pagAnfang <- v$pagAnfang + 10 # Sube 10 líneas. 
      
    }
    
  )
  
  # Función para crear un cuadro de mando para bajar un número determiado de líneas al inicio del texto.  
  observeEvent(
    
    input$pageAnfDown, {
      
      v$pagAnfang <- v$pagAnfang - 10 # Baja 10 líneas. 
      
    }
    
  )
  
  output$letzetable <- renderDataTable({
    
    datatable(as.data.frame(vollstandig(), stringsAsFactor = FALSE),
              escape = FALSE,
              selection = list(mode = "single", selected = schliesslich()),  # Selecciona la primera línea del final del libro.  
              options = list(searching = TRUE, # Con esto quitas (FALSE) o dejas (TRUE) el buscador de la tabla.
                             displayStart = schliesslich() - v$pagSchliesslich, # El comienzo lo marca el inicio del libro. Se pone un poco antes para dar más claridad.  
                             lengthMenu = c(10,20,30,40,50,60,70,80,90,100),
                             pageLength = 15, # Filas mostradas por defecto. 
                             dom = "tiplf" 
              )
    )
  })
  
  # Función para crear un cuadro de mando para subir un número determiado de líneas al final del texto.  
  observeEvent(
    
    input$pageSSUp, {
      
      v$pagSchliesslich <- v$pagSchliesslich + 20 # Sube 20 líneas. 
      
    }
    
  )
  
  # Función para crear un cuadro de mando para bajar un número determiado de líneas al final del texto.  
  observeEvent(
    
    input$pageSSDown, {
      
      v$pagSchliesslich <- v$pagSchliesslich - 20 # Baja 20 líneas. 
      
    }
    
  )
  
  # Función para actualizar el inicio y el final de un libro.
  observeEvent(
    
    input$modifk, 
    
    {
      
      iniciamod <- ifelse(is.null(input$erstetable_rows_selected),
                          anfang(),
                          input$erstetable_rows_selected)
      
      finalizamod <- ifelse(is.null(input$letzetable_rows_selected),
                            schliesslich(),
                            input$letzetable_rows_selected)
      
      v$listaGutenberg <- read.csv("data/Gutenbergv2.csv", stringsAsFactors = F)
      
      # Permite modificar el inicio y el final de cada libro para ser más pulcros. 
      # Esto es útil porque hay libros en los que quizá tras el patrón seleccionado al inicio o antes del patrón del final, existen palabras 
      # de las que se quiere prescindir. 
      
      v$listaGutenberg[v$listaGutenberg$name == input$selektione, ]$beginning <- iniciamod
      
      v$listaGutenberg[v$listaGutenberg$name == input$selektione, ]$ending <- finalizamod
      
      write.csv(v$listaGutenberg, "data/Gutenbergv2.csv", row.names = FALSE)
      
      # Mensaje modal que indique que has efectuado una modificación
      showModal(
        
        modalDialog(
          
          title = "Has realizado una modificación de las líneas de inicio y final",
          h5("Recuerda pulsar de nuevo el botón cambiar para observar la modificación efectuada, amigo."),
          size = "l", # Tamaño de la ventana del cuadro de diálogo. Large en este caso. 
          easyClose = TRUE, # Cerrar con facilidad 
          fade = TRUE, # Incluir animación.
          # Por defecto para salir poner dismiss, para cambiarlo e hace lo siguiente: 
          footer = tagList(modalButton(
            label = "Salida de Moria"
          )) 
          
        )
        
      )
      
    })
  
  # ---- Pestaña Neues Buch -----
  
  # Interfaz dinámica para la introducción de libros nuevos.
  output$Dinamo <- renderUI({
    
    titleBuch <- unlist(strsplit(input$titleID, " "))
    
    # Se pasa a minúsculas. 
    # titleBuch <- tolower(titleBuch)
    
    # Preseleccionar la palabra con mayor número de caracteres:
    worteSelected <- titleBuch[which(nchar(titleBuch) == max(nchar(titleBuch)))] 
    
    selectInput("BuchCode", "Escoge un Código para el Libro:",
                choices = titleBuch, selected = worteSelected)
    
    
  })
  
  # Evento para el guardado del libro nuevo en la carpeta books. 
  observeEvent(
    
    input$saveID,{
      
      # Es necesario poner data path porque al cargarlo el ordenador lo almacena en una ruta 
      # a la que tienes que acceder para efectuar el guardado.
      # neuesBuch <- readLines(input$fichero$datapath, encoding = "UTF-8")
      # writeLines(neuesBuch, paste("books/", input$BuchCode, ".txt", sep = ""))
      
      # Cambias las instrucciones anteriores por la variable reactiva. 
      writeLines(textNeues(), paste("books/", input$BuchCode, ".txt", sep = ""), useBytes = TRUE)
      
      # Se crea un marco de datos con la información del nuevo libro. 
      neuesBuchDataFrame <- data.frame(
        
        # index,"archive","name","beginning","ending"
        index = length(v$listaGutenberg$index) + 1, 
        archive = paste(input$titleID, ".txt", sep = ""),
        name = input$titleID,
        beginning = anfangNeues(),
        ending = schliesslichNeues()
        
      )
      
      # Se combinan ambos marcos de datos. Se llama igual que la primera para no generar conlfictos al leer el nuevo libro.
      # Hay que trabajar con el marco de datos reactivo para que los cambios se ejecuten. 
      v$listaGutenberg <- rbind(v$listaGutenberg, neuesBuchDataFrame)
      
      # Se guarda todo en el csv que contiene la información sobre los libros. 
      write.csv(v$listaGutenberg, "data/Gutenbergv2.csv", row.names = FALSE)
      
      # Se crea un vector con los nombres de los libros del nuevo marco.
      books <- v$listaGutenberg$name
      
      slekted <- input$BuchCode
      
      updateSelectInput(
        
        session = session,
        inputId = "selektione",
        choices = books,
        selected = slekted
        
      )
      
      # Hay que cambiar el updateTabsetPanel() por un updateTabItems(). 
      updateTabItems(
        
        session = session,
        inputId = "mySideMenu",
        selected = "EstudioID"
        
      )
    }
    
  )
  
  # Procesamiento del nuevo libro (variable reactiva). 
  neuesBuchProcessed <- reactive({
    
    input$processID
    
    # Con isolate separas esta parte del resto de la aplicación, se anula en cuanto la reactividad. 
    isolate({
      
      withProgress({
        
        # Se muestra el mensaje para que el usuaio sepa que el proceso se está ejecutando. 
        setProgress(message = "Procesando el fichero seleccionado... ... ...")
        
        # Se llama a la función de procesar el texto y se le suministra el fichero escogido como parámetro. 
        procesado <- handhaben(input$fichero) 
        
        # Condición para evitar que en la inicialización cambie de forma inmediata el estado.
        # Es decir, el cambio en el botón solo se producirá si se ha cargado un fichero, no solamente con pulssar el botón.
        if(procesado[[1]] != 0){
          
          updateActionButton(
            session = session,
            inputId = "saveID",
            icon = icon("picture", lib = "glyphicon"))
        } 
        
        # Hay que pon
        return(procesado) 
        
      })
    })
  })
  
  # Variable reactiva hija de neuesBuchProcessed con la información sobre el número total de líneas. 
  linesNeues <- reactive({
    
    neuesBuchProcessed()[[1]] # Número total de líneas del libro procesado. 
    
  })
  
  # Variable reactiva hija de neuesBuchProcessed con la información sobre el incio del libro. 
  anfangNeues <- reactive({
    
    neuesBuchProcessed()[[2]] # Inicio del libro procesado.
    
  })
  
  # Variable reactiva hija de neuesBuchProcessed con la información sobre el final del libro. 
  schliesslichNeues <- reactive({
    
    neuesBuchProcessed()[[3]] # Final del libro procesado.
    
  })
  
  # Variable reactiva hija de neuesBuchProcessed con la información sobre el texto del libro. 
  textNeues <- reactive({
    
    neuesBuchProcessed()[[4]] # Texto del nuevo libro procesado. 
    
  })
  

  
  # Salidas de las variables creadas:
  output$neuesBuchTitle <- renderText({input$titleID})
  output$neuesBuchCode <- renderText({input$BuchCode})
  
  output$neuesBuchLines <- renderValueBox({
    
    valueBox(value = linesNeues(),
    subtitle = h5(strong("Número Total de Líneas del Nuevo Libro:")),
    color = "red",
    icon = icon("barcode", lib = "glyphicon", class = "small_icon_test"))
    
    
    })
  output$neuesBuchBeginning <- renderValueBox({
    
    valueBox(value = anfangNeues(),
             subtitle =  h5(strong("Primera Línea del Nuevo Libro:")),
             color = "yellow",
             icon = icon("leaf", lib = "glyphicon", class = "small_icon_test"))
    
    })
  output$neuesBuchEnding <- renderValueBox({
    
    valueBox(value = schliesslichNeues(),
             subtitle =  h5(strong("Última Línea del Nuevo Libro:")),
             color = "orange",
             icon = icon("fire", lib = "glyphicon", class = "small_icon_test"))
    
    })
  
  
  output$neuesBuchText <- renderText({
    
    # Para que no salgan NAs en caso de no haber puesto libro al especificar espacio vacío en la función de
    # procesar el libro (handhaben), se hace lo siguiente.
    
    if(linesNeues() == 0){
      
      return("")
      
    } else {
      
      textNeues()[seq(anfangNeues() - v$pagNew, anfangNeues() - v$pagNew + 42)]
      
    }
    
  })
  
  
  # Función para crear un cuadro de mando para subir un número determiado de líneas en el texto del nuevo libro.  
  observeEvent(
    
    input$pageNewUp, {
      
      v$pagNew <- v$pagNew + 20 # Sube 20 líneas. 
      
    }
    
  )
  
  # Función para crear un cuadro de mando para bajar un número determiado de líneas en el txto del nuevo libro.   
  observeEvent(
    
    input$pageNewDown, {
      
      v$pagNew <- v$pagNew - 20 # Baja 20 líneas. 
      
    }
    
  )
  
}




# ----- Lanzamiento de la App -----

shinyApp(ui = interfaz, server = servidore)
