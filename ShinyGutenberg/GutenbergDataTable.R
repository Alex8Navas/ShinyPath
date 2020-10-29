library(DT)
datatable(iris,
          caption = "La Tabla del Lirio", 
          rownames = FALSE)


listaGutenerg <- read.csv("data/ListaGutenberg.csv", encoding = "UTF-8", stringsAsFactors = F)

colnames(listaGutenerg) <- c("index","archive")

listaGutenerg$name <- sub(".txt", "", listaGutenerg$archive)

bookEx <- listaGutenerg$name[1]

bookTextExample <- readLines(sprintf("./books/%s.txt", bookEx), encoding = "UTF-8")

bookDF <- as.data.frame(bookTextExample, stringsAsFactors = FALSE)

datatable(bookDF, caption = "Agua de Nieve")
