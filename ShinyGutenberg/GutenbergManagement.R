library(wordcloud)
library(RColorBrewer)
library(tidyverse)

listaGutenerg <- read.csv("data/ListaGutenberg.csv", encoding = "UTF-8", stringsAsFactors = F)
colnames(listaGutenerg) <- c("index","archive")
listaGutenerg$name <- sub(".txt", "", listaGutenerg$archive)

# Si no se tuviera el .txt, se leería el libro así: 
libro1 <- listaGutenerg$name[1]
texto1 <- readLines(sprintf("./books/%s.txt", libro1), encoding = "UTF-8")

# Alternativa para la lectura: 
texto2 <- readLines(paste("./books/", libro1, ".txt", sep = ""), encoding = "UTF-8")

# Otra alternativa para la lectura. Ahora con el txt ya dado. 
texto3 <- readLines(paste("./books/", listaGutenerg$archive[1], sep = ""), encoding = "UTF-8")

# Ver el texto cargado: 
texto1[c(seq(0, 250))]

# Manejar signos de puntuación: 
description <- texto1[c(seq(0, 250))]
description
description2 <- gsub("[[:punct:]]", " ", description) # cambias los signos de puntuación por espacios en blanco. 
description2
description3 <- gsub("[0-9]", " ", description2) # Eliminación de números.
description3
description4 <- trimws(description3) # Elimina los espacios de los extremos.
description4
# Lista cuyos elementos son los vectores de las palabras por línea. 
wordlist <- strsplit(description4, " ") # El espacio en blanco como elemento de separación para la construcción de vectores.
wordlist[[1]]
wordlist[[3]]

# Se deshace la lista. Quedan elementos en el vector que son espacios en blanco (porque se repitieran en el texto original)
words <- unlist(wordlist)
words <- words[words != ""] # Se eliminan los elementos espacios en blanco.
words <- tolower(words) # Se pasa todo a minúsculas para facilitar el conteo de palabras. 


# ----- Aplicación como Función -----
# Se lee el libro: 
libro <- listaGutenerg$name[1]
texto <- readLines(sprintf("./books/%s.txt", libro), encoding = "UTF-8")

# Se hace así una función con los comandos ejecutados en GutenbergManagement: 
bookProcessing <- function(book){
  
  description <- book
  description2 <- gsub("[[:punct:]]", " ", description) # cambias los signos de puntuación por espacios en blanco. 
  description3 <- gsub("[0-9]", " ", description2) # Eliminación de números.
  description4 <- trimws(description3) # Elimina los espacios de los extremos.
  # Lista cuyos elementos son los vectores de las palabras por línea. 
  wordlist <- strsplit(description4, " ") # El espacio en blanco como elemento de separación para la construcción de vectores.
  # Se deshace la lista. Quedan elementos en el vector que son espacios en blanco (porque se repitieran en el texto original)
  words <- unlist(wordlist)
  words <- words[words != ""] # Se eliminan los elementos espacios en blanco.
  words <- tolower(words) # Se pasa todo a minúsculas para facilitar el conteo de letras.
  
}


bookCorpus <- bookProcessing(texto)
head(bookCorpus)
wordsDF <- as.data.frame(table(bookCorpus))
wordsDF <- wordsDF[order(wordsDF$Freq, decreasing = TRUE), ]

# Paleta de colores. 
numbercolors <- 20
myPalette <- colorRampPalette(brewer.pal(10, "RdYlBu"))(numbercolors)

# Nube de palabras. 
wordcloud(wordsDF$bookCorpus, wordsDF$Freq,
          min.freq = 50, max.words = 100, colors = myPalette)

# Aparecen palabras que son frecuentes pero no aportan información real por su uso generalizado. 
# Estas palabras en castellano se han guardado en el siguiente fichero:
freqwords <- read.table("data/StopWordsCastellano.txt", encoding = "UTF-8")
# Se ha cargado como un marco de datos, luego se pasa a vector de caracteres. 
freqwords <- as.character(freqwords$V1)
# Se eliminan estas palabras frecuentes del vector de caracteres del libro leído:
# bookCorpus <- bookCorpus[!(bookCorpus %in% freqwords)]
wordsDF <- wordDF[!(wordDF$bookCorpus %in% freqwords), ]
wordcloud(wordsDF$bookCorpus, wordsDF$Freq,
          min.freq = 50, max.words = 100, colors = myPalette)


totalwords <- length(bookCorpus) # Número total de palabras. 
totalwords
totaluniquewords <- length(unique(bookCorpus)) # Número total de palabras únicas.
totaluniquewords
specialwords <- dim(wordsDF)[1] # Número de filas del marco de datos tras el filtro de palabras genéricas.
specialwords


