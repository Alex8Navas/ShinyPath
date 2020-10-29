library(viridis)

# Carga del archivo con el nombre de los libros.
listaGutenerg <- read.csv("data/ListaGutenberg.csv", encoding = "UTF-8", stringsAsFactors = F)

colnames(listaGutenerg) <- c("index","archive")

listaGutenerg$name <- sub(".txt", "", listaGutenerg$archive)

# Aparecen palabras que son frecuentes pero no aportan información real por su uso generalizado. 
# Estas palabras en castellano se han guardado en el siguiente fichero:
freqwords <- read.table("data/StopWordsCastellano.txt", encoding = "UTF-8")

# Se ha cargado como un marco de datos, luego se pasa a vector de caracteres. 
freqwords <- as.character(freqwords$V1)

# Función para el procesamiento de Texto: 
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
  
  words <- tolower(words) # Se pasa todo a minúsculas para facilitar el conteo de palabras. 
  
}

# Se carga un libro: 
libro1 <- listaGutenerg$name[2]
texto1 <- readLines(sprintf("./books/%s.txt", libro1), encoding = "UTF-8")
head(texto1)


# Hay una decripción inicial. El libro comienza tras la línea que contiene START OF. 
firstLine <- 0
lastline <- 0

for(i in 1:length(texto1)){
  
  cat(i, "\n")
  
  if(substr(texto1[i], 1, 12) == "*** START OF"){
    
    firstLine <- i
    
  }
  
  if(substr(texto1[i], 1, 10) == "*** END OF"){
    
    lastline <- i
    
    break()
    
  }
}

buchKorp <- bookProcessing(texto1)

worteDF <- as.data.frame(table(buchKorp))

worteDF <- worteDF[order(worteDF$Freq, decreasing = TRUE),]

worteDF <- worteDF[!(worteDF$buchKorp %in% freqwords),]

wordcloud(worteDF$buchKorp, worteDF$Freq, max.words = 100, colors = viridis(7))

# Quitando las palabras inglesas del inicio: 
buchKorp2 <- bookProcessing(texto1[c(seq(firstLine, length(texto1)))])

worteDF2 <- as.data.frame(table(buchKorp2))

worteDF2 <- worteDF2[order(worteDF2$Freq, decreasing = TRUE), ]

worteDF2 <- worteDF2[!(worteDF2$buchKorp %in% freqwords), ]

wordcloud(worteDF2$buchKorp, worteDF2$Freq, max.words = 100, colors = viridis(7))

# Quitando las palabras inglesas del inicio y del final: 
buchKorp3 <- bookProcessing(texto1[c(seq(firstLine, lastline))])

worteDF3 <- as.data.frame(table(buchKorp3))

worteDF3 <- worteDF3[order(worteDF3$Freq, decreasing = TRUE), ]

worteDF3 <- worteDF3[!(worteDF3$buchKorp %in% freqwords), ]

wordcloud(worteDF3$buchKorp, worteDF3$Freq, max.words = 100, colors = viridis(7))

