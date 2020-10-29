# Guardar Inicio y Final de cada texto como variables: 

listaGutenerg <- read.csv("data/ListaGutenberg.csv", encoding = "UTF-8", stringsAsFactors = F)

colnames(listaGutenerg) <- c("index","archive")

listaGutenerg$name <- sub(".txt", "", listaGutenerg$archive)

books <- listaGutenerg$name

# Creamos las variables para las l?neas inicial y final:
listaGutenerg$beginning <- 0
listaGutenerg$ending <- 0

# Bucle para automatizar la b?squeda de inicios y finales de los libros Gutenberg:
for (k in 1:length(books)) {
  
  bookText <- readLines(sprintf("./books/%s.txt", books[k]), encoding = "UTF-8")
  
  firstLine <- 0 
  
  lastline <- 0
  
  # Bucle para hallar d?nde comienza y d?nde acaba el texto para evitar palabras en ingl?s. 
  for(i in 1:length(bookText)){
    
    if(substr(bookText[i], 1, 12) == "*** START OF"){
      
      firstLine <- i
      
    }
    
    if(substr(bookText[i], 1, 10) == "*** END OF"){
      
      lastline <- i
      
      break()
      
    }
  }
  
  listaGutenerg$beginning[k] <- firstLine
  listaGutenerg$ending[k] <- lastline
  
}

write.csv(listaGutenerg, "data/Gutenbergv2.csv", row.names = FALSE)

