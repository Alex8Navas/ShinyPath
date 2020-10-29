# Ir a la web siguiente y bajarse lo libros que se quiera en Plain Text UTF-8:
# https://www.gutenberg.org/browse/languages/es 

# Listado de los libros descargados: 
list.files("books/")

# Se guardan en un vector y se lleva a csv:
listaGutenerg  <- list.files("books/")
write.csv(listaGutenerg, "data/ListaGutenberg.csv")
