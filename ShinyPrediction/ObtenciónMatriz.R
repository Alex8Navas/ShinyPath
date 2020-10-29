library(png) # Para dibujar imágenes.


# Se mide en número de píxeles, esto es, se tienen 25 píxeles. 

seite <- 25

# Se define el grosor del lápiz para dibujar en el gráfico. 

spessore <- 5

# Se definen las dimensiones de la matriz.

ancho <- seite  * 10 + 1
alto <- seite * 10 + 1

# Se crea la matriz de unos con las dimensiones definidas. 

mat <- matrix(1, ncol = ancho, nrow = alto)
mat

# Se quiere que cada píxel tenga 10 elementos de la matriz (cuadrados 10x10). 
# Así se seleccionan las filas de la matriz en las que se desean cambiar los valores.
# Se asigna el valor 0.75 para estas filas. 
seq_len(ceiling(alto/10))*10-9
mat[seq_len(ceiling(alto/10))*10-9, ] <- 0.75
mat

# Se hace lo mismo para las columnas. Se cambia alto por ancho para entender esto conceptualmente,
# si bien en esta ocasión no sería necesario porque es un cuadrado. 

mat[, seq_len(ceiling(ancho/10))*10-9] <- 0.75
mat

# Se crea entonces un array para definir 3D con la mtriz creada. 

arrayX <- array(data = c(mat, mat, mat), dim = c(ancho, alto, 3))

writePNG(image = arrayX, target = "data/arrayX.png")


# Actualización de la imagen tras dibujar sobre el cuadrante. 

# Se leen los ficheros con los valores X e Y guardados tras dibujar. 
valoresX <- read.csv("data/valoresX.csv")
valoresY <- read.csv("data/valoresY.csv")

filas <- round(as.vector(valoresY$x), 0)
columnas <- round(as.vector(valoresX$x), 0)

# Se leen para constituir la matriz. 
for (i in 1:length(filas)){
  
  # Se busca que no haya NAs, algo que sucederá si escribimos varias líneas. 
  if(!is.na(filas[i])){
    
    # Se mira que los valores se encuentren dentro del marco. 
    if (filas[i]-spessore > 0 & filas[i] + spessore < 251
        & columnas[i]-spessore > 0 & columnas[i] + spessore < 251){
      
      mat[seq(filas[i] - spessore, filas[i]+ spessore),
          seq(columnas[i] - spessore, columnas[i]+ spessore)] <- 0
      
    } else{
      
      # En caso de que lo anterior no se cumpla, salta de línea. 
      next()
      
    }
    
  }

}
 

