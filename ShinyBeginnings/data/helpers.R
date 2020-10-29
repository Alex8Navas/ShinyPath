# Nota: el mapa de porcentajes está diseñado para trabajar con el data ser de counties
# Puedo no funcionar correctamente con otros sets de datos si el orden de las filas 
# no encaja exactamente con el orden en que el paquete maps grafica los estados.
percent_map <- function(var, color, legend.title, min = 0, max = 100) {

  # genera el vector de los colores de relleno del mapa
  shades <- colorRampPalette(c("white", color))(100)
  
  # adecúa el gradiente a los porcentajes que hay entre el mínimo y el máximo
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # grafica el mapa choropleth 
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 1, mar = c(0,0,0,0))
  
  # sobreescribe los bordes de los estados
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # añade una leyenda
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}