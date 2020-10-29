# Desde la consola de R.
runApp("First1App.R")

# Desde una carpeta con los archivos global, interfaz y servidor en scripts separados.
# Los nombres ui, server y global son obligatorios para que R entienda que es una aplicación.
# No pongas los nombres en castellano ni elijas los nombres que quieras; dará error. 
runApp("Test/")

# Desde la shell: R -e 'shiny::runApp("Path de la App")'

# Si conoces la URL de la App puedes hacer runURL("url")