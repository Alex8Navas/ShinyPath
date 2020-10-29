# Diálogos Modales
# Constan de tres partes:
# 1. El HTML del diálogo modal. 
# 2. La llamada al comando showModal() para mostrar el aviso. 
# 3. Un observer que acople las dos partes anteriores. 

library(shiny)


# ----- Interfaz -----

interfaz <- basicPage(
  
  
  actionButton("mostrare", "El Diálogo Modal")
  
)

# ----- Servidor -----

servidor <- function(input, output){
  
  observeEvent(input$mostrare,{
    
    # Esto es lo que se conoce en programación como handler.
    # Un handler a grosso modo es un "manejador", un objeto que es capaz de "recibir" un evento,
    # un mensaje, etc y actuar en función al mismo.
    # https://es.stackoverflow.com/questions/173853/qu%C3%A9-es-un-handler 
    showModal(
      
      # modalDialog es 
      modalDialog(
        
        title = "Cuidado, Hermano",
        "Los orcos nos persiguen desde el amanecer",
        easyClose = TRUE, # Con esto pones el botón dismiss para cerrar con rapidez el mensaje emergente. 
        # footer: parámetro para añadir un pie de página. Elimina el dismiss. 
        size = "l", # s" for small, "m" (the default) for medium, or "l" for large.
        fade = TRUE # Mete una animación al dar a dismiss. 
        
      
      )
      
      
    )
    
  }
  )
  
}

# ----- Lanzamieto -----

shinyApp(ui = interfaz, server = servidor)