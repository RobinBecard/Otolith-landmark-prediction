library(shiny)
library(rgl)

ui <- fluidPage(
  rglwidgetOutput("myRGL", width = "400px", height = "300px")
)

server <- function(input, output, session) {
  # Initialiser la scene RGL
  output$myRGL <- renderRglwidget({
    open3d()
    # Ajouter un objet 3D (par exemple, une sphere) [4]
    shade3d(translate3d(cube3d(col = "red"), 0, 0, 0))
    # Definir le mode de la souris a "trackball" [5]
    par3d(mouseMode = "trackball")
    rglwidget()
  })

  # Observer les changements dans la scene RGL
  observe({
    # Recuperer la matrice de l'utilisateur [6]
    matrix <- par3d("userMatrix")
    # Afficher un message dans la console a chaque rotation [7]
    cat("Scene rotated\n")
  })
}

shinyApp(ui = ui, server = server)
