library(shiny)
library(rgl)

# Interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Contrôle de la rotation d'une scène rgl avec des sliders"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("angleX", "Angle X", min = 0, max = 360, value = 0),
      sliderInput("angleY", "Angle Y", min = 0, max = 360, value = 0),
      sliderInput("angleZ", "Angle Z", min = 0, max = 360, value = 0)
    ),
    mainPanel(
      rglwidgetOutput("rglPlot")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Création de la scène rgl
  output$rglPlot <- renderRglwidget({
    open3d()

    print(cur3d())
    print(rgl.dev.list())

    # Exemple simple : un cube
    shade3d(cube3d(col = "blue", alpha = 0.5))
    axes3d()
    rglwidget()
  })

  # Observation des changements des sliders et mise à jour de la scène
  observe({
    # Récupérer les angles des sliders
    angleX <- input$angleX
    angleY <- input$angleY
    angleZ <- input$angleZ

    # Convertir les angles en radians
    angleX_rad <- angleX * pi / 180
    angleY_rad <- angleY * pi / 180
    angleZ_rad <- angleZ * pi / 180

    # Créer les matrices de rotation pour chaque axe
    rotationX <- matrix(c(
      1, 0, 0, 0,
      0, cos(angleX_rad), -sin(angleX_rad), 0,
      0, sin(angleX_rad), cos(angleX_rad), 0,
      0, 0, 0, 1
    ), ncol = 4, byrow = TRUE)

    rotationY <- matrix(c(
      cos(angleY_rad), 0, sin(angleY_rad), 0,
      0, 1, 0, 0,
      -sin(angleY_rad), 0, cos(angleY_rad), 0,
      0, 0, 0, 1
    ), ncol = 4, byrow = TRUE)

    rotationZ <- matrix(c(
      cos(angleZ_rad), -sin(angleZ_rad), 0, 0,
      sin(angleZ_rad), cos(angleZ_rad), 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    ), ncol = 4, byrow = TRUE)

    # Combiner les matrices de rotation (attention à l'ordre !)
    userMatrix <- rotationX %*% rotationY %*% rotationZ

    # Appliquer la nouvelle userMatrix à la scène
    par3d(userMatrix = userMatrix)

    # Envoyer la scène mise à jour au widget rglwidget
    rglwidget()
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
