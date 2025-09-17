library(shiny)
library(rgl)

ui <- fluidPage(
  titlePanel("Contrôle RGL avec Sliders (Mise à jour toutes les secondes)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("theta",
        "Theta (rotation horizontale):",
        min = -180,
        max = 180,
        value = 0
      ),
      sliderInput("phi",
        "Phi (rotation verticale):",
        min = -180,
        max = 180,
        value = 0
      ),
      actionButton("reset", "Réinitialiser la vue"),
      hr(),
      h4("Valeurs actuelles :"),
      textOutput("current_angles")
    ),
    mainPanel(
      rglwidgetOutput("plot3d")
    )
  )
)

server <- function(input, output, session) {
  angles <- reactiveValues(
    theta = 0,
    phi = 0
  )

  # Création de la scène 3D
  output$plot3d <- renderRglwidget({
    open3d()
    shade3d(cube3d(col = "blue", alpha = 0.5))
    view3d(theta = angles$theta, phi = angles$phi)
    rglwidget()
  })

  # Fonction pour mettre à jour les angles
  updateAngles <- function() {
    print(paste("Mise a jour des angles : theta =", angles$theta, "phi =", angles$phi))
    par3d <- par3d(no.readonly = TRUE)
    if (!is.null(par3d$userMatrix)) {
      print("matrice non nulle")
      userMatrix <- par3d$userMatrix
      print(userMatrix)

      theta <- atan2(userMatrix[2, 1], userMatrix[1, 1]) * 180 / pi
      phi <- atan2(-userMatrix[3, 1], sqrt(userMatrix[3, 2]^2 + userMatrix[3, 3]^2)) * 180 / pi


      print(paste("theta =", theta, "phi =", phi))

      updateSliderInput(session, "theta", value = theta)
      updateSliderInput(session, "phi", value = phi)
    }
  }

  # Observateur périodique pour mettre à jour les angles toutes les secondes
  observe({
    print("Observer")
    invalidateLater(1000, session) # Mise à jour toutes les secondes (1000 ms)
    updateAngles()
  })

  # Observer les changements des sliders
  observeEvent(input$theta, {
    angles$theta <- input$theta
    view3d(theta = angles$theta, phi = angles$phi)
  })

  observeEvent(input$phi, {
    angles$phi <- input$phi
    view3d(theta = angles$theta, phi = angles$phi)
  })

  # Réinitialiser la vue
  observeEvent(input$reset, {
    angles$theta <- 0
    angles$phi <- 0
    updateSliderInput(session, "theta", value = 0)
    updateSliderInput(session, "phi", value = 0)
    view3d(theta = 0, phi = 0)
  })

  # Afficher les valeurs de theta et phi en temps réel
  output$current_angles <- renderText({
    paste("Theta :", round(angles$theta, 2), "degrés | Phi :", round(angles$phi, 2), "degrés")
  })
}

shinyApp(ui = ui, server = server)
