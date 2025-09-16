library(shiny)
library(rgl)

ui <- fluidPage(
  titlePanel("Contrôle RGL avec Sliders"),
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
      textOutput("current_angles") # Afficher les valeurs de theta et phi
    ),
    mainPanel(
      rglwidgetOutput("plot3d")
    )
  )
)

server <- function(input, output, session) {
  # Variables réactives pour stocker les angles
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

  # Observer les changements de la scène RGL (rotation avec la souris)
  observe({
    print("Observer")
    # Récupérer les paramètres de la scène RGL avec shinyGetPar3d
    par3d <- shinyGetPar3d("userMatrix", session = session, subscene = 1)
    if (!is.null(par3d$userMatrix)) {
      userMatrix <- par3d$userMatrix

      # Convertir la matrice en angles theta et phi
      theta <- atan2(userMatrix[2, 1], userMatrix[1, 1]) * 180 / pi
      phi <- atan2(-userMatrix[3, 1], sqrt(userMatrix[3, 2]^2 + userMatrix[3, 3]^2)) * 180 / pi

      # Mettre à jour les angles et les sliders
      isolate({
        angles$theta <- theta
        angles$phi <- phi
        updateSliderInput(session, "theta", value = theta)
        updateSliderInput(session, "phi", value = phi)
      })
    }
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
