library(shiny)
library(rgl)
library(orientlib)

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
      actionButton("update", "Mettre à jour les angles"), # Nouveau bouton
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

  output$plot3d <- renderRglwidget({
    cube <- cube3d()
    cube$material$color <- c("red", "blue", "green", "yellow", "purple", "cyan")

    # Ouvrir une nouvelle fenêtre RGL
    shade3d(cube)

    # Ajout du texte au centre de chaque face
    text3d(x = 0, y = 0, z = 1, text = "Devant", color = "black", cex = 4) # Haut
    text3d(x = 0, y = 0, z = -1, text = "Derriere", color = "black", cex = 4) # Bas
    text3d(x = 0, y = 1, z = 0, text = "Dessus", color = "black", cex = 4) # Devant
    text3d(x = 0, y = -1, z = 0, text = "Dessous", color = "black", cex = 4) # Derrière
    text3d(x = 1, y = 0, z = 0, text = "Droite", color = "black", cex = 4) # Droite
    text3d(x = -1, y = 0, z = 0, text = "Gauche", color = "black", cex = 4) # Gauche

    rglwidget()
  })

  updateAngles <- function() {
    print("Mise à jour des angles")
    p <- par3d(no.readonly = TRUE)
    M <- p("userMatrix")

    print(M)

    angles <- rglToBase(M)
    theta <- angles$theta
    phi <- angles$phi
    updateSliderInput(session, "theta", value = theta * 180 / pi)
    updateSliderInput(session, "phi", value = phi * 180 / pi)
  }

  observeEvent(input$theta, {
    angles$theta <- input$theta
    print(paste("Slider theta mis à jour: ", angles$theta))
    view3d(theta = angles$theta, phi = angles$phi)
  })

  observeEvent(input$phi, {
    angles$phi <- input$phi
    print(paste("Slider phi mis à jour: ", angles$phi))
    view3d(theta = angles$theta, phi = angles$phi)
  })

  observeEvent(input$reset, {
    angles$theta <- 0
    angles$phi <- 0
    print("Réinitialisation de la vue")
    updateSliderInput(session, "theta", value = 0)
    updateSliderInput(session, "phi", value = 0)
    view3d(theta = 0, phi = 0)
  })

  observeEvent(input$update, {
    updateAngles()
  })

  output$current_angles <- renderText({
    paste("Theta :", round(angles$theta, 2), "degrés | Phi :", round(angles$phi, 2), "degrés")
  })
}

shinyApp(ui = ui, server = server)
