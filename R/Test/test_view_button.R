library(shiny)
library(rgl)
library(orientlib)

ui <- fluidPage(
  titlePanel("Contrôle RGL avec Sliders (alpha, beta, gamma)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha",
        "Alpha (rotation autour de l'axe x):",
        min = -180,
        max = 180,
        value = 0
      ),
      sliderInput("beta",
        "Beta (rotation autour de l'axe y):",
        min = -180,
        max = 180,
        value = 0
      ),
      sliderInput("gamma",
        "Gamma (rotation autour de l'axe z):",
        min = -180,
        max = 180,
        value = 0
      ),
      actionButton("reset", "Réinitialiser la vue"),
      actionButton("update", "Mettre à jour les angles"),
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
    alpha = 0.0, # Rotation autour de l'axe x
    beta = 0.0, # Rotation autour de l'axe y
    gamma = 0.0 # Rotation autour de l'axe z
  )

  id <- reactiveVal(NULL)

  # Création de la scène 3D
  output$plot3d <- renderRglwidget({
    open3d()
    id(rgl.cur())

    shade3d(cube3d(), col = "blue", alpha = 1)
    updateView() # Appliquer les angles initiaux
    rglwidget()
  })

  # Fonction pour mettre à jour la vue avec les angles actuels
  updateView <- function() {
    print(paste("Mise à jour de la vue : alpha =", angles$alpha, "beta =", angles$beta, "gamma =", angles$gamma))
    set3d(id())
    # Créer une matrice de rotation combinée
    M <- rotationMatrix(angles$alpha * pi / 180, 1, 0, 0) %*% # Rotation autour de x (alpha)
      rotationMatrix(angles$beta * pi / 180, 0, 1, 0) %*% # Rotation autour de y (beta)
      rotationMatrix(angles$gamma * pi / 180, 0, 0, 1) # Rotation autour de z (gamma)
    par3d(userMatrix = M)
  }

  # Fonction pour mettre à jour les angles à partir de la userMatrix
  updateAngles <- function() {
    set3d(id())
    par3d_values <- par3d(no.readonly = TRUE)
    M <- par3d_values$userMatrix
    print(M)

    # Convertir la userMatrix en angles avec rglToLattice
    angles_list <- rglToLattice(M)

    angles$alpha <- angles_list$x # Rotation autour de l'axe x
    angles$beta <- angles_list$y # Rotation autour de l'axe y
    angles$gamma <- angles_list$z # Rotation autour de l'axe z

    # Mettre à jour les sliders
    updateSliderInput(session, "alpha", value = angles$alpha)
    updateSliderInput(session, "beta", value = angles$beta)
    updateSliderInput(session, "gamma", value = angles$gamma)

    print(paste("Angles mis à jour : alpha =", angles$alpha, "beta =", angles$beta, "gamma =", angles$gamma))
  }

  # Observer les changements des sliders
  observeEvent(input$alpha, {
    angles$alpha <- input$alpha
    updateView()
  })

  observeEvent(input$beta, {
    angles$beta <- input$beta
    updateView()
  })

  observeEvent(input$gamma, {
    angles$gamma <- input$gamma
    updateView()
  })

  # Réinitialiser la vue
  observeEvent(input$reset, {
    angles$alpha <- 0
    angles$beta <- 0
    angles$gamma <- 0
    updateSliderInput(session, "alpha", value = 0)
    updateSliderInput(session, "beta", value = 0)
    updateSliderInput(session, "gamma", value = 0)
    updateView()
  })

  # Mettre à jour les angles à partir de la scène actuelle
  observeEvent(input$update, {
    updateAngles()
  })

  # Afficher les valeurs de alpha, beta et gamma en temps réel
  output$current_angles <- renderText({
    paste(
      "Alpha :", round(angles$alpha, 2), "degrés |",
      "Beta :", round(angles$beta, 2), "degrés |",
      "Gamma :", round(angles$gamma, 2), "degrés"
    )
  })
}

shinyApp(ui = ui, server = server)
