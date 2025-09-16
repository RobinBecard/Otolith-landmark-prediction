# Application Shiny pour la visualisation et manipulation d'objets 3D PLY
# avec Plotly, permettant des mises à jour fluides via plotlyProxy

library(shiny)
library(plotly)
library(colourpicker) # Pour les sélecteurs de couleur
library(rgl)
library(Rvcg)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Visualisation et Manipulation d'Objets 3D PLY"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fichier_ply", "Sélectionner un fichier PLY",
        accept = c(".ply")
      ),

      # Panneau apparaissant une fois le fichier chargé
      conditionalPanel(
        condition = "output.fichier_charge == true",
        h4("Propriétés de l'objet"),
        verbatimTextOutput("info_objet"),
        hr(),
        h4("Couleurs"),
        colourInput("couleur_objet", "Couleur de l'objet 3D", "#1E88E5"),
        colourInput("couleur_spheres", "Couleur des sphères", "#FF5722"),
        hr(),
        h4("Manipulation de la caméra"),
        sliderInput("camera_eye_x", "Position X", min = -3, max = 3, value = 1.25, step = 0.1),
        sliderInput("camera_eye_y", "Position Y", min = -3, max = 3, value = 1.25, step = 0.1),
        sliderInput("camera_eye_z", "Position Z", min = -3, max = 3, value = 1.25, step = 0.1),
        actionButton("appliquer_camera", "Appliquer vue",
          icon = icon("camera"),
          class = "btn-primary"
        ),
        hr(),
        h4("Sphères"),
        numericInput("rayon_sphere", "Rayon des sphères:", 0.05, min = 0.01, max = 1, step = 0.01),
        actionButton("ajouter_sphere", "Ajouter une sphère aléatoire",
          icon = icon("plus-circle"),
          class = "btn-success"
        ),
        actionButton("supprimer_spheres", "Supprimer toutes les sphères",
          icon = icon("trash"),
          class = "btn-danger"
        ),
        hr(),
        h4("Affichage"),
        checkboxInput("afficher_volume", "Afficher le volume", value = TRUE),
        checkboxInput("afficher_axes", "Afficher les axes", value = TRUE),
        sliderInput("opacite_objet", "Opacité de l'objet:",
          min = 0, max = 1, value = 0.9, step = 0.1
        )
      )
    ),
    mainPanel(
      plotlyOutput("plot3d", height = "600px")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Variables réactives
  rv <- reactiveValues(
    mesh = NULL,
    spheres = data.frame(x = numeric(0), y = numeric(0), z = numeric(0)),
    volume = NULL,
    surface = NULL,
    fig_initialisee = FALSE
  )

  # Fonction pour obtenir les limites de la boîte englobante
  get_bbox <- reactive({
    req(rv$mesh)
    vertices <- t(rv$mesh$vb[1:3, ] / rv$mesh$vb[4, ])
    list(
      x = range(vertices[, 1]),
      y = range(vertices[, 2]),
      z = range(vertices[, 3])
    )
  })

  # Indicateur si un fichier a été chargé
  output$fichier_charge <- reactive({
    return(!is.null(rv$mesh))
  })
  outputOptions(output, "fichier_charge", suspendWhenHidden = FALSE)

  # Chargement du fichier PLY
  observeEvent(input$fichier_ply, {
    # Message de chargement
    showNotification("Chargement du fichier PLY...", type = "message", duration = NULL, id = "loading")

    # Chargement avec gestion d'erreur
    tryCatch(
      {
        rv$mesh <- Rvcg::vcgImport(input$fichier_ply$datapath)

        # Calcul des propriétés
        rv$volume <- Rvcg::vcgVolume(rv$mesh)
        rv$surface <- Rvcg::vcgArea(rv$mesh)

        # Réinitialiser les sphères si on charge un nouveau fichier
        rv$spheres <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
        rv$fig_initialisee <- FALSE

        # Notification de succès
        removeNotification(id = "loading")
        showNotification("Fichier PLY chargé avec succès!", type = "message")
      },
      error = function(e) {
        removeNotification(id = "loading")
        showNotification(paste("Erreur lors du chargement:", e$message), type = "error")
      }
    )
  })

  # Affichage des informations sur l'objet
  output$info_objet <- renderText({
    req(rv$mesh)

    # Calcul des propriétés
    nb_vertices <- ncol(rv$mesh$vb)
    nb_faces <- ncol(rv$mesh$it)

    # Formatage du texte
    paste0(
      "Nom du fichier: ", input$fichier_ply$name, "\n",
      "Nb de sommets: ", nb_vertices, "\n",
      "Nb de faces: ", nb_faces, "\n",
      "Volume: ", round(rv$volume, 2), " unités³\n",
      "Surface: ", round(rv$surface, 2), " unités²"
    )
  })

  # Création initiale de la visualisation 3D
  output$plot3d <- renderPlotly({
    req(rv$mesh)

    # Extraire les vertices et faces du mesh
    vertices <- t(rv$mesh$vb[1:3, ] / rv$mesh$vb[4, ])
    faces <- t(rv$mesh$it)

    # Créer la figure Plotly
    fig <- plot_ly() %>%
      add_trace(
        type = "mesh3d",
        x = vertices[, 1], y = vertices[, 2], z = vertices[, 3],
        i = faces[, 1] - 1, j = faces[, 2] - 1, k = faces[, 3] - 1, # -1 car Plotly utilise un index à base 0
        facecolor = rep(input$couleur_objet, nrow(faces)),
        opacity = input$opacite_objet,
        name = "Objet 3D"
      )

    # Ajouter les annotations si demandé
    if (input$afficher_volume) {
      fig <- fig %>%
        layout(
          scene = list(
            annotations = list(
              list(
                x = get_bbox()$x[1],
                y = get_bbox()$y[1],
                z = get_bbox()$z[2],
                text = paste("Volume:", round(rv$volume, 2), "unités³"),
                showarrow = FALSE,
                font = list(color = "black", size = 12)
              )
            )
          )
        )
    }

    # Configuration de la scène
    fig <- fig %>%
      layout(
        scene = list(
          aspectmode = "data",
          camera = list(
            eye = list(
              x = input$camera_eye_x,
              y = input$camera_eye_y,
              z = input$camera_eye_z
            )
          ),
          xaxis = list(showticklabels = input$afficher_axes),
          yaxis = list(showticklabels = input$afficher_axes),
          zaxis = list(showticklabels = input$afficher_axes)
        ),
        margin = list(l = 0, r = 0, b = 0, t = 30)
      )

    rv$fig_initialisee <- TRUE
    return(fig)
  })

  # Ajout d'une sphère aléatoire
  observeEvent(input$ajouter_sphere, {
    req(rv$mesh, rv$fig_initialisee)

    # Obtenez la boîte englobante
    bbox <- get_bbox()

    # Générer une position aléatoire à l'intérieur de la boîte englobante
    nouvelle_sphere <- data.frame(
      x = runif(1, bbox$x[1], bbox$x[2]),
      y = runif(1, bbox$y[1], bbox$y[2]),
      z = runif(1, bbox$z[1], bbox$z[2])
    )

    # Ajouter à la liste des sphères
    rv$spheres <- rbind(rv$spheres, nouvelle_sphere)

    # Créer les coordonnées pour une sphère
    rayon <- input$rayon_sphere
    theta <- seq(0, 2 * pi, length.out = 20)
    phi <- seq(0, pi, length.out = 20)
    sphere_x <- nouvelle_sphere$x + rayon * outer(cos(theta), sin(phi))
    sphere_y <- nouvelle_sphere$y + rayon * outer(sin(theta), sin(phi))
    sphere_z <- nouvelle_sphere$z + rayon * outer(rep(1, length(theta)), cos(phi))

    # Ajouter la sphère avec plotlyProxy sans redessiner toute la figure
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("addTraces", list(
        type = "surface",
        x = sphere_x,
        y = sphere_y,
        z = sphere_z,
        colorscale = list(list(0, input$couleur_spheres), list(1, input$couleur_spheres)),
        showscale = FALSE,
        name = paste("Sphère", nrow(rv$spheres))
      ))

    # Notification
    showNotification(
      paste(
        "Sphère ajoutée à la position (",
        round(nouvelle_sphere$x, 2), ", ",
        round(nouvelle_sphere$y, 2), ", ",
        round(nouvelle_sphere$z, 2), ")"
      ),
      type = "message"
    )
  })

  # Supprimer toutes les sphères
  observeEvent(input$supprimer_spheres, {
    req(rv$fig_initialisee, nrow(rv$spheres) > 0)

    # Supprimer toutes les traces sauf la première (l'objet 3D)
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("deleteTraces", seq(1, nrow(rv$spheres)))

    # Réinitialiser la liste des sphères
    rv$spheres <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))

    # Notification
    showNotification("Toutes les sphères ont été supprimées", type = "message")
  })

  # Mise à jour de la couleur de l'objet 3D
  observeEvent(input$couleur_objet, {
    req(rv$mesh, rv$fig_initialisee)

    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke(
        "restyle",
        list(facecolor = rep(input$couleur_objet, nrow(t(rv$mesh$it)))),
        list(0)
      ) # 0 correspond à la première trace (l'objet 3D)
  })

  # Mise à jour de l'opacité de l'objet 3D
  observeEvent(input$opacite_objet, {
    req(rv$mesh, rv$fig_initialisee)

    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke(
        "restyle",
        list(opacity = input$opacite_objet),
        list(0)
      ) # 0 correspond à la première trace (l'objet 3D)
  })

  # Mise à jour de la couleur des sphères
  observeEvent(input$couleur_spheres, {
    req(rv$fig_initialisee, nrow(rv$spheres) > 0)

    for (i in 1:nrow(rv$spheres)) {
      plotlyProxy("plot3d", session) %>%
        plotlyProxyInvoke(
          "restyle",
          list(colorscale = list(
            list(0, input$couleur_spheres),
            list(1, input$couleur_spheres)
          )),
          list(i)
        ) # indice de la sphère
    }
  })

  # Application des paramètres de la caméra
  observeEvent(input$appliquer_camera, {
    req(rv$fig_initialisee)

    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("relayout", list(
        "scene.camera" = list(
          eye = list(
            x = input$camera_eye_x,
            y = input$camera_eye_y,
            z = input$camera_eye_z
          )
        )
      ))

    # Notification
    showNotification("Orientation de la caméra appliquée", type = "message")
  })

  # Affichage/masquage des axes
  observeEvent(input$afficher_axes, {
    req(rv$fig_initialisee)

    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("relayout", list(
        "scene.xaxis.showticklabels" = input$afficher_axes,
        "scene.yaxis.showticklabels" = input$afficher_axes,
        "scene.zaxis.showticklabels" = input$afficher_axes
      ))
  })

  # Affichage/masquage du volume
  observeEvent(input$afficher_volume, {
    req(rv$fig_initialisee)

    if (input$afficher_volume) {
      # Ajouter l'annotation
      plotlyProxy("plot3d", session) %>%
        plotlyProxyInvoke("relayout", list(
          "scene.annotations[0]" = list(
            x = get_bbox()$x[1],
            y = get_bbox()$y[1],
            z = get_bbox()$z[2],
            text = paste("Volume:", round(rv$volume, 2), "unités³"),
            showarrow = FALSE,
            font = list(color = "black", size = 12)
          )
        ))
    } else {
      # Supprimer l'annotation
      plotlyProxy("plot3d", session) %>%
        plotlyProxyInvoke("relayout", list(
          "scene.annotations" = list()
        ))
    }
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
