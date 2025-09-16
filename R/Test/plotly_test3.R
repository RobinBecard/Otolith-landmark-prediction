# Application Shiny pour la visualisation et manipulation d'objets 3D PLY
# avec Plotly, permettant des mises à jour fluides via plotlyProxy

library(shiny)
library(plotly)
library(colourpicker) # Pour les sélecteurs de couleur
library(rgl)
library(Rvcg)
library(geomorph)

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
    vertices = NULL, # Ajout de la variable vertices
    faces = NULL, # Ajout de la variable faces
    spheres = data.frame(x = numeric(0), y = numeric(0), z = numeric(0)),
    volume = NULL,
    surface = NULL,
    fig_initialisee = FALSE
  )

  # Indicateur si un fichier a été chargé
  output$fichier_charge <- reactive({
    return(!is.null(rv$mesh))
  })
  outputOptions(output, "fichier_charge", suspendWhenHidden = FALSE)

  # Chargement du fichier PLY avec geomorph
  observeEvent(input$fichier_ply, {
    # Message de chargement
    showNotification("Chargement du fichier PLY...", type = "message", duration = NULL, id = "loading")

    # Chargement avec gestion d'erreur
    tryCatch(
      {
        # Utiliser read.ply de geomorph
        ply_data <- read.ply(input$fichier_ply$datapath)

        # Stocker dans les variables réactives
        rv$mesh <- ply_data
        rv$vertices <- t(ply_data$vb[1:3, ] / ply_data$vb[4, ]) # Extraire les vertices
        rv$faces <- t(ply_data$it) # Extraire les faces
        rv$volume <- Rvcg::vcgVolume(ply_data)
        rv$surface <- Rvcg::vcgArea(ply_data)

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
    nb_vertices <- nrow(rv$vertices)
    nb_faces <- nrow(rv$faces)

    # Formatage du texte
    paste0(
      "Nom du fichier: ", input$fichier_ply$name, "\n",
      "Nb de sommets: ", nb_vertices, "\n",
      "Nb de faces: ", nb_faces, "\n",
      "Volume: ", round(rv$volume, 2), " unités³\n",
      "Surface: ", round(rv$surface, 2), " unités²"
    )
  })

  # Création de la visualisation 3D
  output$plot3d <- renderPlotly({
    req(rv$mesh, rv$vertices, rv$faces)

    # Créer un ensemble de triangles pour plotly
    i <- rv$faces[, 1] - 1 # Plotly utilise un index à base 0
    j <- rv$faces[, 2] - 1
    k <- rv$faces[, 3] - 1

    # Créer la figure Plotly avec le mesh 3D
    fig <- plot_ly() %>%
      add_trace(
        type = "mesh3d",
        x = rv$vertices[, 1],
        y = rv$vertices[, 2],
        z = rv$vertices[, 3],
        i = i,
        j = j,
        k = k,
        facecolor = rep(input$couleur_objet, length(i)),
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
  })

  # Supprimer toutes les sphères
  observeEvent(input$supprimer_spheres, {
    req(rv$fig_initialisee, nrow(rv$spheres) > 0)

    # Calculer les indices des sphères à supprimer (l'objet 3D est la trace 0)
    sphere_indices <- seq(1, nrow(rv$spheres))

    # Supprimer toutes les traces sauf la première (l'objet 3D)
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("deleteTraces", sphere_indices)

    # Réinitialiser la liste des sphères
    rv$spheres <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))

    # Notification
    showNotification("Toutes les sphères ont été supprimées", type = "message")
  })

  # Mise à jour de la couleur de l'objet 3D
  observeEvent(input$couleur_objet, {
    req(rv$mesh, rv$fig_initialisee, rv$faces)

    # Créer une liste de couleurs pour chaque face
    face_colors <- rep(input$couleur_objet, nrow(rv$faces))

    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke(
        "restyle",
        list(facecolor = face_colors),
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
        ) # indice de la sphère (i) car l'objet 3D est à l'indice 0
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
