# Programme complet pour visualisation et modification interactive d'objets 3D PLY
# avec Plotly et R

# Installation des packages nécessaires (à décommenter si besoin)
# install.packages(c("plotly", "rgl", "Rvcg", "shiny"))

library(plotly)
library(rgl)
library(Rvcg)
library(shiny)

# Fonction principale pour lancer l'application
run_3d_viewer <- function(ply_file_path) {
  # Vérifier si le fichier existe
  if (!file.exists(ply_file_path)) {
    stop("Fichier PLY non trouvé: ", ply_file_path)
  }

  # Charger le fichier PLY
  message("Chargement du fichier PLY...")
  mesh <- vcgImport(ply_file_path)

  # Extraire les coordonnées pour plotly
  vertices <- mesh$vb[1:3, ]
  faces <- mesh$it

  # Préparer les données pour plotly
  x <- vertices[1, ]
  y <- vertices[2, ]
  z <- vertices[3, ]
  i <- faces[1, ] - 1 # Les indices dans plotly commencent à 0
  j <- faces[2, ] - 1
  k <- faces[3, ] - 1

  # Calculer les bornes de l'objet pour le positionnement des sphères
  x_range <- max(x) - min(x)
  y_range <- max(y) - min(y)
  z_range <- max(z) - min(z)
  max_range <- max(c(x_range, y_range, z_range))
  center_x <- mean(range(x))
  center_y <- mean(range(y))
  center_z <- mean(range(z))

  # Définir les positions des sphères autour de l'objet
  sphere_radius <- max_range * 0.05
  sphere_distance <- max_range * 0.6

  # Positions des sphères aux coins d'un cube virtuel autour de l'objet
  sphere_positions <- data.frame(
    x = c(
      center_x - sphere_distance, center_x + sphere_distance,
      center_x - sphere_distance, center_x + sphere_distance,
      center_x - sphere_distance, center_x + sphere_distance,
      center_x - sphere_distance, center_x + sphere_distance
    ),
    y = c(
      center_y - sphere_distance, center_y - sphere_distance,
      center_y + sphere_distance, center_y + sphere_distance,
      center_y - sphere_distance, center_y - sphere_distance,
      center_y + sphere_distance, center_y + sphere_distance
    ),
    z = c(
      center_z - sphere_distance, center_z - sphere_distance,
      center_z - sphere_distance, center_z - sphere_distance,
      center_z + sphere_distance, center_z + sphere_distance,
      center_z + sphere_distance, center_z + sphere_distance
    )
  )

  # Calculer le volume de l'objet
  volume <- vcgVolume(mesh)
  surface_area <- vcgArea(mesh)

  # Interface utilisateur Shiny
  ui <- fluidPage(
    titlePanel(paste("Visualisation 3D interactive -", basename(ply_file_path))),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # Contrôles pour l'objet principal
        h4("Propriétés de l'objet principal"),
        selectInput("obj_color", "Couleur:",
          choices = c("blue", "red", "green", "yellow", "purple", "cyan", "magenta", "orange"),
          selected = "blue"
        ),
        sliderInput("obj_opacity", "Opacité:", min = 0.1, max = 1, value = 0.8, step = 0.05),
        hr(),

        # Contrôles pour les sphères
        h4("Propriétés des sphères"),
        selectInput("sphere_color", "Couleur:",
          choices = c("red", "blue", "green", "yellow", "purple", "cyan", "magenta", "orange"),
          selected = "red"
        ),
        sliderInput("sphere_opacity", "Opacité:", min = 0.1, max = 1, value = 0.6, step = 0.05),
        sliderInput("sphere_size", "Taille:", min = 0.5, max = 5, value = 1, step = 0.1),
        checkboxInput("show_spheres", "Afficher les sphères", value = TRUE),
        hr(),

        # Contrôles pour l'angle de vue
        h4("Angle de vue"),
        sliderInput("camera_eye_x", "Eye X:", min = -3, max = 3, value = 1.25, step = 0.05),
        sliderInput("camera_eye_y", "Eye Y:", min = -3, max = 3, value = 1.25, step = 0.05),
        sliderInput("camera_eye_z", "Eye Z:", min = -3, max = 3, value = 1.25, step = 0.05),
        hr(),

        # Vues prédéfinies
        h4("Vues prédéfinies"),
        actionButton("view_iso", "Isométrique"),
        actionButton("view_top", "Dessus"),
        actionButton("view_front", "Face"),
        actionButton("view_side", "Côté"),
        hr(),

        # Options d'affichage
        h4("Options d'affichage"),
        checkboxInput("show_info", "Afficher les informations", value = TRUE),
        checkboxInput("show_axes", "Afficher les axes", value = TRUE),
        hr(),

        # Exporter la vue actuelle
        actionButton("export_view", "Exporter la vue actuelle", icon = icon("download"))
      ),
      mainPanel(
        width = 9,
        # Zone d'affichage du graphique
        plotlyOutput("plot3d", height = "700px"),

        # Zone d'informations
        conditionalPanel(
          condition = "input.show_info == true",
          wellPanel(
            h4("Informations sur l'objet"),
            verbatimTextOutput("info_panel")
          )
        )
      )
    )
  )

  # Logique serveur Shiny
  server <- function(input, output, session) {
    # Fonction réactive pour créer les sphères
    generate_spheres <- reactive({
      spheres_list <- list()

      if (input$show_spheres) {
        for (i in 1:nrow(sphere_positions)) {
          # Générer les points pour une sphère
          phi <- seq(0, 2 * pi, length.out = 20)
          theta <- seq(0, pi, length.out = 10)

          # Matrice pour les coordonnées
          sphere_x <- matrix(0, 20, 10)
          sphere_y <- matrix(0, 20, 10)
          sphere_z <- matrix(0, 20, 10)

          # Calculer les coordonnées de la sphère
          for (p in 1:20) {
            for (t in 1:10) {
              sphere_x[p, t] <- sphere_positions$x[i] + input$sphere_size * sphere_radius * sin(theta[t]) * cos(phi[p])
              sphere_y[p, t] <- sphere_positions$y[i] + input$sphere_size * sphere_radius * sin(theta[t]) * sin(phi[p])
              sphere_z[p, t] <- sphere_positions$z[i] + input$sphere_size * sphere_radius * cos(theta[t])
            }
          }

          # Ajouter la sphère à la liste
          spheres_list[[i]] <- list(
            x = as.vector(sphere_x),
            y = as.vector(sphere_y),
            z = as.vector(sphere_z),
            type = "mesh3d",
            opacity = input$sphere_opacity,
            color = input$sphere_color,
            name = paste("Sphère", i),
            hoverinfo = "name",
            showlegend = FALSE
          )
        }
      }

      return(spheres_list)
    })

    # Rendu de la figure Plotly
    output$plot3d <- renderPlotly({
      # Créer la figure de base avec l'objet 3D
      fig <- plot_ly() %>%
        add_trace(
          type = "mesh3d",
          x = x, y = y, z = z,
          i = i, j = j, k = k,
          color = input$obj_color,
          opacity = input$obj_opacity,
          name = "Objet 3D",
          hoverinfo = "name"
        )

      # Ajouter les sphères si demandé
      if (input$show_spheres) {
        spheres <- generate_spheres()
        for (sphere in spheres) {
          fig <- fig %>% add_trace(
            type = sphere$type,
            x = sphere$x,
            y = sphere$y,
            z = sphere$z,
            opacity = sphere$opacity,
            color = sphere$color,
            name = sphere$name,
            hoverinfo = sphere$hoverinfo,
            showlegend = sphere$showlegend
          )
        }
      }

      # Ajouter des annotations avec les informations
      if (input$show_info) {
        fig <- fig %>% add_annotations(
          x = 0.02,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = paste("Volume:", round(volume, 2), "unités³"),
          showarrow = FALSE,
          align = "left",
          font = list(size = 14, color = "black"),
          bgcolor = "rgba(255, 255, 255, 0.7)",
          bordercolor = "rgba(0, 0, 0, 0.5)",
          borderwidth = 1,
          borderpad = 4
        )
      }

      # Configuration de la scène
      fig <- fig %>% layout(
        scene = list(
          aspectmode = "data",
          camera = list(
            eye = list(
              x = input$camera_eye_x,
              y = input$camera_eye_y,
              z = input$camera_eye_z
            )
          ),
          xaxis = list(
            title = "X",
            showgrid = input$show_axes,
            showline = input$show_axes,
            zeroline = input$show_axes,
            showticklabels = input$show_axes
          ),
          yaxis = list(
            title = "Y",
            showgrid = input$show_axes,
            showline = input$show_axes,
            zeroline = input$show_axes,
            showticklabels = input$show_axes
          ),
          zaxis = list(
            title = "Z",
            showgrid = input$show_axes,
            showline = input$show_axes,
            zeroline = input$show_axes,
            showticklabels = input$show_axes
          )
        ),
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )

      fig
    })

    # Panneau d'informations
    output$info_panel <- renderPrint({
      cat("Fichier PLY: ", basename(ply_file_path), "\n")
      cat("Nombre de sommets: ", length(x), "\n")
      cat("Nombre de faces: ", nrow(faces), "\n")
      cat("Volume: ", round(volume, 4), " unités³\n")
      cat("Surface: ", round(surface_area, 4), " unités²\n")
      cat("\n")
      cat("Position actuelle de la caméra:\n")
      cat("- X: ", input$camera_eye_x, "\n")
      cat("- Y: ", input$camera_eye_y, "\n")
      cat("- Z: ", input$camera_eye_z, "\n")
    })

    # Observateurs pour les boutons de vue prédéfinis
    observeEvent(input$view_iso, {
      updateSliderInput(session, "camera_eye_x", value = 1.25)
      updateSliderInput(session, "camera_eye_y", value = 1.25)
      updateSliderInput(session, "camera_eye_z", value = 1.25)
    })

    observeEvent(input$view_top, {
      updateSliderInput(session, "camera_eye_x", value = 0)
      updateSliderInput(session, "camera_eye_y", value = 0)
      updateSliderInput(session, "camera_eye_z", value = 2.5)
    })

    observeEvent(input$view_front, {
      updateSliderInput(session, "camera_eye_x", value = 0)
      updateSliderInput(session, "camera_eye_y", value = 2.5)
      updateSliderInput(session, "camera_eye_z", value = 0)
    })

    observeEvent(input$view_side, {
      updateSliderInput(session, "camera_eye_x", value = 2.5)
      updateSliderInput(session, "camera_eye_y", value = 0)
      updateSliderInput(session, "camera_eye_z", value = 0)
    })

    # Gestion de l'export
    observeEvent(input$export_view, {
      # Construire un nom de fichier pour l'export
      export_filename <- paste0(
        "export_",
        gsub("\\.ply$", "", basename(ply_file_path)),
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".html"
      )

      showModal(modalDialog(
        title = "Export de la vue",
        paste("La vue sera exportée dans le fichier:", export_filename),
        easyClose = TRUE,
        footer = tagList(
          actionButton("confirm_export", "Confirmer")
        )
      ))
    })

    observeEvent(input$confirm_export, {
      # Récupérer la figure actuelle
      fig <- plotlyProxy("plot3d", session)

      # Générer la figure pour l'export
      export_fig <- plot_ly() %>%
        add_trace(
          type = "mesh3d",
          x = x, y = y, z = z,
          i = i, j = j, k = k,
          color = input$obj_color,
          opacity = input$obj_opacity,
          name = "Objet 3D",
          hoverinfo = "name"
        )

      # Ajouter les sphères si demandé
      if (input$show_spheres) {
        spheres <- generate_spheres()
        for (sphere in spheres) {
          export_fig <- export_fig %>% add_trace(
            type = sphere$type,
            x = sphere$x,
            y = sphere$y,
            z = sphere$z,
            opacity = sphere$opacity,
            color = sphere$color,
            name = sphere$name,
            hoverinfo = sphere$hoverinfo,
            showlegend = sphere$showlegend
          )
        }
      }

      # Ajouter des annotations avec les informations
      if (input$show_info) {
        export_fig <- export_fig %>% add_annotations(
          x = 0.02,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = paste("Volume:", round(volume, 2), "unités³"),
          showarrow = FALSE,
          align = "left",
          font = list(size = 14, color = "black"),
          bgcolor = "rgba(255, 255, 255, 0.7)",
          bordercolor = "rgba(0, 0, 0, 0.5)",
          borderwidth = 1,
          borderpad = 4
        )
      }

      # Configuration de la scène pour l'export
      export_fig <- export_fig %>% layout(
        title = basename(ply_file_path),
        scene = list(
          aspectmode = "data",
          camera = list(
            eye = list(
              x = input$camera_eye_x,
              y = input$camera_eye_y,
              z = input$camera_eye_z
            )
          ),
          xaxis = list(
            title = "X",
            showgrid = input$show_axes,
            showline = input$show_axes,
            zeroline = input$show_axes,
            showticklabels = input$show_axes
          ),
          yaxis = list(
            title = "Y",
            showgrid = input$show_axes,
            showline = input$show_axes,
            zeroline = input$show_axes,
            showticklabels = input$show_axes
          ),
          zaxis = list(
            title = "Z",
            showgrid = input$show_axes,
            showline = input$show_axes,
            zeroline = input$show_axes,
            showticklabels = input$show_axes
          )
        )
      )

      # Nom de fichier pour l'export
      export_filename <- paste0(
        "export_",
        gsub("\\.ply$", "", basename(ply_file_path)),
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".html"
      )

      # Sauvegarder dans un fichier HTML
      htmlwidgets::saveWidget(as_widget(export_fig), export_filename)

      # Informer l'utilisateur
      showModal(modalDialog(
        title = "Export réussi",
        paste("Vue exportée avec succès dans le fichier:", export_filename),
        easyClose = TRUE
      ))
    })
  }

  # Lancer l'application Shiny
  shinyApp(ui = ui, server = server)
}

run_3d_viewer("Segmentation_01a-8_ASCII.ply")
