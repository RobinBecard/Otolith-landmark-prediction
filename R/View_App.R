# Chargement des dépendances
source("../Setup_ViewApp.R")
source("./utils.R")
source("./utils_viewApp.R")

# Interface utilisateur
ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
    html, body {
      height: 100%;
      margin: 0;
      padding: 0;
      overflow: hidden;
    }
    .container-fluid {
      height: 100vh;
      padding: 0;
      overflow: hidden;
    }
    .sidebar-panel {
      height: 100vh;
      overflow-y: auto;
      padding: 15px;
      border-right: 1px solid #ddd;
    }
    .main-panel {
      height: 100vh;
      padding: 0;
      overflow: hidden;
    }
    .tab-content {
      height: calc(100vh - 42px); /* Hauteur totale - hauteur des onglets */
      overflow: hidden;
    }
    .app-title {
      font-size: 24px;
      font-weight: bold;
      margin-bottom: 20px;
      text-align: center;
      border-bottom: 1px solid #ddd;
      padding-bottom: 10px;
    }
    .grid-container {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 10px;
      height: 100%;
      padding: 10px;
      box-sizing: border-box;
    }
    .grid-item {
      background: white;
      border: 1px solid #ddd;
      border-radius: 5px;
      overflow: hidden;
      display: flex;
      flex-direction: column;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    .figure-title {
      text-align: center;
      font-weight: bold;
      padding: 8px;
      margin: 0;
      background: #f8f9fa;
      border-bottom: 1px solid #ddd;
    }
    .plot-area {
      flex: 1;
      min-height: 0; /* Important pour que flex fonctionne correctement */
    }
    .descripteurs-container {
      padding: 15px;
      height: 100%;
      overflow: auto;
    }
    .btn-group {
      display: flex;
      flex-wrap: wrap;
      gap: 5px;
      margin-bottom: 15px;
    }
    .btn-group .btn {
      flex: 1;
      min-width: 80px;
    }
    /* Style pour les sélecteurs de couleur */
    .color-selectors {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      margin-bottom: 15px;
    }
    .color-selector {
      flex: 1;
      min-width: calc(50% - 5px);
    }
    .shiny-input-container {
      width: 100% !important;
    }
    .welcome-container {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100vh;
      text-align: center;
      padding: 20px;
    }
    .welcome-icon {
      font-size: 80px;
      color: #ccc;
      margin-bottom: 20px;
    }
    .welcome-message {
      font-size: 24px;
      margin-bottom: 20px;
      color: #555;
    }
    .welcome-instructions {
      font-size: 18px;
      max-width: 600px;
      color: #777;
    }
    #mainPanelContent {
      display: none; /* Caché par défaut */
      height: 100%;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      div(class = "app-title", "Analyse d'otolithes"),
      h4("Sélection de fichier"),
      fileInput(
        "fileChosen",
        "Sélectionnez un fichier PLY (ASCII):",
        multiple = FALSE,
        accept = ".ply",
        buttonLabel = "Parcourir...",
        placeholder = "Aucun fichier sélectionné"
      ),
      hr(),
      h4("Vues prédéfinies"),
      div(
        class = "btn-group",
        actionButton("view_iso", "Isométrique", class = "btn btn-outline-primary"),
        actionButton("view_top", "Dessus", class = "btn btn-outline-primary"),
        actionButton("view_front", "Face", class = "btn btn-outline-primary")
      ),
      h4("Affichage"),
      div(
        class = "color-selectors",
        div(
          class = "color-selector",
          colourInput("couleur", "Objet 3D", "#1E88E5")
        ),
        div(
          class = "color-selector",
          colourInput("couleur_points", "Points", "#FF0000")
        )
      ),
      sliderInput("opacite_objet", "Opacité de l'objet:",
        min = 0, max = 1, value = 0.9, step = 0.1
      ),
      checkboxInput("afficher_points_principal", "Afficher points vue principale", value = FALSE),
      width = 3
    ),
    mainPanel(
      div(
        id = "welcomeMessage", class = "welcome-container",
        div(class = "welcome-icon", icon("file-import", "fa-5x")),
        div(class = "welcome-message", "Bienvenue dans l'application d'analyse d'otolithes"),
        div(
          class = "welcome-instructions",
          "Veuillez sélectionner un fichier PLY dans le panneau de gauche pour commencer l'analyse."
        )
      ),
      div(
        id = "mainPanelContent", class = "main-panel",
        tabsetPanel(
          tabPanel(
            "Affichages",
            div(
              class = "grid-container",
              # Vue principale
              div(
                class = "grid-item",
                h5("Vue 3D de l'otolithe", class = "figure-title"),
                div(class = "plot-area", plotlyOutput("main", height = "100%"))
              ),
              # Vue interne
              div(
                class = "grid-item",
                h5("Face Interne", class = "figure-title"),
                div(class = "plot-area", plotlyOutput("internal", height = "100%"))
              ),
              # Vue externe
              div(
                class = "grid-item",
                h5("Face Externe", class = "figure-title"),
                div(class = "plot-area", plotlyOutput("external", height = "100%"))
              )
            )
          ),
          tabPanel(
            "Descripteurs",
            div(
              class = "descripteurs-container",
              h4("Informations sur l'objet 3D"),
              verbatimTextOutput("descripteur"),
              hr(),
              h4("Points de référence"),
              DTOutput("table_points")
            )
          )
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  # Variables réactives
  rv <- reactiveValues(
    specimen_data = NULL,
    points_data = NULL,
    volume = NULL,
    vertices = NULL,
    faces = NULL,
    plots = list(), # Stockage pour les graphiques
    visual_params = list(
      couleur_points = "#eeff00",
      opacite = 1,
      afficher_points_principal = TRUE
    ),
    plots_rendered = FALSE, # pour savoir si les plots ont été rendus
  )

  # Observer lorsque le fichier est choisi
  observeEvent(input$fileChosen, {
    # Afficher le panneau principal et cacher le message de bienvenue
    shinyjs::hide("welcomeMessage")
    shinyjs::show("mainPanelContent")

    # Réinitialiser les variables
    rv$plots_rendered <- FALSE

    # Récupérer les données
    rv$specimen_data <- read.ply(input$fileChosen$datapath, ShowSpecimen = FALSE, addNormals = TRUE)
    rv$vertices <- t(rv$specimen_data$vb[1:3, ] / rv$specimen_data$vb[4, ])
    rv$faces <- t(rv$specimen_data$it)
    rv$volume <- calculate_volume(rv$specimen_data)
    rv$file_name <- input$fileChosen$name

    if (rv$file_name != "") {
      file_id <- sub(".ply", "", rv$file_name)
      rv$points_data <- read_points(file_id)
    }

    # Stocker les paramètres visuels actuels
    rv$visual_params$couleur <- input$couleur
    rv$visual_params$couleur_points <- input$couleur_points
    rv$visual_params$opacite <- input$opacite_objet
    rv$visual_params$afficher_points_principal <- input$afficher_points_principal

    # Générer les graphiques initiaux
    rv$plots$main <- create_main_view(rv)
    rv$plots$internal <- create_internal_view(rv)
    rv$plots$external <- create_external_view(rv)

    # Mettre à jour l'affichage avec configuration spécifique pour chaque vue
    output$main <- renderPlotly({
      rv$plots$main %>%
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("sendDataToCloud", "autoScale2d")
        )
    })

    output$internal <- renderPlotly({
      rv$plots$internal %>%
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "sendDataToCloud", "orbitRotation",
            "resetCameraLastSave3d", "hoverClosest3d",
            "tableRotation", "resetCameraDefault3d"
          ),
          displayModeBar = TRUE
        )
    })

    output$external <- renderPlotly({
      rv$plots$external %>%
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "sendDataToCloud", "orbitRotation",
            "resetCameraLastSave3d", "hoverClosest3d",
            "tableRotation", "resetCameraDefault3d"
          ),
          displayModeBar = TRUE
        )
    })

    # Configuration des axes
    axes_config <- list(
      showaxes = input$afficher_axes,
      showgrid = input$afficher_axes,
      showline = input$afficher_axes,
      linecolor = ifelse(input$afficher_axes, "black", "transparent"),
      gridcolor = ifelse(input$afficher_axes, "#eeeeee", "transparent"),
      zerolinecolor = ifelse(input$afficher_axes, "#dddddd", "transparent")
    )

    # Marquer les plots comme rendus après un délai pour l'initialisation complète
    shinyjs::delay(200, {
      rv$plots_rendered <- TRUE
    })

    # Appliquer les axes
    for (plot_id in c("main", "internal", "external")) {
      plotlyProxy(plot_id, session) %>%
        plotlyProxyInvoke("relayout", list(
          "scene.xaxis" = axes_config,
          "scene.yaxis" = axes_config,
          "scene.zaxis" = axes_config
        ))
    }

    # Texte du volume
    volume_text <- ifelse(input$afficher_volume,
      paste0("Volume: ", sprintf("%.2f μm³", rv$volume)),
      ""
    )

    # Mettre à jour l'annotation sur la vue principale
    plotlyProxy("main", session) %>%
      plotlyProxyInvoke("relayout", list(
        "annotations[0].text" = volume_text
      ))
  })

  observe({
    req(rv$plots_rendered) # Attendre que les plots soient rendus
    req(rv$points_data)


    if (nrow(rv$points_data) >= 3) {
      camera_orientation <- calculate_camera_orientation(rv$points_data)

      # Mettre à jour l'orientation des vues interne et externe
      plotlyProxy("internal", session) %>%
        plotlyProxyInvoke("relayout", list(
          "scene.camera.eye" = camera_orientation$internal,
          "scene.camera.up" = list(x = 0, y = 0, z = 1)
        ))

      # Mettre à jour l'orientation de la vue externe
      plotlyProxy("external", session) %>%
        plotlyProxyInvoke("relayout", list(
          "scene.camera.eye" = camera_orientation$external,
          "scene.camera.up" = list(x = 0, y = 0, z = 1)
        ))
    }
  })

  # Gérer l'affichage initial ou quand on supprime le fichier
  observe({
    if (is.null(input$fileChosen)) {
      shinyjs::show("welcomeMessage")
      shinyjs::hide("mainPanelContent")
    }
  })

  # Observer les différents styles
  observeEvent(input$couleur, {
    req(rv$specimen_data)
    rv$visual_params$couleur <- input$couleur
    style_all_plots()
  })
  observeEvent(input$opacite_objet, {
    req(rv$specimen_data)
    rv$visual_params$opacite <- input$opacite_objet
    style_all_plots()
  })
  observeEvent(input$couleur_points, {
    req(rv$specimen_data)
    rv$visual_params$couleur_points <- input$couleur_points
    update_points_color()
  })

  # Observer pour l'affichage des points sur la vue principale
  observeEvent(input$afficher_points_principal, {
    req(rv$specimen_data, rv$points_data)

    # Mettre à jour le paramètre
    rv$visual_params$afficher_points_principal <- input$afficher_points_principal

    if (input$afficher_points_principal) {
      # Ajouter les points à la vue principale
      if (!is.null(rv$points_data) && nrow(rv$points_data) > 0) {
        plotlyProxy("main", session) %>%
          plotlyProxyInvoke("addTraces", list(
            x = rv$points_data[, 1],
            y = rv$points_data[, 2],
            z = rv$points_data[, 3],
            type = "scatter3d",
            mode = "markers+text",
            marker = list(
              size = 5,
              color = rv$visual_params$couleur_points,
              line = list(color = "black", width = 0.5)
            ),
            text = paste0("P", seq_len(nrow(rv$points_data))),
            textposition = "top center",
            hoverinfo = "text",
            name = "Points de référence"
          ))
      }
    } else {
      # Supprimer les points de la vue principale (indice 1 = deuxième trace)
      plotlyProxy("main", session) %>%
        plotlyProxyInvoke("deleteTraces", 1)
    }
  })

  update_points_color <- function() {
    # Vérifier que les points existent
    if (is.null(rv$points_data) || nrow(rv$points_data) == 0) {
      return()
    }

    # Créer la configuration de marqueur avec la nouvelle couleur
    marker_config <- list(
      color = rv$visual_params$couleur_points,
      size = 5,
      line = list(color = "black", width = 0.5)
    )

    # Mettre à jour la couleur des points dans les vues interne et externe
    for (plot_id in c("internal", "external")) {
      plotlyProxy(plot_id, session) %>%
        plotlyProxyInvoke(
          "restyle",
          list(marker = marker_config),
          list(1) # Les points sont la deuxième trace (indice 1)
        )
    }
  }

  # Fonction pour appliquer le style à tous les plots sans re-rendu
  style_all_plots <- function() {
    # Vérifier que les plots existent
    if (is.null(rv$plots$main) || is.null(rv$plots$internal) || is.null(rv$plots$external)) {
      return()
    }

    # Créer une liste de couleurs pour chaque face
    face_colors <- rep(rv$visual_params$couleur, nrow(rv$faces))

    # Mettre à jour la couleur et l'opacité pour chaque graphique
    for (plot_id in c("main", "internal", "external")) {
      plotlyProxy(plot_id, session) %>%
        plotlyProxyInvoke(
          "restyle",
          list(
            facecolor = list(face_colors),
            opacity = rv$visual_params$opacite
          ),
          list(0)
        )
    }

    # Si nous avons aussi des points, mettre à jour leur couleur
    if (!is.null(rv$points_data) && nrow(rv$points_data) > 0) {
      update_points_color()
    }
  }

  # Affichage des descripteurs
  output$descripteur <- renderText({
    if (is.null(rv$specimen_data)) {
      return("Aucun spécimen chargé")
    }

    paste(
      "Nom du fichier:", rv$file_name, "\n\n",
      "Volume:", sprintf("%.2f μm³", rv$volume), "\n",
      "Nombre de vertices:", nrow(rv$vertices), "\n",
      "Nombre de faces:", nrow(rv$faces), "\n",
      "Dimensions (min/max):", "\n",
      "  X:", sprintf("%.2f / %.2f", min(rv$vertices[, 1]), max(rv$vertices[, 1])), "\n",
      "  Y:", sprintf("%.2f / %.2f", min(rv$vertices[, 2]), max(rv$vertices[, 2])), "\n",
      "  Z:", sprintf("%.2f / %.2f", min(rv$vertices[, 3]), max(rv$vertices[, 3]))
    )
  })

  # Tableau des points de référence
  output$table_points <- renderDT({
    req(rv$points_data)
    if (is.null(rv$points_data) || nrow(rv$points_data) == 0) {
      return(NULL)
    }

    points_df <- as.data.frame(rv$points_data)
    colnames(points_df) <- c("X", "Y", "Z")
    points_df <- cbind(Point = paste0("P", 1:nrow(points_df)), points_df)

    datatable(
      points_df,
      options = list(
        pageLength = 10,
        dom = "tip", # Table, information, pagination (pas de recherche)
        ordering = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    ) %>%
      formatRound(columns = c("X", "Y", "Z"), digits = 4)
  })

  # Observer pour les boutons de vue
  observeEvent(input$view_iso, {
    req(rv$specimen_data)
    plotlyProxy("main", session) %>%
      plotlyProxyInvoke("relayout", list(
        "scene.camera.eye" = list(x = 1.5, y = 1.5, z = 1.5)
      ))
  })

  observeEvent(input$view_top, {
    req(rv$specimen_data)
    plotlyProxy("main", session) %>%
      plotlyProxyInvoke("relayout", list(
        "scene.camera.eye" = list(x = 0, y = 0, z = 2.5)
      ))
  })

  observeEvent(input$view_front, {
    req(rv$specimen_data)
    plotlyProxy("main", session) %>%
      plotlyProxyInvoke("relayout", list(
        "scene.camera.eye" = list(x = 0, y = -2.5, z = 0)
      ))
  })
}

shinyApp(ui = ui, server = server)
