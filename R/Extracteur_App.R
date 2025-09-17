source("../Setup_Extracteur.R")
source("./Landmark_correction.R")
source("./Landmarks_prediction_APP.R")
source("./utils_extracteur.R")
source("./utils.R")
library(DT) # Pour les tableaux interactifs

# CSS personnalisé pour le thème marin
marine_css <- "
.navbar {
  background-color: #1a5276;
  border-color: #154360;
}
.navbar-default .navbar-brand {
  color: #ffffff;
}
body {
  background-color: #eaf2f8;
  color: #154360;
}
.sidebar {
  background-color: #d4e6f1;
  border-right: 1px solid #a9cce3;
}
.well {
  background-color: #d4e6f1;
  border-color: #a9cce3;
}
.btn-primary {
  background-color: #2874a6;
  border-color: #1f618d;
}
.btn-primary:hover {
  background-color: #1f618d;
  border-color: #154360;
}
.title-panel {
  background-color: #2874a6;
  color: white;
  padding: 15px;
  margin-bottom: 20px;
  border-radius: 4px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
}
.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
  background-color: #85c1e9;
  color: #154360;
  font-weight: bold;
}
.instructions-panel {
  background-color: #d4e6f1;
  padding: 15px;
  border-radius: 4px;
  border-left: 5px solid #3498db;
}
.export-panel {
  background-color: #d4e6f1;
  padding: 15px;
  border-radius: 4px;
  margin-top: 15px;
  border-left: 5px solid #2ecc71;
}
.summary-panel {
  background-color: #d4e6f1;
  padding: 15px;
  border-radius: 4px;
  border-left: 5px solid #f39c12;
}
.landmarks-panel {
  background-color: #d4e6f1;
  padding: 15px;
  border-radius: 4px;
  border-left: 5px solid #3498db;
}
#file_summary {
  background-color: #ffffff;
  padding: 10px;
  border-radius: 4px;
  max-height: 400px;
  overflow-y: auto;
}
#dir_path {
  background-color: #ffffff;
  color: #154360;
}
"

# Icônes pour le thème marin
marine_icons <- tags$div(
  tags$style(HTML(marine_css)),
  tags$script(HTML('
    $(document).ready(function() {
      $("h3:contains(\'Sélection des fichiers\')").prepend("<i class=\'fa fa-ship\' style=\'margin-right: 10px; color: #2874a6;\'></i>");
      $("h3:contains(\'Export de landmarks\')").prepend("<i class=\'fa fa-save\' style=\'margin-right: 10px; color: #2874a6;\'></i>");
      $("h3:contains(\'Résumé\')").prepend("<i class=\'fa fa-list-alt\' style=\'margin-right: 10px; color: #2874a6;\'></i>");
      $("h3:contains(\'Instructions\')").prepend("<i class=\'fa fa-info-circle\' style=\'margin-right: 10px; color: #2874a6;\'></i>");
    });
  '))
)

# Interface utilisateur avec thème marin
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$style(HTML(marine_css)),
    tags$script(HTML('
      $(document).ready(function() {
        Shiny.addCustomMessageHandler("welcome-message", function(message) {
          if (!sessionStorage.getItem("welcomeShown")) {
            $("<div id=\'welcome-splash\' style=\'position:fixed; top:0; left:0; width:100%; height:100%; background-color:rgba(26, 82, 118, 0.95); z-index:9999; display:flex; justify-content:center; align-items:center; flex-direction:column; color:white; text-align:center;\'>" +
              "<h1><i class=\'fa fa-water\'></i> Bienvenue dans l\'Extracteur de Points Caractéristiques</h1>" +
              "<p style=\'font-size:1.2em; max-width:800px; margin:20px auto;\'>Cette application vous permet d\'analyser des modèles 3D de spécimens marins et d\'en extraire des points caractéristiques.</p>" +
              "<div style=\'margin:30px; transform:scale(1.5);\'>" +
              "<i class=\'fa fa-fish\' style=\'font-size:2em; margin:0 15px; animation:swim 3s infinite ease-in-out;\'></i>" +
              "<i class=\'fa fa-water\' style=\'font-size:1.5em; margin:0 15px; animation:wave 2s infinite ease-in-out;\'></i>" +
              "<i class=\'fa fa-anchor\' style=\'font-size:2em; margin:0 15px; animation:swing 4s infinite ease-in-out;\'></i>" +
              "</div>" +
              "<button class=\'btn btn-lg\' style=\'background-color:#fff; color:#154360; margin-top:20px; font-weight:bold; padding:10px 30px;\' onclick=\'$(\"#welcome-splash\").fadeOut(500); sessionStorage.setItem(\"welcomeShown\", \"true\");\'>" +
              "Commencer <i class=\'fa fa-arrow-right\'></i></button>" +
              "</div>").appendTo("body");

            // Animations CSS
            $("<style>" +
              "@keyframes swim { 0% { transform: translateX(-10px); } 50% { transform: translateX(10px); } 100% { transform: translateX(-10px); } }" +
              "@keyframes wave { 0% { transform: translateY(-5px); } 50% { transform: translateY(5px); } 100% { transform: translateY(-5px); } }" +
              "@keyframes swing { 0% { transform: rotate(-5deg); } 50% { transform: rotate(5deg); } 100% { transform: rotate(-5deg); } }" +
              "</style>").appendTo("head");
          }
        });
      });
    '))
  ),
  marine_icons,
  div(
    class = "title-panel",
    h2(tags$i(class = "fa fa-water", style = "margin-right: 10px;"), "Extracteur de Points Caractéristiques")
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "sidebar",
        h3("Sélection des fichiers"),
        tabsetPanel(
          id = "input_tabs",
          tabPanel(
            "Fichier(s)",
            div(
              style = "padding: 10px 0;",
              fileInput("ply_files", tags$span(tags$i(class = "fa fa-file-upload", style = "margin-right: 10px; color: #2874a6;"), "Fichiers PLY"),
                multiple = TRUE,
                accept = c(".ply")
              )
            )
          ),
          tabPanel(
            "Dossier",
            div(
              style = "padding: 10px 0;",
              shinyDirButton("dir", "Sélectionner un dossier",
                "Choisir un dossier contenant des fichiers PLY",
                class = "btn-primary icon-folder-open"
              ),
              div(style = "margin-top: 10px;", verbatimTextOutput("dir_path"))
            )
          )
        ),
        hr(style = "border-top: 1px solid #a9cce3;"),
        conditionalPanel(
          condition = "output.files_found == true",
          div(
            class = "export-panel",
            h3("Export de landmarks"),
            div(
              style = "background-color: white; padding: 10px; border-radius: 4px;",
              textInput("landmark_filename", tags$span(tags$i(class = "fa fa-file-alt", style = "margin-right: 10px; color: #2874a6;"), "Nom du fichier:"),
                value = "points.txt"
              )
            ),
            div(
              style = "margin-top: 15px; text-align: center;",
              downloadButton("download_landmarks", tags$span(tags$i(class = "fa fa-download", style = "margin-right: 5px;"), "Exporter landmarks"),
                class = "btn-primary btn-lg"
              )
            )
          )
        )
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.files_found == true",
        div(
          class = "summary-panel",
          h3("Résumé des traitements"),
          verbatimTextOutput("file_summary")
        )
      ),
      conditionalPanel(
        condition = "output.files_found == true",
        div(
          class = "landmarks-panel",
          style = "margin-top: 20px;",
          h3(tags$i(class = "fa fa-map-marker-alt", style = "margin-right: 10px; color: #2874a6;"), "Points caractéristiques"),
          div(
            style = "display: flex; margin-bottom: 10px;",
            div(
              style = "flex: 1;",
              selectInput("selected_file", "Sélectionner un fichier:", choices = NULL),
              # Ajouter un texte d'aide conditionnel
              conditionalPanel(
                condition = "output.processing_status == true",
                tags$div(
                  style = "color: #2874a6; font-style: italic; margin-top: -10px;",
                  "Sélection désactivée pendant l'analyse..."
                )
              )
            )
          ),
          div(
            style = "background-color: white; padding: 10px; border-radius: 4px; max-height: 400px; overflow-y: auto;",
            DT::dataTableOutput("landmarks_table")
          )
        )
      ),
      conditionalPanel(
        condition = "output.files_found != true",
        div(
          class = "instructions-panel",
          h3("Instructions"),
          tags$p("Cette application vous permet d'extraire les points caractéristiques depuis des fichiers PLY en utilisant un modèle de prédiction pour l'analyse d'otolithes."),
          tags$p("Pour commencer:"),
          tags$ol(
            tags$li(tags$b("Sélection de fichiers:"), " Choisissez un ou plusieurs fichiers PLY ou un dossier contenant des modèles 3D d'otolithes."),
            tags$li(tags$b("Suivi de progression:"), " Les fichiers sont analysés automatiquement un par un, et les résultats s'affichent dans le tableau."),
            tags$li(tags$b("Visualisation:"), " Sélectionnez n'importe quel fichier dans la liste pour voir ses points caractéristiques."),
            tags$li(tags$b("Exportation:"), " Sauvegardez les données dans un fichier texte pour utilisation ultérieure.")
          ),
          tags$div(style = "clear: both;")
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Configuration pour la sélection de dossier
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "dir", roots = volumes, session = session)

  # Variable réactive pour stocker les chemins des fichiers PLY et leurs noms originaux
  ply_files_data <- reactiveVal(data.frame(
    path = character(0),
    original_name = character(0),
    is_ascii = logical(0),
    ascii_path = character(0),
    stringsAsFactors = FALSE
  ))

  # Variables réactives pour stocker les landmarks extraits et les volumes
  landmarks_data <- reactiveVal(list())
  volumes_data <- reactiveVal(list())

  # Variable pour stocker les noms de fichiers associés aux IDs
  file_names <- reactiveVal(list())

  # Variable pour stocker l'index du fichier actuellement traité
  current_processing_index <- reactiveVal(0)

  # Variable pour indiquer si le traitement est en cours
  processing_in_progress <- reactiveVal(FALSE)

  selected_dir_path <- reactiveVal("")

  # Mise à jour de la liste déroulante des fichiers
  observe({
    fn_data <- file_names()
    if (length(fn_data) > 0) {
      # Obtenir les noms des fichiers
      file_choices <- unlist(fn_data)
      names(file_choices) <- names(fn_data)

      # Mettre à jour la liste déroulante
      updateSelectInput(session, "selected_file", choices = file_choices)
    }
  })

  # Tableau des landmarks pour le fichier sélectionné
  output$landmarks_table <- DT::renderDataTable({
    # Forcer la réactivité aux changements des landmarks et de l'index actuel
    lm_data <- landmarks_data()
    current_idx <- current_processing_index()

    # Vérifier si des fichiers sont disponibles
    fn_data <- file_names()

    # Vérifier si un fichier est sélectionné
    if (is.null(input$selected_file) || length(lm_data) == 0) {
      return(NULL)
    }

    # Obtenir l'ID du fichier sélectionné
    file_id <- names(which(unlist(fn_data) == input$selected_file))

    if (length(file_id) == 0 || is.null(lm_data[[file_id]])) {
      return(NULL)
    }

    # Récupérer les landmarks du fichier sélectionné
    landmarks <- lm_data[[file_id]]

    # Créer un dataframe pour l'affichage
    landmarks_df <- as.data.frame(landmarks)
    colnames(landmarks_df) <- c("X", "Y", "Z")

    # Ajouter une colonne d'index pour numéroter les points
    landmarks_df <- cbind(Point = 1:nrow(landmarks_df), landmarks_df)

    # Retourner le tableau formaté
    DT::datatable(
      landmarks_df,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        dom = "tip" # Affiche uniquement la table, l'info et la pagination
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("X", "Y", "Z"), digits = 5)
  })

  observeEvent(input$dir, {
    if (!is.null(input$dir)) {
      # Vérifier si c'est le premier clic ou un clic après sélection
      if (is.list(input$dir) && !is.null(input$dir$path)) {
        # C'est une sélection de dossier (deuxième clic ou plus)
        tryCatch(
          {
            # Utiliser parseDirPath correctement pour obtenir le chemin complet
            dir_path <- parseDirPath(volumes, input$dir)

            if (length(dir_path) > 0 && dir_path != "") {
              # Mettre à jour immédiatement le chemin sélectionné
              selected_dir_path(dir_path)

              # Trouver tous les fichiers PLY dans le dossier
              ply_files_in_dir <- list.files(dir_path, pattern = "\\.ply$", full.names = TRUE)

              if (length(ply_files_in_dir) > 0) {
                # Obtenir les noms de fichiers originaux
                original_names <- basename(ply_files_in_dir)

                # Analyser le format de chaque fichier
                format_info <- lapply(ply_files_in_dir, function(path) {
                  tryCatch(
                    {
                      check_ply_format(path)
                    },
                    error = function(e) {
                      list(is_ascii = NA)
                    }
                  )
                })

                # Extraire les informations de format
                is_ascii <- sapply(format_info, function(info) info$is_ascii)

                # Stocker les chemins et les noms originaux
                files_df <- data.frame(
                  path = ply_files_in_dir,
                  original_name = original_names,
                  is_ascii = is_ascii,
                  ascii_path = "", # Sera rempli lors de la conversion
                  stringsAsFactors = FALSE
                )

                ply_files_data(files_df)

                # Réinitialiser les données et l'index de traitement
                landmarks_data(list())
                volumes_data(list())
                file_names(list())
                current_processing_index(1)

                # Démarrer le traitement séquentiel des fichiers
                processing_in_progress(TRUE)
                process_files_sequentially()
              } else {
                # Aucun fichier PLY trouvé dans le dossier
                ply_files_data(data.frame(
                  path = character(0),
                  original_name = character(0),
                  is_ascii = logical(0),
                  ascii_path = character(0),
                  stringsAsFactors = FALSE
                ))
                landmarks_data(list())
                volumes_data(list())
                file_names(list())

                showNotification(
                  "Aucun fichier PLY trouvé dans le dossier sélectionné.",
                  type = "warning",
                  duration = 5
                )
              }
            }
          },
          error = function(e) {
            showNotification(
              paste("Erreur lors de l'accès au dossier:", e$message),
              type = "error",
              duration = 5
            )
          }
        )
      }
    }
  })

  output$dir_path <- renderText({
    current_dir <- selected_dir_path()

    if (current_dir != "") {
      files_df <- ply_files_data()
      num_files <- nrow(files_df)
      return(paste("Dossier sélectionné:", current_dir, "\nNombre de fichiers PLY trouvés:", num_files))
    } else {
      return("Aucun dossier sélectionné")
    }
  })

  # Indiquer si des fichiers ont été trouvés
  output$files_found <- reactive({
    length(landmarks_data()) > 0
  })
  outputOptions(output, "files_found", suspendWhenHidden = FALSE)

  # Message de bienvenue avec animation
  observe({
    session$sendCustomMessage(type = "welcome-message", message = list())
  })

  # Afficher un résumé des fichiers traités
  output$file_summary <- renderText({
    lm_data <- landmarks_data()
    files_df <- ply_files_data()
    vol_data <- volumes_data()

    if (length(lm_data) == 0) {
      return("Aucune donnée disponible.")
    }

    # Compter les fichiers ASCII et binaires
    if (nrow(files_df) > 0 && "is_ascii" %in% names(files_df)) {
      ascii_count <- sum(files_df$is_ascii, na.rm = TRUE)
      binary_count <- sum(!files_df$is_ascii, na.rm = TRUE)
      unknown_count <- sum(is.na(files_df$is_ascii))
      converted_count <- sum(!is.na(files_df$is_ascii) & !files_df$is_ascii & files_df$ascii_path != "")
    } else {
      ascii_count <- binary_count <- unknown_count <- converted_count <- 0
    }

    summary_text <- paste(
      "Nombre de fichiers PLY trouvés:", nrow(files_df),
      "\nNombre de fichiers analysés:", length(lm_data)
    )

    if (processing_in_progress()) {
      current_idx <- current_processing_index()
      if (current_idx > 0 && current_idx <= nrow(files_df)) {
        summary_text <- paste0(
          summary_text,
          "\nTraitement en cours: ",
          current_idx, "/", nrow(files_df), " (",
          round(current_idx / nrow(files_df) * 100), "%)"
        )
      }
    }

    summary_text <- paste(
      summary_text,
      "\nFichiers ASCII:", ascii_count
    )

    if (length(lm_data) > 0) {
      landmark_sample <- lm_data[[1]]
      if (!is.null(landmark_sample)) {
        summary_text <- paste(
          summary_text,
          "\nNombre de landmarks par fichier:", nrow(landmark_sample)
        )
      }
    }

    # Ajouter la liste des fichiers traités avec succès avec leurs volumes
    fn_data <- file_names()
    if (length(fn_data) > 0) {
      summary_text <- paste(summary_text, "\n\nFichiers traités avec succès:")
      for (id in names(fn_data)) {
        volume_info <- ""
        if (!is.null(vol_data[[id]])) {
          volume_info <- paste(" (Volume:", format(vol_data[[id]], digits = 4), "unités³)")
        }

        summary_text <- paste(summary_text, "\n -", fn_data[[id]], volume_info)
      }
    }

    return(summary_text)
  })

  # Aperçu du fichier de landmarks
  output$landmark_preview <- renderText({
    lm_data <- landmarks_data()
    fn_data <- file_names()
    vol_data <- volumes_data() # Ajouter cette ligne pour récupérer les volumes

    if (length(lm_data) == 0) {
      return("Aucune donnée disponible.")
    }

    # Préparer l'aperçu
    preview_content <- character(0)

    # Limiter le nombre de fichiers pour l'aperçu
    preview_files <- min(input$preview_num_files, length(lm_data))
    file_ids <- names(lm_data)[1:preview_files]

    for (file_id in file_ids) {
      landmarks <- lm_data[[file_id]]
      current_file_name <- fn_data[[file_id]]
      current_volume <- vol_data[[file_id]] # Récupérer le volume pour ce fichier

      if (is.null(landmarks) || nrow(landmarks) == 0) {
        preview_content <- c(
          preview_content,
          paste0("# Fichier ", current_file_name, " : Aucun landmark extrait")
        )
        next
      }

      # Ajouter l'en-tête pour ce fichier
      num_lm <- ncol(landmarks)
      num_pt <- nrow(landmarks)
      preview_content <- c(preview_content, paste0("LM", num_lm, "=", num_pt))

      # Ajouter les points avec arrondissement à 5 chiffres après la virgule
      for (i in 1:nrow(landmarks)) {
        preview_content <- c(
          preview_content,
          paste(
            format(round(landmarks[i, 1], 5), nsmall = 5),
            format(round(landmarks[i, 2], 5), nsmall = 5),
            format(round(landmarks[i, 3], 5), nsmall = 5)
          )
        )
      }

      # Ajouter l'ID du fichier sans l'extension .ply
      file_name_no_extension <- sub("\\.ply$", "", current_file_name)
      preview_content <- c(preview_content, paste0("ID=", file_name_no_extension))

      # Ajouter le volume si disponible
      if (!is.null(current_volume)) {
        preview_content <- c(preview_content, paste0("VOLUME=", format(current_volume, digits = 6)))
      }
    }

    # Retourner l'aperçu formaté
    return(paste(preview_content, collapse = "\n"))
  })

  # Téléchargement du fichier de landmarks
  output$download_landmarks <- downloadHandler(
    filename = function() {
      input$landmark_filename
    },
    content = function(file) {
      lm_data <- landmarks_data()
      fn_data <- file_names()
      vol_data <- volumes_data()

      if (length(lm_data) == 0) {
        # Créer un fichier vide avec un message d'erreur
        writeLines("Aucune donnée disponible", file)
        return()
      }

      # Préparer le contenu du fichier landmarks
      landmarks_content <- character(0)

      for (file_id in names(lm_data)) {
        landmarks <- lm_data[[file_id]]
        current_file_name <- fn_data[[file_id]]
        current_volume <- vol_data[[file_id]]

        if (is.null(landmarks) || nrow(landmarks) == 0) {
          next
        }

        # Ajouter l'en-tête pour ce fichier avec le nombre de landmarks et le volume sur la même ligne
        num_lm <- ncol(landmarks)
        num_pt <- nrow(landmarks)
        volume_info <- if (!is.null(current_volume)) {
          paste0(" VOLUME=", format(current_volume, digits = 6))
        } else {
          ""
        }
        landmarks_content <- c(landmarks_content, paste0("LM", num_lm, "=", num_pt, volume_info))

        # Ajouter les points avec arrondissement à 5 chiffres après la virgule
        for (i in 1:nrow(landmarks)) {
          landmarks_content <- c(
            landmarks_content,
            paste(
              format(round(landmarks[i, 1], 5), nsmall = 5),
              format(round(landmarks[i, 2], 5), nsmall = 5),
              format(round(landmarks[i, 3], 5), nsmall = 5)
            )
          )
        }

        # Ajouter l'ID du fichier en utilisant le nom du fichier sans l'extension .ply
        file_name_no_extension <- sub("\\.ply$", "", current_file_name)

        # Formater les landmarks pour ce fichier
        landmarks_text <- format_landmarks_text(landmarks, file_name_no_extension, current_volume)

        # Ajouter au contenu total
        if (length(all_landmarks_text) > 0) {
          all_landmarks_text <- c(all_landmarks_text, "", landmarks_text) # Ajouter une ligne vide entre les entrées
        } else {
          all_landmarks_text <- c(all_landmarks_text, landmarks_text)
        }
      }

      # Écrire le contenu dans le fichier
      writeLines(landmarks_content, file)
    }
  )

  # Fonction pour traiter un seul fichier PLY
  process_single_file <- function(file_idx) {
    # Récupérer les données du fichier
    files_df <- ply_files_data()

    if (file_idx > nrow(files_df)) {
      return(FALSE) # Fin du traitement
    }

    # Mettre à jour l'index actuel
    current_processing_index(file_idx)

    # Récupérer les informations du fichier
    file_path <- files_df$path[file_idx]
    original_name <- files_df$original_name[file_idx]

    # Afficher une notification
    notification_id <- showNotification(
      paste0("Analyse en cours (", file_idx, "/", nrow(files_df), "): ", original_name),
      duration = NULL,
      type = "message",
      closeButton = FALSE
    )

    # Utiliser le fichier ASCII converti si disponible ou le convertir
    if (!is.na(files_df$is_ascii[file_idx]) && !files_df$is_ascii[file_idx]) {
      if (files_df$ascii_path[file_idx] != "") {
        file_path <- files_df$ascii_path[file_idx]
      } else {
        # Tenter de convertir en ASCII
        showNotification(
          paste0("Conversion du fichier ", original_name, " en ASCII..."),
          id = notification_id,
          type = "message",
          closeButton = FALSE
        )

        ascii_path <- convert_ply_to_ascii(file_path)

        if (!is.null(ascii_path)) {
          file_path <- ascii_path
          files_df$ascii_path[file_idx] <- ascii_path
          ply_files_data(files_df) # Mettre à jour le dataframe
        }
      }
    }

    # Obtenir l'ID à partir du nom original sans l'extension
    file_id <- sub("\\.ply$", "", original_name)

    # Récupérer les données actuelles
    all_landmarks <- landmarks_data()
    all_volumes <- volumes_data()
    all_file_names <- file_names()

    # Traiter le fichier
    success <- FALSE
    tryCatch(
      {
        # Extraire les landmarks avec projection toujours activée
        result <- extract_landmarks(model, file_path, apply_projection = TRUE)

        if (!is.null(result)) {
          # Mettre à jour les données réactives
          all_landmarks[[file_id]] <- result$landmarks
          landmarks_data(all_landmarks)

          all_volumes[[file_id]] <- result$volume
          volumes_data(all_volumes)

          all_file_names[[file_id]] <- original_name
          file_names(all_file_names)

          # Mettre à jour le fichier sélectionné pour toujours afficher le fichier courant
          updateSelectInput(session, "selected_file", choices = all_file_names, selected = original_name)

          success <- TRUE
        }
      },
      error = function(e) {
        # Essayer une approche alternative si l'extraction échoue
        tryCatch(
          {
            # Lire directement le fichier PLY et extraire des points comme landmarks
            showNotification(
              paste0("Tentative de méthode alternative pour ", original_name),
              id = notification_id,
              type = "message",
              closeButton = FALSE
            )

            # Essayer de lire le fichier
            points <- read_simple_ply(file_path)

            if (!is.null(points) && nrow(points) >= 6) {
              # Sélectionner 6 points bien répartis dans le fichier
              idx <- round(seq(1, nrow(points), length.out = 6))
              lm <- points[idx, , drop = FALSE]

              # Mettre à jour les données réactives
              all_landmarks[[file_id]] <- lm
              landmarks_data(all_landmarks)

              all_volumes[[file_id]] <- NULL # Pas de volume dans ce cas
              volumes_data(all_volumes)

              all_file_names[[file_id]] <- original_name
              file_names(all_file_names)

              # Mettre à jour le fichier sélectionné pour toujours afficher le fichier courant
              updateSelectInput(session, "selected_file", choices = all_file_names, selected = original_name)

              success <- TRUE
            }
          },
          error = function(e2) {
            showNotification(
              paste0("Erreur lors de l'analyse de ", original_name, " : ", e2$message),
              type = "error",
              duration = 5
            )
          }
        )
      }
    )

    # Supprimer la notification
    removeNotification(notification_id)

    # Retourner le résultat du traitement
    return(TRUE) # Continuer le traitement
  }
  # Fonction pour traiter séquentiellement tous les fichiers PLY
  process_files_sequentially <- function() {
    files_df <- ply_files_data()

    if (nrow(files_df) == 0) {
      processing_in_progress(FALSE)
      return()
    }

    # Récupérer l'index actuel
    current_idx <- current_processing_index()

    # Traiter le fichier actuel
    continue_processing <- process_single_file(current_idx)

    if (continue_processing && current_idx < nrow(files_df)) {
      shinyjs::delay(500, {
        current_processing_index(current_idx + 1)
        process_files_sequentially()
      })
    } else {
      # Tous les fichiers ont été traités
      processing_in_progress(FALSE)
      showNotification(
        HTML(paste0(
          "<div style='display: flex; align-items: center;'>",
          "<i class='fa fa-check-circle' style='color: #2ecc71; font-size: 24px; margin-right: 10px;'></i>",
          "<div><strong>Analyse terminée!</strong><br>",
          length(landmarks_data()), " fichiers traités sur ", nrow(files_df), ".</div></div>"
        )),
        duration = 10,
        type = "message",
        closeButton = TRUE
      )
    }
  }

  # Observer pour traiter les fichiers chargés directement via fileInput
  observeEvent(input$ply_files, {
    if (!is.null(input$ply_files) && nrow(input$ply_files) > 0) {
      # Créer un dataframe avec les chemins des fichiers
      files_df <- data.frame(
        path = input$ply_files$datapath,
        original_name = input$ply_files$name,
        is_ascii = NA, # Sera rempli plus tard
        ascii_path = "", # Sera rempli lors de la conversion
        stringsAsFactors = FALSE
      )

      # Analyser le format de chaque fichier
      for (i in 1:nrow(files_df)) {
        tryCatch(
          {
            format_info <- check_ply_format(files_df$path[i])
            files_df$is_ascii[i] <- format_info$is_ascii
          },
          error = function(e) {
            files_df$is_ascii[i] <- NA
          }
        )
      }

      # Mettre à jour les données
      ply_files_data(files_df)

      # Réinitialiser les données et l'index de traitement
      landmarks_data(list())
      volumes_data(list())
      file_names(list())
      current_processing_index(1)

      # Démarrer le traitement séquentiel des fichiers
      processing_in_progress(TRUE)
      process_files_sequentially()
    }
  })
  observe({
    # Si le traitement est en cours, désactiver la liste déroulante
    if (processing_in_progress()) {
      shinyjs::disable("selected_file")
    } else {
      shinyjs::enable("selected_file")
    }
  })

  # Ajouter un output pour le statut du traitement
  output$processing_status <- reactive({
    processing_in_progress()
  })
  outputOptions(output, "processing_status", suspendWhenHidden = FALSE)

  # Observer pour traiter les fichiers chargés directement via fileInput
  observeEvent(input$ply_files, {
    # Le reste du code reste inchangé
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
