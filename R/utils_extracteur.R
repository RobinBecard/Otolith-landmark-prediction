library(cluster)
library(stringr)

# Fonction pour vérifier si un fichier PLY est au format ASCII ou binaire
check_ply_format <- function(file_path) {
  con <- file(file_path, "rb")
  on.exit(close(con))

  # Lire l'en-tête pour déterminer le format
  header <- character()
  line <- readLines(con, n = 1)

  # Vérifier que c'est bien un fichier PLY
  if (line != "ply") {
    stop("Le fichier n'est pas au format PLY")
  }

  header <- c(header, line)

  # Lire les lignes suivantes jusqu'à trouver la ligne de format
  format_line <- ""
  while (length(line) > 0) {
    line <- readLines(con, n = 1)
    header <- c(header, line)

    if (grepl("^format", line)) {
      format_line <- line
      break
    }
  }

  # Déterminer si le format est ASCII ou binaire
  is_ascii <- grepl("ascii", format_line)

  return(list(
    is_ascii = is_ascii,
    header = header
  ))
}

# Fonction pour extraire les landmarks
extract_landmarks <- function(model, ply_path, apply_projection = TRUE) {
  # Vérifier d'abord le format du fichier PLY
  format_info <- tryCatch(
    {
      check_ply_format(ply_path)
    },
    error = function(e) {
      message("Erreur lors de la vérification du format PLY: ", e$message)
      return(list(is_ascii = FALSE))
    }
  )

  # Si le fichier n'est pas ASCII, essayer de le convertir
  ply_path_to_use <- ply_path

  # Prédire les landmarks
  prediction_result <- predict_landmarks(model, ply_path_to_use)
  if (is.null(prediction_result)) {
    cat("Impossible de charger les données pour le fichier", ply_path_to_use, "\n")
    return(NULL)
  }

  predicted_landmarks <- prediction_result$landmarks
  volume <- NULL # Initialiser le volume

  # Projeter les landmarks prédits sur la surface si demandé
  if (apply_projection) {
    projection_result <- projeter_landmarks_predits(predicted_landmarks, ply_path_to_use)
    projected_landmarks <- projection_result$landmarks_projetes
    volume <- projection_result$volume # Récupérer le volume calculé
  } else {
    projected_landmarks <- predicted_landmarks
  }

  # Retourner les landmarks et le volume
  return(list(
    landmarks = projected_landmarks,
    volume = volume
  ))
}

# Fonction pour lire simplement un fichier PLY comme méthode de secours
read_simple_ply <- function(path) {
  tryCatch(
    {
      con <- file(path, "rb")
      on.exit(close(con))

      # Lire l'en-tête
      header <- character()
      line <- readLines(con, n = 1)
      if (line != "ply") {
        stop("Le fichier n'est pas au format PLY")
      }

      header <- c(header, line)
      vertex_count <- 0
      reading_header <- TRUE

      while (reading_header) {
        line <- readLines(con, n = 1)
        header <- c(header, line)

        if (grepl("element vertex", line)) {
          vertex_count <- as.integer(str_extract(line, "\\d+"))
        }

        if (line == "end_header") {
          reading_header <- FALSE
        }
      }

      # Lire les points
      points_raw <- readLines(con, n = min(vertex_count, 1000)) # Limiter à 1000 points

      # Convertir en matrice numérique
      points_data <- do.call(rbind, lapply(points_raw, function(x) {
        as.numeric(strsplit(x, "\\s+")[[1]])
      }))

      # Retourner les coordonnées
      if (ncol(points_data) >= 3) {
        return(points_data[, 1:3])
      } else {
        return(NULL)
      }
    },
    error = function(e) {
      message("Erreur lors de la lecture du fichier PLY:", e$message)
      return(NULL)
    }
  )
}
