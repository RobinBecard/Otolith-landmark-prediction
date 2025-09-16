# Fonction pour calculer le volume d'un maillage
calculate_volume <- function(mesh) {
  cleaned_mesh <- vcgClean(mesh, sel = 0:6, iterate = TRUE, silent = TRUE)

  # Créer un tmesh3d
  cleaned_mesh_tmesh <- tmesh3d(
    vertices = cleaned_mesh$vb[1:3, ],
    indices = cleaned_mesh$it,
    homogeneous = FALSE
  )

  volume <- vcgVolume(cleaned_mesh_tmesh)

  # Retourner le volume
  return(volume)
}

# Fonction pour lire les points du fichier points.txt
read_points <- function(file_id) {
  points_file <- "../Data/points.txt"
  if (!file.exists(points_file)) {
    cat("Le fichier de points n'existe pas.\n")
    return(NULL)
  }

  cat(paste("L'id de la figure ", file_id, "\n"))

  lines <- readLines(points_file) # Lire les lignes du fichier
  points <- matrix(nrow = 0, ncol = 3) # Matrice pour stocker les coordonnées
  current_id <- NULL
  reading_points <- FALSE
  points_count <- 0

  for (i in seq_along(lines)) {
    line <- trimws(lines[i]) # Enlever les espaces en début/fin de ligne

    # Si la ligne commence par "ID=", on vérifie si c'est l'ID qu'on cherche
    if (startsWith(line, "ID=")) {
      current_id <- sub("ID=", "", line)
      if (current_id == file_id) {
        # On prend les 6 lignes précédentes qui contiennent les coordonnées
        points <- matrix(nrow = 6, ncol = 3) # Initialiser la matrice pour 6 points
        for (j in 1:6) {
          coord_line <- lines[i - (7 - j)] # Les points sont dans l'ordre inverse
          if (!startsWith(coord_line, "LM3=")) { # Ignorer la ligne LM3=6
            coords <- as.numeric(strsplit(coord_line, "\\s+")[[1]])
            if (length(coords) == 3) {
              points[j, ] <- coords
            }
          }
        }
        break # Sortir de la boucle une fois les points trouvés
      }
    }
  }

  return(points)
}

# Fonction pour formater les landmarks pour le fichier texte
format_landmarks_text <- function(landmarks, file_id, volume = NULL) {
  if (is.null(landmarks) || nrow(landmarks) != 6 || ncol(landmarks) != 3) {
    warning("Les landmarks doivent être une matrice 6x3")
    return(NULL)
  }
  landmarks_content <- character(0)
  volume_info <- if (!is.null(volume)) {
    paste0(" VOLUME=", format(volume, digits = 6))
  } else {
    ""
  }
  landmarks_content <- c(landmarks_content, paste0("LM3=6", volume_info))
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

  # Ajouter l'ID du fichier
  landmarks_content <- c(landmarks_content, paste0("ID=", file_id))

  return(paste(landmarks_content, collapse = "\n"))
}

# Fonction pour écrire/ajouter les landmarks dans un fichier
write_landmarks_to_file <- function(landmarks_text, file_path, append = TRUE) {
  if (is.null(landmarks_text) || landmarks_text == "") {
    warning("Texte de landmarks vide, rien à écrire")
    return(FALSE)
  }
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  if (append && file.exists(file_path)) {
    if (file.info(file_path)$size > 0) {
      landmarks_text <- paste0("\n", landmarks_text)
    }
    cat(landmarks_text, file = file_path, append = TRUE)
  } else {
    writeLines(landmarks_text, file_path)
  }

  return(TRUE)
}
