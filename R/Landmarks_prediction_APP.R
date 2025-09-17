get_ply_ids <- function(ply_folder) {
  if (!dir.exists(ply_folder)) {
    stop("Le dossier spécifié n'existe pas.")
  }

  # Liste tous les fichiers PLY
  ply_files <- list.files(ply_folder, pattern = "\\_ASCII.ply$", full.names = FALSE)

  # Extraction des IDs en retirant l'extension .ply
  ply_ids <- sub("_ASCII.ply", "", ply_files)

  return(ply_ids)
}

load_ply <- function(ply_path) {
  if (!file.exists(ply_path)) {
    cat("Le fichier PLY n'existe pas:", ply_path, "\n")
    return(NULL)
  }

  # Charger le nuage de points
  points_3D <- read.ply(ply_path, ShowSpecimen = FALSE, addNormals = TRUE)
  coords <- t(points_3D$vb[1:3, ]) # Matrice de coordonnées (N x 3)

  return(list(points = coords, mesh = points_3D))
}

normalize_points <- function(points) {
  mean_vals <- colMeans(points)
  std_vals <- apply(points, 2, sd)
  return((points - mean_vals) / std_vals)
}

to_tensor <- function(points) {
  return(torch_tensor(as.matrix(points), dtype = torch_float()))
}

prepare_data <- function(ply_path) {
  data <- load_ply(ply_path)
  if (is.null(data)) {
    return(NULL)
  }

  points_norm <- data$points

  # Convertir en tenseur
  input_tensor <- to_tensor(points_norm)

  return(list(input = input_tensor, mesh = data$mesh))
}

# Définition du modèle
# net <- nn_module(
#  initialize = function() {
#   self$fc1 <- nn_linear(3, 128)
#    self$fc2 <- nn_linear(128, 256)
#    self$fc3 <- nn_linear(256, 18)
#  },
#
#  forward = function(x) {
#    x <- torch_relu(self$fc1(x))
#    x <- torch_relu(self$fc2(x))
#    x <- self$fc3(x)
#    return(x)
#  }
# )

# Création du modèle
# model <- net()
# print(model)

# Fonction coût (MSE) et optimiseur (Adam)
# loss_fn <- nn_mse_loss()
# optimizer <- optim_adam(model$parameters, lr=0.0001)


# Chargement des données d'entraînement
# ids_file_ply <- get_ply_ids("../Data/PLY_files_ASCII")
# train_dataset <- list()


# for (file_id in ids_file_ply) {
#  sample <- prepare_data(file_id, "../Data/PLY_files_ASCII", "../Data/points.txt")
#  if (!is.null(sample)) {
#    train_dataset <- append(train_dataset, list(sample))
#  }
# }

# Séparation en train/validation (80/20)
# set.seed(123)
# n <- length(train_dataset)
# train_indices <- sample(n, size = round(0.9 * n))
# train_data <- train_dataset[train_indices]
# valid_data <- train_dataset[-train_indices]

# Fonction pour entraîner le modèle avec recherche d'hyperparamètres
train_model <- function(train_data, valid_data, param_grid) {
  best_loss <- Inf
  best_model <- NULL
  loss_history <- data.frame(epoch = integer(), loss = numeric())

  for (lr in param_grid$learning_rate) {
    for (epochs in param_grid$epochs) {
      model <- net()
      optimizer <- optim_adam(model$parameters, lr = lr)

      for (epoch in 1:epochs) {
        total_loss <- 0
        for (sample in train_data) {
          optimizer$zero_grad()
          output <- model(sample$input)
          loss <- loss_fn(output, sample$target)
          total_loss <- total_loss + loss$item()
          loss$backward()
          optimizer$step()
        }
        cat("Epoch:", epoch, " Loss:", total_loss / length(train_data), "\n")
      }

      valid_loss <- mean(sapply(valid_data, function(sample) {
        loss_fn(model(sample$input), sample$target)$item()
      }))

      if (valid_loss < best_loss) {
        best_loss <- valid_loss
        best_model <- model
      }
    }
  }

  return(best_model)
}

# Grille d'hyperparamètres
tune_params <- list(
  learning_rate = c(0.00004),
  epochs = c(300)
)

# Entraînement avec recherche d'hyperparamètres
# best_model <- train_model(train_data, valid_data, tune_params)
# torch_save(best_model, "Landmarks_NN_4.pt")
# torch_save(optimizer$state_dict(), "Model_Optimizer_4.pt")

model <- torch_load("../Pytorch/Landmarks_NN_4.pt")
optimizer <- torch_load("../Pytorch/Model_Optimizer_4.pt")

transform_matrix <- function(mat) {
  nrow_mat <- nrow(mat)
  ncol_mat <- ncol(mat)

  # Vérification si la matrice a bien 6 lignes
  if (nrow_mat != 6) {
    stop("La matrice doit avoir 6 lignes.")
  }

  # Transformation
  mat_transformed <- matrix(NA, nrow = nrow_mat, ncol = ncol_mat)

  for (i in 1:ncol_mat) {
    mat_transformed[, i] <- c(
      mat[2 * i - 1, 1], mat[2 * i - 1, 2], mat[2 * i - 1, 3],
      mat[2 * i, 1], mat[2 * i, 2], mat[2 * i, 3]
    )
  }

  return(mat_transformed)
}

predict_landmarks <- function(model, ply_path) {
  sample <- prepare_data(ply_path)
  if (is.null(sample)) {
    return(NULL)
  }

  with_no_grad({
    pred <- model(sample$input)
  })

  pred <- torch_mean(pred, dim = 1) # Moyenne si nécessaire
  pred <- pred$view(c(6, 3)) # Reshape en (6,3)
  pred <- as.matrix(pred) # Convertir en matrice

  pred <- transform_matrix(pred)

  return(list(landmarks = pred, mesh = sample$mesh))
}

# Fonction pour lire un fichier PLY ASCII
lire_ply <- function(chemin_fichier) {
  mesh <- vcgImport(chemin_fichier)
  return(mesh)
}

# Fonction auxiliaire: normalisation d'un vecteur
normalise <- function(v) {
  return(v / sqrt(sum(v^2)))
}

# Fonction auxiliaire: produit vectoriel
cross_product <- function(a, b) {
  return(c(
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]
  ))
}

# Fonction auxiliaire: vérifier si un point est dans un triangle (coordonnées barycentriques)
point_dans_triangle <- function(p, v1, v2, v3) {
  # Calculer les vecteurs
  v0 <- v3 - v1
  v1 <- v2 - v1
  v2 <- p - v1

  # Calculer les produits scalaires
  dot00 <- sum(v0 * v0)
  dot01 <- sum(v0 * v1)
  dot02 <- sum(v0 * v2)
  dot11 <- sum(v1 * v1)
  dot12 <- sum(v1 * v2)

  # Calculer les coordonnées barycentriques
  invDenom <- 1 / (dot00 * dot11 - dot01 * dot01)
  u <- (dot11 * dot02 - dot01 * dot12) * invDenom
  v <- (dot00 * dot12 - dot01 * dot02) * invDenom

  # Vérifier si le point est dans le triangle
  return((u >= 0) && (v >= 0) && (u + v <= 1))
}

# Fonction pour trouver le point le plus proche sur le maillage
# (utilisée si la projection orthogonale n'est pas dans un triangle)
trouver_point_plus_proche <- function(mesh, point) {
  vertices <- t(mesh$vb[1:3, ])
  faces <- t(mesh$it)

  min_dist <- Inf
  point_plus_proche <- NULL

  # Vérifier chaque sommet
  for (i in 1:nrow(vertices)) {
    v <- vertices[i, ]
    dist <- sqrt(sum((point - v)^2))

    if (dist < min_dist) {
      min_dist <- dist
      point_plus_proche <- v
    }
  }

  # Vérifier chaque arête
  for (i in 1:nrow(faces)) {
    for (j in 1:3) {
      v1 <- vertices[faces[i, j], ]
      v2 <- vertices[faces[i, (j %% 3) + 1], ]

      # Projection sur l'arête
      edge <- v2 - v1
      t <- sum((point - v1) * edge) / sum(edge * edge)
      t <- max(0, min(1, t)) # Limiter t entre 0 et 1

      p_proj <- v1 + t * edge
      dist <- sqrt(sum((point - p_proj)^2))

      if (dist < min_dist) {
        min_dist <- dist
        point_plus_proche <- p_proj
      }
    }
  }

  return(point_plus_proche)
}

# Fonction pour projeter un point sur la surface de l'objet 3D
projeter_point_sur_objet <- function(mesh, point) {
  # 1. Obtenir les vertices et faces du maillage
  vertices <- mesh$vb[1:3, ]
  vertices <- t(vertices)
  faces <- t(mesh$it)

  # 2. Trouver le triangle le plus proche
  min_dist <- Inf
  point_projete <- NULL
  triangle_plus_proche <- NULL

  for (i in 1:nrow(faces)) {
    # Obtenir les trois sommets du triangle
    v1 <- vertices[faces[i, 1], ]
    v2 <- vertices[faces[i, 2], ]
    v3 <- vertices[faces[i, 3], ]

    # Calculer la normale du triangle (vecteur perpendiculaire à la surface)
    normale <- normalise(cross_product(v2 - v1, v3 - v1))

    # Calculer le plan du triangle (équation ax + by + cz + d = 0)
    d <- -sum(normale * v1)

    # Projeter le point sur le plan du triangle
    t <- -(sum(normale * point) + d) / sum(normale * normale)
    p_proj <- point + t * normale

    # Vérifier si le point projeté est à l'intérieur du triangle
    if (point_dans_triangle(p_proj, v1, v2, v3)) {
      # Calculer la distance entre le point et sa projection
      dist <- sqrt(sum((point - p_proj)^2))

      # Si cette distance est plus petite que la distance minimale actuelle
      if (dist < min_dist) {
        min_dist <- dist
        point_projete <- p_proj
        triangle_plus_proche <- i
      }
    }
  }

  # Si aucun triangle n'a été trouvé (point projeté toujours à l'extérieur)
  # on cherche la projection sur l'arête ou le sommet le plus proche
  if (is.null(point_projete)) {
    point_projete <- trouver_point_plus_proche(mesh, point)
  }

  return(list(
    point_projete = point_projete,
    distance = min_dist,
    triangle = triangle_plus_proche
  ))
}

# Fonction pour projeter les landmarks prédits sur la surface du maillage
projeter_landmarks_predits <- function(landmarks, ply_path) {
  # Charger le maillage PLY
  mesh <- lire_ply(ply_path)

  # Projeter chaque point prédit sur la surface du maillage
  landmarks_projetes <- matrix(NA, nrow = nrow(landmarks), ncol = ncol(landmarks))
  volume <- calculate_volume(mesh)

  for (i in 1:nrow(landmarks)) {
    point <- landmarks[i, ]
    resultat <- projeter_point_sur_objet(mesh, point)
    landmarks_projetes[i, ] <- resultat$point_projete
  }

  return(list(
    landmarks_projetes = landmarks_projetes,
    volume = volume
  ))
}

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

# Exemple d'utilisation
# file_id <- "Segmentation_01a-8"
# ply_folder <- "../Data/PLY_files_ASCII"
# points_file <- "../Data/points.txt"

# landmarks <- extract_landmarks(model, ply_path = "../Data/PLY_files_ASCII/Segmentation_01a-8_ASCII.ply")
# landmarks
