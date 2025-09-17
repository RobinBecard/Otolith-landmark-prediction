source("../Setup.R")

get_ply_ids <- function(ply_folder) {
  if (!dir.exists(ply_folder)) {
    stop("Le dossier spécifié n'existe pas.")
  }
  
  # Liste tous les fichiers PLY
  ply_files <- list.files(ply_folder, pattern = "\\_ASCII.ply$", full.names = FALSE)
  
  # Extraction des IDs en retirant l'extension .ply
  ply_ids <- sub("_ASCII.ply","", ply_files)
  
  return(ply_ids)
}

load_ply_and_landmarks <- function(file_id, ply_folder, points_file) {
  ply_path <- file.path(ply_folder, paste0(file_id, "_ASCII.ply"))
  if (!file.exists(ply_path)) {
    cat("Le fichier PLY n'existe pas pour l'ID", file_id, "\n")
    return(NULL)
  }
  
  # Charger le nuage de points
  points_3D <- read.ply(ply_path, ShowSpecimen = FALSE, addNormals = TRUE)
  coords <- t(points_3D$vb[1:3, ])  # Matrice de coordonnées (N x 3)
  
  # Charger les points caractéristiques
  landmarks <- read_points(file_id)
  
  
  if (is.null(landmarks)) {
    cat("Aucun point caractéristique trouvé pour l'ID", file_id, "\n")
    return(NULL)
  }
  
  return(list(points = coords, landmarks = landmarks))
}

normalize_points <- function(points) {
  mean_vals <- colMeans(points)
  std_vals <- apply(points,2,sd)
  return ((points - mean_vals)/ std_vals)
}

to_tensor <- function(points) {
  return (torch_tensor(as.matrix(points), dtype=torch_float()))
}

prepare_data <- function(file_id, ply_folder, points_file) {
  data <- load_ply_and_landmarks(file_id, ply_folder, points_file)
  if (is.null(data)) return(NULL)
  
  points_norm <- data$points
  landmarks_norm <- as.numeric(data$landmarks)
  landmarks_norm <- matrix(landmarks_norm, ncol = 18)  # 6 points x 3 coordonnées
  
  # Convertir en tenseurs
  input_tensor <- to_tensor(points_norm)
  target_tensor <- to_tensor(landmarks_norm)
  
  return(list(input = input_tensor, target = target_tensor))
}

# Définition du modèle
net <- nn_module(
  initialize = function() {
    # Augmentation des neurones dans les couches existantes
    self$fc1 <- nn_linear(3, 256)       # 128 -> 256 neurones
    self$fc2 <- nn_linear(256, 512)     # 256 -> 512 neurones
    
    # Ajout de couches supplémentaires
    self$fc3 <- nn_linear(512, 512)     # Nouvelle couche
    self$fc4 <- nn_linear(512, 256)     # Nouvelle couche
    self$fc5 <- nn_linear(256, 128)     # Nouvelle couche
    
    # Couche de sortie (inchangée)
    self$fc6 <- nn_linear(128, 18)      # Anciennement fc3
    
    # Ajout de dropout pour réduire l'overfitting
    self$dropout <- nn_dropout(p=0.2)
  },
  
  forward = function(x) {
    x <- torch_relu(self$fc1(x))
    x <- self$dropout(x)
    x <- torch_relu(self$fc2(x))
    x <- self$dropout(x)
    x <- torch_relu(self$fc3(x))
    x <- self$dropout(x)
    x <- torch_relu(self$fc4(x))
    x <- self$dropout(x)
    x <- torch_relu(self$fc5(x))
    x <- self$fc6(x)
    return(x)
  }
)

# Création du modèle
model <- net()
print(model)

# Fonction coût (MSE) et optimiseur (Adam)
loss_fn <- nn_mse_loss()
optimizer <- optim_adam(model$parameters, lr=0.0001)


# Chargement des données d'entraînement
ids_file_ply <- get_ply_ids("../Data/PLY_files_ASCII")
train_dataset <- list()


for (file_id in ids_file_ply) {
  sample <- prepare_data(file_id, "../Data/PLY_files_ASCII", "../Data/points.txt")
  if (!is.null(sample)) {
    train_dataset <- append(train_dataset, list(sample))
  }
}

# Séparation en train/validation (80/20)
set.seed(123)
n <- length(train_dataset)
train_indices <- sample(n, size = round(0.8 * n))
train_data <- train_dataset[train_indices]
valid_data <- train_dataset[-train_indices]

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
        total_loss <-0
        for (sample in train_data) {
          optimizer$zero_grad()
          output <- model(sample$input)
          loss <- loss_fn(output, sample$target)
          total_loss <- total_loss + loss$item()
          loss$backward()
          optimizer$step()
        }
        cat("Epoch:", epoch, " Loss:", total_loss/length(train_data), "\n")
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
  learning_rate = c(0.01,0.001, 0.0001),
  epochs = c(100,150)
)

# Entraînement avec recherche d'hyperparamètres
best_model <- train_model(train_data, valid_data, tune_params)
torch_save(best_model, "Landmarks_NN_enhance_4.pt")
torch_save(optimizer$state_dict(), "Model_Optimizer_enhance_4.pt")

model <- torch_load("Landmarks_NN_enhance_4.pt")
optimizer <- torch_load("Model_Optimizer_enhance_4.pt")

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
    mat_transformed[, i] <- c(mat[2*i-1, 1], mat[2*i-1, 2], mat[2*i-1, 3],
                              mat[2*i, 1], mat[2*i, 2], mat[2*i, 3])
  }
  
  return(mat_transformed)
}

predict_landmarks <- function(model, file_id, ply_folder, points_file) {
  sample <- prepare_data(file_id, ply_folder, points_file)
  if (is.null(sample)) return(NULL)
  
  with_no_grad({
    pred <- model(sample$input)
  })
  
  print("Prediction shape:")
  print(dim(pred))
  
  pred <- torch_mean(pred, dim = 1)  # Moyenne si nécessaire
  pred <- pred$view(c(6, 3))  # Reshape en (6,3)
  pred <- as.matrix(pred)  # Convertir en matrice
  
  pred <- transform_matrix(pred)
  
  return(pred)
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
  vertices <- t(mesh$vb[1:3,])
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
      t <- max(0, min(1, t))  # Limiter t entre 0 et 1
      
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
  vertices <- mesh$vb[1:3,]
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
projeter_landmarks_predits <- function(landmarks, file_id, ply_folder) {
  # Charger le maillage PLY
  ply_path <- file.path(ply_folder, paste0(file_id, "_ASCII.ply"))
  mesh <- lire_ply(ply_path)
  
  # Projeter chaque point prédit sur la surface du maillage
  landmarks_projetes <- matrix(NA, nrow = nrow(landmarks), ncol = ncol(landmarks))
  distances <- numeric(nrow(landmarks))
  
  for (i in 1:nrow(landmarks)) {
    point <- landmarks[i, ]
    resultat <- projeter_point_sur_objet(mesh, point)
    landmarks_projetes[i, ] <- resultat$point_projete
    distances[i] <- resultat$distance
  }
  
  return(list(
    landmarks_projetes = landmarks_projetes,
    distances = distances
  ))
}

visualize_predictions <- function(model, file_id, ply_folder, points_file, apply_projection = TRUE) {
  sample <- prepare_data(file_id, ply_folder, points_file)
  if (is.null(sample)) {
    cat("Impossible de charger les données pour l'ID", file_id, "\n")
    return(NULL)
  }
  
  ply_path <- file.path(ply_folder, paste0(file_id, "_ASCII.ply"))
  points_3D <- read.ply(ply_path, ShowSpecimen = FALSE, addNormals = TRUE)
  
  # Convertir les tenseurs en matrices
  real_landmarks <- as.array(sample$target)
  real_landmarks <- matrix(real_landmarks, nrow = 6, ncol = 3)
  
  print("real_landmarks")
  print(real_landmarks)
  
  # Prédire les landmarks
  predicted_landmarks <- predict_landmarks(model, file_id, ply_folder, points_file)
  
  # Projeter les landmarks prédits sur la surface si demandé
  if (apply_projection) {
    projection_result <- projeter_landmarks_predits(predicted_landmarks, file_id, ply_folder)
    projected_landmarks <- projection_result$landmarks_projetes
    projection_distances <- projection_result$distances
    
    cat("Distances de projection des landmarks:\n")
    print(projection_distances)
    cat("Distance moyenne de projection:", mean(projection_distances), "\n")
  } else {
    projected_landmarks <- NULL
  }
  
  # Afficher les informations sur les points
  print("Points 3D:")
  print(dim(points_3D$vb))
  
  print("Points caractéristiques réels:")
  print(real_landmarks)
  
  print("Points caractéristiques prédits:")
  print(predicted_landmarks)
  
  if (apply_projection) {
    print("Points caractéristiques projetés:")
    print(projected_landmarks)
  }
  
  # Visualiser les résultats
  open3d()
  
  # Afficher le maillage
  shade3d(points_3D, col = "lightblue", alpha = 0.7)
  
  # Afficher les landmarks réels (rouge)
  points3d(real_landmarks, col = "red", size = 15)
  
  # Afficher les landmarks prédits (jaune)
  points3d(predicted_landmarks, col = "yellow", size = 15)
  
  # Si les landmarks ont été projetés
  if (apply_projection && !is.null(projected_landmarks)) {
    # Afficher les landmarks projetés (vert)
    points3d(projected_landmarks, col = "green", size = 15)
    
    # Connecter les landmarks prédits avec leur projection par des lignes
    for (i in 1:nrow(predicted_landmarks)) { 
      segments3d(rbind(
        predicted_landmarks[i, ],
        projected_landmarks[i, ]
      ), col = "purple", lwd = 5)
    }
    
    # Ajouter des étiquettes
    for (i in 1:nrow(predicted_landmarks)) {
      text3d(predicted_landmarks[i, ], texts = paste("Point", i), cex = 0.8, col = "black")
    }
    
    # Calculer l'erreur avant et après projection
    erreur_avant <- mean(sqrt(rowSums((real_landmarks - predicted_landmarks)^2)))
    erreur_apres <- mean(sqrt(rowSums((real_landmarks - projected_landmarks)^2)))
    
    cat("\nErreur moyenne avant projection:", erreur_avant, "\n")
    cat("Erreur moyenne après projection:", erreur_apres, "\n")
    
    if (erreur_apres < erreur_avant) {
      cat("Amélioration grâce à la projection:", (erreur_avant - erreur_apres) / erreur_avant * 100, "%\n")
    } else {
      cat("La projection a augmenté l'erreur de:", (erreur_apres - erreur_avant) / erreur_avant * 100, "%\n")
    }
    
    # Retourner les deux types de landmarks
    return(list(
      landmarks_reels = real_landmarks,
      landmarks_predits = predicted_landmarks,
      landmarks_projetes = projected_landmarks,
      erreur_avant = erreur_avant,
      erreur_apres = erreur_apres
    ))
  } else {
    # Si pas de projection, calculer seulement l'erreur avant projection
    erreur_avant <- mean(sqrt(rowSums((real_landmarks - predicted_landmarks)^2)))
    cat("\nErreur moyenne:", erreur_avant, "\n")
    
    # Retourner les landmarks sans projection
    return(list(
      landmarks_reels = real_landmarks,
      landmarks_predits = predicted_landmarks,
      erreur = erreur_avant
    ))
  }
}

# Fonction pour évaluer le modèle sur plusieurs spécimens
evaluate_model <- function(model, ids, ply_folder, points_file, apply_projection = TRUE) {
  resultats <- list()
  erreurs_avant <- numeric(length(ids))
  erreurs_apres <- numeric(length(ids))
  
  for (i in 1:length(ids)) {
    file_id <- ids[i]
    cat("\nÉvaluation de", file_id, "...\n")
    
    # Visualiser les prédictions pour ce spécimen
    result <- visualize_predictions(model, file_id, ply_folder, points_file, apply_projection)
    
    if (!is.null(result)) {
      if (apply_projection) {
        erreurs_avant[i] <- result$erreur_avant
        erreurs_apres[i] <- result$erreur_apres
        resultats[[file_id]] <- result
      } else {
        erreurs_avant[i] <- result$erreur
        resultats[[file_id]] <- result
      }
    }
  }
  
  # Afficher le résumé des résultats
  cat("\n=== RÉSUMÉ DES RÉSULTATS ===\n")
  cat("Nombre de spécimens évalués:", length(na.omit(erreurs_avant)), "\n")
  cat("Erreur moyenne avant projection:", mean(na.omit(erreurs_avant)), "\n")
  
  if (apply_projection) {
    cat("Erreur moyenne après projection:", mean(na.omit(erreurs_apres)), "\n")
    amelioration <- (erreurs_avant - erreurs_apres) / erreurs_avant * 100
    amelioration <- amelioration[!is.na(amelioration)]
    
    if (mean(amelioration) > 0) {
      cat("Amélioration moyenne grâce à la projection:", mean(amelioration), "%\n")
    } else {
      cat("La projection a augmenté l'erreur en moyenne de:", -mean(amelioration), "%\n")
    }
  }
  
  return(resultats)
}

# Exemple d'utilisation
file_id <- "Segmentation_01a-8"
ply_folder <- "../Data/PLY_files_ASCII"
points_file <- "../Data/points.txt"

# Visualiser les prédictions pour un spécimen avec projection
resultats <- visualize_predictions(model, file_id, ply_folder, points_file, apply_projection = TRUE)

# Pour évaluer le modèle sur plusieurs spécimens, décommenter les lignes suivantes:
# ids_test <- get_ply_ids("../Data/PLY_files_ASCII")[1:5]  # Prendre les 5 premiers IDs pour tester
# resultats_multi <- evaluate_model(model, ids_test, "../Data/PLY_files_ASCII", "../Data/points.txt", apply_projection = TRUE)