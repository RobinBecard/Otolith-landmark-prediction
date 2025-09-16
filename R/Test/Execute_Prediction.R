# Modification de la fonction load_ply_and_landmarks pour qu'elle ne dépende pas de points_file
load_ply_and_landmarks <- function(ply_path, need_landmarks = FALSE) {
  if (!file.exists(ply_path)) {
    cat("Le fichier PLY n'existe pas:", ply_path, "\n")
    return(NULL)
  }
  
  # Charger le nuage de points
  points_3D <- read.ply(ply_path, ShowSpecimen = FALSE, addNormals = TRUE)
  coords <- t(points_3D$vb[1:3, ])  # Matrice de coordonnées (N x 3)
  
  # Si les landmarks ne sont pas nécessaires, renvoyer seulement les points
  if (!need_landmarks) {
    return(list(points = coords, landmarks = NULL))
  } else {
    cat("Avertissement: Les landmarks ne sont pas chargés car points_file n'est pas fourni\n")
    return(list(points = coords, landmarks = NULL))
  }
}

# Modification de la fonction prepare_data pour fonctionner sans landmarks
prepare_data <- function(ply_path) {
  data <- load_ply_and_landmarks(ply_path)
  if (is.null(data)) return(NULL)
  
  points_norm <- data$points
  
  # Convertir en tenseur
  input_tensor <- to_tensor(points_norm)
  
  return(list(input = input_tensor, target = NULL))
}

# Modification de la fonction predict_landmarks pour fonctionner avec le modèle chargé
predict_landmarks <- function(model, ply_path) {
  sample <- prepare_data(ply_path)
  if (is.null(sample)) return(NULL)
  
  with_no_grad({
    pred <- model(sample$input)
  })
  
  pred <- torch_mean(pred, dim = 1)  # Moyenne si nécessaire
  pred <- pred$view(c(6, 3))  # Reshape en (6,3)
  pred <- as.matrix(pred)  # Convertir en matrice
  
  return(pred)
}

# Modification de la fonction visualize_predictions pour ne pas utiliser les landmarks réels
visualize_predictions <- function(model, ply_path, apply_projection = TRUE) {
  sample <- prepare_data(ply_path)
  if (is.null(sample)) {
    cat("Impossible de charger les données pour le chemin", ply_path, "\n")
    return(NULL)
  }
  
  # Charger le maillage
  points_3D <- read.ply(ply_path, ShowSpecimen = FALSE, addNormals = TRUE)
  
  # Prédire les landmarks
  predicted_landmarks <- predict_landmarks(model, ply_path)
  
  # Projeter les landmarks prédits sur la surface si demandé
  if (apply_projection) {
    projection_result <- projeter_landmarks_predits(predicted_landmarks, ply_path)
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
    
    # Retourner les deux types de landmarks
    return(list(
      landmarks_predits = predicted_landmarks,
      landmarks_projetes = projected_landmarks
    ))
  } else {
    # Retourner les landmarks sans projection
    return(list(
      landmarks_predits = predicted_landmarks
    ))
  }
}

# Exemple d'utilisation
file_id <- "Segmentation_01a-8"
ply_folder <- "../Data/PLY_files_ASCII"
ply_path <- "../Data/PLY_files_ASCII/Segmentation_27b-37_ASCII.ply"

# Chargement du modèle
model <- torch_load("Landmarks_NN_4.pt")

# Visualiser les prédictions pour un spécimen avec projection
resultats <- visualize_predictions(model, ply_path, apply_projection = TRUE)