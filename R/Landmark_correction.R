# Fonction pour lire un fichier PLY ASCII
lire_ply <- function(chemin_fichier) {
  mesh <- vcgImport(chemin_fichier)
  return(mesh)
}

# Fonction pour générer un point aléatoire autour de l'objet
generer_point_aleatoire <- function(mesh, marge = 1) {
  vertices <- t(mesh$vb[1:3, ])
  bbox <- rbind(
    apply(vertices, 2, min),
    apply(vertices, 2, max)
  )
  centre <- colMeans(bbox)
  dim_bbox <- (bbox[2, ] - bbox[1, ]) * (1 + marge)
  point <- centre + runif(3, -dim_bbox / 2, dim_bbox / 2)

  return(point)
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

# Fonction principale
projeter_point_sur_maillage <- function(chemin_fichier_ply, visualiser = TRUE) {
  # 1. Charger le maillage
  mesh <- lire_ply(chemin_fichier_ply)

  # 2. Générer un point aléatoire autour de l'objet
  point_aleatoire <- generer_point_aleatoire(mesh)

  # 3. Projeter le point sur l'objet
  resultat <- projeter_point_sur_objet(mesh, point_aleatoire)

  # 4. Afficher les résultats
  cat("Point aléatoire: (", paste(round(point_aleatoire, 4), collapse = ", "), ")\n")
  cat("Point projeté: (", paste(round(resultat$point_projete, 4), collapse = ", "), ")\n")
  cat("Distance de projection:", round(resultat$distance, 4), "\n")

  # 5. Visualiser si demandé
  if (visualiser) {
    open3d()

    shade3d(mesh, col = "lightblue", alpha = 0.7)
    points3d(matrix(point_aleatoire, ncol = 3), col = "red", size = 10)
    points3d(matrix(resultat$point_projete, ncol = 3), col = "green", size = 10)
    segments3d(rbind(point_aleatoire, resultat$point_projete), col = "purple", lwd = 2)
    text3d(point_aleatoire, texts = "Point aleatoire", col = "red", adj = c(1.1, 1.1))
    text3d(resultat$point_projete, texts = "Point projete", col = "green", adj = c(1.1, 1.1))
  }

  return(list(
    point_aleatoire = point_aleatoire,
    point_projete = resultat$point_projete,
    distance = resultat$distance
  ))
}

# Exemple d'utilisation
# resultat <- projeter_point_sur_maillage("C:/Users/Becard/Documents/GitHub/Projet_Tech_Objets_3D/Data/PLY_files_ASCII/Segmentation_01a-8_ASCII.ply")
