# Fonction pour obtenir les limites de la boîte englobante à partir de l'objet
get_bbox <- function(rv) {
    return(list(
        x = range(rv$vertices[, 1]),
        y = range(rv$vertices[, 2]),
        z = range(rv$vertices[, 3])
    ))
}

# Fonction pour créer la vue principale
create_main_view <- function(rv) {
    req(rv$vertices, rv$faces)

    # Créer les coordonnées i, j, k pour plotly
    i <- rv$faces[, 1] - 1
    j <- rv$faces[, 2] - 1
    k <- rv$faces[, 3] - 1

    # Créer le plot avec la trace de base
    p <- plot_ly(source = "main") %>%
        add_trace(
            type = "mesh3d",
            x = rv$vertices[, 1],
            y = rv$vertices[, 2],
            z = rv$vertices[, 3],
            i = i,
            j = j,
            k = k,
            facecolor = rep(rv$visual_params$couleur, nrow(rv$faces)),
            opacity = rv$visual_params$opacite,
            lighting = list(
                ambient = 0.8,
                diffuse = 0.9,
                specular = 0.5,
                roughness = 0.5,
                fresnel = 0.2
            ),
            name = "Spécimen"
        )

    # Ajouter les points si l'option est activée
    if (rv$visual_params$afficher_points_principal &&
        !is.null(rv$points_data) &&
        is.matrix(rv$points_data) &&
        ncol(rv$points_data) == 3) {
        p <- p %>%
            add_trace(
                type = "scatter3d",
                x = rv$points_data[, 1],
                y = rv$points_data[, 2],
                z = rv$points_data[, 3],
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
            )
    }

    # Configuration de la mise en page - interactive
    p <- p %>%
        layout(
            scene = list(
                camera = list(
                    eye = list(x = 1.5, y = 1.5, z = 1.5)
                ),
                aspectmode = "data",
                xaxis = list(
                    title = "X",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                ),
                yaxis = list(
                    title = "Y",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                ),
                zaxis = list(
                    title = "Z",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                )
            ),
            margin = list(l = 0, r = 0, b = 0, t = 0),
            # Ajouter une annotation pour afficher le volume
            annotations = list(
                list(
                    x = 0.01,
                    y = 0.99,
                    xref = "paper",
                    yref = "paper",
                    text = paste0("Volume: ", sprintf("%.2f μm³", rv$volume)),
                    showarrow = FALSE,
                    font = list(size = 12, color = "rgba(0,0,0,0.7)"),
                    bgcolor = "rgba(255,255,255,0.7)",
                    bordercolor = "rgba(0,0,0,0.3)",
                    borderwidth = 1,
                    borderpad = 4,
                    align = "left"
                )
            )
        )

    return(p)
}

# Fonction pour créer la vue interne (face interne)
create_internal_view <- function(rv) {
    req(rv$vertices, rv$faces)

    # Créer les coordonnées i, j, k pour plotly
    i <- rv$faces[, 1] - 1
    j <- rv$faces[, 2] - 1
    k <- rv$faces[, 3] - 1

    # Créer le plot
    p <- plot_ly(source = "internal") %>%
        add_trace(
            type = "mesh3d",
            x = rv$vertices[, 1],
            y = rv$vertices[, 2],
            z = rv$vertices[, 3],
            i = i,
            j = j,
            k = k,
            facecolor = rep(rv$visual_params$couleur, nrow(rv$faces)),
            opacity = rv$visual_params$opacite,
            lighting = list(
                ambient = 0.8,
                diffuse = 0.9,
                specular = 0.5,
                roughness = 0.5,
                fresnel = 0.2
            ),
            name = "Spécimen"
        )

    # Ajouter les points si disponibles
    if (!is.null(rv$points_data) && is.matrix(rv$points_data) && ncol(rv$points_data) == 3) {
        p <- p %>%
            add_trace(
                type = "scatter3d",
                x = rv$points_data[, 1],
                y = rv$points_data[, 2],
                z = rv$points_data[, 3],
                mode = "markers+text",
                marker = list(
                    size = 5,
                    color = rv$visual_params$couleur_points, # Utiliser la couleur des points de visual_params
                    line = list(color = "black", width = 0.5)
                ),
                text = paste0("P", seq_len(nrow(rv$points_data))),
                textposition = "top center",
                hoverinfo = "text",
                name = "Points de référence"
            )
    }

    # Configuration de la mise en page avec une vue de face interne - rotation fixe
    p <- p %>%
        layout(
            scene = list(
                camera = list(
                    eye = list(x = 0, y = -2, z = 0),
                    up = list(x = 0, y = 0, z = 1)
                ),
                # Utiliser dragmode pan au lieu de turntable pour empêcher la rotation
                dragmode = "pan",
                aspectmode = "data",
                xaxis = list(
                    title = "X",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                ),
                yaxis = list(
                    title = "Y",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                ),
                zaxis = list(
                    title = "Z",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                )
            ),
            # Ajouter des instructions plus claires pour l'utilisateur
            annotations = list(
                list(
                    showarrow = FALSE,
                    x = 0.95,
                    y = 0.95,
                    xref = "paper",
                    yref = "paper",
                    font = list(size = 10, color = "rgba(0,0,0,0.6)")
                )
            ),
            margin = list(l = 0, r = 0, b = 0, t = 0)
        )

    return(p)
}

# Fonction pour créer la vue externe (face externe)
create_external_view <- function(rv) {
    req(rv$vertices, rv$faces)

    # Créer les coordonnées i, j, k pour plotly
    i <- rv$faces[, 1] - 1
    j <- rv$faces[, 2] - 1
    k <- rv$faces[, 3] - 1

    # Créer le plot
    p <- plot_ly(source = "external") %>%
        add_trace(
            type = "mesh3d",
            x = rv$vertices[, 1],
            y = rv$vertices[, 2],
            z = rv$vertices[, 3],
            i = i,
            j = j,
            k = k,
            facecolor = rep(rv$visual_params$couleur, nrow(rv$faces)),
            opacity = rv$visual_params$opacite,
            lighting = list(
                ambient = 0.8,
                diffuse = 0.9,
                specular = 0.5,
                roughness = 0.5,
                fresnel = 0.2
            ),
            name = "Spécimen"
        )

    # Ajouter les points si disponibles
    if (!is.null(rv$points_data) && is.matrix(rv$points_data) && ncol(rv$points_data) == 3) {
        p <- p %>%
            add_trace(
                type = "scatter3d",
                x = rv$points_data[, 1],
                y = rv$points_data[, 2],
                z = rv$points_data[, 3],
                mode = "markers+text",
                marker = list(
                    size = 5,
                    color = rv$visual_params$couleur_points, # Utiliser la couleur des points de visual_params
                    line = list(color = "black", width = 0.5)
                ),
                text = paste0("P", seq_len(nrow(rv$points_data))),
                textposition = "top center",
                hoverinfo = "text",
                name = "Points de référence"
            )
    }

    # Configuration de la mise en page avec une vue de face externe - rotation fixe
    p <- p %>%
        layout(
            scene = list(
                camera = list(
                    eye = list(x = 0, y = 2, z = 0),
                    up = list(x = 0, y = 0, z = 1)
                ),
                # Utiliser dragmode pan au lieu de turntable pour empêcher la rotation
                dragmode = "pan",
                aspectmode = "data",
                xaxis = list(
                    title = "X",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                ),
                yaxis = list(
                    title = "Y",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                ),
                zaxis = list(
                    title = "Z",
                    showspikes = FALSE,
                    showaxes = TRUE,
                    showgrid = TRUE,
                    showline = TRUE
                )
            ),
            # Ajouter des instructions plus claires pour l'utilisateur
            annotations = list(
                list(
                    showarrow = FALSE,
                    x = 0.95,
                    y = 0.95,
                    xref = "paper",
                    yref = "paper",
                    font = list(size = 10, color = "rgba(0,0,0,0.6)")
                )
            ),
            margin = list(l = 0, r = 0, b = 0, t = 0)
        )

    return(p)
}

# Fonction pour calculer l'orientation optimale de la caméra
calculate_camera_orientation <- function(points, distance = 2) {
    # Vérifier que nous avons assez de points pour définir un plan
    if (is.null(points) || nrow(points) < 3) {
        # Orientation par défaut si pas assez de points
        return(list(
            internal = list(x = 0, y = -distance, z = 0),
            external = list(x = 0, y = distance, z = 0)
        ))
    }

    # Centrer les points
    centered_points <- scale(points, center = TRUE, scale = FALSE)

    # Utiliser la SVD (décomposition en valeurs singulières) pour trouver le plan optimal
    # C'est équivalent à une PCA (analyse en composantes principales)
    svd_result <- svd(centered_points)

    # Le vecteur singulier correspondant à la plus petite valeur singulière
    # est le vecteur normal au plan optimal
    normal_vector <- svd_result$v[, ncol(svd_result$v)]

    # S'assurer que le vecteur est de longueur unitaire
    normal_vector <- normal_vector / sqrt(sum(normal_vector^2))

    # Multiplier par la distance souhaitée
    camera_external <- normal_vector * distance
    camera_internal <- -normal_vector * distance # Direction opposée

    # Retourner les positions de la caméra pour les vues interne et externe
    return(list(
        internal = list(x = camera_internal[1], y = camera_internal[2], z = camera_internal[3]),
        external = list(x = camera_external[1], y = camera_external[2], z = camera_external[3])
    ))
}
