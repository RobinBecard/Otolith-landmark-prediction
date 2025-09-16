# Fonction pour créer une vue 3D
edit_view <- function(input, output, id, data, name, fonction) {
  # vérifier la validité des paramètres
  if (is.null(id)) {
    return()
  }

  try(
    {
      set3d(id)

      # Appliquer la fonction sur la donnée
      fonction(data)

      # Afficher sur la scène
      output[[view_type]] <- renderRglwidget({
        rglwidget()
      })
    },
    silent = TRUE
  )
}
