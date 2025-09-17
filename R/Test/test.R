source("../Setup.R") # Assurez-vous que ce fichier ne redéfinit pas options(rgl.useNULL = ...)

# options(rgl.useNULL = TRUE) # Cette ligne n'est pas nécessaire ici, elle est potentiellement dans Setup.R

ui <- fluidPage(
  titlePanel("Gestion de deux scènes 3D avec rgl"),
  sidebarLayout(
    sidebarPanel(
      actionButton("create_scene1", "Créer la Scène 1"),
      actionButton("create_scene2", "Créer la Scène 2"),
      actionButton("modify_scene1", "Modifier la Scène 1"),
      actionButton("modify_scene2", "Modifier la Scène 2"),
      actionButton("clear_scene1", "Effacer la Scène 1"),
      actionButton("clear_scene2", "Effacer la Scène 2"),
      actionButton("close_all", "Fermer toutes les Scènes")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 6,
          tabPanel("Scène 1", rglwidgetOutput("scene1")),
        ),
        column(
          width = 6,
          tabPanel("Scène 2", rglwidgetOutput("scene2"))
        )
      ),
    )
  )
)

server <- function(input, output, session) {
  scene1_id <- reactiveVal(NULL)
  scene2_id <- reactiveVal(NULL)

  observeEvent(input$create_scene1, {
    if (is.null(scene1_id())) {
      # open3d(useNULL = TRUE) # NE PAS appeler open3d ici !
      scene1_id(rgl.cur()) # sauvegarder l'id de la scene (vaudrait mieux le récupérer APRES le plot)
      output$scene1 <- renderRglwidget({
        open3d(useNULL = TRUE) # **Appeler open3d ICI, dans renderRglwidget**
        plot3d(rnorm(100), rnorm(100), rnorm(100), col = "red", size = 3) # afficher des points sur la scène
        scene1_id(rgl.cur()) # Récupérer l'id de la scene APRÈS le plot pour être sûr qu'elle est créée dans le bon contexte
        rglwidget()
      }) # rendre la scène dans le widget rgl
    } else {
      showNotification("Scène 1 déjà créée.", type = "warning")
    }
  })

  observeEvent(input$create_scene2, {
    if (is.null(scene2_id())) {
      # open3d(useNULL = TRUE) # NE PAS appeler open3d ici !
      scene2_id(rgl.cur()) # sauvegarder l'id de la scene (vaudrait mieux le récupérer APRES le plot)
      output$scene2 <- renderRglwidget({
        open3d(useNULL = TRUE) # **Appeler open3d ICI, dans renderRglwidget**
        plot3d(rnorm(100), rnorm(100), rnorm(100), col = "blue", size = 3)
        scene2_id(rgl.cur()) # Récupérer l'id de la scene APRÈS le plot
        rglwidget()
      })
    } else {
      showNotification("Scène 2 déjà créée.", type = "warning")
    }
  })

  observeEvent(input$modify_scene1, {
    if (!is.null(scene1_id())) {
      set3d(scene1_id())
      output$scene1 <- renderRglwidget({ # Le renderRglwidget doit englober toute la définition de la scène à afficher.
        open3d(useNULL = TRUE) # **Appeler open3d ICI**
        set3d(scene1_id()) # Réappliquer la scène active (redondant ici mais peut être utile dans des cas plus complexes)
        spheres3d(rnorm(10), rnorm(10), rnorm(10), col = "green", radius = 0.1)
        rglwidget()
      })
    } else {
      showNotification("Scène 1 n'existe pas.", type = "error")
    }
  })

  observeEvent(input$modify_scene2, {
    if (!is.null(scene2_id())) {
      set3d(scene2_id())
      output$scene2 <- renderRglwidget({ # Mettre à jour TOUT le renderRglwidget
        open3d(useNULL = TRUE) # **Appeler open3d ICI**
        set3d(scene2_id()) # Réappliquer la scène active
        spheres3d(rnorm(10), rnorm(10), rnorm(10), col = "yellow", radius = 0.1)
        rglwidget()
      })
    } else {
      showNotification("Scène 2 n'existe pas.", type = "error")
    }
  })

  observeEvent(input$clear_scene1, {
    if (!is.null(scene1_id())) {
      set3d(scene1_id())
      clear3d() # Efface tout le contenu de la scène active
      scene1_id(NULL) # Réinitialise l'ID pour permettre une recréation
      output$scene1 <- renderRglwidget({
        NULL # Pour effacer l'affichage, render NULL, pas besoin d'open3d ici car pas de scène rgl à afficher.
      })
    } else {
      showNotification("Scène 1 n'existe pas ou est déjà vide.", type = "error")
    }
  })

  observeEvent(input$clear_scene2, {
    if (!is.null(scene2_id())) {
      set3d(scene2_id())
      clear3d() # Efface tout le contenu de la scène active
      scene2_id(NULL) # Réinitialise l'ID pour permettre une recréation
      output$scene2 <- renderRglwidget({
        NULL # Pour effacer l'affichage, render NULL
      })
    } else {
      showNotification("Scène 2 n'existe pas ou est déjà vide.", type = "error")
    }
  })

  observeEvent(input$close_all, {
    rgl.quit()
    scene1_id(NULL)
    scene2_id(NULL)
    output$scene1 <- renderRglwidget({
      NULL # Pour effacer l'affichage
    })
    output$scene2 <- renderRglwidget({
      NULL # Pour effacer l'affichage
    })
  })
}

shinyApp(ui, server)
