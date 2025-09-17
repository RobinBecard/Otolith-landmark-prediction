source("Setup.R")

options(rgl.useNULL = TRUE)

ui <- fluidPage(
  titlePanel("Sélection de fichier"),
  tags$head(
    tags$style(HTML("
      /* Titre du panel réduit */
      .shiny-title { font-size: 8px; }

      /* Bordure autour du panel principal (container) */
      .tab-content {
        border: 2px solid #333;  /* Bordure autour du contenu principal */
        padding: 10px;
      }

      /* Bordure autour du panneau latéral (sidebar) */
      .sidebar {
        border-right: 2px solid #333; /* Bordure droite autour du sidebar */
      }

      /* Optionnel : Bordure autour des onglets */
      .nav-tabs > li > a {
        border: 1px solid #333;  /* Bordure autour des onglets */
      }

      /* Centrer le contenu du plot 3D */
      .rglwidget-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
      }
    "))
  ),
  fileInput(
    inputId = "fileChosen",
    label = "Sélectionnez un fichier PLY (ASCII):",
    multiple = FALSE,
    accept = ".ply",
    buttonLabel = "Parcourir...",
    placeholder = "Aucun fichier sélectionné"
  ),
  textOutput("fileInfo"),
  textOutput("fileDetails"),
  rglwidgetOutput("plot3d", width = "100%", height = "600px") # Pour afficher le modèle 3D
)

server <- function(input, output, session) {
  # Informations de base sur le fichier
  output$fileInfo <- renderText({
    if (is.null(input$fileChosen)) {
      "Aucun fichier sélectionné."
    } else {
      paste(
        "Nom du fichier :", input$fileChosen$name, "\n",
        "Chemin du fichier :", input$fileChosen$datapath
      )
    }
  })

  # Observer la sélection de fichier
  observeEvent(input$fileChosen, {
    if (!is.null(input$fileChosen)) {
      chemin1 <- input$fileChosen$datapath

      # Lire le fichier PLY
      specimen <- read.ply(chemin1, ShowSpecimen = FALSE, addNormals = TRUE)

      # Afficher des détails textuels
      output$fileDetails <- renderText({
        paste(
          "Classe de l'objet :", class(specimen), "\n",
          "Structure de l'objet :", capture.output(str(specimen))
        )
      })

      rgl.clear() # Nettoyer les précédentes scènes
      shade3d(specimen, color = "gray") # Rendu sans ombre forte
      bg3d(color = "lightblue") # Définir un fond bleu clai

      # Intégrer la scène 3D dans Shiny
      output$plot3d <- renderRglwidget({
        rglwidget()
      })
    }
  })
}

shinyApp(ui = ui, server = server)
