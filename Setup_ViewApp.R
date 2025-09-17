# setup.R
libraries <- c("geomorph", "shiny", "plotly", "bslib", "Rvcg", "shinyjs", "colourpicker", "DT")
# Fonction pour charger ou installer automatiquement les librairies
load_libraries <- function(libraries) {
  for (lib in libraries) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}
load_libraries(libraries)
