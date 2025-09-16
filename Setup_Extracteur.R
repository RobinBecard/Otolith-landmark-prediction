# setup.R
libraries <- c("geomorph", "shiny", "rgl", "bslib", "Rvcg", "shinyjs", "torch", "shinyFiles", "DT", "data.table", "cluster", "stringr")

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
