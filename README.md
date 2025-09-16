# Analyse d'Objets 3D - Otolithes

Ce projet permet l'analyse d'objets 3D, spécifiquement des otolithes (structures calcifiées de l'oreille interne des poissons), à travers deux applications complémentaires : une pour l'extraction de points caractéristiques (landmarks) et une pour la visualisation interactive des modèles 3D.

## Fonctionnalités principales

### Application d'extraction de points caractéristiques (Extracteur_App)

- Chargement de fichiers PLY (ASCII ou binaire avec conversion automatique)
- Traitement par lots de plusieurs fichiers
- Extraction automatique de points caractéristiques via un modèle de prédiction
- Projection des points sur la surface du maillage 3D
- Calcul du volume de l'objet
- Export des données dans un format texte standardisé

### Application de visualisation (View_App)

- Visualisation interactive des modèles 3D
- Affichage des faces interne et externe des otolithes
- Visualisation des points caractéristiques
- Configuration des paramètres visuels (couleur, opacité)
- Analyse des descripteurs (volume, dimensions)

## Structure du projet

```
Projet_Tech_Objets_3D/
│
├── R/                          # Scripts R principaux
│   ├── Extracteur_App.R        # Application d'extraction de landmarks
│   ├── View_App.R              # Application de visualisation 3D
│   ├── Landmark_correction.R   # Fonctions de correction des landmarks
│   ├── Landmarks_prediction_APP.R # Prédiction de points caractéristiques
│   ├── utils.R                 # Fonctions utilitaires générales
│   ├── utils_extracteur.R      # Fonctions spécifiques à l'extracteur
│   └── utils_ViewApp.R         # Fonctions spécifiques à la visualisation
│
├── Setup_Extracteur.R          # Configuration des librairies pour l'extracteur
├── Setup_ViewApp.R             # Configuration des librairies pour la visualisation
│
├── Pytorch/                    # Modèles PyTorch pour la prédiction
│   ├── Landmarks_NN_4.pt       # Modèle entraîné pour la prédiction de landmarks
│   └── Model_Optimizer_4.pt    # État de l'optimiseur du modèle
│
└── Data/                       # Dossier pour les données (à créer)
    ├── PLY_files_ASCII/        # Fichiers PLY au format ASCII
    └── points.txt              # Fichier d'export des landmarks
```

## Prérequis

Ce projet nécessite R (version 4.0 ou supérieure) et les packages suivants :

- geomorph
- shiny
- plotly
- bslib
- Rvcg
- shinyjs
- colourpicker
- DT
- torch
- rgl
- shinyFiles
- data.table
- cluster
- stringr

## Installation

1. Clonez ce dépôt :

```
git clone https://github.com/votre-utilisateur/Projet_Tech_Objets_3D.git
```

2. Lancez R et installez les dépendances requises :

```r
# Pour l'extracteur
source("Setup_Extracteur.R")

# Pour la visionneuse
source("Setup_ViewApp.R")
```

## Utilisation

### Démarrage des applications

1. Pour l'extracteur de points caractéristiques :

```r
source("Extracteur_App.R")
```

2. Pour la visionneuse 3D :

```r
source("View_App.R")
```

### Application d'extraction

1. Sélectionnez un ou plusieurs fichiers PLY ou un dossier contenant des fichiers PLY
2. Les fichiers seront traités automatiquement et les points caractéristiques extraits
3. Consultez le tableau pour voir les points extraits pour chaque fichier
4. Exportez les landmarks dans un fichier texte

### Application de visualisation

1. Sélectionnez un fichier PLY à visualiser
2. Utilisez les différentes vues (Isométrique, Dessus, Face) pour explorer le modèle
3. Ajustez les paramètres visuels (couleur, opacité)
4. Consultez l'onglet "Descripteurs" pour les informations détaillées

## Format des données

### Format PLY

L'application accepte les fichiers au format PLY (Polygon File Format), également connu sous le nom de Stanford Triangle Format. Les fichiers PLY peuvent être en format ASCII ou binaire (conversion automatique).

### Format de sortie des landmarks

Le fichier d'export des landmarks suit un format spécifique :

```
LM3=6 VOLUME=123.456
0.12345 0.23456 0.34567
0.23456 0.34567 0.45678
...
ID=nom_du_fichier
```

## Remarques techniques

- Le modèle de prédiction utilise PyTorch via le package torch pour R
- La projection des landmarks sur la surface s'effectue par calcul géométrique des triangles les plus proches
- Le volume est calculé à partir du maillage fermé de l'objet 3D

## Auteurs

Adrien, Rémi et Robin
