# 3D objects analysis  - Otoliths

This project focuses on the automation and analysis of 3D objects. Otoliths, small calcified structures found in the inner ear of fish, are commonly used in fisheries science to estimate age, growth, and population structure. By leveraging machine learning and 3D geometry analysis, this tool aims to identify key morphological landmarks on otolith meshes (.ply files), enabling faster and more consistent analysis for ecological and biological research.

Two applications were developed as part of this project: one for landmark extraction, and another for visualization and analysis of the 3D otolith objects.

## Main features

### Landmark Extraction Application (Extract_App)

- Loading of PLY files (ASCII or binary with automatic conversion)
- Batch processing of multiple files
- Automatic extraction of characteristics points using a prediction model
- Projection of landmarks onto the surface of the 3D mesh
- Volume calculation of the object
- Export of data in a standardized text format

### Visualization Application (View_App)

- Interactive visualization of 3D models
- Display of internal and external surfaces of otoliths
- Visualization of characteristic points
- Configuration of visual parameters (color, opacity)
- Analysis of descriptors (volume, dimensions=

## Project structure

```
Projet_Tech_Objets_3D/
│
├── R/                          # Main R scripts
│   ├── Extractor_App.R        # Landmarks extraction application
│   ├── View_App.R              # 3D visualization application
│   ├── Landmark_correction.R   # Landmarks adjustment functions
│   ├── Landmarks_prediction_APP.R # Landmarks prediction
│   ├── utils.R                 # General Utility Functions
│   ├── utils_extractor.R      # Extractor-Specific Functions
│   └── utils_ViewApp.R         # Visualization-Specific Functions
│
├── Setup_Extractor.R          # Library configuration for the extraction 
├── Setup_ViewApp.R             # Library configuration for the visualization 
│
├── Pytorch/                    # PyTorch models for landmark prediction
│   ├── Landmarks_NN_4.pt       # Trained model for landmark prediction
│   └── Model_Optimizer_4.pt    # Optimizer state for the model
│
└── Data/                       # Data folder
    ├── PLY_files_ASCII/        # PLY files in ASCII format
    └── points.txt              # Exported landmark data
```

## Prerequisites

This project requires R (version 4.0 or higher) and the following packages:

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

1. Clone this repository:

```
git clone https://github.com/your-user/Otolith-landmark-prediction.git
```

2. Open R and install the required dependencies:

```r
# For the extractor
source("Setup_Extracteur.R")

# For the viewer
source("Setup_ViewApp.R")
```

## Usage

### Launching the Applications

1. To start the landmark extraction application:

```r
source("Extracteur_App.R")
```

2. To start the 3D viewer application:

```r
source("View_App.R")
```

### Landmark Extraction Application

1. Select one or more PLY files, or a folder containing PLY files.
2. The files will be processed automatically and characteristic points will be extracted.
3. View the table to see the extracted landmarks for each file.
4. Export the landmarks to a standardized text file.

### Visualization Application

1. Select a PLY file to visualize.
2. Use different views (Isometric, Top, Front) to explore the model.
3. Adjust visual parameters (color, opacity).
4. Check the "Descriptors" tab for detailed information.

## Data Format

### PLY Format

The application supports PLY files (Polygon File Format), also known as Stanford Triangle Format. Files can be in ASCII or binary format (automatic conversion is handled).

### Landmark Output Format

The exported landmark file follows a specific format:

```
LM3=6 VOLUME=123.456
0.12345 0.23456 0.34567
0.23456 0.34567 0.45678
...
ID=nom_du_fichier
```

## Technical Notes

- The prediction model uses PyTorch via the torch package for R.
- Landmark projection onto the surface is performed using geometric calculations of the nearest triangles.
- Volume is computed from the closed mesh of the 3D object.

## Auteur

Robin BECARD
