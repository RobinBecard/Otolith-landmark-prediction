import trimesh
import os

def convertir_ply_binaire_ascii(input_dir, output_dir):
    # Créer le dossier de sortie s'il n'existe pas
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # Lister tous les fichiers PLY dans le dossier d'entrée
    fichiers_ply = [f for f in os.listdir(input_dir) if f.endswith('.ply')]

    # Traiter chaque fichier
    for fichier in fichiers_ply:
        input_file = os.path.join(input_dir, fichier)
        output_file = os.path.join(output_dir, os.path.splitext(fichier)[0] + ".ply")
        try:
            # Charger et convertir
            mesh = trimesh.load(input_file, file_type='ply')
            mesh.export(output_file, encoding='ascii')
            print(f"Converti : {fichier} -> {output_file}")
        except Exception as e:
            print(f"Erreur lors de la conversion de {fichier} : {e}")

# Exemple d'utilisation
dossier_entree = "../Data/PLY_files_BINARY"  # Dossier contenant les fichiers binaires
dossier_sortie = "../Data/PLY_files_CLEARED" # Dossier pour les fichiers convertis
convertir_ply_binaire_ascii(dossier_entree, dossier_sortie)