# Retentio

**Retentio** est une interface Shiny développée dans le cadre d’un stage à l’INSERM U1173, sur le site de l’Université de Versailles Saint-Quentin-en-Yvelines (UVSQ).  
Elle permet de **traiter, nettoyer, visualiser et contrôler la qualité des données GC×GC-MS** issues de l’analyse métabolomique de plasma humain et d’échantillons Tenax.

## 🎯 Objectifs principaux

- Automatiser l’importation de fichiers CSV de chromatographie  
- Suivre et corriger les erreurs d’intégration (bruit, pics manqués, doublons)  
- Visualiser dynamiquement les aires, CV (%) et cinétiques des standards internes et FAMEs  
- Exporter des tableaux formatés avec les *"best hits"* et les CV globaux par composé  
- Permettre un ajout incrémental de nouvelles données dans les tableaux de suivi

## 🧬 Contexte

Le projet s’inscrit dans une approche de métabolomique non-ciblée appliquée à la recherche biomédicale (infections, inflammation).  
Il vise à fiabiliser l’intégration des données chromatographiques, détecter les dérives instrumentales et produire un suivi analytique rigoureux.

## 🛠️ Technologies

- **R** et **Shiny**
- `dplyr`, `tidy`, `DT`, `ggplot2`, `readr`, `lubridate`, `stringr`, `purrr`  
- Interface mono-fichier ou projet modulaire `app.R`

## 🛡️ Licence

Tous droits réservés.  
Ce projet est protégé par une licence personnalisée : **aucune réutilisation, copie, modification ou diffusion n’est autorisée sans l’accord écrit de l’auteur**.

📩 Pour toute demande d’utilisation ou d’adaptation, merci de contacter : **shairaags@gmail.com**

---
Développé dans le cadre d’un stage en spectrométrie de masse au sein de l’équipe *Infection et Inflammation* (INSERM U1173).
