# xGShotMap Shiny App ⚽

Application R Shiny interactive pour l'analyse de matches de football avec visualisation des Expected Goals (xG) et cartes de tirs détaillées.

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=RStudio&logoColor=white)

<div align="center">
  <a href="https://rakostats.shinyapps.io/xGShotMap_RakoStats/" target="_blank">
    <svg width="300" height="180" xmlns="http://www.w3.org/2000/svg">
      <!-- Fond terrain de football -->
      <rect width="300" height="180" rx="15" ry="15" fill="url(#grassGradient)" stroke="#ffffff" stroke-width="3"/>
      
      <!-- Dégradé vert terrain -->
      <defs>
        <linearGradient id="grassGradient" x1="0%" y1="0%" x2="100%" y2="100%">
          <stop offset="0%" style="stop-color:#2d5a27"/>
          <stop offset="50%" style="stop-color:#4a7c59"/>
          <stop offset="100%" style="stop-color:#2d5a27"/>
        </linearGradient>
        
        <!-- Effet de surbrillance -->
        <filter id="glow">
          <feGaussianBlur stdDeviation="3" result="coloredBlur"/>
          <feMerge> 
            <feMergeNode in="coloredBlur"/>
            <feMergeNode in="SourceGraphic"/> 
          </feMerge>
        </filter>
      </defs>
      
      <!-- Ligne centrale -->
      <line x1="150" y1="25" x2="150" y2="155" stroke="white" stroke-width="2"/>
      
      <!-- Cercle central -->
      <circle cx="150" cy="90" r="25" fill="none" stroke="white" stroke-width="2"/>
      
      <!-- Surface de réparation gauche -->
      <rect x="25" y="55" width="35" height="70" fill="none" stroke="white" stroke-width="2"/>
      
      <!-- Surface de réparation droite -->
      <rect x="240" y="55" width="35" height="70" fill="none" stroke="white" stroke-width="2"/>
      
      <!-- But gauche -->
      <rect x="20" y="78" width="5" height="24" fill="white"/>
      
      <!-- But droit -->
      <rect x="275" y="78" width="5" height="24" fill="white"/>
      
      <!-- Ballon animé -->
      <text x="260" y="35" font-size="20" fill="white" opacity="0.8">⚽</text>
      
      <!-- Texte principal -->
      <text x="150" y="75" font-family="Arial, sans-serif" font-size="28" font-weight="bold" 
            fill="#ffff00" text-anchor="middle" filter="url(#glow)">xG</text>
      
      <text x="150" y="95" font-family="Arial, sans-serif" font-size="12" font-weight="bold" 
            fill="white" text-anchor="middle" letter-spacing="2px">SHOTMAP</text>
      
      <text x="150" y="115" font-family="Arial, sans-serif" font-size="10" 
            fill="#e0e0e0" text-anchor="middle" font-style="italic">🚀 Cliquez pour lancer l'app</text>
    </svg>
  </a>
</div>
## ✨ Fonctionnalités

- **Analyse par équipe** : Sélectionnez votre équipe favorite dans 5 ligues européennes
- **Visualisation xG en temps réel** : Timeline des Expected Goals pendant le match
- **Cartes de tirs interactives** : Positionnement et résultat de chaque tir
- **Interface intuitive** : Dashboard moderne avec Shiny

## 🏆 Ligues supportées

- **EPL** (Premier League anglaise)
- **Ligue 1** (France) 
- **Bundesliga** (Allemagne)
- **Serie A** (Italie)
- **La Liga** (Espagne)

## 🚀 Installation et utilisation

### Prérequis
- R version 4.0.0 ou supérieure
- RStudio (recommandé)

### Installation des packages

```r
# Packages requis
packages <- c(
  "shiny", "shinydashboard", "DT", "tidyverse", 
  "ggplot2", "ggsoccer", "patchwork", "ggtext", 
  "ggrepel", "worldfootballR"
)

# Installation automatique des packages manquants
install.packages(packages[!packages %in% installed.packages()[,"Package"]])
