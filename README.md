# geometric-depth-r

This repository contains an R-based workflow for analyzing basketball shot data with geometric and statistical depth methods. The focus is on comparing shooting patterns across players and games, identifying typical versus atypical shot behavior, and exploring spatial structure through clustering and distance-based approaches.

## What the project does

The analysis pipeline combines:

- NBA shot data retrieval from the NBA stats API
- conversion of raw shot data into per-game spatial objects
- Tukey / directional depth computations for centrality and outlier analysis
- DD-plots to compare made and missed shots
- spatial visualizations such as shot charts, heatmaps, and KDE-based density maps
- clustering and distance methods (MDS, hierarchical clustering, KDE distance, Wasserstein, Baddeley) to compare players or matches

## Repository structure

- R/ : reusable helper functions for downloading NBA data, reshaping it, and producing depth-based plots
- notebooks/ : Quarto/R notebooks for exploratory analyses, clustering, robustness tests, and depth comparisons
- data/ : CSV and RData files used by the analyses
- figures/ : generated plots and exported figures
- experiments/ : additional experiments and study scripts

## Typical workflow

1. Load shot data for one or several players.
2. Transform the data into per-game objects with coordinates and made/missed flags.
3. Compute depth values to identify typical and atypical games.
4. Compare distributions using DD-plots and spatial distance measures.
5. Explore player typologies with clustering and multidimensional scaling.

## Quick start

```r
source("R/nba-functions.R")
source("R/nba_functions_database.R")

shots <- nba_data("Stephen Curry", 2019)
games <- nba_data_depth(shots)
made  <- nba_data_depth_made(games)
missed <- nba_data_depth_missed(games)

out <- ddplot_nba(missed, made, Ndirs = 250, parConst1 = -2, parConst2 = 5)
```

## Main dependencies

The notebooks and scripts rely mainly on:

- httr, jsonlite, dplyr
- ggplot2, ggpubr, patchwork
- curveDepth
- spatstat
- transport

## Notes

- Some notebooks assume the working directory is the project root or the notebooks folder, so paths may need to be adjusted depending on where you run them.
- NBA data access depends on network availability and may be rate-limited by the API.

This project is research-oriented and is mainly intended for exploratory analysis rather than production-ready packaging.
