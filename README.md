# geometric-depth-r

An R project for statistical depth, geometric analysis, and spatial point process data.

The main focus is on spatial event data, especially applications such as basketball shot locations and movement trajectories.

---

## Overview

This project provides tools for analyzing spatial and geometric data using depth-based and distance-based methods.

It is designed for exploratory and computational work on spatial point patterns and related structured data.

---

## Main Focus

The primary application area is **spatial point processes**, including:

* basketball shot locations
* player movement trajectories
* event-based spatial datasets
* general spatial pattern analysis

---

## Core Components

### Spatial Analysis

* representation of spatial point patterns
* basic spatial structure analysis
* trajectory and event handling

### Depth-Based Methods

* spatial depth concepts
* centrality measures
* robust ranking of spatial configurations
* detection of spatial outliers

### Geometry & Distances

* distance functions between point patterns
* geometric comparison of spatial structures
* similarity measures for configurations

---

## Project Structure

```text id="k3n9pq"

geometric-depth-r/
│
├── R/              # reusable functions (geometry, depth, utilities)experiments
├── notebooks/      # explanatory analyses (Rmd )
├── data/           # datasets
├── experiments/    # simulations and basketball studies
├── figures/        # saved plots
│
├── geometric-depth-r.Rproj
├── README.md
├── .gitignore
└── LICENSE
```

---

## Workflow

* **R/** → reusable functions (tools and methods)
* **scripts/** → computations and experiments
* **notebooks/** → explained analyses with results and plots
* **experiments/** → exploratory studies (e.g. basketball simulations)

---

## Typical Use Cases

* analyzing basketball shot distributions
* studying spatial event patterns
* comparing spatial configurations
* simulating point processes
* visualizing spatial structures

---

## Installation

```r id="d8s2lm"
git clone https://github.com/yourname/geometric-depth-r.git
```

---

## Status

In development, focused on spatial point process analysis and geometric methods.
