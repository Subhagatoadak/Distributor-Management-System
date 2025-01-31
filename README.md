# Distributor Mapping & Paper Optimization - Shiny App
## Overview
This Shiny app visualizes distributor locations across West Bengal and optimizes paper distribution to ensure that each distributor receives papers with minimal waste (only one extra per distributor).

## Key Features
✅ Interactive Map – Displays distributor locations on a West Bengal map
✅ Paper Distribution Optimization – Ensures minimal waste per distributor
✅ Custom Inputs – Allows users to modify distributor count, paper demand, and allocation rules
✅ Real-time Updates – Adjusts optimization dynamically based on input parameters

## Installation & Setup
1. Install Required Packages
Ensure you have R and Shiny installed. Then, install dependencies:
```r
install.packages(c("shiny", "leaflet", "dplyr", "ggplot2", "sf", "lpSolve"))
```
2. Run the App
Clone the repository and open the app in RStudio or R terminal:

```r
library(shiny)
runApp("path_to_app_folder")
```

## Usage
1. Mapping Distributors
The app loads distributor data and plots their locations using Leaflet maps.
Users can filter distributors by region, demand, or category.
2. Optimizing Paper Distribution
The app runs an optimization algorithm to allocate papers efficiently.
Ensures each distributor gets exactly one extra paper, minimizing waste.
Provides a summary table with optimal allocations.
3. Visualization & Export
Displays before-and-after paper distribution metrics.
Allows users to export optimized allocations as a CSV file.


Customization
Modify input dataset (data/distributors.csv) to include custom distributors.
Adjust optimization constraints in server.R.
Change map themes in ui.R.
Contributing
Pull requests and issue reports are welcome! To contribute:

Fork the repository
Create a feature branch
Commit and push changes
Submit a pull request
License
This project is licensed under MIT License. See LICENSE for details.
