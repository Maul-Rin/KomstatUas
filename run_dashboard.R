# Script untuk menjalankan Dashboard Analisis Kerentanan Sosial Indonesia
# Pastikan semua dependencies terinstall

# Function to install and load packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Required packages
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr",
  "leaflet", "sf", "jsonlite", "spdep", "htmltools", "plotly",
  "corrplot", "RColorBrewer", "viridis", "shinyjs", "shinycssloaders"
)

cat("=== Dashboard Analisis Kerentanan Sosial Indonesia ===\n")
cat("Checking and installing required packages...\n\n")

# Install and load packages
install_and_load(required_packages)

cat("\n=== All packages loaded successfully! ===\n")
cat("Starting dashboard...\n")
cat("Dashboard will open in your default web browser.\n")
cat("Press Ctrl+C to stop the dashboard.\n\n")

# Run the dashboard
if (file.exists("vulnera_dashboard_fixed.R")) {
  source("vulnera_dashboard_fixed.R")
} else {
  cat("Error: vulnera_dashboard_fixed.R not found!\n")
  cat("Please make sure the dashboard file is in the current directory.\n")
}
