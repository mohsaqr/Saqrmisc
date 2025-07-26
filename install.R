# Saqrmisc Package Installation Script
# 
# This script helps you install the Saqrmisc package from GitHub.
# Run this script in R to install the package and its dependencies.

# Check if devtools is installed, if not install it
if (!require(devtools, quietly = TRUE)) {
  message("Installing devtools...")
  install.packages("devtools")
}

# Install Saqrmisc from GitHub
message("Installing Saqrmisc package from GitHub...")
devtools::install_github("mohsaqr/Saqrmisc")

# Load the package
message("Loading Saqrmisc package...")
library(Saqrmisc)

# Display installation success message
message("Saqrmisc package has been successfully installed and loaded!")
message("You can now use functions like:")
message("- run_full_moe_analysis()")
message("- generate_comparison_plots()")
message("- mosaic_analysis()")
message("- view_results()")

# Show package information
message("\nPackage information:")
packageVersion("Saqrmisc") 