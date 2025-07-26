
# Prompt: How to Build the Saqrmisc R Package

This document outlines the steps to build the `Saqrmisc` R package from the provided source files.

## Context

The current directory contains the necessary components for an R package:
- A `DESCRIPTION` file with package metadata.
- An `R/` directory containing the R source code for the functions.
- A `man/` directory for documentation.
- A `NAMESPACE` file.
- A `runThis.R` script to automate the build process.

## Action

To compile and build the R package, please follow these steps:

1.  **Open your R or RStudio console.**
2.  **Set the working directory** to the root of the `Saqrmisc` project. You can do this with the following command, adjusting the path if necessary:
    ```R
    setwd("C:/Users/USER/Documents/GitHub/Saqrmisc")
    ```
3.  **Execute the build script** by running the following command in your R console:
    ```R
    source("runThis.R")
    ```

## Expected Outcome

The `runThis.R` script will perform the following actions:
1.  Install the `devtools` and `roxygen2` packages if they are not already present.
2.  Use `roxygen2` to generate the official package documentation from the comments in your R scripts and update the `NAMESPACE` file.
3.  Run `devtools::check()` to perform a thorough check of the package, looking for any errors, warnings, or notes. It is important to review the output of this step.
4.  Use `devtools::build()` to compile the package into a `.tar.gz` file, which will be located in the parent directory (`C:/Users/USER/Documents/GitHub/`).

This `.tar.gz` file is the distributable version of your package that can be shared and installed by others.
