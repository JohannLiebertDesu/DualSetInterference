# Quarto Analysis Template

A standardized template for data analysis projects using Quarto documents in RStudio.

## Overview

This repository serves as a template for data analysis projects using Quarto (.qmd) files in RStudio. It provides a consistent structure for your analysis workflows, making it easier to organize your code, data, and outputs.

## Features

- Pre-configured Quarto document with common sections
- Organized folder structure for data, scripts, and outputs
- Standard YAML header with useful rendering options
- Pre-loaded code chunks for common data analysis tasks
- Consistent styling and formatting

## Getting Started

### Using this Template

1. Click the green "Use this template" button at the top of this repository
2. Select "Create a new repository"
3. Name your repository and click "Create repository from template"
4. Clone the new repository to your local machine
5. Open the project in RStudio by clicking on the .Rproj file

### Alternative Method

If you prefer to use RStudio directly:

1. In RStudio, go to File → New Project → Version Control → Git
2. Paste the repository URL: `https://github.com/chenyu-psy/AnalysisTemplate.git`
3. Choose a directory for your project
4. Click "Create Project"

## Project Structure

```
AnalysisTemplate/
├── README.md                 # Project documentation
├── AnalysisTemplate.Rproj  # RStudio project file
├── template.qmd              # Main Quarto template file
├── data/                     # Data directory
│   └── .gitkeep              # Placeholder for git
├── scripts/                  # R scripts directory
│   └── helper-functions.R    # Helper functions
└── outputs/                  # Generated outputs
    └── .gitkeep              # Placeholder for git
```

## Template Usage

1. Open the `template.qmd` file in RStudio
2. Modify the YAML header with your project details
3. Fill in the template sections with your analysis
4. Use the pre-configured code chunks as starting points
5. Render the document using the "Render" button in RStudio

## Customization

Feel free to customize this template to fit your specific needs:

- Add or remove sections in the Quarto document
- Modify the YAML header to change output formats
- Add additional helper scripts or functions
- Adjust the folder structure as needed

## Requirements

- RStudio (latest version recommended)
- Quarto (installed with recent versions of RStudio)
- R packages:
  - tidyverse
  - ggplot2
  - (add other packages as needed)

## Contributing

If you have suggestions for improving this template, please open an issue or submit a pull request.

---

Created for data analysts who value reproducibility and organization.

