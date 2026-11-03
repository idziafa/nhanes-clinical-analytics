# Clinical Health Indicators — NHANES R Analytics Portfolio

**Exploratory analysis • Reproducible Quarto report • Interactive Shiny dashboard**

A practical R data analytics project examining adult BMI, blood pressure, age relationships and data completeness using the CRAN `NHANES` package's educational resample of US NHANES 2009–2012 data.

> **Important limitation:** These unweighted, unique-ID descriptive summaries are **not nationally representative prevalence estimates**. The source educational sample includes resampled repeated participant IDs. This project selects one consistent record per adult ID for teaching participant-level analysis; it does not reconstruct the original complex survey design. No diagnosis or causal conclusion is intended.

## Explore the Project

**[View Interactive Shiny Dashboard](https://idziafa-analytics.shinyapps.io/nhanes-clinical-analytics/)**

Explore the clinical data through interactive charts, filters,
summary statistics, and downloadable results.

**[Read the Analytical Report](https://idziafa.github.io/nhanes-clinical-analytics/)**

View the reproducible Quarto report covering data preparation,
descriptive analysis, visualisations, and interpretation.

## Repository layout

```text
nhanes_clinical_analytics/
├── README.md
├── .gitignore
├── data/                       # derived RDS files (generated locally)
├── scripts/
│   ├── 01_prepare_data.R       # derive datasets, audit repeated IDs
│   └── 02_export_summaries.R   # create descriptive CSV tables
├── reports/
│   └── clinical_health_report.qmd  # executable Quarto report
├── dashboard/
│   ├── app.R                   # working Shiny v2 application
│   └── app_v2.R                # named copy of the same application
└── outputs/                    # generated summary CSV files
```

## Requirements

- R (tested by author on R 4.3.3)
- R packages: `NHANES`, `dplyr`, `ggplot2`, `shiny`, `bslib`, `knitr`, `rmarkdown`
- Quarto CLI for HTML report rendering (check `quarto --version`). The Quarto CLI is a separate program from the R packages.

Check installed packages in R:

```r
needed <- c("NHANES", "dplyr", "ggplot2", "shiny", "bslib", "knitr", "rmarkdown")
setdiff(needed, rownames(installed.packages()))
```

If this returns `character(0)`, all listed R packages are available. If needed, install only missing packages from CRAN. If your Linux R environment has restricted CRAN access, download source package archives through a browser and install locally, as with the NHANES package.

## Reproduce the complete analysis

Open a terminal in **the project root** (the folder containing `scripts/`, `data/` and `dashboard/`):

```bash
Rscript scripts/01_prepare_data.R
Rscript scripts/02_export_summaries.R
quarto render reports/clinical_health_report.qmd
```

The generated report is `reports/clinical_health_report.html`; open it in a browser. The CSV results appear in `outputs/`. The report must render *on a machine with R, the required packages and Quarto*; the HTML output is shareable independently after rendering.

To run the interactive application, open RStudio in the project root and enter:

```r
shiny::runApp("dashboard")
```

The dashboard reads `../data/adult_unique_data.rds` relative to its own directory. **Do not execute `dashboard/app.R` line by line from the project root.**

## Method summary

1. Load `NHANES::NHANES` (10,000 rows in the user's installed version; variable count may vary by version).
2. Retain 9 clinical variables and filter to adults (`Age >= 18`).
3. Identify repeated participant IDs and **check consistency across all selected measurements**. Stop and report IDs if any conflict.
4. Retain one row per adult ID for the descriptive dashboard, leaving missing measurements as `NA`.
5. Create BMI categories with standard adult thresholds.
6. Generate descriptive tables, missingness counts, clinical visualisations and a Quarto report.
7. Offer interactive sex/age/BMI filters and filter-specific descriptive interpretations in Shiny.

## Reproducibility and ethics notes

- The `data/*.rds` files are generated, not the source of truth; `scripts/01_prepare_data.R` is the reproducible preparation process.
- The package's resampling can result in repeated participant IDs. One-record-per-ID and unweighted descriptive summaries are **educational choices**, not survey-inference methods.
- Observations should not be removed solely because they have missing clinical measurements or extreme BMI.
- BMI classification requires adult records; BP missingness among young children must be interpreted against NHANES eligibility rules.
- This public portfolio is a demonstration of data analytics workflow, not clinical decision support.

## Data source and reference documentation

- [NHANES educational package](https://cran.r-project.org/package=NHANES)
- [CDC 2011–2012 BP examination documentation](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/BPX_G.htm)
- [Quarto documentation for R](https://quarto.org/docs/computations/r.html)

## Future extensions

Improved dashboard documentation, report deployment, unit tests, survey-design-aware analyses using appropriate original NHANES data, and a separate guided course in Shiny application programming.
