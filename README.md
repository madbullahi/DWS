# DWS: Daphnia for wastewater treatment

Data and analysis for *"Harnessing waterfleas for water reclamation: a nature-based
tertiary wastewater treatment technology"*. The study measures how well *Daphnia*
remove organic micropollutants, nutrients, microplastics and PFAS from wastewater,
and how they change its bacterial community (16S rRNA).

## Quick start

```sh
Rscript run_all.R
```

This rebuilds every chemistry table and figure from the raw data and refreshes the
dashboard data. Then open `dashboard/index.html` in a browser.

Required R packages: `here`, `readxl`, `dplyr`, `tidyr`, `ggplot2`, `lmerTest`,
`emmeans`, `openxlsx`, `jsonlite`.

## Repository layout

```
data/
  raw/chemistry/    Removal-efficiency spreadsheets (prototype, inorganic, MP/PFAS)
  raw/microbiome/   QIIME2 outputs (feature table, taxonomy, tree), sample metadata
  raw/blast/        BLAST results against antibiotic-resistance genes
  processed/        Tables derived from raw data (read stats, top genera, ...)
analysis/
  00_setup.R                Shared packages, paths and helpers
  01_organic_removal.R      Organic micropollutants, 12-week prototype
  02_inorganic_removal.R    Ammonia, COD, phosphates, TSS (with and without COD)
  03_microplastics_pfas.R   Microplastics alone and with PFOS/PFOA, two genotypes
  04_dashboard_data.R       Exports summaries to dashboard/data.js
  WastewaterProof.rmd       16S microbiome analysis (QIIME2 / phyloseq)
  legacy/                   Original exploratory scripts, kept for reference
results/
  figures/   Plots (PNG/PDF/SVG) and PowerPoint exports
  tables/    ANOVA and post-hoc results (.txt and .xlsx)
  reports/   Rendered R notebooks
dashboard/   Interactive results page (index.html + data.js)
docs/
  dataset/   Dataset descriptions (Dryad README)
  notes/     Analysis notes
```

## Analyses

| Script | Input | Model | Outputs |
|---|---|---|---|
| `01_organic_removal.R` | `Stat_table_Removal.xlsx` | `Removal ~ Week + Chemical + (1 \| Chemical:Replicates)` | `anova_results.*`, `organic_removal_by_week.png`, `organic_residuals_qq.png` |
| `02_inorganic_removal.R` | `Prototype_Inorganic_Removal.xlsx` (Sheet1, Sheet2 = no COD) | `Removal ~ Inorganic + Week + Phase + (1 \| Inorganic:Replicates)` + Tukey post-hoc | `anova_results_inorganic.*`, `anova_results_organic_COD_absent.*`, `posthoc_results_*.txt`, `inorganic_removal_by_week.png` |
| `03_microplastics_pfas.R` | `MP_data.xlsx`, `MP_ANOVA.xlsx` | `Removal ~ Genotype * Day + (1 \| Genotype:Replicates)` per treatment | `anova_results_mp*.{txt,xlsx}`, `mp_removal_by_day.png` |

Removal efficiency (%) = (initial − final) / initial × 100. Negative values mean the
concentration increased.

### Microbiome

`analysis/WastewaterProof.rmd` needs Bioconductor/GitHub packages (`phyloseq`,
`qiime2R`, `microbiome`, `DESeq2`, `MicrobiotaProcess`, `vegan`) and is rendered
separately from `run_all.R`. All its file paths use `here::here()`, so it runs from
any working directory inside the project.

## Dashboard

`dashboard/index.html` is a self-contained page with four views: organic
micropollutants, nutrients and solids, microplastics with PFAS, and the bacterial
community. It reads `dashboard/data.js`, which `04_dashboard_data.R` writes. It can
be opened locally or served with GitHub Pages.
