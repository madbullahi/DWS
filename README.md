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
  reference/        Pathogen target list (WHO drinking-water guidelines)
analysis/
  00_setup.R                Shared packages, paths and helpers
  01_organic_removal.R      Organic micropollutants, 12-week prototype
  02_inorganic_removal.R    Ammonia, COD, phosphates, TSS (with and without COD)
  03_microplastics_pfas.R   Microplastics alone and with PFOS/PFOA, two genotypes
  04_pathogen_screening.R   Pathogen screen of 16S data + strict check of ARG BLAST hits
  05_dashboard_data.R       Exports summaries to dashboard/data.js
  pathogen_functions.R      Reusable screening and BLAST-parsing functions
  WastewaterProof.rmd       16S microbiome analysis (QIIME2 / phyloseq)
  legacy/                   Original exploratory scripts, kept for reference
results/
  figures/   Plots (PNG/PDF/SVG) and PowerPoint exports
  tables/    ANOVA and post-hoc results (.txt and .xlsx)
  reports/   Rendered R notebooks
dashboard/   Interactive results page (index.html + data.js)
tests/       Unit tests: Rscript -e 'testthat::test_dir("tests")'
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

### Pathogen screening

`04_pathogen_screening.R` matches the genus-level 16S table against
`data/reference/pathogen_targets.csv` (WHO waterborne and opportunistic pathogens,
plus faecal indicators). It then compares wastewater with Daphnia against the
no-Daphnia control, by day and pooled, using a Wilcoxon test with Benjamini–Hochberg
correction. It also parses the BLAST output against the SARG resistance-gene
database and flags which hits meet annotation thresholds (≥ 80 % identity over
≥ 75 % of the read, E ≤ 1e-10).

It also checks the ASV sequences for leftover Illumina adapter (TruSeq and Nextera,
both strands), which can cause false BLAST matches. Trim adapters with `cutadapt`
before DADA2 if any are reported.

Outputs: `results/tables/pathogen_*.csv`, `results/tables/arg_blast_hits.csv`,
`results/tables/asv_adapter_check.csv` and `results/figures/pathogen_abundance.png`.
Draft methods and results text for the paper: `docs/methods_pathogens_and_ARGs.md`.

To screen a new sequencing batch:

```sh
Rscript analysis/04_pathogen_screening.R --genus-table new/genus.tsv \
  --metadata new/metadata.tsv --blast new/blast.txt \
  --fasta new/dna-sequences.fasta --asv-table new/feature-table.tsv \
  --out results/new_batch
```

16S amplicons resolve genus, not species. A match such as *Pseudomonas* or
*Legionella* flags a group to follow up with culture or species-specific qPCR; it
does not confirm *P. aeruginosa* or *L. pneumophila*.

### Microbiome

`analysis/WastewaterProof.rmd` needs Bioconductor/GitHub packages (`phyloseq`,
`qiime2R`, `microbiome`, `DESeq2`, `MicrobiotaProcess`, `vegan`) and is rendered
separately from `run_all.R`. All its file paths use `here::here()`, so it runs from
any working directory inside the project.

## Dashboard

`dashboard/index.html` is a self-contained page with five views: organic
micropollutants, nutrients and solids, microplastics with PFAS, the bacterial
community, and pathogens. It reads `dashboard/data.js`, which `04_dashboard_data.R` writes. It can
be opened locally or served with GitHub Pages.
