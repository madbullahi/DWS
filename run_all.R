# Rebuild every chemistry table and figure from the raw data.
#   Rscript run_all.R
# The 16S microbiome analysis (analysis/WastewaterProof.rmd) needs QIIME2 / phyloseq
# packages and is rendered separately; see README.md.

scripts <- c("01_organic_removal.R",
             "02_inorganic_removal.R",
             "03_microplastics_pfas.R",
             "04_pathogen_screening.R",
             "05_dashboard_data.R")

for (s in scripts) {
  message("\n==== Running ", s, " ====")
  source(here::here("analysis", s), local = new.env())
}
message("\nDone. Tables in results/tables, figures in results/figures, dashboard data in dashboard/data.js")
