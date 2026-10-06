# Export summarised results for the interactive dashboard (dashboard/index.html).
# Run after scripts 01-03. Writes dashboard/data.js, which the page loads directly,
# so the dashboard also works when opened as a local file.

source(here::here("analysis", "00_setup.R"))
library(jsonlite)

organic <- read_excel(raw_chem("Stat_table_Removal.xlsx")) %>%
  summarise_se(Removal_Efficiency, Chemical, Week)

inorganic <- read_excel(raw_chem("Prototype_Inorganic_Removal.xlsx"), sheet = "Sheet1") %>%
  summarise_se(Removal, Inorganic, Phase, Week)

mp <- read_excel(raw_chem("MP_data.xlsx"), sheet = "Sheet3") %>%
  summarise_se(Removal, Treatment, Genotype, Day)

genera <- read.csv(here("data", "processed", "top_15_genera_abundance_by_genotype_and_day.csv"))

reads <- read.csv(here("data", "processed", "ww_data_filtered.csv"), check.names = FALSE) %>%
  select(sample = sample.id, Genotype, Day, Treatment, reads = `non-chimeric`)

anova_files <- c(organic = "anova_results", inorganic = "anova_results_inorganic",
                 mp = "anova_results_mp", mp_pfos = "anova_results_mp_PFOS",
                 mp_pfoa = "anova_results_mp_PFOA", mp_pfoa_pfos = "anova_results_mp_PFOA_PFOS")
anova <- lapply(anova_files, function(f) {
  t <- read.table(table_path(paste0(f, ".txt")), check.names = FALSE)
  data.frame(term = rownames(t), F = round(t[["F value"]], 3), p = signif(t[["Pr(>F)"]], 3))
})

out <- list(
  generated = format(Sys.Date()),
  organic   = organic,
  inorganic = inorganic,
  mp        = mp,
  genera    = genera,
  reads     = reads,
  anova     = anova
)

writeLines(paste0("window.DWS_DATA = ", toJSON(out, dataframe = "rows", digits = 4, na = "null", auto_unbox = TRUE), ";"),
           here("dashboard", "data.js"))
message("Wrote dashboard/data.js")
