# Pathogen screening of the wastewater 16S data, plus a strict check of the
# BLAST hits against the SARG antibiotic-resistance-gene database.
#
# Run on this project's data:
#   Rscript analysis/04_pathogen_screening.R
# Run on a new sequencing batch (all arguments optional):
#   Rscript analysis/04_pathogen_screening.R --genus-table new/genus.tsv \
#     --metadata new/metadata.tsv --blast new/blast.txt --out results/new_batch
#
# Inputs : genus-level relative-frequency table (QIIME2, exported to TSV),
#          sample metadata (sample-id, Treatment, Genotype, Day),
#          data/reference/pathogen_targets.csv, optional BLAST pairwise output.
# Outputs: <out>/tables/pathogen_*.csv, <out>/tables/arg_blast_hits.csv,
#          <out>/figures/pathogen_abundance.png

source(here::here("analysis", "00_setup.R"))
source(here::here("analysis", "pathogen_functions.R"))

arg <- function(flag, default) {
  a <- commandArgs(trailingOnly = TRUE)
  i <- match(flag, a)
  if (is.na(i) || i == length(a)) default else a[i + 1]
}
opt <- list(
  genus_table = arg("--genus-table", here("data", "raw", "microbiome", "frequency_genus-table.tsv")),
  metadata    = arg("--metadata",    here("data", "raw", "microbiome", "MetData_Pathogen_removal.tsv")),
  targets     = arg("--targets",     here("data", "reference", "pathogen_targets.csv")),
  blast       = arg("--blast",       here("data", "raw", "blast", "Blast_result.txt")),
  treatment   = arg("--treatment",   "Wastewater_Daphnia"),
  control     = arg("--control",     "Wastewater_Control"),
  out         = arg("--out",         here("results"))
)
out_table <- function(f) file.path(opt$out, "tables", f)
out_fig   <- function(f) file.path(opt$out, "figures", f)
dir.create(dirname(out_table("x")), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_fig("x")),   recursive = TRUE, showWarnings = FALSE)

# ---- 1. Which target genera are present, and how abundant? -------------------

targets  <- read.csv(opt$targets, stringsAsFactors = FALSE)
metadata <- read.delim(opt$metadata, check.names = FALSE) %>% rename(sample = `sample-id`)

genus <- read_genus_table(opt$genus_table) %>%
  inner_join(metadata, by = "sample") %>%
  mutate(target = match_targets(genus, targets))

# One row per sample x target, including zeros, so absent genera count as 0.
pathogen_long <- genus %>%
  filter(!is.na(target)) %>%
  group_by(sample, target) %>%
  summarise(rel_abundance = sum(rel_abundance), .groups = "drop") %>%
  complete(sample = metadata$sample, target = targets$genus, fill = list(rel_abundance = 0)) %>%
  left_join(metadata, by = "sample") %>%
  left_join(select(targets, target = genus, group, species_of_concern), by = "target")

write.csv(pathogen_long, out_table("pathogen_abundance_by_sample.csv"), row.names = FALSE)

detected <- pathogen_long %>%
  group_by(target, group, species_of_concern) %>%
  summarise(samples_detected = sum(rel_abundance > 0),
            samples_total    = n(),
            max_rel_abundance_pct = max(rel_abundance) * 100,
            .groups = "drop") %>%
  arrange(desc(samples_detected), desc(max_rel_abundance_pct))
write.csv(detected, out_table("pathogen_detection_summary.csv"), row.names = FALSE)
cat("\nTarget genera detected in at least one sample:\n")
print(as.data.frame(filter(detected, samples_detected > 0)), row.names = FALSE)

# ---- 2. Do Daphnia reduce them compared with the no-Daphnia control? ---------

present <- detected$target[detected$samples_detected > 0]
pathogen_long_present <- filter(pathogen_long, target %in% present)

by_day  <- compare_to_control(pathogen_long_present, opt$treatment, opt$control, by = c("target", "group", "Day"))
overall <- compare_to_control(pathogen_long_present, opt$treatment, opt$control, by = c("target", "group")) %>%
  mutate(Day = "All days")
comparison <- bind_rows(overall, by_day) %>% arrange(target, Day)
write.csv(comparison, out_table("pathogen_daphnia_vs_control.csv"), row.names = FALSE)
cat("\nDaphnia vs control, all days pooled:\n")
print(as.data.frame(overall %>% arrange(q_value) %>%
                      mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)

# Total burden of each group per sample.
burden <- pathogen_long %>%
  group_by(sample, Treatment, Genotype, Day, group) %>%
  summarise(rel_abundance = sum(rel_abundance), .groups = "drop")
write.csv(burden, out_table("pathogen_group_burden_by_sample.csv"), row.names = FALSE)

# ---- 3. Figure ----------------------------------------------------------------

plot_data <- pathogen_long_present %>%
  filter(Treatment %in% c(opt$treatment, opt$control)) %>%
  mutate(Treatment = ifelse(Treatment == opt$control, "Control (no Daphnia)", "With Daphnia")) %>%
  summarise_se(rel_abundance * 100, target, Treatment, Day)

order <- overall %>% arrange(desc(mean_control)) %>% pull(target)
p <- ggplot(plot_data, aes(Day, mean, fill = Treatment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                position = position_dodge(width = 0.8), width = 0.25, colour = "grey30") +
  facet_wrap(~ factor(target, levels = order), scales = "free_y") +
  scale_fill_manual(values = c("Control (no Daphnia)" = "#9aa5a4", "With Daphnia" = "#2a78d6")) +
  labs(title = "Potential pathogen and faecal-indicator genera",
       subtitle = "Mean relative abundance \u00b1 SE (16S rRNA, genus level)",
       x = NULL, y = "Relative abundance (%)", fill = NULL) +
  theme_dws()
ggsave(out_fig("pathogen_abundance.png"), p, width = 13, height = 9, dpi = 300)

# ---- 4. Antibiotic-resistance-gene BLAST hits ---------------------------------

if (!is.na(opt$blast) && file.exists(opt$blast)) {
  hits <- classify_arg_hits(parse_blast_pairwise(opt$blast))
  write.csv(select(hits, -subject), out_table("arg_blast_hits.csv"), row.names = FALSE)
  cat(sprintf("\nARG BLAST: %d query-subject hits from %d ASVs; %d pass identity/coverage/E-value thresholds.\n",
              nrow(hits), length(unique(hits$query)), sum(hits$passes)))
}
