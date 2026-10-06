# Mass balance and removal efficiency: tidy the raw data, cross-check the
# condensed supplementary tables, and draw box plots.
#
# Inputs (repo root):
#   Mass Balance individual data points_PFAS_final.xlsx   PFAS + MP recovery (% of nominal)
#   As_massbalance.xlsx                                    Arsenic in water and Daphnia tissue
#   Abdullahi_etal_Table S2- individual chemicals raw.xlsx Final water concentrations
#   Abdullahi_etal_Table S3- individual chemicals removal efficiency.xlsx
#
# Outputs: mass_balance/output/*.csv and mass_balance/output/figures/*.{pdf,png}
#
# Run from the repo root:  Rscript mass_balance/mass_balance_plots.R

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

out_dir <- file.path("mass_balance", "output")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

day_cols  <- c(D1 = "#2a78d6", D2 = "#eb6834", D3 = "#1baf7a")
pair_cols <- c("#2a78d6", "#eb6834")

# Arsenic tissue values are concentrations in the digested Daphnia samples
# (ug/L of digest; the "ng/L" header in As_massbalance.xlsx is wrong).
# Daphnia were not weighed, so no per-mass unit is possible.
tissue_unit <- "\u00b5g/L in digest"
# Recalled by the experimenter, not recorded in the data files: used only to
# add per-sample and per-individual columns to the matched arsenic table.
digest_volume_L    <- 0.001  # 1 mL digest
daphnia_per_sample <- 10

theme_mb <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = "grey70"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(colour = "black"),
        legend.position = "top")

save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}

# ---------------------------------------------------------------------------
# 1. PFAS mass balance (individual replicates)
# ---------------------------------------------------------------------------
# Each block is a header row followed by 3 days x 3 replicates.
# Column groups: A-D (PFOS), G-J (PFOA), M-P (PFOS+PFOA); label, Medium, Tissue, Sum.
pfas_raw <- read_excel("Mass Balance individual data points_PFAS_final.xlsx",
                       sheet = "PFAS", col_names = FALSE, range = "A1:P43",
                       .name_repair = "minimal")

pfas_blocks <- expand.grid(header_row = c(2, 13, 24, 34), first_col = c(1, 7, 13))

pfas <- bind_rows(lapply(seq_len(nrow(pfas_blocks)), function(i) {
  hr <- pfas_blocks$header_row[i]
  fc <- pfas_blocks$first_col[i]
  label <- as.character(pfas_raw[[fc]][hr])
  rows <- hr + 1:9
  tibble(
    block     = label,
    Day       = rep(c("D1", "D2", "D3"), each = 3),
    Replicate = rep(1:3, times = 3),
    Medium    = as.numeric(pfas_raw[[fc + 1]][rows]),
    Daphnia   = as.numeric(pfas_raw[[fc + 2]][rows])
  )
})) %>%
  mutate(
    Total    = Medium + Daphnia,
    PET      = ifelse(grepl("PET", block), "With PET", "Without PET"),
    Exposure = sub("_.*$", "", gsub("\\+PET", "", block)),
    Exposure = factor(Exposure, levels = c("PFOS", "PFOA", "PFOS+PFOA")),
    Genotype = ifelse(grepl("LRV", block), "LRV0_1", "LRII_36"),
    PET      = factor(PET, levels = c("Without PET", "With PET"))
  ) %>%
  select(Genotype, Exposure, PET, Day, Replicate, Medium, Daphnia, Total, block)

write.csv(pfas, file.path(out_dir, "pfas_mass_balance_tidy.csv"), row.names = FALSE)

pfas_long <- pfas %>%
  pivot_longer(c(Medium, Daphnia, Total), names_to = "Compartment", values_to = "Recovery") %>%
  mutate(Compartment = factor(Compartment, levels = c("Medium", "Daphnia", "Total"),
                              labels = c("Medium", "Daphnia tissue", "Total")))

p_pfas <- ggplot(pfas_long, aes(Compartment, Recovery, fill = PET)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  geom_boxplot(outlier.shape = NA, alpha = 0.35, width = 0.7,
               position = position_dodge(width = 0.8)) +
  geom_point(aes(colour = PET), size = 1.6, alpha = 0.8,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8, seed = 1)) +
  facet_grid(Genotype ~ Exposure) +
  scale_fill_manual(values = pair_cols, name = NULL) +
  scale_colour_manual(values = pair_cols, name = NULL) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 125, 25)) +
  labs(x = NULL, y = "Recovery (% of nominal)",
       title = "PFAS mass balance: medium vs Daphnia tissue",
       subtitle = "Each point is one replicate (3 replicates x 3 days); dashed line = 100% recovery") +
  theme_mb
save_fig(p_pfas, "Fig_PFAS_mass_balance_boxplot", 10, 7)

# ---------------------------------------------------------------------------
# 2. Microplastic (PET) mass balance: only one value per day per treatment
# ---------------------------------------------------------------------------
mp_raw <- read_excel("Mass Balance individual data points_PFAS_final.xlsx",
                     sheet = "MPs", col_names = FALSE, range = "A1:X20",
                     .name_repair = "minimal")

mp_blocks <- expand.grid(title_row = c(2, 8), first_col = c(2, 8, 14, 20))
mp_treat  <- c(`2` = "PET", `8` = "PET+PFOS", `14` = "PET+PFOA", `20` = "PET+PFOS+PFOA")

mp <- bind_rows(lapply(seq_len(nrow(mp_blocks)), function(i) {
  tr <- mp_blocks$title_row[i]
  fc <- mp_blocks$first_col[i]
  rows <- tr + 2:4
  tibble(
    Genotype  = ifelse(tr == 2, "LRII_36", "LRV0_1"),
    Treatment = mp_treat[[as.character(fc)]],
    Day       = c("D1", "D2", "D3"),
    Medium    = as.numeric(mp_raw[[fc + 1]][rows]),
    Daphnia   = as.numeric(mp_raw[[fc + 2]][rows])
  )
})) %>%
  mutate(Total = Medium + Daphnia,
         Treatment = factor(Treatment, levels = mp_treat))

write.csv(mp, file.path(out_dir, "mp_mass_balance_tidy.csv"), row.names = FALSE)

mp_long <- mp %>%
  pivot_longer(c(Medium, Daphnia, Total), names_to = "Compartment", values_to = "Recovery") %>%
  mutate(Compartment = factor(Compartment, levels = c("Medium", "Daphnia", "Total"),
                              labels = c("Medium", "Daphnia tissue", "Total")))

p_mp <- ggplot(mp_long, aes(Compartment, Recovery)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  geom_boxplot(outlier.shape = NA, fill = "grey90", width = 0.6) +
  geom_point(aes(colour = Day, shape = Treatment), size = 2.2,
             position = position_jitter(width = 0.15, seed = 1)) +
  facet_wrap(~ Genotype) +
  scale_colour_manual(values = day_cols) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 125, 25)) +
  labs(x = NULL, y = "Recovery (% of nominal)",
       title = "PET microplastic mass balance: medium vs Daphnia tissue",
       subtitle = "One value per treatment and day (no replicate-level data in the source file)") +
  theme_mb + theme(legend.box = "vertical")
save_fig(p_mp, "Fig_MP_mass_balance_boxplot", 9, 6)

# ---------------------------------------------------------------------------
# 2b. Combined mass balance: water (dashed) and tissue (solid) in one panel,
#     one colour per compound. PFAS treatments are PFAS recovery (3 replicates
#     x 3 days); PET alone is PET recovery (one value per day).
# ---------------------------------------------------------------------------
compound_levels <- c("PET", "PFOS", "PFOA", "PFOS+PET", "PFOA+PET", "PFOS+PFOA", "PFOS+PFOA+PET")
compound_cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                            "#e87ba4", "#008300", "#4a3aa7"), compound_levels)

combined <- bind_rows(
  pfas %>% transmute(Genotype, Day, Replicate,
                     Compound = ifelse(PET == "With PET", paste0(Exposure, "+PET"),
                                       as.character(Exposure)),
                     Medium, Daphnia),
  mp %>% filter(Treatment == "PET") %>%
    transmute(Genotype, Day, Replicate = 1L, Compound = "PET", Medium, Daphnia)
) %>%
  pivot_longer(c(Medium, Daphnia), names_to = "Compartment", values_to = "Recovery") %>%
  mutate(Compound = factor(Compound, levels = compound_levels),
         Compartment = factor(Compartment, levels = c("Medium", "Daphnia"),
                              labels = c("Water (medium)", "Daphnia tissue")))

write.csv(combined, file.path(out_dir, "mass_balance_water_tissue_combined.csv"), row.names = FALSE)

p_combined <- ggplot(combined, aes(Compound, Recovery,
                                   colour = Compound, fill = Compound,
                                   linetype = Compartment,
                                   group = interaction(Compound, Compartment))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.15, linewidth = 0.6, width = 0.75,
               position = position_dodge(width = 0.85)) +
  geom_point(aes(shape = Compartment), size = 1.3, alpha = 0.8,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.85, seed = 1)) +
  facet_wrap(~ Genotype, ncol = 1) +
  scale_colour_manual(values = compound_cols, guide = "none") +
  scale_fill_manual(values = compound_cols, guide = "none") +
  scale_linetype_manual(values = c("Water (medium)" = "dashed", "Daphnia tissue" = "solid"),
                        name = NULL) +
  scale_shape_manual(values = c("Water (medium)" = 1, "Daphnia tissue" = 16), name = NULL) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 100, 20)) +
  guides(linetype = guide_legend(override.aes = list(colour = "grey20", fill = NA))) +
  labs(x = NULL, y = "Recovery (% of nominal)",
       title = "Mass balance: water vs Daphnia tissue by compound",
       subtitle = "Dashed = water, solid = tissue; days pooled. PFAS treatments: 3 replicates x 3 days; PET: 1 value per day") +
  theme_mb
save_fig(p_combined, "Fig_mass_balance_water_tissue_combined", 11, 7.5)

# ---------------------------------------------------------------------------
# 3. Removal efficiency of individual chemicals (recomputed from Table S2 raw)
# ---------------------------------------------------------------------------
chem_raw <- read_excel("Abdullahi_etal_Table S2- individual chemicals raw.xlsx",
                       sheet = "individual chemicals")
names(chem_raw) <- c("Genotype", "Replicate", "Day", "PFOS", "Diclofenac", "Atrazine", "Arsenic")

chem_long <- chem_raw %>%
  pivot_longer(PFOS:Arsenic, names_to = "Chemical", values_to = "Conc")

# Initial concentration = mean of the no-Daphnia controls on the same day
# (this reproduces the IC column of Table S3).
ic <- chem_long %>%
  filter(Genotype == "Control") %>%
  group_by(Chemical, Day) %>%
  summarise(IC = mean(Conc), .groups = "drop")

removal <- chem_long %>%
  filter(Genotype != "Control") %>%
  left_join(ic, by = c("Chemical", "Day")) %>%
  mutate(RE = (IC - Conc) / IC * 100,
         Chemical = factor(Chemical, levels = c("PFOS", "Diclofenac", "Atrazine", "Arsenic")))

write.csv(removal, file.path(out_dir, "removal_efficiency_recomputed.csv"), row.names = FALSE)

# Compare with the published Table S3
s3_raw <- read_excel("Abdullahi_etal_Table S3- individual chemicals removal efficiency.xlsx",
                     col_names = FALSE, skip = 3, .name_repair = "minimal")
s3 <- s3_raw[, 1:15]
names(s3) <- c("Genotype", "Replicate", "Day",
               paste(rep(c("PFOS", "Diclofenac", "Atrazine", "Arsenic"), each = 3),
                     c("IC", "FC", "RE"), sep = "_"))
s3_long <- s3 %>%
  filter(!is.na(Genotype)) %>%
  mutate(across(-c(Genotype, Day), as.numeric)) %>%
  pivot_longer(-c(Genotype, Replicate, Day),
               names_to = c("Chemical", ".value"), names_sep = "_")

s3_check <- removal %>%
  mutate(Chemical = as.character(Chemical)) %>%
  full_join(s3_long, by = c("Genotype", "Replicate", "Day", "Chemical"),
            suffix = c("_raw", "_S3")) %>%
  mutate(status = case_when(
    is.na(FC)                          ~ "missing from Table S3",
    is.na(Conc)                        ~ "missing from raw Table S2",
    abs(Conc - FC) > 0.005             ~ "final concentration differs",
    abs(RE_raw - RE_S3) > 0.5          ~ "RE differs",
    TRUE                               ~ "ok")) %>%
  arrange(status != "ok", Chemical, Genotype, Day, Replicate)

write.csv(s3_check, file.path(out_dir, "check_TableS3_vs_raw.csv"), row.names = FALSE)

p_re <- ggplot(removal, aes(Genotype, RE)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_boxplot(outlier.shape = NA, fill = "grey90", width = 0.6) +
  geom_point(aes(colour = Day), size = 2, position = position_jitter(width = 0.15, seed = 1)) +
  facet_wrap(~ Chemical, nrow = 1) +
  scale_colour_manual(values = day_cols) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = NULL, y = "Removal efficiency (%)",
       title = "Removal of individual chemicals from water by Daphnia genotype",
       subtitle = "RE = (control - exposed) / control x 100; 2 replicates x 3 days per genotype") +
  theme_mb + theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p_re, "Fig_removal_efficiency_boxplot", 11, 5)

# ---------------------------------------------------------------------------
# 4. Arsenic: match water and tissue by genotype, day and replicate
# ---------------------------------------------------------------------------
as_water <- read_excel("As_massbalance.xlsx", sheet = "water")
# The Treatment and Day headers are swapped in the source sheet.
names(as_water) <- c("Replicate", "Genotype", "Day", "Treatment", "Water_ugL")
# DM1900 D1 and D3 have both rows labelled replicate 1; the second row is
# replicate 2 (confirmed against Table S2 raw).
as_water <- as_water %>%
  group_by(Genotype, Day, Treatment) %>%
  mutate(Replicate = row_number()) %>%
  ungroup()

as_tissue <- read_excel("As_massbalance.xlsx", sheet = "tissue")
names(as_tissue) <- c("Replicate", "Genotype", "Day", "Treatment", "Tissue")

as_ic <- as_water %>%
  filter(Genotype == "CONTROL") %>%
  group_by(Day) %>%
  summarise(IC = mean(Water_ugL), .groups = "drop")

as_matched <- as_tissue %>%
  filter(Treatment == "ARSENIC") %>%
  select(Genotype, Day, Replicate, Tissue) %>%
  left_join(as_tissue %>% filter(Treatment == "CONTROL") %>%
              select(Genotype, Day, Replicate, Tissue_control = Tissue),
            by = c("Genotype", "Day", "Replicate")) %>%
  left_join(as_water %>% filter(Genotype != "CONTROL") %>%
              select(Genotype, Day, Replicate, Water_ugL),
            by = c("Genotype", "Day", "Replicate")) %>%
  left_join(as_ic, by = "Day") %>%
  mutate(Water_RE = (IC - Water_ugL) / IC * 100,
         Tissue_ng_per_sample = Tissue * digest_volume_L * 1000,
         Tissue_pg_per_daphnia = Tissue_ng_per_sample / daphnia_per_sample * 1000) %>%
  arrange(Genotype, Day, Replicate)

write.csv(as_matched, file.path(out_dir, "arsenic_water_tissue_matched.csv"), row.names = FALSE)

as_long <- bind_rows(
  as_matched %>% transmute(Genotype, Day, Replicate, Panel = "Removal from water (%)", Value = Water_RE),
  as_tissue %>% transmute(Genotype, Day, Replicate,
                          Panel = paste0("Arsenic in Daphnia tissue (", tissue_unit, ")"),
                          Treatment = ifelse(Treatment == "ARSENIC", "Arsenic-exposed", "Control"),
                          Value = Tissue)
) %>%
  mutate(Treatment = factor(coalesce(Treatment, "Arsenic-exposed"),
                            levels = c("Arsenic-exposed", "Control")),
         Panel = factor(Panel, levels = c("Removal from water (%)",
                                         paste0("Arsenic in Daphnia tissue (", tissue_unit, ")"))))

p_as <- ggplot(as_long, aes(Genotype, Value, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.35, width = 0.7,
               position = position_dodge(width = 0.8)) +
  geom_point(aes(colour = Treatment), size = 1.8,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8, seed = 1)) +
  facet_wrap(~ Panel, scales = "free_y") +
  scale_fill_manual(values = pair_cols, name = NULL) +
  scale_colour_manual(values = pair_cols, name = NULL) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL,
       title = "Arsenic: removal from water and accumulation in Daphnia tissue",
       subtitle = "2 replicates x 3 days per genotype") +
  theme_mb
save_fig(p_as, "Fig_arsenic_water_tissue_boxplot", 10, 5)

rho <- cor.test(as_matched$Water_RE, as_matched$Tissue, method = "spearman", exact = FALSE)

p_as_scatter <- ggplot(as_matched, aes(Tissue, Water_RE)) +
  geom_point(aes(colour = Day, shape = Genotype), size = 2.6) +
  scale_colour_manual(values = day_cols) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(x = paste0("Arsenic in Daphnia tissue (", tissue_unit, ")"), y = "Arsenic removal from water (%)",
       title = "Arsenic: water removal vs tissue concentration (matched replicates)",
       subtitle = sprintf("Spearman rho = %.2f, p = %.2f, n = %d",
                          rho$estimate, rho$p.value, nrow(as_matched))) +
  theme_mb + theme(panel.grid.major.x = element_line(colour = "grey92"),
                   legend.position = "right")
save_fig(p_as_scatter, "Fig_arsenic_water_vs_tissue_scatter", 7, 5.5)

# ---------------------------------------------------------------------------
# 5. Data checks
# ---------------------------------------------------------------------------
# PET blocks that reuse replicates from the matching non-PET block
dup_check <- pfas %>%
  filter(PET == "With PET") %>%
  inner_join(pfas %>% filter(PET == "Without PET"),
             by = c("Genotype", "Exposure", "Day", "Medium", "Daphnia"),
             suffix = c("_PET", "_noPET")) %>%
  select(Genotype, Exposure, Day, Replicate_PET, Replicate_noPET, Medium, Daphnia)
write.csv(dup_check, file.path(out_dir, "check_PET_blocks_duplicating_noPET.csv"), row.names = FALSE)

cat("\nTable S3 vs raw Table S2:\n"); print(table(s3_check$status))
cat("\nPET replicates identical to a non-PET replicate:", nrow(dup_check), "of",
    sum(pfas$PET == "With PET"), "\n")
cat("\nArsenic water vs tissue: Spearman rho =", round(rho$estimate, 2),
    "p =", signif(rho$p.value, 2), "\n")
