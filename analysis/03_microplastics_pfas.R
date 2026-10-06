# Removal of microplastics (MP) alone and combined with PFOS/PFOA by two Daphnia genotypes.
# Input : data/raw/chemistry/MP_data.xlsx (Sheet3), data/raw/chemistry/MP_ANOVA.xlsx
# Output: results/tables/anova_results_mp*.{txt,xlsx}, results/figures/mp_removal_by_day.png

source(here::here("analysis", "00_setup.R"))

mp <- read_excel(raw_chem("MP_data.xlsx"), sheet = "Sheet3")
mp_summary <- summarise_se(mp, Removal, Treatment, Genotype, Day)

p <- ggplot(mp_summary, aes(Day, mean, colour = Genotype, group = Genotype)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ Treatment, nrow = 2) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  labs(title = "Removal efficiency of microplastics and PFAS",
       x = "Day", y = "Removal efficiency (%)") +
  theme_dws()
ggsave(fig_path("mp_removal_by_day.png"), p, width = 10, height = 8, dpi = 300)

# One nested mixed-effects model per treatment.
sheets <- c(MP = "anova_results_mp",
            MP_PFOS = "anova_results_mp_PFOS",
            MP_PFOA = "anova_results_mp_PFOA",
            MP_PFOA_PFOS = "anova_results_mp_PFOA_PFOS")

for (sheet in names(sheets)) {
  d <- read_excel(raw_chem("MP_ANOVA.xlsx"), sheet = sheet)
  model <- lmer(Removal ~ Genotype * Day + (1 | Genotype:Replicates), data = d)
  cat("\n##", sheet, "\n")
  print(save_anova(anova(model), sheets[[sheet]]))
}
