# Removal of inorganic parameters (Ammonia, COD, Ortho-phosphates, Phosphates, TSS)
# across weeks and phases.
# Input : data/raw/chemistry/Prototype_Inorganic_Removal.xlsx
#         (Sheet1 = all parameters, Sheet2 = same data without COD)
# Output: results/tables/anova_results_inorganic*, posthoc_results_*, results/figures/inorganic_*.png

source(here::here("analysis", "00_setup.R"))

read_inorganic <- function(sheet) {
  read_excel(raw_chem("Prototype_Inorganic_Removal.xlsx"), sheet = sheet) %>%
    rename(Replicates = Replicaates) %>%
    mutate(Week = factor(Week), Inorganic = factor(Inorganic), Phase = factor(Phase))
}

fit_and_save <- function(data, name) {
  model <- lmer(Removal ~ Inorganic + Week + Phase + (1 | Inorganic:Replicates), data = data)
  print(save_anova(anova(model), paste0("anova_results_", name)))
  posthoc <- emmeans(model, pairwise ~ Inorganic)
  print(posthoc)
  write.table(as.data.frame(posthoc$contrasts),
              table_path(paste0("posthoc_results_", name, ".txt")))
  model
}

inorganic        <- read_inorganic("Sheet1")
inorganic_no_cod <- read_inorganic("Sheet2")

fit_and_save(inorganic, "inorganic")
fit_and_save(inorganic_no_cod, "organic_COD_absent")

inorganic_summary <- summarise_se(inorganic, Removal, Inorganic, Phase, Week)

p <- ggplot(inorganic_summary,
            aes(as.integer(as.character(Week)), mean, colour = Phase, group = Phase)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) +
  geom_point(size = 1.5) +
  facet_wrap(~ Inorganic, scales = "free_y") +
  scale_colour_manual(values = c("1" = "#2a6f97", "2" = "#e07a2f")) +
  labs(title = "Removal of inorganic parameters", x = "Week",
       y = "Removal efficiency (%)", colour = "Phase") +
  theme_dws()
ggsave(fig_path("inorganic_removal_by_week.png"), p, width = 11, height = 7, dpi = 300)
