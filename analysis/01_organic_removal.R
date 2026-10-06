# Removal efficiency of organic micropollutants in the outdoor prototype (12 weeks).
# Input : data/raw/chemistry/Stat_table_Removal.xlsx (long format, one row per replicate)
# Output: results/tables/anova_results.{txt,xlsx}, results/figures/organic_removal_*.png

source(here::here("analysis", "00_setup.R"))

organic <- read_excel(raw_chem("Stat_table_Removal.xlsx")) %>%
  mutate(Week = factor(Week), Chemical = factor(Chemical))

# Linear mixed-effects model with replicate nested in chemical.
model <- lmer(Removal_Efficiency ~ Week + Chemical + (1 | Chemical:Replicates), data = organic)
print(save_anova(anova(model), "anova_results"))

# Normality of residuals.
print(shapiro.test(residuals(model)))
png(fig_path("organic_residuals_qq.png"), width = 6, height = 6, units = "in", res = 200)
qqnorm(residuals(model)); qqline(residuals(model))
dev.off()

# Mean +/- SE removal per chemical and week.
organic_summary <- summarise_se(organic, Removal_Efficiency, Chemical, Week)

p <- ggplot(organic_summary, aes(as.integer(as.character(Week)), mean)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = "#2a6f97") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3, colour = "#2a6f97") +
  geom_point(colour = "#2a6f97", size = 1.2) +
  facet_wrap(~ Chemical, ncol = 6) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  labs(title = "Removal efficiency of organic micropollutants",
       x = "Week", y = "Removal efficiency (%)") +
  theme_dws()
ggsave(fig_path("organic_removal_by_week.png"), p, width = 14, height = 9, dpi = 300)
