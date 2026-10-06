# Shared setup for the analysis pipeline: packages, paths and small helpers.
# Every script sources this file, so paths always resolve from the project root.

suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lmerTest)
  library(emmeans)
  library(openxlsx)
})

raw_chem   <- function(...) here("data", "raw", "chemistry", ...)
fig_path   <- function(...) here("results", "figures", ...)
table_path <- function(...) here("results", "tables", ...)

dir.create(here("results", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("results", "tables"),  recursive = TRUE, showWarnings = FALSE)

# Mean and standard error of removal efficiency per group (replaces Rmisc::summarySE).
summarise_se <- function(df, value, ...) {
  df %>%
    filter(!is.na({{ value }})) %>%
    group_by(...) %>%
    summarise(n    = n(),
              mean = mean({{ value }}),
              se   = sd({{ value }}) / sqrt(n()),
              .groups = "drop")
}

# Save an ANOVA table both as plain text and as an Excel sheet.
save_anova <- function(anova_tbl, name) {
  df <- as.data.frame(anova_tbl)
  write.table(df, table_path(paste0(name, ".txt")))
  wb <- createWorkbook()
  sheet <- substr(name, 1, 31)  # Excel limits sheet names to 31 characters
  addWorksheet(wb, sheet)
  writeData(wb, sheet, df, rowNames = TRUE)
  saveWorkbook(wb, table_path(paste0(name, ".xlsx")), overwrite = TRUE)
  invisible(df)
}

theme_dws <- function() {
  theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))
}
