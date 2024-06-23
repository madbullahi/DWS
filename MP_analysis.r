# load libraries.

library(lmerTest)
library(readxl)
library(ggplot2)
library(dplyr)


Mp_data <- read_excel(file.choose(), sheet = "Sheet3")
Mp_data


library(Rmisc)

#calculate the summary statistics.
sE_Mp_data <- summarySE(Mp_data, measurevar = "RE", groupvars = c("Genotype", "Day", "Treatment"))
sE_Mp_data 


#plot the data.

ggplot(sE_Mp_data, aes(x = Day, y = RE, color =Genotype)) + 
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = RE - se, ymax = RE + se), 
                width = 0.2, size = 1, position = position_dodge(width = 0.5)) + 
  theme_minimal(base_size = 14) +
  labs(title = "Mean of Removal Efficiency (%)", 
       x = "Day",
       y = "RE") + 
  theme(legend.position = "top",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"))


#New plot
library(ggplot2)

ggplot(sE_Mp_data, aes(x = Day, y = RE, color = Genotype)) + 
  geom_line(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = RE - se, ymax = RE + se), 
                width = 0.2, size = 1, position = position_dodge(width = 0.5)) + 
  theme_minimal(base_size = 14) +
  labs(title = "Mean of Removal Efficiency (%)", 
       x = "Day",
       y = "RE") + 
  theme(legend.position = "top",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold")) +
  ylim(min(sE_Mp_data$RE - sE_Mp_data$se), max(sE_Mp_data$RE + sE_Mp_data$se)) +
  expand_limits(y = c(0, max(sE_Mp_data$RE + sE_Mp_data$se) * 1.05))


###########################

# make the plot for the individual chemicals
library(ggplot2)

# Define a color palette
color_palette <- c("#E69F00", "#56B4E9")

ggplot(sE_Mp_data, aes(x = Day, y = RE, color = Genotype, group = interaction(Genotype, Treatment))) + 
  geom_line(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = RE - se, ymax = RE + se), 
                width = 0.2, size = 1, position = position_dodge(width = 0.5)) + 
  facet_wrap(~ Treatment, nrow = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean of Removal Efficiency (%)", 
       x = "Day",
       y = "RE") + 
  theme(legend.position = "top",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_line(color = "gray90", size = 0.2),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = color_palette) +
  ylim(min(sE_Mp_data$RE - sE_Mp_data$se), max(sE_Mp_data$RE + sE_Mp_data$se)) +
  expand_limits(y = c(0, max(sE_Mp_data$RE + sE_Mp_data$se) * 1.05))




# export plot to ppt.
library(ggplot2)
library(officer)
library(rvg)

# Create the plot
color_palette <- c("#E69F00", "#56B4E9")

ggplot(sE_Mp_data, aes(x = Day, y = RE, color = Genotype, group = interaction(Genotype, Treatment))) + 
  geom_line(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = RE - se, ymax = RE + se), 
                width = 0.2, size = 1, position = position_dodge(width = 0.5)) + 
  facet_wrap(~ Treatment, nrow = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean of Removal Efficiency (%)", 
       x = "Day",
       y = "RE") + 
  theme(legend.position = "top",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_line(color = "gray90", size = 0.2),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = color_palette) +
  ylim(min(sE_Mp_data$RE - sE_Mp_data$se), max(sE_Mp_data$RE + sE_Mp_data$se)) +
  expand_limits(y = c(0, max(sE_Mp_data$RE + sE_Mp_data$se) * 1.05))
# Create a PowerPoint presentation
ppt <- read_pptx()

# Add a new slide to the presentation
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

# Convert the ggplot to an editable PowerPoint object
plot_dml <- rvg::dml(ggobj = plot)

# Add the plot to the slide
ppt <- ph_with(ppt, value = plot_dml, location = ph_location_fullsize())

# Save the PowerPoint presentation
print(ppt, target = "removal_efficiency.pptx")


# calculate the ANOVA.
library(lme4)

Mp_anova <- read_excel(file.choose(), sheet = "MP")
Mp_anova

# Fit the nested mixed-effect model
model <- lmer(RE ~ Genotype * Day + (1|Genotype:Replicates), data = Mp_anova)

# Perform ANOVA on the model
anova_result <- anova(model)

# Print the ANOVA table
print(anova_result)


# Convert the ANOVA table to a data frame
anova_df <- as.data.frame(anova_result)


# Create a new workbook and add a worksheet
wb <- createWorkbook()
addWorksheet(wb, "ANOVA_Results_Mp")

# Write the ANOVA table to the worksheet
writeData(wb, "ANOVA_Results_Mp", anova_df, rowNames = TRUE)

# Save the workbook to an Excel file
saveWorkbook(wb, "anova_results_mp.xlsx", overwrite = TRUE)
