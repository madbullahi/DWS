# load libraries.

library(lmerTest)
library(readxl)
library(ggplot2)
library(dplyr)


Mp_data <- read_excel(file.choose(), sheet = "Sheet3")
Mp_data


library(Rmisc)

#calculate the summary statistics.
 sE_Mp_data <- summarySE(Mp_data, measurevar = "RE", groupvars = c("Genotype", "Day"))
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


# export plot to ppt.
library(ggplot2)
library(officer)
library(rvg)

# Create the plot
plot <- ggplot(sE_Mp_data, aes(x = Day, y = RE, color = Genotype)) + 
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

