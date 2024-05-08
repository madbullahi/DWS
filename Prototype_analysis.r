 # To create a nice line plot for the 30 different chemicals across 12 weeks.

# Load the necessary libraries
library(tidyr)
library(dplyr)


# Assuming your original data is in a wide format.

original_data <-  data.frame(Week = 1:12, Chemical1 = ..., Chemical2 = ..., Chemical3 = ..., ..., Chemical30 = ...)


# Convert the data to long format.

tidy_data <- pivot_longer(original_data, cols = -Week, names_to = "Chemical", values_to = "Value")


# Load the ggplot2 library and create the line plot.

library(ggplot2)

ggplot(tidy_data, aes(x = Week, y = Value, color = Chemical, group = Chemical)) +
  geom_line() +
  labs(title = "Chemical Concentrations Over Time",
       x = "Week",
       y = "Value",
       color = "Chemical") +
  theme_bw()


#  Customise the plot appearance by adding titles, adjusting axis labels, and modifying the theme.


ggplot(tidy_data
       ,aes(x = Week, y = Value, color = Chemical, group = Chemical)) +
  geom_line() +
  labs(title = "Chemical Concentrations Over Time",
       x = "Week",
       y = "Chemical Concentration",
       color = "Chemical",
       subtitle = "Phase 1 (Spring and Summer)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#  Optionally, you can add a legend.

ggplot(tidy_data, 
       aes(x = Week, y = Value, color = Chemical, group = Chemical)) +
  geom_line(size = 0.8) +
  labs(...) +
  labs(title = "Chemical Concentrations Over Time",
       x = "Week",
       y = "Chemical Concentration",
       color = "Chemical",
       subtitle = "Phase 1 (Spring and Summer)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")



#*********************Excluding chemicals  that are less than 50% present******************###
#******************************************************************************************###

# calculate the % of non-missing values for each chemical.

chemical_presence <- tidy_data %>%
  group_by(Chemical) %>%
  summarise(pct_pesent = mean(!is.na(Value))) %>%
  filter(pct_present >= 0.5)


# Filter the data to include only the chemicals with at least 50% presence.

tidy_data_filtered <- tidy_data %>%
  semi_join(chemical_presence, by = "Chemical")


# ******Adding standard error bars to the plot*************.

# Group data by week and chemical, calculate the mean and standard error.

tidy_data_summarised <- tidy_data_filtered %>%
  group_by(Week, Chemical) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE),
            se_value = sd(Value, na.rm = TRUE) / sqrt(n()))



# Create the line plot with standard error bars.

ggplot(tidy_data_summarised, 
       aes(x = Week, y = mean_value, color = Chemical, group = Chemical)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
  labs(...) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")



  
  
