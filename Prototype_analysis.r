 # To create a nice line plot for the 30 different chemicals across 12 weeks.

# Load the necessary libraries
library(tidyr)
library(dplyr)
library(readxl)

# Import the data from an Excel file.
original_data <- read_excel("Prototype_Removal.xlsx")
original_data 


# Assuming your original data is in a wide format.

original_data <-  data.frame(Week = 1:12, Chemical1 =  "Metformin", Chemical2 =  "Nicotine", Chemical3 = "Acetaminophen", Chemical4 = "Amoxicillin", Chemical5 = "Gabapentin", Chemical6 = "Codeine", 
                             Chemical7 = "Caffeine", Chemical8 =  "Trimethoprim", Chemical9 = "Sulfamethoxazole", Chemical10 =  "Tramadol", Chemical11 =  "Metoprolo", Chemical12 = "Doxycycline", 
                             Chemical13 = "Propranolol", Chemical14 = "Carbamazepine",
                             Chemical15 = "Hydrocortisone", Chemical16 =  "Erythromycin",
                             Chemical17 =  "DEET", Chemical18 = "Clotrimazole", Chemical19 =  "Mefloquine", 
                             Chemical20 = "Oxazepam",
                             Chemical21 = "Diazepam", Chemical22 =  "Valsartan", Chemical23 = "Ibuprofen", 
                             Chemical24 = "Naproxen", Chemical25 = "Diclofenac", Chemical26 = "Meclofenamic",
                             Chemical27 = "Glyburide", Chemical28 = "Gemfibrozil", Chemical29 = "EthinylEstradiole", 
                             Chemical30 = "Estradiol", Chemical31 = "PFOS", Chemical32 = "PFOA")


# Convert the data to long format.

tidy_data <- pivot_longer(original_data, cols = -Week, names_to = "Chemical", values_to = "Value")

tidy_data
# Load the ggplot2 library and create the line plot.

library(ggplot2)

ggplot(original_data, aes(x = "Week", y = Value, color = Chemical, group = Chemical)) +
  geom_line() +
  labs(title = "Chemical Concentrations Over Time",
       x = "Week",
       y = "Value",
       color = "Chemical") +
  theme_bw()


#  Customise the plot appearance by adding titles, adjusting axis labels, and modifying the theme.


ggplot(t_data
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


# Calculate percentage presence for each chemical across weeks.
chemical_presence <- original_data %>%                                           
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Concentration") %>%    # here we are converting the data to long format.
  group_by(Chemical) %>%                                                         # we are grouping the data by chemical, 
  summarise(pct_present = mean(!is.na(Concentration)) * 100) %>%                # we are calculating the percentage presence of each chemical (i.e non-missing values).
  ungroup()

# Filter out chemicals with less than 50% presence  
original_data_filtered <- original_data %>%
  semi_join(chemical_presence %>% filter(pct_present >= 70), by = "Chemical")    # here we are filtering out chemicals with less than 50% presence.

original_data_filtered

write.csv(original_data_filtered, "original_data_filtered.csv")

# ******Adding standard error bars to the plot*************.

# Reshape the data into long format.
library(tidyr)
library(dplyr)

long_data <- original_data_filtered %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Concentration")

long_data
  
# Calculate the mean and standard error for each week and chemical combination.

summarized_data <- long_data %>%
  group_by(Chemical, Week) %>%
  summarise(mean_conc = mean(Concentration, na.rm = TRUE),
            se_conc = sd(Concentration, na.rm = TRUE) / sqrt(n()))

summarized_data

# Create the line plot with error bars.

library(ggplot2)

ggplot(summarized_data, aes(x = Week, y = mean_conc, color = Chemical, group = Chemical)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_conc - se_conc, ymax = mean_conc + se_conc), width = 0.2) +
  labs(x = "Week", y = "Mean Concentration", color = "Chemical") +
  theme_minimal()






#************Run ANOVA test to determine if there are significant differences in chemical concentrations over time.*************#
#*******************************************************************************************************************************#
#*******************************************************************************************************************************#


# Load the necessary libraries.
library(lmerTest)

# The data is in long format, so we can use the Week variable as a numeric predictor.

data_anova <- read_excel("Stat_table_Removal.xlsx")

# Convert Week and Chemical to factors.

data_anova$Week <- as.factor(data_anova$Week)
data_anova$Chemical <- as.factor(data_anova$Chemical)


# fit thee linear mixed-effects model.

model1 <- lmer(Removal_Efficiency ~ Chemical * Week + (1 | Chemical: Replicates), data = data_anova)
model2 <- lmer(Removal_Efficiency ~ Week + Chemical +(1|Chemical:Replicates), data = data_anova)
anova(model2)# Perform the ANOVA test.

write.table(anova(model2), "anova_results.txt")
anova_results <- anova(model1)
summary(anova_results)
anova_results


## Normality test for residuals.

shapiro.test(residuals(model1))

# Plot the residuals to check for normality.

qqnorm(residuals(model1))
qqline(residuals(model1))


# save the residual plot.
pdf("residual_plot.pdf")
