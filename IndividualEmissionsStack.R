# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(tidyr)

# Your data
df<- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/IndividualEmissionsStack.csv")

# df<- read.csv("D:/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/AnnualEmissions.csv")

df$Scenario <- str_to_title(df$Scenario)

# Pivot data to longer format
df_long <- df %>%
  pivot_longer(cols = c("Electricity","Heat","Steam","Hydrogen.via.SMR","Other.Emissions"),
               names_to = "Category", 
               values_to = "Value")

# Change names for Electrification Scenarios column
AbbNames<-c(Electricity = "Electricity", Heat = "Heat",Steam = "Steam", Hydrogen.via.SMR = "Hydrogen via SMR", Other.Emissions = "Other Emissions")
df_long$Category <- as.character(AbbNames[df_long$Category])
df_long$Category <- as.factor(df_long$Category)

# Create a unique Year_Scenario
df_long <- df_long %>%
  mutate(Year_Scenario = interaction(Year, Scenario, sep = "_"))

# Define the order of factors
df_long$Year_Scenario <- factor(df_long$Year_Scenario, levels = unique(df_long$Year_Scenario))

# Define Labels for each Scenario
scenario_labels <- c("Baseline\n2019", "High\n", "Medium\n2035", "Low\n", "High\n", "Medium\n2050", "Low\n")

# Create the label vector
label_x <- df_long %>%
  select(Year_Scenario) %>%
  distinct() %>%
  arrange(Year_Scenario) %>%
  mutate(Label = scenario_labels) %>%
  pull(Label)

my_palette <- brewer.pal(n = length(unique(df_long$Category)), name = "Spectral")

# Plot
p <- ggplot(df_long, aes(x = Year_Scenario, y = Value, fill = Category)) +
  geom_bar(stat = 'identity', position = 'stack',width = 0.5) +
  scale_x_discrete(labels = label_x) +
  scale_fill_manual(values = my_palette, name = "Refinery Energy Types")+
  labs(x = NULL, y = "Value") +
  theme_classic() +
  labs(x = "Year & Scenario",
       y = expression("Volume-weighted Average Refinery GHG Emissions Intensity (kg CO"[2]*"-eq bbl"^{-1}*")")) +
  theme(plot.margin = unit(c(1,1,2,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5,size = 16))

# Add vertical lines

p <- p + geom_vline(xintercept = c(1.5,4.5), color = "black")

p<-p+
  annotate("text",x = 1, y = 40, label = "2019", size = 6.5)+
  annotate("text",x = 4, y = 40, label = "2035", size = 6.5)+
  annotate("text",x = 7, y = 40, label = "2050", size = 6.5)

print(p)

