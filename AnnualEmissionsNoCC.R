# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)

# Your data
df<- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/AnnualEmissionsNoCC.csv")

# df<- read.csv("D:/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/AnnualEmissions.csv")

df$Scenario <- str_to_title(df$Scenario)

# Create labels programmatically
label_x <- ifelse(df$Scenario %in% c("Baseline", "Medium"), 
                  paste(df$Scenario, "\n", df$Year), 
                  paste(df$Scenario, "\n"))
df$Scenario <- str_to_title(df$Scenario)


# Create new variable combining Year and Scenario
df$Year_Scenario <- factor(paste(df$Year, df$Scenario), 
                           levels = paste(df$Year, df$Scenario))
# Create a named vector for your custom color palette
my_palette <- brewer.pal(n = length(unique(df$Scenario)), name = "BrBG")

# Plot
p <- ggplot(df, aes(x = Year_Scenario)) +
  geom_bar(aes(y=avg, fill = Scenario), stat = 'identity', width = 0.5) +
  geom_errorbar(aes(ymin = avg - min, ymax = avg + max), 
                color = "black", 
                width = 0.2) +
  geom_text(aes(y=avg, label=round(avg,0)), vjust = -2, size = 5)+
  scale_x_discrete(labels = label_x) +
  scale_fill_manual(values = my_palette, name = "Transportation Fuel Demand")+
  labs(x = NULL, y = "Average") +
  theme_classic() +
  labs(x = "Year & Transportation Fuel Demand Level",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"), 
       title = "Projections on Annual GHG Emissions in U.S. Refining Sector")+
  theme(plot.margin = unit(c(1,1,2,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5,size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "horizontal")
# Add vertical lines
unique_years <- unique(df$Year)
for (i in 2:length(unique_years)) {
  year <- unique_years[i]
  year_index <- which.min(abs(df$Year - year))
  p <- p + geom_vline(xintercept = year_index-0.5, color = "black")
}
p<-p+
  annotate("text",x = 1, y = 250, label = "2019", size = 6.5)+
  annotate("text",x = 4, y = 250, label = "2035", size = 6.5)+
  annotate("text",x = 7, y = 250, label = "2050", size = 6.5)

print(p)