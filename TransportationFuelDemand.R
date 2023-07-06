# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# read csv file
df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/TransportationFuelDemand.csv")

df$Year <- as.factor(df$Year)
df$Scenario <- as.factor(df$Scenario)

df <- df %>% 
  mutate_at(vars(3:8), ~ . / 1000000)

df_long<-df %>%
  pivot_longer(cols = c("Diesel.Fuel","Gasoline.Fuel","Jet.Fuel","LPG.Fuel","Lubricants","Residual.Fuel.Oil"),
               names_to = "Category", 
               values_to = "Value")

AbbNames<-c(Gasoline.Fuel = "Gasoline", Jet.Fuel = "Jet-A/AVTUR",Diesel.Fuel = "ULSD", Residual.Fuel.Oil = "Residual Fuel Oil", LPG.Fuel="LPG", Lubricants = "Lube" )

df_long$Category <- as.character(AbbNames[df_long$Category])
df_long$Category <- as.factor(df_long$Category)

# Calculate lower and upper bounds
bounds <- df_long %>%
  filter(Scenario %in% c("Low", "High")) %>%
  group_by(Year, Category, Scenario) %>%
  summarize(Value = mean(Value)) %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  rename(lower = Low, upper = High)

# Join the main dataframe with the bounds dataframe
df_medium <- df_long %>% 
  filter(Scenario == "Medium") %>%
  left_join(bounds, by = c("Year", "Category"))

p <- df_medium %>%
  ggplot(aes(x = Year,
             y = Value,
             color = Category,
             linetype = Category,
             group = Category)) + 
  geom_point(aes(shape = Category, 
                 color = Category),
             size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05) +
  labs(x = "Year", 
       y = expression("Projected Annual Transportation Fuel Demand in U.S., (mmbbl y"^{-1}*")"),
       color = "Transportation Fuel",  # modify the legend title
       linetype = "Transportation Fuel",# modify the linetype legend title if needed
       shape = "Transportation Fuel") +  # modify the Shape legend title if needed
  scale_color_brewer(palette = "Spectral") +  # change the color scheme
  theme_classic() +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(p)
