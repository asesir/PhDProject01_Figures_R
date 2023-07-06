# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)

# Your data
df<- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/AnnualEmissions.csv")

# df<- read.csv("D:/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/AnnualEmissions.csv")

CurrentEmissions<-210

df1 <- df %>%
  mutate(ScenarioID = cumsum(TransportationFuelScenario=="High")) %>% 
  mutate(id = row_number()) %>%
  ungroup()

# Vector for labels created programmatically
label_x <- paste(strwrap(df1$TransportationFuelScenario, width = 1), "\n\n",
                 strwrap(format(df1$TransportationFuelDemand, big.mark = ",", scientific = FALSE), width = 1),"\n\n", 
                 ifelse(df1$TransportationFuelScenario == "Medium", df1$Year, ""), sep = "")


# Plot
ggplot(df1, aes(x=factor(id))) +
  geom_point(aes(y=NoCarbonCapture, color = "No Carbon Capture"), size=5) +
  geom_point(aes(y=CarbonCapture, color = "Carbon Capture on All Possible Units"), size=5, shape=15) +
  geom_text(aes(y=NoCarbonCapture, label=round(NoCarbonCapture,0)), vjust = -2, size = 5)+
  geom_text(aes(y=CarbonCapture, label=round(CarbonCapture,0)), vjust = -2, size = 5)+
  scale_x_discrete(breaks = 1:6, labels= label_x) +
  scale_y_continuous(limits = c(0,250))+
  theme_classic()+
  theme(plot.margin = unit(c(1,1,2,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal", # Change direction of legend
        legend.box = "horizontal") +
  coord_cartesian(clip = "off") +
  labs(x = "Year & Volume of Transportation Fuel Demand (MMbbl/yr) & Level of Transportation Fuel Demand", 
       y = expression("Annual U.S. Refineries GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")")) +
  geom_hline(aes(yintercept = CurrentEmissions, linetype = "2019 U.S. Refinery GHG Emissions Baseline with 5346 MMbbl/y of Transportation Fuel Demand"), color = "blue", size = 1.5) +
  scale_color_manual(values = c("No Carbon Capture" = "darkred", "Carbon Capture on All Possible Units" = "black"), name = "") +
  scale_linetype_manual(values = c("2019 U.S. Refinery GHG Emissions Baseline with 5346 MMbbl/y of Transportation Fuel Demand" = "dashed"), name = "")+
  geom_vline(xintercept = 3.5, linetype = "solid", color = "black")+ # Add vertical lines
  annotate("text", x = -Inf, y = CurrentEmissions, label = "2019 GHG Emissions Baseline", hjust = -0.5, vjust = 1.5, color = "blue", size = 6.5)+
  annotate("text",x = 3, y = 250, label = "2035", size = 6.5)+
  annotate("text",x = 6, y = 250, label = "2050", size = 6.5)
