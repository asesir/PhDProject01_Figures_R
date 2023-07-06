# Load necessary packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(forcats)
library(tidyverse)
library(patchwork)
library(cowplot)
library(ggtext)
# Read the data
df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/EmissiosReductionProcessUnit.csv")

# df <- read.csv("D:/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/EmissiosReductionProcessUnit.csv")

# Convert Year and Scenario to factors
df$Year <- as.factor(df$Year)
df$Scenario <- as.factor(df$Scenario)

# Adjust the units for the columns
df <- df %>%
  mutate(across(starts_with("Emissions"), ~ . / 1000000))

# Subsetting data (example: 2035 & Light/Sweet Crude)
df1 <- df %>%
  filter(Year == "2035", Scenario == "Light/Sweet") %>%
  mutate(Process.Units = fct_reorder(Process.Units, Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, .desc = TRUE))

# Create the plot for df1
p1 <- ggplot(df1, aes(x=Process.Units)) +
  geom_point(aes(y=Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, color = "black"), size=3) +
  geom_errorbar(aes(ymin = Emissions.Reduction.Potential.Low, 
                    ymax = Emissions.Reduction.Potential.High, 
                    color = "black"), 
                width = 0.2) +
  
  geom_point(aes(y=Emissions.Under.Medium.Transportation.Fuel.Demand, color = "darkred"), size=3, shape=15) +
  geom_errorbar(aes(ymin =  Emissions.Low, 
                    ymax = Emissions.High, color = "darkred"), 
                width = 0.2) +
  
  scale_color_identity(
    name = "",
    breaks = c("black", 
               "darkred"),
    labels = c("Emissions Reduction Potential Under Medium Transportation Fuel Demand", 
               "Emissions Under Medium Transportation Fuel Demand"),
    guide = "legend"
  ) +
  
  scale_y_continuous(limits = c(0,40)) +
  theme_classic() +
  labs(x = "Refinery Individual Process Units",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"), 
       title = paste0("A. ", unique(df1$Year), " - ", unique(df1$Scenario), " Crude"))+
  theme(legend.text = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 10)),
        legend.title = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5,size = 14))

p_legend<-get_legend(p1)

p1<-p1+
  theme(legend.position = "none", axis.title.y=element_blank())

# p2.
df2 <- df %>%
  filter(Year == "2035", Scenario == "Heavy/Sour") %>%
  mutate(Process.Units = fct_reorder(Process.Units, Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, .desc = TRUE))
p2 <- ggplot(df2, aes(x=Process.Units)) +
  geom_point(aes(y=Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, color = "Emissions Reduction Potential"), size=3) +
  geom_errorbar(aes(ymin = Emissions.Reduction.Potential.Low, 
                    ymax = Emissions.Reduction.Potential.High, color = "Emissions Reduction Potential"), 
                width = 0.2) +
  geom_point(aes(y=Emissions.Under.Medium.Transportation.Fuel.Demand, color = "Emissions Under Medium Demand"), size=3,shape=15) +
  geom_errorbar(aes(ymin =  Emissions.Low, 
                    ymax = Emissions.High, color = "Emissions Under Medium Demand"), 
                width = 0.2) +
  scale_color_manual(
    values = c("Emissions Reduction Potential" = "black", "Emissions Under Medium Demand" = "darkred"),
    labels = c("Emissions Reduction Potential", "Emissions Under Medium Demand")
  ) +
  scale_y_continuous(limits = c(0,40)) +
  theme_classic() +
  labs(x = "Refinery Individual Process Units",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"), 
       title = paste0("B. ", unique(df2$Year), " - ", unique(df2$Scenario), " Crude"))+
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5,size = 14))

# p3.
df3 <- df %>%
  filter(Year == "2050", Scenario == "Light/Sweet") %>%
  mutate(Process.Units = fct_reorder(Process.Units, Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, .desc = TRUE))


p3 <- ggplot(df3, aes(x=Process.Units)) +
  geom_point(aes(y=Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, color = "Emissions Reduction Potential"), size=3) +
  geom_errorbar(aes(ymin = Emissions.Reduction.Potential.Low, 
                    ymax = Emissions.Reduction.Potential.High, color = "Emissions Reduction Potential"), 
                width = 0.2) +
  geom_point(aes(y=Emissions.Under.Medium.Transportation.Fuel.Demand, color = "Emissions Under Medium Demand"), size=3, shape=15) +
  geom_errorbar(aes(ymin =  Emissions.Low, 
                    ymax = Emissions.High, color = "Emissions Under Medium Demand"), 
                width = 0.2) +
  scale_color_manual(
    values = c("Emissions Reduction Potential" = "black", "Emissions Under Medium Demand" = "darkred"),
    labels = c("Emissions Reduction Potential", "Emissions Under Medium Demand")
  ) +
  scale_y_continuous(limits = c(0,40)) +
  theme_classic() +
  labs(x = "Refinery Individual Process Units",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"), 
       title = paste0("C. ", unique(df3$Year), " - ", unique(df3$Scenario), " Crude"))+
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5,size = 14))

# p4.
df4 <- df %>%
  filter(Year == "2050", Scenario == "Heavy/Sour") %>%
  mutate(Process.Units = fct_reorder(Process.Units, Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, .desc = TRUE))

p4 <- ggplot(df4, aes(x=Process.Units)) +
  geom_point(aes(y=Emissions.Reduction.Potential.Under.Medium.Transportation.Fuel.Demand, color = "Emissions Reduction Potential"), size=3) +
  geom_errorbar(aes(ymin = Emissions.Reduction.Potential.Low, 
                    ymax = Emissions.Reduction.Potential.High, color = "Emissions Reduction Potential"), 
                width = 0.2) +
  geom_point(aes(y=Emissions.Under.Medium.Transportation.Fuel.Demand, color = "Emissions Under Medium Demand"), size=3, shape=15) +
  geom_errorbar(aes(ymin =  Emissions.Low, 
                    ymax = Emissions.High, color = "Emissions Under Medium Demand"), 
                width = 0.2) +
  scale_color_manual(
    values = c("Emissions Reduction Potential" = "black", "Emissions Under Medium Demand" = "darkred"),
    labels = c("Emissions Reduction Potential", "Emissions Under Medium Demand")
  ) +
  scale_y_continuous(limits = c(0,40)) +
  theme_classic() +
  labs(x = "Refinery Individual Process Units",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"), 
       title = paste0("D. ", unique(df4$Year), " - ", unique(df4$Scenario), " Crude"))+
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5,size = 14))

#common label
y_label <- ggplot() + 
  theme_void() + 
  labs(y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")")) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5,size = 14))

p1<-p1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))


p2<-p2 + 
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
p3<-p3 + 
  theme(axis.title.x=element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
p4<-p4 + 
  theme(axis.title.x=element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))




# Then combine individual plots into one plot using patchwork
emission_plot_nolegend <- plot_grid(
  plot_grid(plot_grid(p1, p3, align = "h", ncol = 1, rel_heights = c(1.2, 1.2)),
            plot_grid(p2, p4, align = "h", ncol = 1, rel_heights = c(1.2, 1.2)),
            ncol = 2,
            rel_widths = c(1, 1)  # increase space for legend
  )
)
emission_plot<-plot_grid(
  emission_plot_nolegend,p_legend,align = "h", ncol = 1, rel_heights = c(1,0.05)
)
combined_plot <- plot_grid(
  y_label,
  emission_plot,
  ncol = 2,
  rel_widths = c(.03, 1),  # adjust as needed
  align = 'h'
)

print(combined_plot)
