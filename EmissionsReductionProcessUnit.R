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
    name = "Legend",
    breaks = c("black", 
               "darkred"),
    labels = c("Emissions Reduction - \n Potential Under - \n Medium Transportation - \n Fuel Demand", 
               "Emissions Under - \n Medium Transportation - \n Fuel Demand"),
    guide = "legend"
  ) +

  scale_y_continuous(limits = c(0,40)) +
  theme_classic() +
  labs(x = "Refinery Individual Process Units",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"), 
       title = paste0("A. ", unique(df1$Year), " - ", unique(df1$Scenario), " Crude"))+
  theme(legend.text = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 10)),
        legend.title = element_text(size = 14),
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



# dff<-read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/EmissionsReductionProcessUnits_2.csv")

dff<-read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/EmissionsReductionProcessUnits_2.csv")


dff$Emissions.Reduction.Potentials <- dff$Emissions.Reduction.Potentials / 1000000
dff <- dff %>% filter(Emissions.Reduction.Potentials >0.001)

labels <- sapply(levels(interaction(dff$Configuration, dff$PADD)), function(x) strsplit(x, "\\.")[[1]][1])


# Compute the x-intercepts for the vertical lines
# vlines <- dff %>% 
#   group_by(PADD) %>% 
#   summarise(max_Configuration = max(as.numeric(Configuration))) %>% 
#   mutate(cumulative_max_Configuration = cumsum(max_Configuration))

# Create the new box plot
box_plot <- ggplot(dff, aes(x = interaction(Configuration, PADD), y = Emissions.Reduction.Potentials, fill = interaction(PADD))) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 10),
                     labels = scales::number_format(accuracy = 1)) +
  scale_x_discrete(labels = labels) +
  scale_fill_brewer(palette = "Set3", name = "PADD") +
  labs(x = "Refinery Configuration", 
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       title = "E.Distribution of Emissions Reduction Potential in Different Refinery Configurations & PADD Regions") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) 


# Set the midpoints manually based on the information you provided
midpoints <- c(5.5, 10.5, 17.5, 22.5)

# Add the vertical lines to the plot at these midpoints
for (i in midpoints) {
  box_plot <- box_plot + geom_vline(xintercept = i, linetype = "dashed")
}

# Add the PADD labels
padd_labels <- paste0("PADD ", 1:4)
for (i in seq_along(midpoints)) {
  box_plot <- box_plot + 
    annotate("text", x = midpoints[i] - 1, y = 10, label = padd_labels[i])
}


box_plot <- box_plot +
  annotate("text", x = 28.5, y = 10, label = "PADD 5")


box_plot_legend<-get_legend(box_plot)

box_plot <- box_plot +
  theme(axis.title.x = element_text(), axis.text.x = element_text(size = 12),axis.title.y = element_blank(), axis.text.y = element_text(),legend.position = "none")


legends <- plot_grid(p_legend, box_plot_legend, ncol = 1, align = 'v',rel_heights = c(1.2,.5))


# Then combine individual plots into one plot using patchwork
emission_plot <- plot_grid(
  plot_grid(plot_grid(p1, p3, align = "h", ncol = 1, rel_heights = c(1.2, 1.2)),
            plot_grid(p2, p4, align = "h", ncol = 1, rel_heights = c(1.2, 1.2)),
            ncol = 2,
            rel_widths = c(1, 1)  # increase space for legend
  )
)
combined_plot <- plot_grid(
  y_label,
  plot_grid(emission_plot, box_plot, nrow = 2, rel_heights = c(2.5, 1)),
  legends,
  ncol = 3,
  rel_widths = c(.05, 1, 0.2),  # adjust as needed
  align = 'h'
)

print(combined_plot)
