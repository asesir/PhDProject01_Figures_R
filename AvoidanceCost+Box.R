library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggmacc)
library(cowplot)

df1 <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/MACC_2035_HS_R.csv")

df2 <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/MACC_2050_HS_R.csv")


df1$Capture.Scenario <- factor(df1$Capture.Scenario)
df1$PADD_factor <- factor(df1$PADD)
df2$Capture.Scenario <- factor(df2$Capture.Scenario)
df2$PADD_factor <- factor(df2$PADD)

# Convert the units in your dataframes
df1$Total.Avoided.CO2.kt.y <- df1$Total.Avoided.CO2.kt.y / 1000
df2$Total.Avoided.CO2.kt.y <- df2$Total.Avoided.CO2.kt.y / 1000

df1_filtered <- df1 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)
df2_filtered <- df2 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)

# Concatenate the dataframes
df_all__filtered <- rbind(df1_filtered, df2_filtered)


full_macc_2035_HS_Unit <- df1 %>%
  ggmacc(abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = Capture.Scenario,
         zero_line = FALSE, threshold_line = FALSE)

full_macc_2035_HS_PADD <- df1 %>%
  ggmacc(abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = PADD_factor,
         zero_line = FALSE, threshold_line = FALSE)

full_macc_2050_HS_Unit <- df2 %>%
  ggmacc(abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = Capture.Scenario,
         zero_line = FALSE, threshold_line = FALSE)

full_macc_2050_HS_PADD <- df2 %>%
  ggmacc(abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = PADD_factor,
         zero_line = FALSE, threshold_line = FALSE)


p1 <- full_macc_2035_HS_Unit +
  scale_x_continuous(labels = scales::number_format(),breaks=seq(0,105,15),limits = c(0, 110)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_brewer(palette = "BrBG") +
  labs(title = expression("A. CO "[2]*" Avoidance Cost by Process Unit in 2035"),
       fill = "Captured Units",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

p2 <- full_macc_2035_HS_PADD +
  scale_x_continuous(labels = scales::number_format(),breaks=seq(0,105,15),limits = c(0, 110)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = expression("B. CO "[2]*" Avoidance Cost by PADD in 2035"),
       fill = "PADD",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

p3 <- full_macc_2050_HS_Unit +
  scale_x_continuous(labels = scales::number_format(),breaks=seq(0,105,15),limits = c(0, 110)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_brewer(palette = "BrBG") +
  labs(title = expression("C. CO "[2]*" Avoidance Cost by Process Unit in 2050"),
       fill = "Captured Units",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

p4 <- full_macc_2050_HS_PADD +
  scale_x_continuous(labels = scales::number_format(),breaks=seq(0,105,15),limits = c(0, 110)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = expression("D. CO "[2]*" Avoidance Cost by PADD in 2050"),
       fill = "PADD",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))



# Extract the legend
p_legend_Unit <- get_legend(p1)
p_legend_PADD <- get_legend(p2)


# Remove legend for each plot
p1 <- p1 + 
  theme(axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12),axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12),legend.position = "none") 

p3 <- p3 + 
  theme(axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12),axis.title.y = element_blank(), axis.text.y = element_blank(),legend.position = "none") 

p2 <- p2 + 
  theme(axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12),axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12),legend.position = "none") 

p4 <- p4 + 
  theme(axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12),axis.title.y = element_blank(), axis.text.y = element_blank(),legend.position = "none") 
  




# Extract the 'Config' value from the interaction variable for x-axis labels
labels <- sapply(levels(interaction(df_all__filtered$Config, df_all__filtered$PADD)), function(x) strsplit(x, "\\.")[[1]][1])


# Compute the x-intercepts for the vertical lines
vlines <- df_all__filtered %>% 
  group_by(PADD) %>% 
  summarise(max_config = max(as.numeric(Config))) %>% 
  mutate(cumulative_max_config = cumsum(max_config))

# Create the new box plot
box_plot_PADD <- ggplot(df_all__filtered, aes(x = interaction(Config, PADD), y = Marginal.Abatement.Cost....t, fill = interaction(PADD))) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 400)) +
  scale_x_discrete(labels = labels) +
  scale_fill_brewer(palette = "Spectral", name = "PADD") +
  labs(x = "Refinery Configuration", 
       y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")"),
       title = expression ("E.Distribution of CO"[2]*" Avoidance Cost for Carbon Capture in Different Refinery Configurations")) +
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
  box_plot_PADD <- box_plot_PADD + geom_vline(xintercept = i, linetype = "dashed")
}

# Add the PADD labels
padd_labels <- paste0("PADD ", 1:4)
for (i in seq_along(midpoints)) {
  box_plot_PADD <- box_plot_PADD + 
    annotate("text", x = midpoints[i] - 1, y = 10, label = padd_labels[i])
}


box_plot_PADD <- box_plot_PADD +
  annotate("text", x = 28.5, y = 10, label = "PADD 5")

box_plot_PADD_legend<-get_legend(box_plot_PADD)

box_plot_PADD <- box_plot_PADD +
  theme(axis.title.x = element_text(), axis.text.x = element_text(size = 12),axis.title.y = element_blank(), axis.text.y = element_text(),legend.position = "none")


# Remove y-axis title from individual plots
p1 <- p1 + theme(axis.title.y=element_blank())
p2 <- p2 + theme(axis.title.y=element_blank())
p3 <- p3 + theme(axis.title.y=element_blank())
p4 <- p4 + theme(axis.title.y=element_blank())
box_plot_PADD <- box_plot_PADD + theme(axis.title.y=element_blank())

# Create a plot that only contains the y-axis label
y_label <- ggplot() + 
  theme_void() + 
  labs(y = expression("CO "[2]*" Avoidance Cost ($ tonne CO"[2]*"-eq"^{-1}*")")) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5,size = 16))


# Combine the legends vertically and left aligned
legends <- plot_grid(p_legend_Unit, p_legend_PADD, box_plot_PADD_legend, ncol = 1, align = 'v',rel_heights = c(1.1,1.2,1))

# Arrange the MAC plots
mac_plots <- plot_grid(
  plot_grid(p1, p2, align = "h", ncol = 1, rel_heights = c(1.1, 1.2)),
  plot_grid(p3, p4, align = "h", ncol = 1, rel_heights = c(1.1, 1.2)),
  ncol = 2,
  rel_widths = c(1, 1)  
)

# Combine all the plots with the common y-axis label
combined_plot <- plot_grid(
  y_label,
  plot_grid(mac_plots, box_plot_PADD, nrow = 2, rel_heights = c(2.3, 1)),
  legends,
  ncol = 3,
  rel_widths = c(.05, 1, 0.1),# adjust as needed
  align = 'h'
)

# Display the plot
print(combined_plot)



