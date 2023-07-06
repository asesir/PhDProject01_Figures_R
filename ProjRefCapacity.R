# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# read csv file
df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/ProjCapacityReport.csv")

# Change names for Electrification Scenarios column
AbbNames <- c(BASELINE = "BL", HIGH = "LOW", MEDIUM = "MED.", LOW = "HIGH", REFERENCE ="REF")
df$Electrification.Scenarios <- as.character(AbbNames[df$Electrification.Scenarios])

df<-subset(df,df$Electrification.Scenarios!="REF")


df$Year <- as.factor(df$Year)
df$PADD <- as.factor(df$PADD)
df$Attribute <- as.factor(df$Attribute)
df <- df %>%
  mutate(PADD_Year_Scenario = interaction(PADD, Year, Electrification.Scenarios, sep = "_"))

df$PADD_Year_Scenario<-factor(df$PADD_Year_Scenario,levels = unique(c("1_2019_BL",
                                                                      "1_2035_HIGH",
                                                                      "1_2050_HIGH",
                                                                      "1_2035_MED.",
                                                                      "1_2050_MED.",
                                                                      "1_2035_LOW",
                                                                      "1_2050_LOW",
                                                                      "2_2019_BL",
                                                                      "2_2035_HIGH",
                                                                      "2_2050_HIGH",
                                                                      "2_2035_MED.",
                                                                      "2_2050_MED.",
                                                                      "2_2035_LOW",
                                                                      "2_2050_LOW",
                                                                      "3_2019_BL",
                                                                      "3_2035_HIGH",
                                                                      "3_2050_HIGH",
                                                                      "3_2035_MED.",
                                                                      "3_2050_MED.",
                                                                      "3_2035_LOW",
                                                                      "3_2050_LOW",
                                                                      "4_2019_BL",
                                                                      "4_2035_HIGH",
                                                                      "4_2050_HIGH",
                                                                      "4_2035_MED.",
                                                                      "4_2050_MED.",
                                                                      "4_2035_LOW",
                                                                      "4_2050_LOW",
                                                                      "5_2019_BL",
                                                                      "5_2035_HIGH",
                                                                      "5_2050_HIGH",
                                                                      "5_2035_MED.",
                                                                      "5_2050_MED.",
                                                                      "5_2035_LOW",
                                                                      "5_2050_LOW",
                                                                      "U.S._2019_BL",
                                                                      "U.S._2035_HIGH",
                                                                      "U.S._2050_HIGH",
                                                                      "U.S._2035_MED.",
                                                                      "U.S._2050_MED.",
                                                                      "U.S._2035_LOW",
                                                                      "U.S._2050_LOW")))
# Create a named vector for your custom color palette
my_palette <- brewer.pal(n = length(unique(df$Attribute)), name = "Spectral")

df1 <- subset(df, (df$API.Level == "LIGHT" & df$Sul.Level == "SWEET") | (df$API.Level == "REF" & df$Sul.Level == "REF"))
df2 <- subset(df, (df$API.Level == "HEAVY" & df$Sul.Level == "SOUR") | (df$API.Level == "REF" & df$Sul.Level == "REF"))
# Create a separate data frame for the labels
labels_df <- df1 %>%
  select(PADD_Year_Scenario, Year, Electrification.Scenarios, PADD) %>%
  unique() %>%
  mutate(Label = paste(Year, Electrification.Scenarios, PADD, sep = "\n")) %>%
  arrange(desc(PADD_Year_Scenario))

prepare_labels <- function(padd, year, scenario) {
  labels <- ifelse(year == "2019", 
                   paste( "\n\n\n", scenario, "\n\n"),
                   ifelse(year == "2035" & scenario == "HIGH", 
                          paste( "\n\n\n","        ",scenario,"\n\n"),
                          ifelse(year == "2050" & scenario == "HIGH",
                                 paste("\n\n\n\n\n"),
                                 ifelse(year == "2035" & scenario == "MED.", 
                                        paste( "\n\n\n","        ",scenario,"\n","        ",padd),
                                        ifelse(year == "2050" & scenario == "MED.",
                                               paste("\n\n\n\n\n"),
                                               ifelse(year == "2035" & scenario == "LOW", 
                                                      paste( "\n\n\n","        ",scenario,"\n\n"),
                                                      ifelse(year == "2050" & scenario == "LOW", 
                                                             paste("\n\n\n\n\n"),
                                                             paste("\n\n\n\n\n", scenario))))))))
  return(labels)
}

labels_df$Label <- prepare_labels(labels_df$PADD, labels_df$Year, labels_df$Electrification.Scenarios)


for (i in unique(labels_df$PADD)){
labels_df$Label<-prepare_labels(labels_df$PADD,labels_df$Year,labels_df$Electrification.Scenarios)}

intercept<-c(7.5, 14.5, 21.5, 28.5,35.5)


# Create a list with the labels
labels <- setNames(labels_df$Label, labels_df$PADD_Year_Scenario)

p1 <- ggplot(df1, aes(x = PADD_Year_Scenario, y = Value, fill = Attribute)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  coord_cartesian(clip = "off")+
  theme_classic() +
  labs(x = "PADD & Transporation Fuel Demand Level & Year", 
       y = expression("Refinery Throughput (mbbl d"^{-1}*")"),
       title = "A.Light/Sweet Crude") +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5,size = 16)) +
  scale_x_discrete(labels = labels)+
  scale_fill_manual(values = my_palette, name = "Refinery Configuration")+
  geom_vline(xintercept = intercept, color = "black")+
  annotate("text",x = 6, y = 18000, label = "PADD 1", size = 6.5)+
  annotate("text",x = 13, y = 18000, label = "PADD 2", size = 6.5)+
  annotate("text",x = 20, y = 18000, label = "PADD 3", size = 6.5)+
  annotate("text",x = 27, y = 18000, label = "PADD 4", size = 6.5)+
  annotate("text",x = 34, y = 18000, label = "PADD 5", size = 6.5)+
  annotate("text",x = 41.5, y = 18000, label = "U.S.", size = 6.5)

p_legend<-get_legend(p1)

p1<-p1+
  theme(legend.position = "none")

y_label <- ggplot() + 
  theme_void() + 
  labs(y = expression("Refinery Throughput (mbbl d"^{-1}*")")) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5,size = 14))



p2 <- ggplot(df2, aes(x = PADD_Year_Scenario, y = Value, fill = Attribute)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  geom_text(data = labels_df, aes(label = Year, x = PADD_Year_Scenario, y = 0), 
            angle = 90, hjust = 2, vjust = 0.5, size = 4.5,inherit.aes = FALSE) +
  coord_cartesian(clip = "off")+
  theme_classic() +
  labs(x = "PADD & Transporation Fuel Demand Level & Year", 
       y = expression("Refinery Throughput (mbbl bbl"^{-1}*")"),
       title = "B.Heavy/Sour Crude") +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 16)) +
  scale_x_discrete(labels = labels)+
  scale_fill_manual(values = my_palette)+
  geom_vline(xintercept = intercept, color = "black")+
  annotate("text",x = 6, y = 18000, label = "PADD 1", size = 6.5)+
  annotate("text",x = 13, y = 18000, label = "PADD 2", size = 6.5)+
  annotate("text",x = 20, y = 18000, label = "PADD 3", size = 6.5)+
  annotate("text",x = 27, y = 18000, label = "PADD 4", size = 6.5)+
  annotate("text",x = 34, y = 18000, label = "PADD 5", size = 6.5)+
  annotate("text",x = 41.5, y = 18000, label = "U.S.", size = 6.5)

box_chart_plot <- plot_grid(p1, p2, p_legend, align = "v", ncol = 1, rel_heights = c(0.8, 1.1,0.1))



combined_plot <- plot_grid(y_label,
                           box_chart_plot,
                           ncol = 2,
                           rel_widths = c(.02, 1),# adjust as needed
                           align = 'h')

print(combined_plot)