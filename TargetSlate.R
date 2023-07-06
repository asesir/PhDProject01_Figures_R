# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# read csv file
df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/PhD Thesis/Results/Results Analysis_3rd Run/RefineryCapacity_NoHistoricalLimit_PADDAvgChangedPercentbyConfig/Reruned Figures/TargetSlate.csv")

df$Year <- as.factor(df$Year)
df$PADD <- as.factor(df$PADD)

df <- df %>%
  mutate(PADD_Year= interaction(PADD, Year, sep = "_"))
df <- df %>%
  mutate(Total = select(., Blended.Gasoline:Lube) %>% rowSums(na.rm = TRUE))

df$PADD_Year<-factor(df$PADD_Year,levels = unique(c("1_2019",
                                                              "1_2035",
                                                              "1_2050",
                                                              "2_2019",
                                                              "2_2035",
                                                              "2_2050",
                                                              "3_2019",
                                                              "3_2035",
                                                              "3_2050",
                                                              "4_2019",
                                                              "4_2035",
                                                              "4_2050",
                                                              "5_2019",
                                                              "5_2035",
                                                              "5_2050",
                                                              "U.S._2019",
                                                              "U.S._2035",
                                                              "U.S._2050")))



df_long<-df %>%
  pivot_longer(cols = c("Blended.Gasoline","Jet.A.AVTUR","ULSD","Fuel.Oil","Coke","Liquid.Heavy.Ends","Liquified.Petroleum.Gas..LPG.", "Lube"),
               names_to = "Category", 
               values_to = "Value")



# Change names for Electrification Scenarios column
AbbNames<-c(Blended.Gasoline = "Gasoline", Jet.A.AVTUR = "Jet-A/AVTUR",ULSD = "ULSD", Fuel.Oil = "Fuel Oil", Coke = "Coke", Liquid.Heavy.Ends = "LHE", Liquified.Petroleum.Gas..LPG.="LPG", Lube = "Lube" )
df_long$Category <- as.character(AbbNames[df_long$Category])
df_long$Category <- as.factor(df_long$Category)




# Create a named vector for your custom color palette
my_palette <- brewer.pal(n = length(unique(df_long$Category)), name = "Spectral")


# Create a separate data frame for the labels
labels_df <- df_long %>%
  select(PADD, Year, PADD_Year, Category) %>%
  unique() %>%
  mutate(Label = paste(PADD, Year, sep = "\n")) %>%
  arrange(order(PADD_Year))

prepare_labels <- function(padd, year) {
  labels <- ifelse(year == "2019",
                   paste(year, "\n\n"),
                   ifelse(year == "2035",
                          paste(year,"\n", padd),
                          paste(year, "\n\n")))
  return(labels)
}
#
labels_df$Label <- prepare_labels(labels_df$PADD, labels_df$Year)


for (i in unique(labels_df$PADD)){
  labels_df$Label<-prepare_labels(labels_df$PADD,labels_df$Year)}

intercept<-c(3.5, 6.5, 9.5, 12.5, 15.5)


# Create a list with the labels
labels <- setNames(labels_df$Label, labels_df$PADD_Year)





p <- ggplot(df_long, aes(x = PADD_Year)) +
  geom_bar(aes(y = Value, fill = Category),
           stat = 'identity', 
           position = 'stack', 
           width = 0.5) +
  geom_errorbar(data = df,
                aes(ymin = Total - LB,
                    ymax = Total + UB),
                color = "black",
                width = 0.2)+
  coord_cartesian(clip = "off")+
  theme_classic() +
  labs(x = "PADD & Year", 
       y = expression("Refinery Production (mmbbl y"^{-1}*")")) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_x_discrete(labels = labels)+
  scale_fill_manual(values = my_palette,
                    name = "Refinery Product")+
  geom_vline(xintercept = intercept, color = "black")+
  annotate("text",x = 3, y = 8000, label = "PADD 1", size = 6.5)+
  annotate("text",x = 6, y = 8000, label = "PADD 2", size = 6.5)+
  annotate("text",x = 9, y = 8000, label = "PADD 3", size = 6.5)+
  annotate("text",x = 12, y = 8000, label = "PADD 4", size = 6.5)+
  annotate("text",x = 15, y = 8000, label = "PADD 5", size = 6.5)+
  annotate("text",x = 18, y = 8000, label = "U.S.", size = 6.5)

print(p)