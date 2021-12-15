#-------------------------------------------------------------------------------
# FMPH 402 - Health Behavior Final Paper and Presentation Plots
#-------------------------------------------------------------------------------

## Set-Up ##

#Working directory
getwd()
setwd("/Users/rj/Documents/School/UCSD/Fall_2021/FMPH-402_Health-Behavior/FMPH-402_HB_Final")

#Read in dataset
FinalPapers <- read.csv("FMPH-402_Final_RPlot_Data.csv")

#Libraries
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)

## Data Preparation ##

# Create Count for Association and Significance
AssociationSignificance <- FinalPapers %>%
  group_by(Association, Significance) %>%
  summarise(Count = n()) %>%
  mutate( Association = factor(Association),
          Ratio = Count / sum(Count),
          Prcnt = percent(Ratio %>% round(3)))

#####
# Adding new rows (v. 2)
ASAdjustv2 <- data.frame(c("All", "All", "All"),
                         c("Yes", "Partial", "No"),
                         c(12, 2, 1), 
                         c(0.80, 0.13333, 0.066667),
                         c("80%", "13%", "7%")) 

names(ASAdjustv2) <- c("Association", "Significance", "Count", "Ratio", "Prcnt")

AssociationSignificance$Significance <- 
  ordered(AssociationSignificance$Significance,
          levels = c("Yes", "Partial", "No"))

ASAdjustv2$Significance <- 
  ordered(ASAdjustv2$Significance,
          levels = c("Yes", "Partial", "No"))

ArtAssocSigLvl <- ggplot(AssociationSignificance, 
       aes(x = Association, y = Ratio, label = Count, fill = Significance)) + 
  geom_col(position = "stack") + 
  geom_text(position = position_stack(vjust = 0.5), color = "white", size = 8) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent of Found Significance") + 
#  ggtitle("Significance Found in the Articles by Articles' Association Type") +
  scale_fill_manual(values = c("#546C54","#98bc9c","#7c8cab")) +
  theme_bw() + theme(axis.title.x = element_blank(), legend.position = "none",
                     axis.title.y = element_text(size =16), axis.text.y = 
                       element_text(size = 16), 
                     axis.text.x = element_text(size = 16, face = "bold"))

ArtAssocSigAll <- ggplot(ASAdjustv2, 
       aes(x = Association, y = Ratio, label = Count, fill = Significance)) + 
  geom_col(position = "stack") + 
  geom_text(position = position_stack(vjust = 0.5), color = "white", size = 8) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent) +
#  ylab("Count of Found Significance") + 
#  ggtitle("Significance Found in the Articles by Articles' Association Type") +
  scale_fill_manual(values = c("#546C54","#98bc9c","#7c8cab")) +
  theme_bw() + theme(axis.title.x = element_blank(), legend.position = "right",
                     axis.title.y = element_blank(), axis.text.y = element_blank(),
                     axis.ticks = element_blank(), 
                     axis.text.x = element_text(size = 16, face = "bold"),
                     legend.text = element_text(size = 14), 
                     legend.title = element_text(size = 18))

grid.arrange(ArtAssocSigLvl, ArtAssocSigAll, nrow = 1, widths = c(2, 1),
             top = textGrob("Significance Found in the Articles by Articles' Association Type", 
                            gp=gpar(fontsize=20,font=8)))

## Pie chart ##

# Create Count for Association and Significance
AssociationDF <- FinalPapers %>%
  group_by(Association) %>%
  summarise(Count = n()) %>%
  mutate( Association = factor(Association),
          Ratio = Count / sum(Count),
          Prcnt = percent(Ratio %>% round(3)))

ggplot(AssociationDF, aes(x="", y=Ratio, fill=Association)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  scale_fill_manual(values = c("#9aa9bf","#957da3","#737d73"))




