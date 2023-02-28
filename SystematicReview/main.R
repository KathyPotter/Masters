
# attach libraries
library(dplyr)
library(ggplot2)
library(here)
library(maptools)
library(readxl)
library(scatterpie)
library(skimr) 
library(tidyverse)
library(vistime)


# clean work space
rm(list=ls())


# load data
SysReviewData <- read_xlsx(here("data/SystematicReviewData.xlsx"),sheet = "Original")

#inspect loaded data for any missing data
skimr::skim(SysReviewData)

# Generate table of counts

tableOfCounts <- data.frame(Name="Total sources",Count = nrow(SysReviewData))
tableOfCounts <- tableOfCounts  %>% 
  add_row(Name = "Sources with a Behaviour term", Count = sum(as.numeric(SysReviewData$AnyBehaviour)) )  %>%
  add_row(Name = "Sources with a Morphology term", Count = sum(as.numeric(SysReviewData$AnyMorphology)) ) %>%
  add_row(Name = "Sources with General Antipredator Behaviour", Count = sum(as.numeric(SysReviewData$GeneralAntipredatorBehaviour)) ) %>%
  add_row(Name = "Sources with Morphology only Hypothesis", Count = sum(as.numeric(SysReviewData$MorphologyOnly)) ) 

write.csv(tableOfCounts, here("tables/TotalCounts.csv"), row.names=FALSE)  

# Hypothesis Tables   

tableofHypothesis <- data.frame(Name="General Antipredator Behaviour",Count = sum(as.numeric(SysReviewData$GeneralAntipredatorBehaviour)))
tableofHypothesis <- tableofHypothesis  %>% 
  add_row(Name = "ASP", Count = sum(as.numeric(SysReviewData$ASP)) )  %>%
  add_row(Name = "Deimatism", Count = sum(as.numeric(SysReviewData$Deimatism)) ) %>%
  add_row(Name = "Startle", Count = sum(as.numeric(SysReviewData$Startle)) ) %>%
  add_row(Name = "D And ASP", Count = sum(as.numeric(SysReviewData$DAndASP)) ) %>%
  add_row(Name = "ITP", Count = sum(as.numeric(SysReviewData$ITP)) ) %>%
  add_row(Name = "Other Hypothesis", Count = sum(as.numeric(SysReviewData$OtherHypothesis)) ) 

write.csv(tableofHypothesis, here("tables/TotalHypothesis.csv"), row.names=FALSE)  


SysReviewData$Class[SysReviewData$Class=="na"] <-"other"
SysReviewData$Class[SysReviewData$Class=="general"] <-"other"
SysReviewData$Class[SysReviewData$Class=="simulated prey"] <-"other"
SysReviewData$Class[SysReviewData$Class=="various"] <-"other"
SysReviewData$Class[SysReviewData$Class=="Various"] <-"other"
SysReviewData$Class <- factor(SysReviewData$Class,levels =c("amphibians",  "invertebrates", "reptiles", "birds", "mammals", "other"))

HypothesisClass <- SysReviewData %>%
  group_by(Class) %>%
  summarize("Morphology Only" = sum(MorphologyOnly, na.rm=TRUE),
            "General Antipredator Behaviour" = sum(GeneralAntipredatorBehaviour, na.rm=TRUE),
            ASP = sum(ASP, na.rm=TRUE),
            Deimatism = sum(Deimatism, na.rm=TRUE),
            "Deimatism and ASP" = sum(DAndASP, na.rm=TRUE),
            Startle = sum(Startle, na.rm=TRUE),
            ITP = sum(ITP, na.rm=TRUE),
            "Other Hypothesis" = sum(OtherHypothesis, na.rm=TRUE)
  ) 

write.csv(HypothesisClass, here("tables/HypothesisVsClass.csv"), row.names=FALSE)  


OtherClassCount <- SysReviewData %>% count(Class) %>%
  mutate(perc = round(100 * `n` / sum(`n`)) / 100) %>% 
  mutate(labels = scales::percent(perc))



myPlot<- ggplot(OtherClassCount, aes(x="", y=n, fill=Class))+
  geom_bar(width = 1, stat = "identity", colour = "black", size=2)+
  coord_polar(theta = "y") +
  scale_fill_brewer(name = "Taxonomic Group",palette = "PuBu") +
  geom_text(aes(label = labels),position = position_stack(vjust = 0.58),size=7) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 30),
    legend.text = element_text(color = "black", size = 20)  )
myPlot
           


MorphologyTerm <- SysReviewData %>%
  filter(MorphologyOnly == 1) %>%
  summarize("Flash Behaviour" = sum(FlashBehaviour, na.rm=TRUE),
            "Flash Colour" = sum(FlashColour, na.rm=TRUE),
            "Flash Colouration" = sum(FlashColouration, na.rm=TRUE),
            "Flash Display" = sum(FlashDisplay, na.rm=TRUE),
            "Flash Mark" = sum(FlashMark, na.rm=TRUE),
            "Other" = sum(Other, na.rm=TRUE)
  ) %>% t()
colnames(MorphologyTerm) = c("Morphology Only")

write.csv(MorphologyTerm, here("tables/MorphologyTerm.csv"), row.names=TRUE)  

OtherHypothesis <- SysReviewData %>%
  filter(OtherHypothesis == 1) %>%
  group_by(OtherHypothesisText) %>%
  summarize(count= n())
OtherHypothesis <- OtherHypothesis %>%
     add_row(OtherHypothesisText = "Total", count = sum(as.numeric(SysReviewData$OtherHypothesis)), .before = 1  )
colnames(OtherHypothesis) = c("Other Hypothesis Text","Count")

write.csv(OtherHypothesis, here("tables/OtherHypothesis.csv"), row.names=FALSE)  


GAPB <- SysReviewData %>% 
  filter(GeneralAntipredatorBehaviour == 1) 
GAPB$Hypothesis <- "GAPB"
ASP <- SysReviewData %>% 
  filter(ASP == 1) 
ASP$Hypothesis <- "ASP"
Deimatism <- SysReviewData %>% 
  filter(Deimatism == 1) 
Deimatism$Hypothesis <- "Deimatism"
DAndASP <- SysReviewData %>% 
  filter(DAndASP == 1) 
DAndASP$Hypothesis <- "DAndASP"
Startle <- SysReviewData %>% 
  filter(Startle == 1) 
Startle$Hypothesis <- "Startle"
ITP <- SysReviewData %>% 
  filter(ITP == 1) 
ITP$Hypothesis <- "ITP"
OtherHypothesis <- SysReviewData %>% 
  filter(OtherHypothesis == 1) 
OtherHypothesis$Hypothesis <- "OtherHypothesis"

Hypothesis <- rbind(GAPB,ASP,Deimatism,DAndASP,Startle,ITP,OtherHypothesis)

HypothesisTerms <- Hypothesis %>%
  group_by(Hypothesis) %>%
  summarize("Total Hypothesis" = n(),
            "Flash Behaviour" = sum(FlashBehaviour, na.rm=TRUE),
            "Flash Colour" = sum(FlashColour, na.rm=TRUE),
            "Flash Colouration" = sum(FlashColouration, na.rm=TRUE),
            "Flash Display" = sum(FlashDisplay, na.rm=TRUE),
            "Flash Mark" = sum(FlashMark, na.rm=TRUE),
            Other = sum(Other, na.rm=TRUE)
  ) %>%
  t()

write.csv(HypothesisTerms, here("tables/HypothesisTerms.csv"), row.names=FALSE)  


HypothesisClassVTerms <- Hypothesis %>%
  group_by(Hypothesis,Class) %>%
  summarize("Total Hypothesis" = n(),
            "Flash Behaviour" = sum(FlashBehaviour, na.rm=TRUE),
            "Flash Colour" = sum(FlashColour, na.rm=TRUE),
            "Flash Colouration" = sum(FlashColouration, na.rm=TRUE),
            "Flash Display" = sum(FlashDisplay, na.rm=TRUE),
            "Flash Mark" = sum(FlashMark, na.rm=TRUE),
            Other = sum(Other, na.rm=TRUE)
  )

write.csv(HypothesisClassVTerms, here("tables/HypothesisClassVTerms.csv"), row.names=FALSE)  

BehaviourTerms <- SysReviewData %>%
  filter(AnyBehaviour == 1) %>%
  summarize("FlashBehaviour" = sum(FlashBehaviour, na.rm=TRUE),
            "FlashColour" = sum(FlashColour, na.rm=TRUE),
            FlashColouration = sum(FlashColouration, na.rm=TRUE),
            FlashDisplay = sum(FlashDisplay, na.rm=TRUE),
            "FlashMark" = sum(FlashMark, na.rm=TRUE),
            Other = sum(Other, na.rm=TRUE),
  )

write.csv(BehaviourTerms, here("tables/BehaviourTerms.csv"), row.names=FALSE)  



MorpTerms <- SysReviewData %>%
  filter(AnyMorphology == 1) %>%
  summarize(
            "FlashBehaviour" = sum(FlashBehaviour, na.rm=TRUE),
            "FlashColour" = sum(FlashColour, na.rm=TRUE),
            FlashColouration = sum(FlashColouration, na.rm=TRUE),
            FlashDisplay = sum(FlashDisplay, na.rm=TRUE),
            "FlashMark" = sum(FlashMark, na.rm=TRUE),
            Other = sum(Other, na.rm=TRUE)
  )

write.csv(MorpTerms, here("tables/MorpTerms.csv"), row.names=FALSE)  


BothTerms <- bind_rows(BehaviourTerms,MorpTerms)
BothTerms <-bind_cols(c("Behaviour", "Morphology"),BothTerms )
colnames(BothTerms)[1] = ""

write.csv(BothTerms, here("tables/BothTerms.csv"), row.names=FALSE)




timeDate <- data.frame( y_lab = rep(c(" "),each=5),
                        term = c("FlashColour","FlashColouration","FlashDisplay","FlashBehaviour","Other"),
                        start = c("1906-01-01","1932-01-01","1948-01-01","1977-01-01","1929-01-01"),
                        end = rep(c("2022-01-01"),each=5)
)

timeDateFamily <- data.frame(y_lab = rep(c(" "),each=7),
                             term = c("Flash Colour","Flash Colouration","Flash Display","Flash Mark","Flash Behaviour","Other","Flashes"),
                             start =c("1906-01-01", "1932-01-01",     "1940-01-01",   "1950-01-01","1975-01-01",    "1987-01-01","1929-01-01"),
                             end =        c("2022-01-01","2022-01-01","2022-01-01","2022-01-01","2022-01-01","2021-01-01","2022-01-01"),
                             color = rep(c("steelblue3"),each=7)
)

myPlot <- gg_vistime(timeDateFamily, col.event = "y_lab", col.group = "term", title = "Accretion of terms over time") +
  theme( plot.title = element_text( size=40),
         axis.text.x = element_text(size = 30),
         axis.text.y = element_text(size = 30),
         panel.border = element_rect(linetype = "dashed", fill=NA)
         #         panel.background = element_rect(fill = 'green')
  )
myPlot

pivotted_data <- read.csv(here("./data/", "CountryTerms.csv"), stringsAsFactors = F)

# Getting the coordinates of each country
country_lookup <- read.csv(here("data/referenceData/", "countries.csv"), stringsAsFactors = F)
names(country_lookup)[1] <- "country_code"

# Combining data
final_data <- merge(x = pivotted_data, y = country_lookup, by.x = "country", by.y = "name", all.x = T)

# Data cleaning for plotting
final_data <- unique(final_data)
multiplier <- .2 + log10(final_data$Total) / log10(max(final_data$Total))
final_data <- cbind(final_data, multiplier)

# Using map_data()
worldmap <- map_data ("world")

mapplot <- ggplot(worldmap) + 
  geom_map(data = worldmap, map = worldmap, aes(x=long, y=lat, map_id=region), col = "white", fill = "gray50") +
  geom_scatterpie(aes(x=longitude, y=latitude, group = country, r = multiplier*6), 
                  data = final_data, cols = colnames(final_data[,c(2:8)])) +
  ylim(-55,80) + xlim(-160,170) +
  scale_fill_manual(values = c("chartreuse","chartreuse4", "cyan","cornflowerblue",  "darkorchid","darkorchid4", "gold1")) +
  labs(title = "Terms by country", x = "Longitude", y = "Latitude") +
  theme(legend.position = "top") +
  annotate(geom="text", x=-130, y=8, label="KEY: Total Papers", color="black") +
  annotate(geom="text", x=-125, y=0, label="1", color="black") +
  annotate(geom="text", x=-125, y=-7, label="3", color="black") +
  annotate(geom="text", x=-125, y=-16, label="10", color="black") +
  annotate(geom="text", x=-125, y=-28, label="30", color="black") +
  annotate(geom="text", x=-125, y=-43, label="100", color="black") 

mapplot


