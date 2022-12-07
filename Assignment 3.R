setwd("/Users/bradleywood-maclean/Desktop/POL304R")
library(tidyverse)
library(magrittr)
library(rvest)

##Load the data 
WarList <- read.csv("INTRA-STATE_WARS_v5.1_CSV.csv")
Religion <- read.csv("WRP_nationalCSV.csv")

#Step 1 Clean the data
##Dependent variable WarList
##remove columns 1, 3-5, 8-43 are irrelevant data

WarList2 <- WarList[-1]
WarList3 <- WarList2[-2:-4]
WarList4 <- WarList3[-3:-39] 
WarListFinal <- WarList4[-c(1:279), ] #rows after 280 are from the last 50 years
WarListFinal <- rename(WarListFinal, "Country" = "SideA")

##Independent variable Religion

Religion$Diverse <- NA
ls(Religion)
class(Religion$chrstgen) #integer

##remove different Jewish divisions (columns 10-13)

Religion2 <- Religion[-10:-13]
##remove different Buddhist divisions (columns 19-21)

Religion3 <- Religion2[-19:-21]

##remove duplicate data 

Religion4 <- Religion3[-34:-77]
Religion5 <- Religion4[-2]

#Remove past years

Religion6 <- Religion5[-grep("1945", Religion5$year),]
Religion7 <- Religion6[-grep("1950", Religion6$year),]
Religion8 <- Religion7[-grep("1955", Religion7$year),]
Religion9 <- Religion8[-grep("1960", Religion8$year),]
Religion10 <- Religion9[-grep("1965", Religion9$year),]
Religion11 <- Religion10[-grep("1970", Religion10$year),]
Religion12 <- Religion11[-grep("1975", Religion11$year),]
Religion13 <- Religion12[-grep("1980", Religion12$year),]
Religion14 <- Religion13[-grep("1985", Religion13$year),]
Religion15 <- Religion14[-grep("1990", Religion14$year),]
Religion16 <- Religion15[-grep("1995", Religion15$year),]
Religion17 <- Religion16[-grep("2000", Religion16$year),]
Religion18 <- Religion17[-grep("2005", Religion17$year),]

##code for diverse 1 or non-diverse 0
##code for diversity

Religion18$Diverse <- NA

Religion18$Diverse[(Religion18$anmgen/Religion18$pop)>0.5 | 
                     (Religion18$bahgen/Religion18$pop)>0.5 |
                     (Religion18$budgen/Religion18$pop)>0.5 |
                     (Religion18$chrstang/Religion18$pop)>0.5 |
                     (Religion18$chrstgen/Religion18$pop)>0.5 |
                     (Religion18$chrstorth/Religion18$pop)>0.5 |
                     (Religion18$chrstothr/Religion18$pop)>0.5 |
                     (Religion18$chrstprot/Religion18$pop)>0.5 |
                     (Religion18$confgen/Religion18$pop)>0.5 |
                     (Religion18$hindgen/Religion18$pop)>0.5 |
                     (Religion18$islmahm/Religion18$pop)>0.5 |
                     (Religion18$islmalw/Religion18$pop)>0.5 |
                     (Religion18$islmgen/Religion18$pop)>0.5 |
                     (Religion18$islmibd/Religion18$pop)>0.5 |
                     (Religion18$islmnat/Religion18$pop)>0.5 |
                     (Religion18$islmothr/Religion18$pop)>0.5 |
                     (Religion18$islmshi/Religion18$pop)>0.5 |
                     (Religion18$islmsun/Religion18$pop)>0.5 |
                     (Religion18$jaingen/Religion18$pop)>0.5 |
                     (Religion18$judgen/Religion18$pop)>0.5 |
                     (Religion18$nonrelig/Religion18$pop)>0.5 |
                     (Religion18$othrgen/Religion18$pop)>0.5 |
                     (Religion18$shntgen/Religion18$pop)>0.5 |
                     (Religion18$sikhgen/Religion18$pop)>0.5 |
                     (Religion18$syncgen/Religion18$pop)>0.5 |
                     (Religion18$taogen/Religion18$pop)>0.5 |
                     (Religion18$zorogen/Religion18$pop)>0.5 ] <- 0 #if 1 group is greater than 50% undiverse

Religion18$Diverse[is.na(Religion18$Diverse)] = 1 #What's left is diverse

##Code for civil war outbreak
##Export Religion18 and manually code for a war based on WarListFinal

library(dplyr)
write.csv(Religion18, "Religion18.csv")

##Load the new data (manually edited version of Religion18)

ReligionCombined <- read.csv("Religion19.csv")

##Remove useless data

ReligionCombined2 <- ReligionCombined[-1]
ReligionCombined3 <- ReligionCombined2[-1]
ReligionWar <- ReligionCombined3[-2:-31]

#Step 2 Analysis 

class(ReligionWar$Diverse) #integer
class(ReligionWar$War) #integer

##How many countries are diverse? Undiverse?

length(ReligionWar$Diverse[ReligionWar$Diverse==1]) #16 diverse countries
length(ReligionWar$Diverse[ReligionWar$Diverse==0]) #178 non-diverse countries

#Cross-Sectional Design

DivWar <- mean(ReligionWar$War[ReligionWar$Diverse==1])
DivWar #0.1875 mean number of wars for diverse countries (treatment)

UnDivWar <- mean(ReligionWar$War[ReligionWar$Diverse==0])
UnDivWar #0.3202247 mean number of wars for non-diverse countries (control)
