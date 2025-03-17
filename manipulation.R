
# Data Manipulation Script
# 
# UPDATE: March 17, 2025
# > CIA World Factbook URLs scraped to obtain `Govt` and `Climate` no longer accessible; web scraping code commented-out
# 
# UPDATE: April 29, 2020
# > Result available for download here: https://www.kaggle.com/datasets/mgjurajkaggle/stat-230-project-dataset

library(car)
library(leaps)
library(lubridate)
library(rvest)
library(olsrr)
library(corrplot)

# Creating the dataset - https://data.worldbank.org/indicator
setwd("data")
wbtemp <- read.csv("AgriculturalLand.csv", as.is = TRUE)
wbtemp <- wbtemp[, c(3, 5)]
names(wbtemp)[2] <- "AgriLand"
WB <- wbtemp
variables <- c("RuralPopulation.csv", "AFR.csv", "CO2.csv", "Exports.csv", "FertilityRate.csv", "GNI.csv", "Imports.csv", "LiteracyRate.csv", "PopGrowth.csv", "PopulationTotal.csv", "UrbanPopulation.csv", "Pop14below.csv", "Diabetes.csv", "DPTVaccine.csv", "Pop65up.csv", "MeaslesVacc.csv", "LegalRights.csv", "PM25.csv", "PopDensity.csv", "Cellphones.csv", "AirDepartures.csv", "InboundTourism.csv", "InfantMort.csv", "LifeExp.csv", "ThreatPlant.csv", "ThreatMammal.csv", "ThreatFish.csv")

var_name <- c("RuralPop", "AdoFertRate", "CO2", "Exports", "FertRate", "GNI", "Imports", "Literacy", "PopGrowth", "PopTotal", "UrbanPop", "Pop14Below", "Diabetes", "DPTVaccine", "Pop65Above", "MeaslesVacc", "LegalRights", "PM25", "PopDensity", "Cellphones", "AirTravel", "TourismIn", "InfantMort", "LifeExp", "ThreatPlant", "ThreatMammal", "ThreatFish")
for (i in 1:length(variables))
{
  wbtemp <- read.csv(variables[i], as.is = TRUE)
  wbtemp <- wbtemp[, c(3, 5)]
  names(wbtemp)[2] <- var_name[i]
  WB <- merge(WB, wbtemp, by = "Country.Name")
  WB <- WB[!(WB$Country.Name == ""), ]
}
names(WB)[1] <- "Country"
setwd("..")

# https://ourworldindata.org/coronavirus-source-data
covidcases <- read.csv("data/CoronaCases422.csv", as.is = TRUE)
dim(covidcases)
covidcases <- covidcases[114, ]
# HELP - https://rpubs.com/Mentors_Ubiqum/Transpose_Dataframe
rownames(covidcases) <- covidcases$date
covidcases$date <- NULL
covidcases$World <- NULL
covidcases <- as.data.frame(t(as.matrix(covidcases)))
covidcases$Country <- rownames(covidcases)
covidcases$CovidCases <- covidcases$`2020-04-22`
covidcases$`2020-04-22` <- NULL
rownames(covidcases) <- NULL
covidcases
# https://ourworldindata.org/coronavirus-source-data
coviddeaths <- read.csv("data/CoronaDeaths422.csv", as.is = TRUE)
dim(coviddeaths)
coviddeaths <- coviddeaths[114, ]
rownames(coviddeaths) <- coviddeaths$date
coviddeaths$date <- NULL
coviddeaths$World <- NULL
coviddeaths <- as.data.frame(t(as.matrix(coviddeaths)))
coviddeaths$Country <- rownames(coviddeaths)
coviddeaths$CovidDeaths <- coviddeaths$`2020-04-22`
coviddeaths$`2020-04-22` <- NULL
rownames(coviddeaths) <- NULL
coviddeaths

?merge
covid <- merge(covidcases, coviddeaths, by = "Country", all = TRUE)
covid$Country <- gsub("[[:punct:]]", " ", covid$Country)
WB <- merge(WB, covid, by = "Country", all = TRUE)
del <- which(!is.na(WB$CovidDeaths) & is.na(WB$AgriLand))
WB <- WB[-del, ]

# # Scraping
# url <- "https://www.cia.gov/library/publications/the-world-factbook/fields/299.html"
# browseURL(url)
# webpage <- read_html(url)
# countryGovtHTML <- html_text(html_nodes(webpage, '.text , .country'))
# Country <- c(NA)
# for (i in 1:236) {
#   Country[i] <- countryGovtHTML[i*2]
# }
# Country <- gsub("\n", "", Country)
# Country <- gsub("  ", "", Country)
# head(Country)
# 
# # Cleaning and Grouping
# govt <- c(NA)
# for (i in 1:236) {
#   govt[i] <- countryGovtHTML[(i*2)+1]
# }
# govt <- gsub("\n", "", govt)
# govt <- gsub("                  ", "", govt)
# govt <- gsub("              ", "", govt)
# govt <- gsub(".*(monarchy).*", "\\1", govt)
# govt <- gsub("(parliamentary democracy).*", "\\1", govt)
# govt <- gsub("(communist).*", "\\1", govt)
# govt <- gsub(".*(presidential republic).*", "\\1", govt)
# govt <- gsub("presidential Islamic republic", "presidential republic", govt)
# govt <- gsub("federal ", "", govt)
# govt <- gsub("semi-presidential republic", "presidential republic", govt)
# govt <- gsub("unitary parliamentary republic", "parliamentary republic", govt)
# govt[which(nchar(govt) > 1000)] <- "other"
# tab <- sort(table(govt), decreasing = TRUE)
# groups <- names(tab[1:4])
# vals <- which(govt == groups[1] | govt == groups[2] | govt == groups[3] | govt == groups[4])
# govt[!(c(1:length(govt)) %in% vals)] <- "other"
# countryGovt <- data.frame(Country, govt)
# 
# # Merge
# WB <- merge(WB, countryGovt, by = "Country", all = TRUE)
# del <- which(!is.na(WB$govt) & is.na(WB$AgriLand))
# WB <- WB[-del, ]

# # Scraping, Cleaning, and Grouping
# url <- "https://www.cia.gov/library/publications/the-world-factbook/fields/284.html"
# browseURL(url)
# web <- read_html(url)
# Country <- as.character(html_text(html_nodes(web,'tr > :nth-child(1)')))
# Country <- gsub("\n          ","",Country)
# Country <- gsub("\n      ","",Country)
# length(Country)
# Climate <- as.character(html_text(html_nodes(web,'.text , th+ th')))
# Climate <- gsub("\n          \n        ","",Climate) 
# Climate <- gsub("\n        \n      ","",Climate)
# Climate <- gsub(";.*","",Climate)
# Climate <- Climate[-(267:270)] #multiple entries for world
# Climate <- Climate[-1]
# Climate[grepl("temperate", Climate)] <- "temperate"
# Climate[grepl("arid", Climate)] <- "arid"
# Climate[grepl("tropical", Climate)] <- "tropical"
# Climate[grepl("desert", Climate)] <- "desert"
# Climate[grepl("Mediterranean", Climate)] <- "mediterranean"
# Climate[grepl("arctic", Climate)] <- "arctic"
# Climate[!Climate %in% c("tropical","arid","temperate","desert","mediterranean","arctic","Climate")] <- "other"
# Climate[265] <- "other" #entry for world
# df <- data.frame(Country,Climate)
# df <- df[-1,]
# 
# # Merge
# WB <- merge(WB, df, by = "Country", all = TRUE)
# del <- which(!is.na(WB$Climate) & is.na(WB$AgriLand))
# WB <- WB[-del, ]
# 
# # More Cleaning
# WB$Climate[WB$Climate == "arctic"] <- "other"

# DATA CLEANING
for (i in 2:ncol(WB))
{
  temp <- WB[, i]
  temp[temp == ".."] <- NA
  WB[, i] <- temp
}
WB$LegalRights <- round(as.integer(WB$LegalRights), 0)
WB$LegalRights <- recode(WB$LegalRights, "c('0', '1', '2', '3') = 'weak'; c('4', '5', '6') = 'relatively weak'; c('7', '8', '9') = 'relatively strong'; c('10', '11', '12') = 'strong'")
WB$AgriLand <- round(as.numeric(WB$AgriLand), 3)
WB$RuralPop <- round(as.numeric(WB$RuralPop), 3)
WB$AdoFertRate <- round(as.numeric(WB$AdoFertRate), 3)
WB$CO2 <- round(as.numeric(WB$CO2), 3)
WB$Exports <- round(as.numeric(WB$Exports), 3)
WB$FertRate <- round(as.numeric(WB$FertRate), 3)
WB$GNI <- round(as.numeric(WB$GNI), 3)
WB$Imports <- round(as.numeric(WB$Imports), 3)
WB$Literacy <- round(as.numeric(WB$Literacy), 3)
WB$PopGrowth <- round(as.numeric(WB$PopGrowth), 3)
WB$PopTotal <- as.integer(WB$PopTotal)
WB$UrbanPop <- round(as.numeric(WB$UrbanPop), 3)
WB$Pop14Below <- round(as.numeric(WB$Pop14Below), 3)
WB$Diabetes <- round(as.numeric(WB$Diabetes), 3)
WB$DPTVaccine <- round(as.numeric(WB$DPTVaccine), 3)
WB$Pop65Above <- round(as.numeric(WB$Pop65Above), 3)
WB$MeaslesVacc <- round(as.numeric(WB$MeaslesVacc), 3)
WB$PM25 <- round(as.numeric(WB$PM25), 3)
WB$PopDensity <- round(as.numeric(WB$PopDensity), 3)
WB$Cellphones <- round(as.numeric(WB$Cellphones), 3)
WB$AirTravel <- round(as.numeric(WB$AirTravel), 3)
WB$TourismIn <- round(as.numeric(WB$TourismIn), 3)
WB$InfantMort <- round(as.numeric(WB$InfantMort), 3)
WB$LifeExp <- round(as.numeric(WB$LifeExp), 3)
WB$ThreatPlant <- as.integer(WB$ThreatPlant)
WB$ThreatMammal <- as.integer(WB$ThreatMammal)
WB$ThreatFish <- as.integer(WB$ThreatFish)
# names(WB)[32] <- "Govt"
# WB$Govt <- as.character(WB$Govt)
# WB$Climate <- as.character(WB$Climate)
