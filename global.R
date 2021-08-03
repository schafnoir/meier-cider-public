####  Meier Cider Public version 1.0 ####

### Meier Cider Shiny application for monitoring fermentation progress and mapping trees
#   Install airtableR from Git: devtools::install_github("bergant/airtabler")
#   Load required libraries
library(plyr)
library(dplyr)
library(airtabler)
library(leaflet)
library(ggplot2)
library(glue)
library(plotly)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)



### Define functions #### --> Converts NULL list elements from Airtable retrieval to NA
nullToNA <- function(x){
  x[is.null(x)] <- NA
  return(x)
}



### Acquire Meier Cider data from Airtable  ##############################################################################
#   Set Meier Cider Airtable API Key into system environment
Sys.setenv(AIRTABLE_API_KEY = "")

#   Define Airtable tables
meierCider <- airtable(
  base = "",
  tables = c("trees", "juice", "blends", "batches", "fermentation")
  )



##########################################################################################################################
### Prepare 'juice' data for 'summary' tab ###############################################################################

### Obtain 'juice' data and standardize dataframe structure
#   Get 'juice' data from Airtable
juice <- meierCider$juice$select_all()
juice <- juice %>% 
  select(id, juiceID, treeID, treeName, treePercent, harvestDate, pressingDate, sweatDuration, SG, sgTemp, sgMethod,
         hardTanninRank, softTanninRank, tasteAcid, notRipe, juiceTA, pH, pressed, fruitYield) %>%
  rename(atJuiceID = id, atTreeID = treeID)

#   Assign correct dataTypes for date columns
juice$harvestDate <- as.Date(juice$harvestDate)
juice$pressingDate <- as.Date(juice$pressingDate)

#   Create 'year' column, replace 'NA' in 'fruitYield' column, create 'blendIDs' column
juice <- juice %>% mutate(year = as.numeric(format(harvestDate, "%Y"))) %>%
  mutate(fruitYield = ifelse(is.na(fruitYield), 0, fruitYield))
  



##########################################################################################################################
### Prepare 'blends' data for 'fermentation tab ##########################################################################

### Obtain 'blends' data
blends <- meierCider$blends$select_all()
blends <- blends %>% 
  dplyr::select(id, blendID, year, galTotalVol, juiceIDs, juicePercent) %>%
  dplyr::rename(atBlendID = id) %>%
  dplyr::mutate(galTotalVol = round(galTotalVol, digits = 1))



##########################################################################################################################
### Prepare 'batches' data for 'summary' and 'fermentation' tabs #########################################################

### Obtain 'batches' data and standardize dataframe structure
#   Get batches data
batches <- meierCider$batches$select_all()
batches <- batches %>% select(batchID, batchName, initialBatchVol_L, blendID, blendName, startDate, initialSG,
                              pH, initialTA, bottleTA, hardTannin, softTannin, targetStyle, strainYeast, remarks)

#   Remove list structure from columns with lists
for(j in 1:length(colnames(batches))){
  if(is.list(batches[j])){
    name <- colnames(batches[j])
    temp <- plyr::ldply(.data = batches[,name], .fun = nullToNA)
    batches[j] <- temp
  }
}
rm(j,name,temp)

#   Assign 'batches$startDate' dataType
batches$startDate <- as.Date(batches$startDate)

#   Reduce 'initialTA' and 'bottleTA' to 2 decimals
batches$initialTA <- round(batches$initialTA, digits = 2)
batches$bottleTA <- round(batches$bottleTA, digits = 2)

#   Create 'year' column
batches$year <- as.numeric(format(batches$startDate, "%Y"))

#   Arrange 'batches' columns
batches <- batches[c("year", "batchID", "batchName", "initialBatchVol_L", "blendID", "blendName", "startDate", "initialSG",
                     "pH", "initialTA", "bottleTA", "hardTannin", "softTannin", "targetStyle", "strainYeast", "remarks")]




##########################################################################################################################
### Prepare 'fermentation' data for 'fermentation' tab ###################################################################

### Obtain 'fermentation' data and standardize dataframe structure
#   Get fermentation data from Airtable
ferment <- meierCider$fermentation$select_all()
ferment <- ferment %>% select(fermentID, batchName, date, temp, action, SG, volume_gal, sulfites, remarks)

#   Remove list structure of dataframe columns
for(j in 1:length(colnames(ferment))){
  if(is.list(ferment[j])){
    name <- colnames(ferment[j])
    temp <- plyr::ldply(.data = ferment[,name], .fun = nullToNA)
    ferment[j] <- temp
  }
}
rm(j,name,temp)

#   Assign dataType and round numerics
ferment$date <- as.Date(ferment$date)
ferment$SG <- round(ferment$SG, digits = 4)

#   Create human readable blendID, batchID, and replace existing batchID; '\\' required since '.' is a special character
batchDF <- plyr::ldply(stringr::str_split(ferment$fermentID, "\\."))
batchDF <- batchDF %>%
  mutate(batchID = paste(V1, V2, V3, V4, sep = ".")) %>%
  mutate(blendID = paste(V2, V3, sep = ".")) %>%
  mutate(year = stringr::str_extract(V2, "20[0-9]{2}"))

#   Select desired columns, calculate volume_L, account for sulfites = NA, sort by new batchID and date
ferment <- ferment %>%
  mutate(batchID = batchDF$batchID) %>%
  mutate(blendID = batchDF$blendID) %>%
  mutate(year = batchDF$year) %>%
  mutate(volume_L = round(volume_gal*3.78541, digits=1)) %>%
  mutate(sulfites = ifelse(is.na(sulfites), 0, sulfites)) %>%
  arrange(blendID, batchID, date)

#   Assign 'year' dataType
ferment$year <- as.integer(ferment$year)

#   Arrange columns
ferment <- ferment[c("fermentID", "year", "blendID", "batchID", "batchName", "date", "temp", "action",
                     "SG", "volume_gal", "volume_L", "sulfites", "remarks")]



### Identify available years for 'year' drop-down rendered in server.R
theYears <- ferment %>% select(year) %>% distinct() %>% arrange(desc(year))




##########################################################################################################################
### Prepare data for 'sulfites' tab ######################################################################################

##  Read in Lea dataset
pHLea <- read.csv("data/sulfite_pH_curve.csv", header = TRUE, stringsAsFactors = FALSE)


##  Create dataset: Observed pH, predicted SO2, g KMG/L
#   Input pH values
pHSeq <- seq(from=3, to=3.8, by=0.01)

#   Predicted SO2 values
l <- loess(SO2_ppm ~ pH, data = pHLea)
predSO2 <- predict(l, pHSeq)

#   Create dataframe
df <- data.frame(pHSeq, predSO2, stringsAsFactors = FALSE)

#   Calculate g KMS/L
df <- df %>% 
  mutate(KMSgPerL = (predSO2/1000)*2) %>%
  rename(SO2ppm = predSO2, pH = pHSeq)

df$SO2ppm <- round(df$SO2ppm, digits = 0)
df$KMSgPerL <- round(df$KMSgPerL, digits = 3)


##  Create pH and sulfite plotly object for output
#   Define plotly object
pHPlotly <- plot_ly(data = df, x = ~pH, height = 600) %>%
  add_lines(y = ~fitted(loess(SO2ppm ~ pH)),
            text = paste("KMS g/L: ", df$KMSgPerL)) %>%
  add_markers(data = pHLea, x = ~pH, y = ~SO2_ppm, showlegend = FALSE, marker=list(color='#F15A29')) %>%
  layout(
    xaxis = list(title = 'pH units'),
    yaxis = list(title = 'SO2 concentration (ppm)')
    
  )



##########################################################################################################################
### Prepare data for 'bottling' tab ######################################################################################

##  Read in 'NCMH Carbonation Table' and 'priming inputs' datasets ####
carbNCMH <- read.csv("data/ncmhCarbonationTable.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(carbNCMH) <- c("Carbonation Style", "Volumes CO2", "Dissolved CO2 (g L-1)", "Fermentable Sugar (g L-1)",
                        "SG drop", "Pressure at 25C (atm)", "Bottle Type")
cjCarbData <- read.csv("data/primingInputs.csv", header = TRUE, stringsAsFactors = FALSE)
cjCarbData$dropSG <- round(cjCarbData$dropSG, digits = 6)
cjCarbData$dissolvedCO2 <- round(cjCarbData$dissolvedCO2, digits = 2)

##  Create 'loess' models to predict 'Dissolved CO2', 'SG drop', 'DAPppm', and 'Fermentable Sugar'
dCO2_loess <- loess(dissolvedCO2 ~ primingSugar, data = cjCarbData)
sg_loess <- loess(dropSG ~ primingSugar, data = cjCarbData)
dap_loess <- loess(DAPppm ~ primingSugar, data = cjCarbData)
sugDrop_loess <- loess(primingSugar ~ dropSG, data = cjCarbData)
dapDrop_loess <- loess(DAPppm ~ dropSG, data = cjCarbData)

##  Create carbonation plotly object for output
#   Define plotly object
carbPlotly <- plot_ly(data = cjCarbData, x = ~primingSugar, height = 600) %>%
  add_lines(y = ~fitted(loess(dissolvedCO2 ~ primingSugar)),
            text = paste("SG Drop: ", cjCarbData$dropSG)) %>%
  layout(
    xaxis = list(title = 'Fermentable Sugar (g L-1)'),
    yaxis = list(title = 'Dissolved CO2 (g L-1)')
  )




##########################################################################################################################
### Prepare data for 'Trees Map' tab #####################################################################################


### Obtain 'trees' data and standardize dataframe structure ####
#   Get 'trees' data from Airtable, select columns, keep trees with remove==FALSE
trees <- meierCider$trees$select_all()
trees <- trees %>% 
  select(id, latitude, longitude, locationDescription, locationAccess, statusAccess, treeName, treeSize,
         fruitSet2019, fruitSet2020, fruitSet2021, treeType, treeID, address, remarks, ripeningMonths,
         fruitMeanDiameter, fruitDescription, flavorDescription, juice, generalProductivity, remove, diseasesAndPests,
         croppingHabit, phone) %>%
  mutate(remove = ifelse(is.na(remove), FALSE, remove)) %>%
  filter(remove==FALSE) %>%
  rename(atTreeID = id)

#   Create randomized latitude and longitude with offset
trees$latitude <- sample(trees$latitude + 0.08)
trees$longitude <- sample(trees$longitude + 0.09)



### Join with 'juice' table to obtain SG and TA values from ripe fruit: Calculate average SG,TA ####
### if > 1 ripe datum (e.g., multiple years or multiple measurements in a year). Will group_by(treeName)
#   Create 'simpleJ' simplified juice table with fields relevant to map, remove blends and notRipe==TRUE
simpleJ <- juice %>% 
  select(atJuiceID, juiceID, atTreeID, SG, juiceTA, notRipe,
                            pressed, year, hardTanninRank, softTanninRank) %>%
  mutate(notRipe = ifelse(is.na(notRipe), FALSE, notRipe)) %>%
  filter(notRipe==FALSE, lengths(atTreeID)==1)

simpleJ$atTreeID <- unlist(simpleJ$atTreeID)
simpleJ$treeName <- unlist(simpleJ$treeName)

#   Summarise to calculate mean SG, TA, hT, sT
simpleJ <- simpleJ %>% dplyr::group_by(atTreeID) %>%
  filter(!is.na(SG)) %>%
  summarise(meanSG = round(mean(SG, na.rm = TRUE), digits = 4), meanTA = mean(juiceTA, na.rm = TRUE), 
            meanHT = round(mean(hardTanninRank, na.rm = TRUE), digits = 2), meanST = mean(softTanninRank, na.rm = TRUE))

#   Remove 'NaN' values for 'meanTA' and replace with 'NA'
simpleJ[simpleJ$meanTA == "NaN", ][,"meanTA"] <- NA



### Join 'summaryJ' table with 'trees' table for subsequent mapping and create map labels ####
trees <- left_join(trees, simpleJ, by = "atTreeID")



### Identify 'Trees Map' tab drop-down elements ####
#   Drop-down elements: treeName
treeIDs <- sort(unique(trees$treeName))

# Drop-down elements: treeType
types <- sort(unique(trees$treeType))

# Drop-down elements: locationAccess
access <- sort(unique(trees$locationAccess))

# Drop-down elements: statusAccess
status <- sort(unique(trees$statusAccess))

# Drop-down elements: fruitSetYear
yearCols <- stringr::str_extract(names(trees), "20[0-9]{2}")
fruitSetYears <- NULL
for (i in 1:length(yearCols)){
  if(!is.na(yearCols[i])){
    year <- names(trees)[i]
    fruitSetYears <- c(fruitSetYears, year)
  }
}
fruitSetYears <- sort(fruitSetYears, decreasing = TRUE)

# Drop-down elements: fruitYearStatus
fruitYearStatus <- c("notAssessed", "0 - noFruitSet", "1 - poorFruitSet", "2 - moderateFruitSet", "3 - goodFruitSet",
                     "4 - heavyFruitSet")



### For fruitSetYear columns set 'NA' values to 'notAssessed' for pickerInput filtering ####
for (i in 1:length(fruitSetYears)){
  trees <- trees %>%
    mutate(new = ifelse(is.na(!!as.symbol(fruitSetYears[i])), "notAssessed", !!as.symbol(fruitSetYears[i]))) %>%
    select(-!!as.symbol(fruitSetYears[i]))
  names(trees)[names(trees)=="new"] <- fruitSetYears[i]
}