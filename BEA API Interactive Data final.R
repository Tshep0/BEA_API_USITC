# User sets folder where files are written

#setwd("/Users/Tshepo1/Desktop/R")
setwd("Z:/BEA API R/data/Interactive data by industry")

#user sets file name

file_name3 <- "Missing_Data_Country_Interactive.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   c("EU","Canada","Mexico","Japan","China", "UnitedKingdom", "Germany", "Ireland", "Switzerland")
countries2 <- as.vector(unlist(strsplit(countries,",")),mode="list")
affiliation <- "AllAffiliations"
years <- 2011:2016
Years_for_CAGR <- 2011:2015
dataset <- "IntlServTrade"
trade_direction <- "Exports,Imports"

indicators <- c("AccountAuditBookkeep",
                "Advertising",
                "AllServiceTypes",
                "ArchAndEng",
                "ArchAndEngAbroadUs",
                "ArchAndEngExpend",
                "ArchAndEngUsAbroad",
                "BusMgmtConsPubRel",
                "ChargesForTheUseOfIpNie",
                "CipAudVisRelated",
                "CipBooksSoundRecord",
                "CipBroadcastLiveRecord",
                "CipCompSoft",
                "CipCompSoftIct",
                "CipFranchiseFees",
                "CipIndProcess",
                "CipMoviesTv",
                "CipOth",
                "CipTrademarks",
                "Comp",
                "Const",
                "ConstAbroadUs",
                "ConstExpend",
                "ConstFgnExpend",
                "Financial",
                "FinCredCardOthCredRelated",
                "FinFinAdv",
                "FinFinMan",
                "FinFinManFinAdvCust",
                "FinOth",
                "FinSecBrok",
                "FinSecBrokUwRelated",
                "FinSecLendEftOth",
                "FinUwRelated",
                "GovtGoodsAndServicesNie",
                "IctServ",
                "IndEng",
                "Info",
                "InsDirectPremiumsPaid",
                "InsDirectPremiumsReceived",
                "InsLossesPaid",
                "InsLossesPaidDirect",
                "InsLossesPaidRe",
                "InsLossesRecovered",
                "InsLossesRecoveredDirect",
                "InsLossesRecoveredRe",
                "InsPremiumsPaid",
                "InsPremiumsReceived",
                "InsRePremiumsPaid",
                "InsRePremiumsReceived",
                "Insurance",
                "InsuranceAuxIns",
                "InsuranceDirect",
                "InsuranceDirectAuxIns",
                "InsurancePremSupp",
                "InsuranceReIns",
                "InsuranceRiskPool",
                "Legal",
                "MaintenanceAndRepairNie",
                "Mining",
                "MiningAbroadUs",
                "MiningExpend",
                "OperatingLeasing",
                "OthBusinessNie",
                "OthBusinessNieCtry",
                "OtherBusiness",
                "PotIctEnServ",
                "PotIctEnServOthBusServ",
                "PotIctEnServOthTechTrdOthBus",
                "PotIctEnServTechTrdOthBus",
                "ProfMgmtConsult",
                "ResearchAndDev",
                "SportsPerformArts",
                "TechTradeRelatedOth",
                "Telecom",
                "TelecomCompAndInfo",
                "TradeRelated",
                "Training",
                "Transport",
                "TransportAir",
                "TransportAirFreight",
                "TransportAirPass",
                "TransportAirPort",
                "TransportOth",
                "TransportPostal",
                "TransportRoadAndOth",
                "TransportSea",
                "TransportSeaFreight",
                "TransportSeaPort",
                "Travel",
                "TravelBusiness",
                "TravelBusinessOth",
                "TravelBusinessPersonalOth",
                "TravelEducation",
                "TravelHealth",
                "TravelPersonal",
                "TravelPersonalOth",
                "TravelShortTermWork")

#Loads neccessary libraries

library(jsonlite)

library(dplyr)

library(reshape2)

library(readr)

#Concatenates years

if (years == "X") {
  year_list <- "X"
} else {
  year_list <- paste(years, collapse = ",")
}

if (countries == "X") {
  country_list <- "X"
} else {
  country_list <- paste(countries, collapse = ",")
}

#URL for calling BEA API - add your own BEA API User ID

url1 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=YOUR_BEA_API_USER_ID",
  
  "&method=GetData&DataSetName=", dataset,
  
  "&TypeOfService=")


url2 <- paste0(
  
  "&TradeDirection=", trade_direction,
  
  "&Affiliation=", affiliation,
  
  "&AreaOrCountry=", country_list,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
  
)

#creates a table to store results

final.result1 <- data.frame()

#Loop that calls API for each sector and creates a table of data


for (i in 1:length(indicators)) {
  this.indicator <- indicators[[i]]
  this.call <- paste0(url1, this.indicator, url2)	
  this.raw.result <- readLines(this.call,encoding="UTF-8", warn=FALSE)
  this.raw.result1 <- fromJSON(this.raw.result)
  
  this.result2 <- this.raw.result1$BEAAPI$Results$Data
  
  final.result1 <- rbind.data.frame(final.result1, this.result2)
}

#Formats table

end_date <- paste0("",(Years_for_CAGR[length(Years_for_CAGR)]),"",sep="")
beginning_date <- paste0("",Years_for_CAGR[1],"",sep="")
final_year <- paste0("",(years[length(years)]),"",sep="")

CAGR_name <- paste0("CAGR ", beginning_date, " to ", end_date)
Year_to_year_name <- paste0("Growth from ", end_date, " to ", final_year)

Combined_table1 <- select(final.result1, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue)
Combined_table2  <- split(Combined_table1, Combined_table1$AreaOrCountry)  
Combined_table3 <- lapply(seq_along(Combined_table2), function(x) as.data.frame(Combined_table2[[x]])[, 1:6]) 
names(Combined_table3) <- countries
list2env(Combined_table3, envir = .GlobalEnv)



function.format.aggregate.calculate <- function(df,TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue) {
  df <- dcast(df, TypeOfService + TradeDirection + Affiliation + AreaOrCountry ~ TimePeriod, value.var = "DataValue", fill=0)
  df$Unique_ID <- do.call(paste, c(df[c("TypeOfService", "TradeDirection", "Affiliation")], sep = "."))
  df <- as.data.frame.matrix(df, row.names=df$Unique_ID)
  df[5: (4 + length(years))] = lapply(df[5:(4 + length(years))], function(x) gsub("[^0-9\\.]", "", x))
  df[5:(4 + length(years))] <- lapply(df[5:(4 + length(years))], as.numeric)
  df <- as.data.frame.matrix(df, col.names=TRUE)
  df <- df[,-(5 + length(years))]
  df[is.na(df)] <- 0
  Distribution_Exports <- df["TransportSea.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportOth.Exports.AllAffiliations", 5:(4 + length(years))] #+
    #df["TradeRelated.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Distribution_Exports) <- "Distribution Services Exports"
  
  Electronic_Exports <- df["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["Telecom.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Comp.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Info.Exports.AllAffiliations", 5:(4 + length(years))] 
  row.names(Electronic_Exports) <- "Electronic Services Exports"
  
  Finance_Exports <- df["Financial.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["Insurance.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Finance_Exports) <- "Financial Services Exports"
  
  Travel_Exports <- df["Travel.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirPass.Exports.AllAffiliations", 5:(4 + length(years))]
   row.names(Travel_Exports) <- "Travel Services Exports"
  
  ChargesForTheUseOfIpNie_Exports <- df["CipIndProcess.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipTrademarks.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipFranchiseFees.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["CipOth.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(ChargesForTheUseOfIpNie_Exports) <- "Charges for the use of Intellectual Property Exports"
  
  Professional_Exports <- df["BusMgmtConsPubRel.Exports.AllAffiliations", 5:(4 + length(years))] + 
    df["ArchAndEng.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["IndEng.Exports.AllAffiliations", 5:(4 + length(years))] +
    #df["Training.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["ResearchAndDev.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["MaintenanceAndRepairNie.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["AccountAuditBookkeep.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Legal.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["Advertising.Exports.AllAffiliations", 5:(4 + length(years))]
   row.names(Professional_Exports) <- "Professional Services Exports"
  
  Other_Exports <- df["Const.Exports.AllAffiliations", 5:(4 + length(years))] + 
    #df["Mining.Exports.AllAffiliations", 5:(4 + length(years))] +
    #df["SportsPerformArts.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))] +
    df["OthBusinessNieCtry.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(Other_Exports) <- "Other Services Exports"
  
  TotalPrivate_Exports <- df["AllServiceTypes.Exports.AllAffiliations", 5:(4 + length(years))] - 
    df["GovtGoodsAndServicesNie.Exports.AllAffiliations", 5:(4 + length(years))]
  row.names(TotalPrivate_Exports) <- "Total Private Services Exports"
  
  Distribution_Imports <- df["TransportSea.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirFreight.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportAirPort.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["TransportOth.Imports.AllAffiliations", 5:(4 + length(years))] #+
    #df["TradeRelated.Exports.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Distribution_Imports) <- "Distribution Services Imports"
  
  Electronic_Imports <- df["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["Telecom.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Comp.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Info.Imports.AllAffiliations", 5:(4 + length(years))] 
  row.names(Electronic_Imports) <- "Electronic Services Imports"
  
  Finance_Imports <- df["Financial.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["Insurance.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Finance_Imports) <- "Financial Services Imports"
  
  Travel_Imports <- df["Travel.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["TransportAirPass.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Travel_Imports) <- "Travel Services Imports"
  
  ChargesForTheUseOfIpNie_Imports <- df["CipIndProcess.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipTrademarks.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipFranchiseFees.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["CipOth.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(ChargesForTheUseOfIpNie_Imports) <- "Charges for the use of Intellectual Property Imports"
  
  Professional_Imports <- df["BusMgmtConsPubRel.Imports.AllAffiliations", 5:(4 + length(years))] + 
    df["ArchAndEng.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["IndEng.Imports.AllAffiliations", 5:(4 + length(years))] +
    #df["Training.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["ResearchAndDev.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["MaintenanceAndRepairNie.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["AccountAuditBookkeep.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Legal.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["Advertising.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Professional_Imports) <- "Professional Services Imports"
  
  Other_Imports <- df["Const.Imports.AllAffiliations", 5:(4 + length(years))] + 
    #df["Mining.Imports.AllAffiliations", 5:(4 + length(years))] +
    #df["SportsPerformArts.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))] +
    df["OthBusinessNieCtry.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(Other_Imports) <- "Other Services Imports"
  
  TotalPrivate_Imports <- df["AllServiceTypes.Imports.AllAffiliations", 5:(4 + length(years))] - 
    df["GovtGoodsAndServicesNie.Imports.AllAffiliations", 5:(4 + length(years))]
  
  row.names(TotalPrivate_Imports) <- "Total Private Services Imports"
  
  df <- rbind(Distribution_Exports,Electronic_Exports,Finance_Exports,Professional_Exports,ChargesForTheUseOfIpNie_Exports,Travel_Exports,Other_Exports,TotalPrivate_Exports,
              Distribution_Imports,Electronic_Imports,Finance_Imports,Professional_Imports,ChargesForTheUseOfIpNie_Imports,Travel_Imports,Other_Imports,TotalPrivate_Imports)
  
  # Performs aggregations and calculations
  function.cagr <- function(df,col_name1,end_date,start_date,periods) {
    df[[col_name1]] <- ((df[[end_date]]/df[[start_date]])^(1/(length(periods)-1)))-1
    df
  }
  
  function.growth <- function(df,col_name2,final_year,end_date) {
    df[[col_name2]] <- (df[[final_year]]-df[[end_date]])/df[[end_date]]
    df
  }
  
  
  CAGR <- function.cagr(df, CAGR_name, end_date,beginning_date,Years_for_CAGR)
  Year_on_Year <- function.growth(df, Year_to_year_name, final_year, end_date)

  
  df <- cbind(CAGR, Year_on_Year)
  df <- df %>% subset(., select=which(!duplicated(names(.)))) 
}

final.countries <- data.frame()

for (i in 1:length(countries)) {
  arg1 <- "TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue"
  arg2 <- "function.format.aggregate.calculate("
  arg3 <- paste0(arg2, countries[[i]], ", ",arg1,")", sep="")
  final.countries1 <- eval(parse(text=arg3))
  final.countries2 <- cbind(countries[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)

}

final.countries[,length(years)+2] <- final.countries[,length(years)+2]*100
final.countries[,length(years)+3]<- final.countries[,length(years)+3]*100
round(final.countries[,length(years)+2], 2)
round(final.countries[,length(years)+3], 2)
names(final.countries)[names(final.countries)=="countries[[i]]"] <- "Countries"


#use levels of final.countries$Countries to name the split files
country.names <- levels(final.countries$Countries)
final.countries2  <- split(final.countries, final.countries$Countries)
names(final.countries2) <- country.names
list2env(final.countries2, envir = .GlobalEnv)
#rm(final.countries1, final.countries2, final.result1, Combined_table1)
na_test5 <- Combined_table1[rowSums(is.na(Combined_table1)) > 0,]
na_test5$Identifier <- rownames(na_test5)

BEA_lookup_table <- read_csv("Z:/BEA API R/BEA_lookup_table.csv")

Missing_data <- merge(na_test5, BEA_lookup_table, by.x = "Identifier", by.y = "Sector")

sapply(names(final.countries2), 
       function (x) write.csv(final.countries2[[x]], file=paste(x, "csv", sep=".")))
write.csv(Missing_data, file = file_name3, row.names = TRUE)

