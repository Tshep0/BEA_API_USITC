# User sets folder where files are written

setwd("/Users/Tshepo1/Desktop/R")

#user sets file name

file_name1 <- "test_1.csv"
file_name2 <- "test_2.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   "AllCountries"
affiliation <- "AllAffiliations"
years <- 2010:2015
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



#Concatenates years

if (years == "X") {
  year_list <- "X"
} else {
  year_list <- paste(years, collapse = ",")
}


#URL for calling BEA API

url1 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=YOUR_BEA_API_USER_ID",
  
  "&method=GetData&DataSetName=", dataset,
  
  "&TypeOfService=")


url2 <- paste0(
  
  "&TradeDirection=", trade_direction,
  
  "&Affiliation=", affiliation,
  
  "&AreaOrCountry=", countries,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
  
)

#creates a table to store results

final.result <- data.frame()

#Loop that calls API for each sector and creates a table of data


for (i in 1:length(indicators)) {
  this.indicator <- indicators[[i]]
  this.call <- paste0(url1, this.indicator, url2)	
  this.raw.result <- fromJSON(this.call)
  this.result <- this.raw.result$BEAAPI$Results$Data
  
  final.result <- rbind.data.frame(final.result, this.result)
}

#Formats table 

Combined_table1 <- select(final.result, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue)
Combined_table2 <- dcast(Combined_table1, TypeOfService + TradeDirection + AreaOrCountry +Affiliation ~ TimePeriod, value.var = "DataValue", fill=0)
Combined_table3 <- as.data.frame.matrix(Combined_table2, col.names=TRUE)
Combined_table3$Unique_ID <- do.call(paste, c(Combined_table3[c("TypeOfService", "TradeDirection", "AreaOrCountry", "Affiliation")], sep = "."))
Combined_table3 <- as.data.frame.matrix(Combined_table3, row.names=Combined_table3$Unique_ID)
Combined_table3$Unique_ID <- NULL
Combined_table3[5:(4 + length(years))] <- lapply(Combined_table3[5:(4 + length(years))], as.numeric)


#Industry aggregate category (fix countries issue)
Distribution_Exports <- Combined_table3["TransportSea.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirFreight.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportAirPort.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportOth.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TradeRelated.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Distribution_Exports) <- "Distribution Services Exports"

Electronic_Exports <- Combined_table3["CipAudVisRelated.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Telecom.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Comp.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Info.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] 

row.names(Electronic_Exports) <- "Electronic Services Exports"

Finance_Exports <- Combined_table3["Financial.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Insurance.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Finance_Exports) <- "Financial Services Exports"

Travel_Exports <- Combined_table3["Travel.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirPass.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Travel_Exports) <- "Travel Services Exports"

ChargesForTheUseOfIpNie_Exports <- Combined_table3["ChargesForTheUseOfIpNie.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(ChargesForTheUseOfIpNie_Exports) <- "Charges for the use of Intellectual Property Exports"

Professional_Exports <- Combined_table3["ProfMgmtConsult.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["ArchAndEng.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["IndEng.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Training.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["ResearchAndDev.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["MaintenanceAndRepairNie.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] 

row.names(Professional_Exports) <- "Professional Services Exports"

Other_Exports <- Combined_table3["Const.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Mining.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["SportsPerformArts.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OperatingLeasing.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OthBusinessNie.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Other_Exports) <- "Other Services Exports"

TotalPrivate_Exports <- Combined_table3["AllServiceTypes.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))] - 
  Combined_table3["GovtGoodsAndServicesNie.Exports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(TotalPrivate_Exports) <- "Total Private Services Exports"

Aggregate_Exports <- rbind(TotalPrivate_Exports,Distribution_Exports,Electronic_Exports,Finance_Exports,Travel_Exports,ChargesForTheUseOfIpNie_Exports,Professional_Exports,
                           Other_Exports)

Aggregate_Exports <- cbind(TradeDirection="Exports",AreaOrCountry=countries, Affiliation="AllAffiliates",Aggregate_Exports)
Aggregate_Exports$TypeOfService <- NULL

Distribution_Imports <- Combined_table3["TransportSea.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirFreight.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportAirPort.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportOth.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TradeRelated.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Distribution_Imports) <- "Distribution Services Imports"

Electronic_Imports <- Combined_table3["CipAudVisRelated.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Telecom.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Comp.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Info.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] 

row.names(Electronic_Imports) <- "Electronic Services Imports"

Finance_Imports <- Combined_table3["Financial.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Insurance.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Finance_Imports) <- "Financial Services Imports"

Travel_Imports <- Combined_table3["Travel.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirPass.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Travel_Imports) <- "Travel Services Imports"

ChargesForTheUseOfIpNie_Imports <- Combined_table3["ChargesForTheUseOfIpNie.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(ChargesForTheUseOfIpNie_Imports) <- "Charges for the use of Intellectual Property Imports"

Professional_Imports <- Combined_table3["ProfMgmtConsult.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["ArchAndEng.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["IndEng.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Training.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["ResearchAndDev.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["MaintenanceAndRepairNie.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] 

row.names(Professional_Imports) <- "Professional Services Imports"

Other_Imports <- Combined_table3["Const.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Mining.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["SportsPerformArts.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OperatingLeasing.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OthBusinessNie.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(Other_Imports) <- "Other Services Imports"

TotalPrivate_Imports <- Combined_table3["AllServiceTypes.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))] - 
  Combined_table3["GovtGoodsAndServicesNie.Imports.AllCountries.AllAffiliations", 5:(4 + length(years))]

row.names(TotalPrivate_Imports) <- "Total Private Services Imports"

Aggregate_Imports <- rbind(TotalPrivate_Imports,Distribution_Imports,Electronic_Imports,Finance_Imports,Travel_Imports,ChargesForTheUseOfIpNie_Imports,Professional_Imports,
                           Other_Imports)

Aggregate_Imports <- cbind(TradeDirection="Imports",AreaOrCountry=countries, Affiliation="AllAffiliates",Aggregate_Imports)

Total_aggregate <- rbind(Aggregate_Exports,Aggregate_Imports)
Total_aggregate$TypeOfService <- NULL

# Performs aggregations and calculations
function.cagr <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Total_aggregate[[end_date]]/Total_aggregate[[start_date]])^(1/periods))-1
  df
}
  
function.growth <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Total_aggregate[[final_year]]-Total_aggregate[[end_date]])/Total_aggregate[[end_date]]
}

function.cagr2 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Combined_table3[[end_date]]/Combined_table3[[start_date]])^(1/periods))-1
  df
}

function.growth2 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Combined_table3[[final_year]]-Combined_table3[[end_date]])/Combined_table3[[end_date]]
}

end_date <- paste0("",(years[1]+(length(years)-2)),"",sep="")
beginning_date <- paste0("",years[1],"",sep="")
final_year <- paste0("",(years[1]+(length(years)-1)),"",sep="")

CAGR <- function.cagr(Total_aggregate, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year <- function.growth(Total_aggregate, "Year-on-Year Growth", final_year, end_date)

Total_aggregate <- cbind(CAGR, Year_on_Year)

CAGR2 <- function.cagr2(Combined_table3, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year2 <- function.growth2(Combined_table3, "Year-on-Year Growth", final_year, end_date)

All_values <- cbind(CAGR2,Year_on_Year2)


#exports final file to csv
write.csv(Total_aggregate, file = file_name1, row.names = TRUE)
write.csv(All_values, file = file_name2, row.names = FALSE)


