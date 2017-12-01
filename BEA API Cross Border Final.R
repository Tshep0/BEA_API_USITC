# User sets folder where files are written


setwd("Z:/BEA API R/data/Cross-Border data")
#setwd("~/Drive/R")

#user sets file name

file_name1 <- "Cross-border data Summary_Table.csv"
file_name2 <- "Cross-border detailed data.csv"
file_name3 <- "Missing_Data_Cross_Border.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   "AllCountries"
affiliation <- "AllAffiliations"
years <- 2008:2016
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


#URL for calling BEA API - add your own BEA API User ID

url1 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
  
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

final.result1 <- data.frame()

#Loop that calls API for each sector and creates a table of data


for (i in 1:length(indicators)) {
  this.indicator <- indicators[[i]]
  this.call <- paste0(url1, this.indicator, url2)	
  this.raw.result1 <- readLines(this.call,encoding="UTF-8", warn=FALSE)
  this.raw.result2 <- fromJSON(this.raw.result1)
  
  this.result <- this.raw.result2$BEAAPI$Results$Data
  
  final.result1 <- rbind.data.frame(final.result1, this.result)
}

#Formats table 

Combined_table1 <- select(final.result1, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue)
Combined_table2 <- dcast(Combined_table1, TypeOfService + TradeDirection + AreaOrCountry + Affiliation ~ TimePeriod, value.var = "DataValue", fill=0)
Combined_table3 <- as.data.frame.matrix(Combined_table2, col.names=TRUE)
Combined_table3$Unique_ID <- do.call(paste, c(Combined_table3[c("TypeOfService", "TradeDirection", "Affiliation")], sep = "."))
Combined_table3 <- as.data.frame.matrix(Combined_table3, row.names=Combined_table3$Unique_ID)
Combined_table3$Unique_ID <- NULL
Combined_table3[5:(4 + length(years))] = lapply(Combined_table3[5:(4 + length(years))], function(x) gsub("[^0-9\\.]", "", x))
Combined_table3[5:(4 + length(years))] <- lapply(Combined_table3[5:(4 + length(years))], as.numeric)
Combined_table3[is.na(Combined_table3)] <- 0


#Industry aggregate category
Distribution_Exports <- Combined_table3["TransportSea.Exports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportOth.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TradeRelated.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(Distribution_Exports) <- "Distribution Services Exports"

Electronic_Exports <- Combined_table3["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Telecom.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Comp.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Info.Exports.AllAffiliations", 5:(4 + length(years))] 

row.names(Electronic_Exports) <- "Electronic Services Exports"

Finance_Exports <- Combined_table3["Financial.Exports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Insurance.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(Finance_Exports) <- "Financial Services Exports"

Travel_Exports <- Combined_table3["Travel.Exports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirPass.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(Travel_Exports) <- "Travel Services Exports"

ChargesForTheUseOfIpNie_Exports <- Combined_table3["CipIndProcess.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipTrademarks.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipFranchiseFees.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipOth.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(ChargesForTheUseOfIpNie_Exports) <- "Charges for the use of Intellectual Property Exports"

Professional_Exports <- Combined_table3["BusMgmtConsPubRel.Exports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["ArchAndEng.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["IndEng.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Training.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["ResearchAndDev.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["MaintenanceAndRepairNie.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["AccountAuditBookkeep.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Legal.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Advertising.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(Professional_Exports) <- "Professional Services Exports"

Other_Exports <- Combined_table3["Const.Exports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Mining.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["SportsPerformArts.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OthBusinessNie.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(Other_Exports) <- "Other Services Exports"

TotalPrivate_Exports <- Combined_table3["AllServiceTypes.Exports.AllAffiliations", 5:(4 + length(years))] - 
  Combined_table3["GovtGoodsAndServicesNie.Exports.AllAffiliations", 5:(4 + length(years))]

row.names(TotalPrivate_Exports) <- "Total Private Services Exports"

Aggregate_Exports <- rbind(TotalPrivate_Exports,Distribution_Exports,Electronic_Exports,Finance_Exports,Travel_Exports,ChargesForTheUseOfIpNie_Exports,Professional_Exports,
                           Other_Exports)

Aggregate_Exports <- cbind(TradeDirection="Exports",AreaOrCountry=countries, Affiliation="AllAffiliates",Aggregate_Exports)
Aggregate_Exports$TypeOfService <- NULL

Distribution_Imports <- Combined_table3["TransportSea.Imports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirFreight.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportAirPort.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TransportOth.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["TradeRelated.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(Distribution_Imports) <- "Distribution Services Imports"

Electronic_Imports <- Combined_table3["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Telecom.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Comp.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Info.Imports.AllAffiliations", 5:(4 + length(years))] 

row.names(Electronic_Imports) <- "Electronic Services Imports"

Finance_Imports <- Combined_table3["Financial.Imports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Insurance.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(Finance_Imports) <- "Financial Services Imports"

Travel_Imports <- Combined_table3["Travel.Imports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["TransportAirPass.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(Travel_Imports) <- "Travel Services Imports"

ChargesForTheUseOfIpNie_Imports <- Combined_table3["CipIndProcess.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipTrademarks.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipFranchiseFees.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["CipOth.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(ChargesForTheUseOfIpNie_Imports) <- "Charges for the use of Intellectual Property Imports"

Professional_Imports <- Combined_table3["BusMgmtConsPubRel.Imports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["ArchAndEng.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["IndEng.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Training.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["ResearchAndDev.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["MaintenanceAndRepairNie.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["AccountAuditBookkeep.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Legal.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["Advertising.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(Professional_Imports) <- "Professional Services Imports"

Other_Imports <- Combined_table3["Const.Imports.AllAffiliations", 5:(4 + length(years))] + 
  Combined_table3["Mining.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["SportsPerformArts.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))] +
  Combined_table3["OthBusinessNie.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(Other_Imports) <- "Other Services Imports"

TotalPrivate_Imports <- Combined_table3["AllServiceTypes.Imports.AllAffiliations", 5:(4 + length(years))] - 
  Combined_table3["GovtGoodsAndServicesNie.Imports.AllAffiliations", 5:(4 + length(years))]

row.names(TotalPrivate_Imports) <- "Total Private Services Imports"

Aggregate_Imports <- rbind(TotalPrivate_Imports,Distribution_Imports,Electronic_Imports,Finance_Imports,Travel_Imports,ChargesForTheUseOfIpNie_Imports,Professional_Imports,
                           Other_Imports)

Aggregate_Imports <- cbind(TradeDirection="Imports",AreaOrCountry=countries, Affiliation="AllAffiliates",Aggregate_Imports)

Total_aggregate <- rbind(Aggregate_Exports,Aggregate_Imports)
Total_aggregate$TypeOfService <- NULL
Total_aggregate$TradeDirection <- NULL
Total_aggregate$Affiliation <- NULL
Total_aggregate$AreaOrCountry <- NULL


#Create industry groups

Distribution_Exports_sum <- rbind(Distribution_Exports, Combined_table3["TransportSea.Exports.AllAffiliations", 5:(4 + length(years))], 
                                    Combined_table3["TransportAirFreight.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportAirPort.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportOth.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TradeRelated.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Distribution_Exports_sum) <- c("Total Distribution Exports", "Sea Transport Exports", "Air Transport (freight) Exports", "Air Transport (port) Exports",
"Other modes of transport Exports", "Trade related services Exports")


Distribution_Imports_sum <- rbind(Distribution_Imports, Combined_table3["TransportSea.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportAirFreight.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportAirPort.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TransportOth.Imports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["TradeRelated.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Distribution_Imports_sum) <- c("Total Distribution Imports", "Sea Transport Import", "Air Transport (freight) Imports", "Air Transport (port) Imports",
                                         "Other modes of transport Imports", "Trade related services Imports")

Electronic_Exports_sum <- rbind(Electronic_Exports, Combined_table3["CipAudVisRelated.Exports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Telecom.Exports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Comp.Exports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Info.Exports.AllAffiliations", 5:(4 + length(years))] 
)

row.names(Electronic_Exports_sum) <- c("Total Electronic Exports", "Audio-visual and Related Products Exports", "Telecommunications Services Exports",
                                       "Computer Services Exports", "Information Services Exports")

Electronic_Imports_sum <- rbind(Electronic_Imports, Combined_table3["CipAudVisRelated.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Telecom.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Comp.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Info.Imports.AllAffiliations", 5:(4 + length(years))] 
)

row.names(Electronic_Imports_sum) <- c("Total Electronic Imports", "Audio-visual and Related Products Imports", "Telecommunications Services Imports",
                                        "Computer Services Imports", "Information Services Imports")

Finance_Exports_sum <- rbind(Finance_Exports, Combined_table3["FinSecBrokUwRelated.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["FinFinManFinAdvCust.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["FinCredCardOthCredRelated.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["FinSecLendEftOth.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["Insurance.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["InsuranceDirect.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["InsuranceReIns.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["InsuranceAuxIns.Exports.AllAffiliations", 5:(4 + length(years))])

row.names(Finance_Exports_sum) <- c("Total Financial Exports", "Securities brokerage, underwriting, and related services Exports",
                                    "Financial management, financial advisory, and custody services Exports",
                                    "Credit card and other credit-related services Exports",
                                    "Securities lending, electronic funds transfer, and other services Exports", 
                                    "Insurance Exports", "Direct Insurance Exports", "Reinsurance Exports", "Auxiliary Insurance Exports")

Finance_Imports_sum <- rbind(Finance_Imports,Combined_table3["FinSecBrokUwRelated.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["FinFinManFinAdvCust.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["FinCredCardOthCredRelated.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["FinSecLendEftOth.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["Insurance.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["InsuranceDirect.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["InsuranceReIns.Imports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["InsuranceAuxIns.Imports.AllAffiliations", 5:(4 + length(years))])


row.names(Finance_Imports_sum) <- c("Total Financial Imports", "Securities brokerage, underwriting, and related services Imports",
                                      "Financial management, financial advisory, and custody services Imports",
                                      "Credit card and other credit-related services Imports",
                                      "Securities lending, electronic funds transfer, and other services Imports", 
                                      "Insurance Imports", "Direct Insurance Imports", "Reinsurance Imports", "Auxiliary Insurance Imports")

Professional_Exports_sum <- rbind(Professional_Exports, Combined_table3["BusMgmtConsPubRel.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["ArchAndEng.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["IndEng.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["Training.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["ResearchAndDev.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["MaintenanceAndRepairNie.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["AccountAuditBookkeep.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["Legal.Exports.AllAffiliations", 5:(4 + length(years))],
                                    Combined_table3["Advertising.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Professional_Exports_sum) <- c("Total Professional Exports", "Business and Mangement Consulting Exports", "Architecture and Engineering Exports", 
                                          "Industrial Engineering Exports","Training Exports", "Research and Development Exports", 
                                         "Maintenance and Repair Exports", "Accounting and Auditing Exports", "Legal Exports",
                                         "Advertising Exports")
                                         
Professional_Imports_sum <- rbind(Professional_Imports, Combined_table3["BusMgmtConsPubRel.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["ArchAndEng.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["IndEng.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Training.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["ResearchAndDev.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["MaintenanceAndRepairNie.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["AccountAuditBookkeep.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Legal.Imports.AllAffiliations", 5:(4 + length(years))],
                                  Combined_table3["Advertising.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Professional_Imports_sum) <- c("Total Professional Imports", "Business and Mangement Consulting Imports", "Architecture and Engineering Imports", 
                                         "Industrial Engineering Imports","Training Imports", "Research and Development Imports", 
                                         "Maintenance and Repair Imports", "Accounting and Auditing Imports", "Legal Imports",
                                         "Advertising Imports")

ChargesForTheUseOfIpNie_Exports_sum <- rbind(ChargesForTheUseOfIpNie_Exports, Combined_table3["CipIndProcess.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipCompSoft.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipTrademarks.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipFranchiseFees.Exports.AllAffiliations", 5:(4 + length(years))],
                                               Combined_table3["CipOth.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(ChargesForTheUseOfIpNie_Exports_sum) <- c("Total Charges for the use of Intellectual Property Exports",
                                                    "Industrial Processes Exports", "Computer Software Exports", "Trademarks Exports", 
                                                    "Franchise Fees Exports", "Other Intellectual Property Exports")

ChargesForTheUseOfIpNie_Imports_sum <- rbind(ChargesForTheUseOfIpNie_Imports, Combined_table3["CipIndProcess.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipCompSoft.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipTrademarks.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipFranchiseFees.Imports.AllAffiliations", 5:(4 + length(years))],
                                             Combined_table3["CipOth.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(ChargesForTheUseOfIpNie_Imports_sum) <- c("Total Charges for the use of Intellectual Property Imports",
                                                    "Industrial Processes Imports", "Computer Software Imports", "Trademarks Imports", 
                                                    "Franchise Fees Imports", "Other Intellectual Property Imports")

Travel_Exports_sum <- rbind(Travel_Exports, Combined_table3["TransportAirPass.Exports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelBusiness.Exports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelPersonal.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Travel_Exports_sum) <- c("Total Travel Exports", "Air Passenger Fares Exports", "Business Travel Exports", "Personal Travel Exports")

Travel_Imports_sum <- rbind(Travel_Imports, Combined_table3["TransportAirPass.Imports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelBusiness.Imports.AllAffiliations", 5:(4 + length(years))],
                              Combined_table3["TravelPersonal.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Travel_Imports_sum) <- c("Total Travel Imports", "Air Passenger Fares Imports", "Business Travel Imports", "Personal Travel Imports")

Other_Exports_sum <- rbind(Other_Exports, Combined_table3["Const.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["Mining.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["SportsPerformArts.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["OperatingLeasing.Exports.AllAffiliations", 5:(4 + length(years))],
                             Combined_table3["OthBusinessNie.Exports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Other_Exports_sum) <- c("Total Other Exports", "Construction Exports", "Mining Exports",
                                  "Sports and Performing Arts Exports", "Operating and Leasing Exports",
                                  "Other Business Services Exports")

Other_Imports_sum <- rbind(Other_Imports, Combined_table3["Const.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["Mining.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["SportsPerformArts.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["OperatingLeasing.Imports.AllAffiliations", 5:(4 + length(years))],
                           Combined_table3["OthBusinessNie.Imports.AllAffiliations", 5:(4 + length(years))]
)

row.names(Other_Imports_sum) <- c("Total Other Imports", "Construction Imports", "Mining Imports",
                                  "Sports and Performing Arts Imports", "Operating and Leasing Imports",
                                  "Other Business Services Imports")

Govt_exports <- Combined_table3["GovtGoodsAndServicesNie.Exports.AllAffiliations", 5:(4 + length(years))]
row.names(Govt_exports) <- "Government Services Exports"

Govt_imports <- Combined_table3["GovtGoodsAndServicesNie.Imports.AllAffiliations", 5:(4 + length(years))]
row.names(Govt_imports) <- "Government Services Imports"

Summary_Table <- rbind(TotalPrivate_Exports, TotalPrivate_Imports, Distribution_Exports_sum, Distribution_Imports_sum, Electronic_Exports_sum, Electronic_Imports_sum, Finance_Exports_sum, Finance_Imports_sum,
                       ChargesForTheUseOfIpNie_Exports_sum, ChargesForTheUseOfIpNie_Imports_sum, Travel_Exports_sum, Travel_Imports_sum,
                       Professional_Exports_sum, Professional_Imports_sum, Other_Exports_sum, Other_Imports_sum, Govt_exports, Govt_imports)

# Performs aggregations and calculations
function.cagr <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- (((Total_aggregate[[end_date]]/Total_aggregate[[start_date]])^(1/(length(periods)-1))-1))
  df
}

function.growth <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Total_aggregate[[final_year]]-Total_aggregate[[end_date]])/Total_aggregate[[end_date]]
  df
}

function.cagr2 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- (((Summary_Table[[end_date]]/Summary_Table[[start_date]])^(1/(length(periods)-1))-1))
  df
}

function.growth2 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Summary_Table[[final_year]]-Summary_Table[[end_date]])/Summary_Table[[end_date]]
  df
}

end_date <- paste0("",(Years_for_CAGR[length(Years_for_CAGR)]),"",sep="")
beginning_date <- paste0("",Years_for_CAGR[1],"",sep="")
final_year <- paste0("",(years[length(years)]),"",sep="")

CAGR_name <- paste0("CAGR ", beginning_date, " to ", end_date)
Year_to_year_name <- paste0("Growth from ", end_date, " to ", final_year)

CAGR <- function.cagr(Total_aggregate, CAGR_name, final_year,beginning_date,Years_for_CAGR)
Year_on_Year <- as.data.frame(function.growth(Total_aggregate, Year_to_year_name, final_year, end_date))


CAGR2 <- function.cagr2(Summary_Table, CAGR_name, final_year,beginning_date,Years_for_CAGR)
Year_on_Year2 <- as.data.frame(function.growth2(Summary_Table, Year_to_year_name, final_year, end_date))


Total_aggregate <- cbind(CAGR, Year_on_Year)
Total_aggregate <- Total_aggregate %>% subset(., select=which(!duplicated(names(.)))) 

Summary_Table <- cbind(CAGR2, Year_on_Year2)
Summary_Table <- Summary_Table %>% subset(., select=which(!duplicated(names(.)))) 

Summary_Table[,length(years)+1] <- Summary_Table[,length(years)+1]*100
Summary_Table[,length(years)+1] <- round(Summary_Table[,length(years)+1],2)
Summary_Table[,length(years)+2] <- Summary_Table[,length(years)+2]*100
Summary_Table[,length(years)+2] <- round(Summary_Table[,length(years)+2],2)
Total_aggregate[,length(years)+1] <- Total_aggregate[,length(years)+1]*100
Total_aggregate[,length(years)+1] <- round(Total_aggregate[,length(years)+1],2)
Total_aggregate[,length(years)+2] <- Total_aggregate[,length(years)+2]*100
Total_aggregate[,length(years)+2]<- round(Total_aggregate[,length(years)+2],2)

Combined_table5 <- select(final.result1, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue)
Combined_table5 <- dcast(Combined_table5, TypeOfService + TradeDirection + AreaOrCountry + Affiliation ~ TimePeriod, value.var = "DataValue", fill=0)
Combined_table5 <- as.data.frame.matrix(Combined_table5, col.names=TRUE)
Combined_table5$Unique_ID <- do.call(paste, c(Combined_table5[c("TypeOfService", "TradeDirection", "Affiliation")], sep = "."))
Combined_table5 <- as.data.frame.matrix(Combined_table5, row.names=Combined_table5$Unique_ID)
Combined_table5$Unique_ID <- NULL

Combined_table5[5:(4 + length(years))] <- lapply(Combined_table5[5:(4 + length(years))], as.numeric)

na_test5 <- Combined_table5[rowSums(is.na(Combined_table5)) > 0,]
na_test5$Identifier <- rownames(na_test5)

BEA_lookup_table <- read_csv("Z:/BEA API R/BEA_lookup_table.csv")

Missing_data <- merge(na_test5, BEA_lookup_table, by.x = "Identifier", by.y = "Sector")

#exports final file to csv
write.csv(Total_aggregate, file = file_name1, row.names = TRUE)
write.csv(Summary_Table, file = file_name2, row.names = TRUE)
write.csv(na_test5, file = file_name3, row.names = TRUE)


