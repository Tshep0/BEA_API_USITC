# User sets folder where files are written

setwd("Z:/BEA API R/data/Preliminary cross-border")
#setwd("~/Drive/R")

#user sets file name

file_name1 <- "Table1.1.csv"
file_name2 <- "Missing_Data_ITA.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   "AllCountries"
frequency <- "A"
years <- 2015:2016
dataset3 <- "ITA"

indicators3 <- c("ExpServ",
                 "ExpServChargesForTheUseOfIpNie",
                 "ExpServCipAudVisRelated",
                 "ExpServCipCompSoft",
                 "ExpServCipIndProcess",
                 "ExpServCipOth",
                 "ExpServCipTrademarkFranchiseFees",
                 "ExpServComp",
                 "ExpServFinancial",
                 "ExpServFinCredCardOthCredRelated",
                 "ExpServFinFinManFinAdvCust",
                 "ExpServFinSecBrokUwRelated",
                 "ExpServFinSecLendEftOth",
                 "ExpServGovtGoodsAndServicesNie",
                 "ExpServInfo",
                 "ExpServInsurance",
                 "ExpServInsuranceAuxIns",
                 "ExpServInsuranceDirect",
                 "ExpServInsuranceReIns",
                 "ExpServMaintenanceAndRepairNie",
                 "ExpServOtherBusiness",
                 "ExpServProfMgmtConsult",
                 "ExpServResearchAndDev",
                 "ExpServTechTradeRelatedOth",
                 "ExpServTelecom",
                 "ExpServTelecomCompAndInfo",
                 "ExpServTransport",
                 "ExpServTransportAir",
                 "ExpServTransportAirFreight",
                 "ExpServTransportAirPass",
                 "ExpServTransportAirPort",
                 "ExpServTransportOth",
                 "ExpServTransportSea",
                 "ExpServTransportSeaFreight",
                 "ExpServTransportSeaPort",
                 "ExpServTravel",
                 "ExpServTravelBusiness",
                 "ExpServTravelBusinessOth",
                 "ExpServTravelEducation",
                 "ExpServTravelHealth",
                 "ExpServTravelPersonal",
                 "ExpServTravelPersonalOth",
                 "ImpServ",
                 "ImpServChargesForTheUseOfIpNie",
                 "ImpServCipAudVisRelated",
                 "ImpServCipCompSoft",
                 "ImpServCipIndProcess",
                 "ImpServCipOth",
                 "ImpServCipTrademarkFranchiseFees",
                 "ImpServComp",
                 "ImpServFinancial",
                 "ImpServFinCredCardOthCredRelated",
                 "ImpServFinFinManFinAdvCust",
                 "ImpServFinSecBrokUwRelated",
                 "ImpServFinSecLendEftOth",
                 "ImpServGovtGoodsAndServicesNie",
                 "ImpServInfo",
                 "ImpServInsurance",
                 "ImpServInsuranceAuxIns",
                 "ImpServInsuranceDirect",
                 "ImpServInsuranceReIns",
                 "ImpServMaintenanceAndRepairNie",
                 "ImpServOtherBusiness",
                 "ImpServProfMgmtConsult",
                 "ImpServResearchAndDev",
                 "ImpServTechTradeRelatedOth",
                 "ImpServTelecom",
                 "ImpServTelecomCompAndInfo",
                 "ImpServTransport",
                 "ImpServTransportAir",
                 "ImpServTransportAirFreight",
                 "ImpServTransportAirPass",
                 "ImpServTransportAirPort",
                 "ImpServTransportOth",
                 "ImpServTransportSea",
                 "ImpServTransportSeaFreight",
                 "ImpServTransportSeaPort",
                 "ImpServTravel",
                 "ImpServTravelBusiness",
                 "ImpServTravelBusinessOth",
                 "ImpServTravelEducation",
                 "ImpServTravelHealth",
                 "ImpServTravelPersonal",
                 "ImpServTravelPersonalOth",
                 "ImpServTravelShortTermWork")
                 

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


#URL for calling BEA API

url1 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=E4D5D03E-0CA3-4515-A904-1F1AE91B42BB",
  
  "&method=GetData&DataSetName=", dataset3,
  
  "&Indicator=")


url2 <- paste0(
  
  "&AreaOrCountry=", countries,
  
  "&Frequency=", frequency,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
)

#creates a table to store results

final.result4 <- data.frame()

#Loop that calls API for each sector and creates a table of data

for (i in 1:length(indicators3)) {
  this.indicator3 <- indicators3[[i]]
  this.call3 <- paste0(url1, this.indicator3, url2)	
  this.raw.result1 <- readLines(this.call3,encoding="UTF-8", warn=FALSE)
  this.raw.result2 <- fromJSON(this.raw.result1)
  this.result3 <- this.raw.result2$BEAAPI$Results$Data
  
  final.result4 <- rbind.data.frame(final.result4, this.result3)
}

#Formats table 

Combined_table1 <- select(final.result4, TimeSeriesDescription, Indicator, Year, DataValue)
Combined_table2 <- dcast(Combined_table1, TimeSeriesDescription + Indicator ~ Year, value.var = "DataValue")
Combined_table3 <- as.data.frame.matrix(Combined_table2, col.names=TRUE)
Combined_table3 <- as.data.frame.matrix(Combined_table2, row.names=Combined_table2$Indicator)
Combined_table3 <- Combined_table3[, -c(2)]
Combined_table3[2:(1 + length(years))] = lapply(Combined_table3[2:(1 + length(years))], function(x) gsub("[^0-9\\.]", "", x))
Combined_table3[2:(1+length(years))] <- lapply(Combined_table3[2:(1+length(years))], as.numeric)

#Industry aggregate category 

Total_services_exports <- Combined_table3["ExpServ", 2:(1+length(years))]
row.names(Total_services_exports) <- "Total Services Exports"

Total_services_imports <- Combined_table3["ImpServ", 2:(1+length(years))]
row.names(Total_services_imports) <- "Total Services Imports"

Travel_exports <- Combined_table3["ExpServTravel", 2:(1+length(years))] + 
  Combined_table3["ExpServTransportAirPass", 2:(1+length(years))] 
row.names(Travel_exports) <- "Travel Exports"

Travel_imports <- Combined_table3["ImpServTravel", 2:(1+length(years))] + 
  Combined_table3["ImpServTransportAirPass", 2:(1+length(years))] 
row.names(Travel_imports) <- "Travel imports"

Charges_exports <- Combined_table3["ExpServCipIndProcess", 2:(1+length(years))] +
  Combined_table3["ExpServCipCompSoft", 2:(1+length(years))] +
  Combined_table3["ExpServCipTrademarkFranchiseFees",  2:(1+length(years))] +
  Combined_table3["ExpServCipOth", 2:(1+length(years))] +
  Combined_table3["ExpServCipAudVisRelated", 2:(1+length(years))]
row.names(Charges_exports) <- "Charges for the Use of Intellectual Property exports"

Charges_imports <- Combined_table3["ImpServCipIndProcess", 2:(1+length(years))] +
  Combined_table3["ImpServCipCompSoft", 2:(1+length(years))] +
  Combined_table3["ImpServCipTrademarkFranchiseFees",  2:(1+length(years))] +
  Combined_table3["ImpServCipOth", 2:(1+length(years))] +
  Combined_table3["ImpServCipAudVisRelated", 2:(1+length(years))]
row.names(Charges_imports) <- "Charges for the Use of Intellectual Property imports"

Professional_management_exports <- Combined_table3["ExpServProfMgmtConsult", 2:(1+length(years))]
row.names(Professional_management_exports) <- "Professional Management Services exports"

Professional_management_imports <- Combined_table3["ImpServProfMgmtConsult", 2:(1+length(years))]
row.names(Professional_management_imports) <- "Professional Management Services imports" 

Financial_exports <- Combined_table3["ExpServFinancial",  2:(1+length(years))]
row.names(Financial_exports) <- "Financial exports"

Financial_imports <- Combined_table3["ImpServFinancial",  2:(1+length(years))]
row.names(Financial_imports) <- "Financial imports"

Research_dev_exports <- Combined_table3["ExpServResearchAndDev",  2:(1+length(years))]
row.names(Research_dev_exports) <- "Research and development exports"

Research_dev_imports <- Combined_table3["ImpServResearchAndDev",  2:(1+length(years))]
row.names(Research_dev_imports) <- "Research and development imports"

Technical_exports <- Combined_table3["ExpServTechTradeRelatedOth",  2:(1+length(years))]
row.names(Technical_exports) <- "Trade related and technical services exports"

Technical_imports <- Combined_table3["ImpServTechTradeRelatedOth",  2:(1+length(years))]
row.names(Technical_imports) <- "Trade related and technical services imports"

Maintenance_repair_exports <- Combined_table3["ExpServMaintenanceAndRepairNie",  2:(1+length(years))]
row.names(Maintenance_repair_exports) <- "Maintenance and repair exports"

Maintenance_repair_imports <- Combined_table3["ImpServMaintenanceAndRepairNie",  2:(1+length(years))]
row.names(Maintenance_repair_imports) <- "Maintenance and repair imports"

Insurance_exports <- Combined_table3["ExpServInsurance", 2:(1+length(years))]
row.names(Insurance_exports) <- "Insurance exports"

Insurance_imports <- Combined_table3["ImpServInsurance", 2:(1+length(years))]
row.names(Insurance_imports) <- "Insurance imports"

Air_transport_exports <- Combined_table3["ExpServTransportAir", 2:(1+length(years))] -  Combined_table3["ExpServTransportAirPass", 2:(1+length(years))] 
row.names(Air_transport_exports) <- "Air transport exports"

Air_transport_imports <- Combined_table3["ImpServTransportAir", 2:(1+length(years))] -  Combined_table3["ExpServTransportAirPass", 2:(1+length(years))] 
row.names(Air_transport_imports) <- "Air transport imports"

Sea_transport_exports <- Combined_table3["ExpServTransportSea", 2:(1+length(years))]
row.names(Sea_transport_exports) <- "Sea transport exports"

Sea_transport_imports <- Combined_table3["ImpServTransportSea", 2:(1+length(years))]
row.names(Sea_transport_imports) <- "Sea transport imports"

Computers_exports <- Combined_table3["ExpServComp", 2:(1+length(years))]
row.names(Computers_exports) <- "Computer services exports"

Computers_Imports <- Combined_table3["ImpServComp", 2:(1+length(years))]
row.names(Computers_Imports) <- "Computer services imports"

Other_exports <-  Total_services_exports - (Travel_exports + Charges_exports + Financial_exports + Professional_management_exports +
                                            Research_dev_exports + Technical_exports + Maintenance_repair_exports + Air_transport_exports +
                                            Insurance_exports)
row.names(Other_exports) <- "Other services exports"

Other_imports <- Total_services_imports - (Travel_imports + Insurance_imports + Charges_imports + Professional_management_imports +
                                           Sea_transport_imports + Research_dev_imports + Computers_Imports + Financial_imports + Technical_imports)
                                           
row.names(Other_imports) <- "Other services imports"

Table_1.1 <- rbind(Travel_exports, Charges_exports, Financial_exports, Professional_management_exports,
                   Research_dev_exports, Technical_exports, Maintenance_repair_exports, Air_transport_exports,
                   Insurance_exports, Other_exports, Total_services_exports, Travel_imports, Insurance_imports, Charges_imports,
                   Professional_management_imports, Sea_transport_imports, Research_dev_imports, Computers_Imports,
                   Financial_imports, Technical_imports, Other_imports, Total_services_imports)

end_date <- paste0("",(years[1]+(length(years)-2)),"",sep="")
beginning_date <- paste0("",years[1],"",sep="")
final_year <- paste0("",(years[1]+(length(years)-1)),"",sep="")

table_1.2 <- Table_1.1[grep("exports", rownames(Table_1.1)), ]
table_1.2[order(table_1.2[,2], decreasing = TRUE),]
table_1.3 <- Table_1.1[grep("imports", rownames(Table_1.1)), ]
table_1.3[order(table_1.3[,2], decreasing = TRUE),]
table_1.4 <- rbind(table_1.2,Total_services_exports, table_1.3, Total_services_imports)

function.growth <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Table_1.4[[final_year]]-Table_1.4[[end_date]])/Table_1.4[[end_date]]
}

Year_on_Year <- function.growth(Table_1.4, "Year-on-Year Growth", final_year, end_date)

Table_1.4 <- cbind(Table_1.4, Year_on_Year)
Table_1.4$Year_on_Year <- Table_1.4$Year_on_Year*100
Table_1.4$Year_on_Year <- round(Table_1.4$Year_on_Year, 2)


Combined_table5 <- select(final.result4, TimeSeriesDescription, Indicator, AreaOrCountry, TimePeriod, DataValue)
Combined_table5 <- dcast(Combined_table5, TimeSeriesDescription + Indicator + AreaOrCountry ~ TimePeriod, value.var = "DataValue", fill=0)
Combined_table5 <- as.data.frame.matrix(Combined_table5, col.names=TRUE)
Combined_table5$Unique_ID <- do.call(paste, c(Combined_table5[c("TimeSeriesDescription", "Indicator")], sep = "."))
Combined_table5 <- as.data.frame.matrix(Combined_table5, row.names=Combined_table5$Unique_ID)
Combined_table5$Unique_ID <- NULL

Combined_table5[4:(3 + length(years))] <- lapply(Combined_table5[4:(3 + length(years))], as.numeric)

na_test5 <- Combined_table5[rowSums(is.na(Combined_table5)) > 0,]
na_test5$Identifier <- rownames(na_test5)

BEA_lookup_table <- read_csv("Z:/BEA API R/BEA_lookup_table.csv")

Missing_data <- merge(na_test5, BEA_lookup_table, by.x = "Identifier", by.y = "Sector")
write.csv(table_1.4, file = file_name1, row.names = TRUE)
write.csv(Missing_data, file = file_name2, row.names = TRUE)
