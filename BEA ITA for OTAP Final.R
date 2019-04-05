# User sets folder where files are written

#setwd("Z:/BEA API R/data/OTAP/Test")
setwd("/Users/Tshepo1/Drive/R work/Test")

#user sets file name

file_name1 <- "Table1.2.csv"
file_name2 <- "Table_for_growth_trends.csv"
file_name3 <- "Table_for_chart.csv"
file_name4 <- "Missing_Data_ITA.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   "All"
frequency <- "A"
years <- 2014:2018
dataset3 <- "ITA"
Years_for_CAGR <- 2014:2017

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

Combined_table1 <- select(final.result4, TimeSeriesDescription, AreaOrCountry, Indicator, Year, DataValue)
Combined_table2 <- dcast(Combined_table1, TimeSeriesDescription + AreaOrCountry + Indicator ~ Year, value.var = "DataValue")
Combined_table3 <- as.data.frame.matrix(Combined_table2, col.names=TRUE)
Combined_table3 <- as.data.frame.matrix(Combined_table2, row.names=Combined_table2$Indicator)
Table_1.1 <- Combined_table3[grep("exports|Exports", Combined_table3$TimeSeriesDescription), ]
Table_1.1$TradeDirection <- rep("Exports")
Table_1.2 <- Combined_table3[grep("imports|Imports", Combined_table3$TimeSeriesDescription), ]
Table_1.2$TradeDirection <- rep("Imports")
Combined_table4 <- rbind(Table_1.1, Table_1.2)
Combined_table4$Unique_ID <- do.call(paste0, c(Combined_table4[c("Indicator", "TradeDirection")], sep = ".")) 
Combined_table4 <- as.data.frame.matrix(Combined_table4, row.names=Combined_table4$Unique_ID) 
Combined_table4[4:(3 + length(years))] <- lapply(Combined_table4[4:(3 + length(years))], as.numeric) 
Combined_table4 <- Combined_table4[, -c(1,3)]
Combined_table4[3:(1 + length(years))] = lapply(Combined_table4[3:(1 + length(years))], function(x) gsub("[^0-9\\.]", "", x))
Combined_table4[3:(1+length(years))] <- lapply(Combined_table4[3:(1+length(years))], as.numeric)
Combined_table6 <- is.na(Combined_table4)
Combined_table4[Combined_table6] <- rep(0, sum(Combined_table6))

rem <- c("ResidualSeas", "AllOthSeas", "OthAfrica", "IntOrgAndUnalloc", "Africa", "AsiaAndPac", "EuroArea",
         "Europe", "EuropeExclEU", "LatAmAndOthWestHem", "MiddleEast", "Opec", "OthAsiaAndPac", "OthEU", "OthEuroArea",
         "OthSouthAndCenAm", "SouthAndCenAm", "OthWestHem", "UnitedKingdom", "Germany", "France", "Netherlands",
         "Italy", "Belgium", "Luxembourg")


Combined_table4 <- Combined_table4[!Combined_table4$AreaOrCountry %in% rem, ]

Combined_table5  <- split(Combined_table4, Combined_table4$AreaOrCountry)    
Combined_table5 <- lapply(seq_along(Combined_table5), function(x) as.data.frame(Combined_table5[[x]])[, 1:(2 + length(years))])  
countries2 <- unique(Combined_table4$AreaOrCountry, incomparables = FALSE) 
countries2 <- sort(countries2)

names(Combined_table5) <- countries2 
list2env(Combined_table5, envir = .GlobalEnv)  

end_date <- length(years)-1
beginning_date <- 2
final_year <- length(years)

end_date2 <- paste0("",(Years_for_CAGR[length(Years_for_CAGR)]),"",sep="")
beginning_date2 <- paste0("",Years_for_CAGR[1],"",sep="")
final_year2 <- paste0("",(years[length(years)]),"",sep="")

CAGR_name <- paste0("CAGR ", beginning_date2, " to ", end_date2)
Year_to_year_name <- paste0("Growth from ", end_date2, " to ", final_year2)


function.format.aggregate.calculate <- function(df) {
  
  TotalPrivate_Exports <- df["ExpServExports.", 2:(length(years)+1)] - 
    df["ExpServGovtGoodsAndServicesNieExports.", 2:(length(years)+1)]
  row.names(TotalPrivate_Exports) <- "Total Private Services Exports"
  
  
  TotalPrivate_Imports <- df["ImpServImports.", 2:(length(years)+1)] - 
    df["ImpServGovtGoodsAndServicesNieImports.", 2:(length(years)+1)]
    row.names(TotalPrivate_Imports) <- "Total Private Services Imports"
    
  Trade_Balance <- TotalPrivate_Exports - TotalPrivate_Imports
  row.names(Trade_Balance) <- "Trade Balance"
  
  Two_way_trade <- TotalPrivate_Exports + TotalPrivate_Imports
  row.names(Two_way_trade) <- "Two way trade"
  
  df <- rbind(TotalPrivate_Exports,TotalPrivate_Imports, Trade_Balance, Two_way_trade)
}

final.countries <- data.frame()

for (i in 1:length(countries2)) {
  
  arg1 <- "function.format.aggregate.calculate(" 
  arg2 <- paste0(arg1,countries2[[i]],")", sep="")
  final.countries1 <- eval(parse(text=arg2))
  final.countries2 <- cbind(countries2[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)
}

names(final.countries)[1] <- "Country"

function.cagr <- function(df,col_name1,end_date,beginning_date,periods) {
  df[[col_name1]] <- (((final.countries[[end_date]]/final.countries[[beginning_date]])^(1/(length(periods)-1))-1))
  df
}

function.growth <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (final.countries[[final_year]]-final.countries[[end_date]])/final.countries[[end_date]]
  df
}

CAGR <- function.cagr(final.countries, CAGR_name, end_date,beginning_date,Years_for_CAGR)
Year_on_Year <- as.data.frame(function.growth(final.countries, Year_to_year_name, final_year, end_date))

final.countries2 <- final.countries

final.countries <- cbind(CAGR, Year_on_Year)
final.countries <- final.countries %>% subset(., select=which(!duplicated(names(.)))) 

final.countries2 <- subset(final.countries2, select = -c(2:(length(years))))
final.countries2$Trade <- row.names(final.countries2)
names(final.countries2)[2] <- "Year"
final.countries2$Trade= lapply(final.countries2$Trade, function(x) gsub('[[:digit:]]+', '', x))
#final.countries2[4] <- NULL
#final.countries2[3] <- NULL
final.countries3 <- final.countries2[grep("Total Private Services Exports", final.countries2$Trade),]
names(final.countries3)[2] <- "Total Private Services Exports"
final.countries3[3] <- NULL
final.countries4 <- final.countries2[grep("Total Private Services Imports", final.countries2$Trade),]
names(final.countries4)[2] <- "Total Private Services Imports"
final.countries4[3] <- NULL
final.countries5 <- final.countries2[grep("Trade Balance", final.countries2$Trade),]
names(final.countries5)[2] <- "Trade Balance"
final.countries5[3] <- NULL
final.countries6 <- final.countries2[grep("Two way trade", final.countries2$Trade),]
names(final.countries6)[2] <- "Two-way trade (exports plus imports)"
final.countries6[3] <- NULL
final.countries7 <- cbind(final.countries3, final.countries4, final.countries5, final.countries6)
final.countries7[3] <- NULL
final.countries7[4] <- NULL
final.countries7[5] <- NULL
row.names(final.countries7) <- final.countries7$Country
final.countries7[1] <- NULL
final.countries7 <- final.countries7[order(-final.countries7$`Total Private Services Exports`),]
final.countries8 <- final.countries7
final.countries8[4] <- NULL
final.countries8[3] <- NULL

#Need to fix NA list
na_test5 <- final.countries[rowSums(is.na(final.countries)) > 0,]

write.csv(final.countries7, file = file_name1, row.names = TRUE)
write.csv(final.countries, file = file_name2, row.names = TRUE)
write.csv(final.countries8, file = file_name3, row.names = TRUE)
write.csv(na_test5, file = file_name4, row.names = TRUE)



