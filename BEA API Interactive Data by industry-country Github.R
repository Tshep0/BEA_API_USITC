# User sets folder where files are written


setwd("Z:/BEA API R/data")
#setwd("~/Drive/R")

#user sets file name


file_name3 <- "Missing_Data_Industry_Interactive.csv"

#Users input their desired datasets, indicators (exports, imports, sectors), countries and years

countries <-   "All"
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
  
  "https://www.bea.gov/api/data/?&UserID=",

  "Insert_your_own_BEA_API_key",
  
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

Combined_table1 <- select(final.result1, TypeOfService, TradeDirection, Affiliation, AreaOrCountry, TimePeriod, DataValue) 
Combined_table1 <- dcast(Combined_table1, TypeOfService + TradeDirection + Affiliation + AreaOrCountry ~ TimePeriod, value.var = "DataValue", fill=0) 
Combined_table1$Unique_ID <- do.call(paste, c(Combined_table1[c("TypeOfService", "TradeDirection", "Affiliation")], sep = ".")) 
Combined_table1 <- as.data.frame.matrix(Combined_table1, row.names=Combined_table1$Unique_ID) 
Combined_table1[5:(4 + length(years))] <- lapply(Combined_table1[5:(4 + length(years))], as.numeric) 
Combined_table1 <- as.data.frame.matrix(Combined_table1, col.names=TRUE) 
Combined_table2 <- Combined_table1[,-(5 + length(years))] 
Combined_table2[is.na(Combined_table1)] <- 0 
countries2 <- unique(Combined_table2$AreaOrCountry, incomparables = FALSE)


Combined_table3  <- split(Combined_table2, Combined_table1$AreaOrCountry)    
Combined_table3 <- lapply(seq_along(Combined_table3), function(x) as.data.frame(Combined_table3[[x]])[, 1:(4 + length(years))])  
names(Combined_table3) <- countries2 
list2env(Combined_table3, envir = .GlobalEnv)  

end_date <- paste0("",(years[1]+(length(years)-2)),"",sep="")
beginning_date <- paste0("",years[1],"",sep="")
final_year <- paste0("",(years[1]+(length(years)-1)),"",sep="")


function.format.aggregate.calculate <- function(df) {
  #df <- dcast(df, TypeOfService + TradeDirection + Affiliation + AreaOrCountry ~ TimePeriod, value.var = "DataValue", fill=0)
  #df$Unique_ID <- do.call(paste, c(df[c("TypeOfService", "TradeDirection", "Affiliation")], sep = "."))
  #df <- as.data.frame.matrix(df, row.names=df$Unique_ID)
  #df[5:(4 + length(years))] <- lapply(df[5:(4 + length(years))], as.numeric)
  #df <- as.data.frame.matrix(df, col.names=TRUE)
  #df <- df[,-(5 + length(years))]
  #df[is.na(df)] <- 0
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
    #df["TechTradeRelatedOth.Imports.AllAffiliations", 5:(4 + length(years))]
  
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
    df[[col_name1]] <- ((df[[end_date]]/df[[start_date]])^(1/periods))-1
    df
  }
  
  function.growth <- function(df,col_name2,final_year,end_date) {
    df[[col_name2]] <- (df[[final_year]]-df[[end_date]])/df[[end_date]]
  }
  
  
  CAGR <- function.cagr(df, "CAGR", end_date,beginning_date,length(years)-2)
  Year_on_Year <- function.growth(df, "Year-on-Year Growth", final_year, end_date)
  
  df <- cbind(CAGR, Year_on_Year)
}


#apply(countries2,function.format.aggregate.calculate)


final.countries <- data.frame()

for (i in 1:length(countries2)) {

  arg1 <- "function.format.aggregate.calculate(" 
  arg2 <- paste0(arg1,countries2[[i]],")", sep="")
  final.countries1 <- eval(parse(text=arg2))
  final.countries2 <- cbind(countries2[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)
}

rm(Africa,AllCountries,AsiaAndPac,Australia,Austria,Bahrain,Belgium,Bermuda,Brazil,Brunei,
   Bulgaria,CaftaDrCountries,Canada,Chile,China,Colombia,CostaRica,Croatia,Cyprus,CzechRep,Denmark,
   DominicanRep,ElSalvador,Estonia,EU,EuroArea,Europe,Finland,France,Germany,Greece,Guatemala,Honduras,
   HongKong,Hungary,India,Indonesia,IntOrgAndUnalloc,Ireland,Israel,Italy,Japan,Jordan,KoreaRepOf,
   LatAmAndOthWestHem,Latvia,Lithuania,Luxembourg,Malaysia,Malta,Mexico,MiddleEast,Morocco,NaftaCountries,
   Netherlands,NewZealand,Nicaragua,Nigeria,Norway,Oman,OthAfricaIst,OthAsiaAndPacIst,OthEuropeIst,
   OthMiddleEastIst,OthSouthAndCenAmIst,OthWestHem,OthWestHemOthIst,Panama,Peru,Philippines,Poland,Portugal,
   Romania,Russia,SaudiArabia,Singapore,Slovakia,Slovenia,SouthAfrica,SouthAndCenAm,Spain,Sweden,Switzerland,
   Taiwan,Thailand,Turkey,UkIslandsCarib,UnitedKingdom,Venezuela,Vietnam)

final.countries$CAGR <- final.countries$CAGR*100
final.countries$Year_on_Year <- final.countries$Year_on_Year*100
round(final.countries$CAGR, 2)
round(final.countries$Year_on_Year, 2)
final.countries <- cbind(row.names(final.countries), final.countries)
names(final.countries)[names(final.countries) == "countries2[[i]]"] <- "Countries"
names(final.countries)[names(final.countries) == "row.names(final.countries)"] <- "Industries"
final.countries$Industries = gsub("[[:digit:]]", "", final.countries$Industries)
last_column <- paste0("final.countries[order(final.countries$Industries, -final.countries$'", final_year, "'),]", sep="")
final.countries2 <- eval(parse(text=last_column))
regions <- c("Africa","AllCountries","AsiaAndPac","CaftaDrCountries","EuroArea","Europe","IntOrgAndUnalloc","LatAmAndOthWestHem","MiddleEast",
             "NaftaCountries","OthAfricaIst","OthAsiaAndPacIst","OthEuropeIst","OthMiddleEastIst","OthSouthAndCenAmIst","OthWestHem",
             "OthWestHemOthIst","SouthAndCenAm")
final.countries3 <- final.countries2[!grepl(paste(regions, collapse = "|"), final.countries2$Countries),]

countries <- unique(final.countries3$Countries, incomparables = FALSE)
industries2 <- unique(final.countries3$Industries, incomparables = FALSE)
industries2 <- industries2[order(industries2)]

final.countries3  <- split(final.countries3, final.countries3$Industries)

names(final.countries3) <- industries2
list2env(final.countries, envir = .GlobalEnv)


na_test5 <- Combined_table1[rowSums(is.na(Combined_table1)) > 0,]

sapply(names(final.countries3), 
       function (x) write.csv(final.countries3[[x]], file=paste(x, "csv", sep=".")))
write.csv(na_test5, file = file_name3, row.names = TRUE)
