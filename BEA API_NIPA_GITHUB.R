# User sets folder where files are written

#setwd("\\\\hq-fs-1/arthur.chambers$/Desktop/BEA API")
setwd("~/Drive/R")

#user sets file names

file_name1 <- "test_3.csv"
file_name2 <- "test_4.csv"
file_name3 <- "test_5.csv"
file_name4 <- "test_6.csv"

#Users input their desired datasets, indicators (table IDs), and years

years <- 2010:2015
dataset2 <- "NIPA" 
dataset3 <- "GDPBYINDUSTRY"
frequency <- "A"
industry <- "ALL"

indicators2 <- c("189",
		"197"
		)
indicators3 <- "10"
                
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

#URL for calling BEA API for NIPA tables

url3 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=YOUR BEA API USER ID",
  
  "&method=GetData&DataSetName=", dataset2,
  
  "&TableID=")


url4 <- paste0(
 
  "&Frequency=", frequency,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
  
)

#creates a table to store results

final.result2 <- data.frame()

#Loop that calls API for each sector and creates a table of data

for (i in 1:length(indicators2)) {
  this.indicator2 <- indicators2[[i]]
  this.call2 <- paste0(url3, this.indicator2, url4)	
  this.raw.result2 <- fromJSON(this.call2)
  this.result2 <- this.raw.result2$BEAAPI$Results$Data
  
  final.result2 <- rbind.data.frame(final.result2, this.result2)
}

# Just grabs the one table for real GDP by industry

final.result3 <- data.frame()

url5 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=E4D5D03E-0CA3-4515-A904-1F1AE91B42BB",
  
  "&method=GetData&DataSetName=", dataset3,
  
  "&TableID=", indicators3,
  
  "&Industry=", industry,
 
  "&Frequency=", frequency,
  
  "&Year=", year_list,
  
  "&ResultFormat=JSON"
)
this.call3 <- paste0(url5)	
  this.raw.result3 <- fromJSON(this.call3)
  this.result3 <- this.raw.result3$BEAAPI$Results$Data
  
  final.result3 <- rbind.data.frame(final.result3, this.result3)
  
#formates data tables
Combined_table1.1 <- select(final.result3, Industry, IndustrYDescription, Year, DataValue)
Combined_table1.1 <- Combined_table1.1[,c("Industry", "IndustrYDescription", "DataValue",  "Year")]
Combined_table2.1 <- dcast(Combined_table1.1, Industry + IndustrYDescription ~ Year, value.var = "DataValue", fill=0)

Combined_table3.1 <- as.data.frame.matrix(Combined_table2.1, col.names=TRUE)
Combined_table3.1$Unique_ID <- do.call(paste, c(Combined_table3.1[c("Industry", "IndustrYDescription")], sep = "."))
Combined_table3.1 <- as.data.frame.matrix(Combined_table3.1, row.names=Combined_table3.1$Unique_ID)
Combined_table3.1$Unique_ID <- NULL
Combined_table3.1$Industry <- NULL
Combined_table3.1[2:(1 + length(years))] <- lapply(Combined_table3.1[2:(1 + length(years))], as.numeric)

Goods_GDP <- Combined_table3.1["11.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] +
  Combined_table3.1["21.Mining", 2:(1 + length(years))] + Combined_table3.1["23.Construction", 2:(1 + length(years))] +
  Combined_table3.1["31G.Manufacturing", 2:(1 + length(years))]

#aggregates categories for GDP

row.names(Goods_GDP) <- "Goods"

Distribution_GDP <- Combined_table3.1["42.Wholesale trade", 2:(1 + length(years))] + 
  Combined_table3.1["44RT.Retail trade", 2:(1 + length(years))] + 
  Combined_table3.1["48TW.Transportation and warehousing",2:(1 + length(years))] 

row.names(Distribution_GDP) <- "Distribution"

Electronic_GDP <- Combined_table3.1["512.Motion picture and sound recording industries", 2:(1 + length(years))] + 
  Combined_table3.1["513.Broadcasting and telecommunications", 2:(1 + length(years))] +
  Combined_table3.1["514.Data processing, internet publishing, and other information services",2:(1 + length(years))] +
  Combined_table3.1["5415.Computer systems design and related services", 2:(1 + length(years))]

row.names(Electronic_GDP) <- "Electronic"

Financial_GDP <- Combined_table3.1["52.Finance and insurance", 2:(1 + length(years))] + 
  Combined_table3.1["53.Real estate and rental and leasing", 2:(1 + length(years))]

row.names(Financial_GDP) <- "Financial"

Professional_GDP <- Combined_table3.1["5411.Legal services", 2:(1 + length(years))] + 
  Combined_table3.1["5412OP.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))] +
  Combined_table3.1["55.Management of companies and enterprises",2:(1 + length(years))] +
  Combined_table3.1["562.Waste management and remediation services", 2:(1 + length(years))] +
  Combined_table3.1["6.Educational services, health care, and social assistance", 2:(1 + length(years))]

row.names(Professional_GDP) <- "Professional"

Other_GDP <- Combined_table3.1["22.Utilities", 2:(1 + length(years))] + 
  Combined_table3.1["511.Publishing industries, except internet (includes software)", 2:(1 + length(years))] +
  Combined_table3.1["531.Real estate", 2:(1 + length(years))] + 
  Combined_table3.1["561.Administrative and support services",  2:(1 + length(years))] + 
  Combined_table3.1["7.Arts, entertainment, recreation, accommodation, and food services", 2:(1 + length(years))] +
  Combined_table3.1["81.Other services, except government", 2:(1 + length(years))]
                                                                              
row.names(Other_GDP) <- "Other"

#separates wages and employment 

Employment_Wages <- split.data.frame(final.result2, final.result2$TableID)

Wages <- Employment_Wages$'189'

Employment <- Employment_Wages$'197'

#formats tables for wages and employment

Combined_table1.2 <- select(Wages, SeriesCode, LineDescription, TimePeriod, DataValue)
Combined_table1.2 <- Combined_table1.2[,c("SeriesCode", "LineDescription", "DataValue",  "TimePeriod")]
Combined_table2.2 <- dcast(Combined_table1.2, SeriesCode + LineDescription ~ TimePeriod, value.var = "DataValue", fill=0)

Combined_table3.2 <- as.data.frame.matrix(Combined_table2.2, col.names=TRUE)
Combined_table3.2$Unique_ID <- do.call(paste, c(Combined_table3.2[c("SeriesCode", "LineDescription")], sep = "."))
Combined_table3.2 <- as.data.frame.matrix(Combined_table3.2, row.names=Combined_table3.2$Unique_ID)
Combined_table3.2$Unique_ID <- NULL
Combined_table3.2$SeriesCode <- NULL

Combined_table3.2[,2:(1 + length(years))] <- lapply(Combined_table3.2[,2:(1 + length(years))], 
    function(x) as.numeric(gsub(",", "", as.character(x))))

Combined_table4.2 <- select(Employment, SeriesCode, LineDescription, TimePeriod, DataValue)
Combined_table4.2 <- Combined_table4.2[,c("SeriesCode", "LineDescription", "DataValue",  "TimePeriod")]
Combined_table4.2 <- dcast(Combined_table4.2, SeriesCode + LineDescription ~ TimePeriod, value.var = "DataValue", fill=0)

Combined_table5.2 <- as.data.frame.matrix(Combined_table4.2, col.names=TRUE)
Combined_table5.2$Unique_ID <- do.call(paste, c(Combined_table5.2[c("SeriesCode", "LineDescription")], sep = "."))
Combined_table5.2 <- as.data.frame.matrix(Combined_table5.2, row.names=Combined_table5.2$Unique_ID)
Combined_table5.2$Unique_ID <- NULL
Combined_table5.2$SeriesCode <- NULL

Combined_table5.2[,2:(1 + length(years))] <- lapply(Combined_table5.2[,2:(1 + length(years))], 
                                                    function(x) as.numeric(gsub(",", "", as.character(x))))

#aggregates categories for employment

Goods_Employ <- Combined_table5.2["N4304C.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] + 
  Combined_table5.2["N4307C.Mining", 2:(1 + length(years))] + 
  Combined_table5.2["N4312C.Construction", 2:(1 + length(years))] +
  Combined_table5.2["N4313C.Manufacturing", 2:(1 + length(years))] 
row.names(Goods_Employ) <- "Goods"

Distribution_Employ <- Combined_table5.2["N4335C.Wholesale trade", 2:(1 + length(years))] +
  Combined_table5.2["N4338C.Retail trade", 2:(1 + length(years))] +
  Combined_table5.2["N4343C.Transportation and warehousing", 2:(1 + length(years))]
row.names(Distribution_Employ) <- "Distribution"

Electronic_Employ <- Combined_table5.2["N4354C.Motion picture and sound recording industries", 2:(1 + length(years))] +
  Combined_table5.2["N4355C.Broadcasting and telecommunications", 2:(1 + length(years))] +
  Combined_table5.2["N4356C.Information and data processing services", 2:(1 + length(years))] +
  Combined_table5.2["N4367C.Computer systems design and related services", 2:(1 + length(years))]
row.names(Electronic_Employ) <- "ELectronic"

Financial_Employ <- Combined_table5.2["N4357C.Finance and insurance", 2:(1 + length(years))] +
  Combined_table5.2["N4364C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]
row.names(Financial_Employ) <- "Financial"

Professional_Employ <- Combined_table5.2["N4366C.Legal services", 2:(1 + length(years))] +
  Combined_table5.2["N4368C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))] +
  Combined_table5.2["N4369C.Management of companies and enterprises", 2:(1 + length(years))] +
  Combined_table5.2["N4372C.Waste management and remediation services", 2:(1 + length(years))] +
  Combined_table5.2["N4373C.Educational services", 2:(1 + length(years))] +
  Combined_table5.2["N4374C.Health care and social assistance", 2:(1 + length(years))] 
row.names(Professional_Employ) <- "Professional"

Other_Employ <- Combined_table5.2["N4311C.Utilities", 2:(1 + length(years))] +
  Combined_table5.2["N4353C.Publishing industries (includes software)", 2:(1 + length(years))] +
  Combined_table5.2["N4363C.Real estate", 2:(1 + length(years))] +
  Combined_table5.2["N4371C.Administrative and support services", 2:(1 + length(years))] +
  Combined_table5.2["N4393C.Arts, entertainment, and recreation", 2:(1 + length(years))] +
  Combined_table5.2["N4399C.Other services, except government", 2:(1 + length(years))]
row.names(Other_Employ) <- "Other"

#aggregates categories for wages
#wages should be avg not sum - FIX!!!

Goods_Wages <- Combined_table3.2["N4104C.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] + 
  Combined_table3.2["N4107C.Mining", 2:(1 + length(years))] + 
  Combined_table3.2["N4112C.Construction", 2:(1 + length(years))] +
  Combined_table3.2["N552RC.Manufacturing", 2:(1 + length(years))] 

Goods_Wages_per_fte <- Goods_Wages/(Goods_Employ*1000)
row.names(Goods_Wages_per_fte) <- "Goods"

Distribution_Wages <- Combined_table3.2["N4137C.Wholesale trade", 2:(1 + length(years))] +
  Combined_table3.2["N4140C.Retail trade", 2:(1 + length(years))] +
  Combined_table3.2["N4145C.Transportation and warehousing", 2:(1 + length(years))]

Distribution_Wages_per_fte <- Distribution_Wages/(Distribution_Employ*1000)
row.names(Distribution_Wages_per_fte) <- "Distribution"

Electronic_Wages <- Combined_table3.2["N4156C.Motion picture and sound recording industries", 2:(1 + length(years))] +
  Combined_table3.2["N4157C.Broadcasting and telecommunications", 2:(1 + length(years))] +
  Combined_table3.2["N4158C.Information and data processing services", 2:(1 + length(years))] +
  Combined_table3.2["N4169C.Computer systems design and related services", 2:(1 + length(years))]

Electronic_Wages_per_fte <- Electronic_Wages/(Electronic_Employ*1000)
row.names(Electronic_Wages_per_fte) <- "Electronic"

Financial_Wages <- Combined_table3.2["N4159C.Finance and insurance", 2:(1 + length(years))] +
  Combined_table3.2["N4166C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]

Financial_Wages_per_fte <- Financial_Wages/(Financial_Employ*1000)
row.names(Financial_Wages_per_fte) <- "Financial"

Professional_Wages <- Combined_table3.2["N4168C.Legal services", 2:(1 + length(years))] +
  Combined_table3.2["N4170C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))] +
  Combined_table3.2["N4171C.Management of companies and enterprises", 2:(1 + length(years))] +
  Combined_table3.2["N4174C.Waste management and remediation services", 2:(1 + length(years))] +
  Combined_table3.2["N4175C.Educational services", 2:(1 + length(years))] +
  Combined_table3.2["N4176C.Health care and social assistance", 2:(1 + length(years))] 

Professional_Wages_per_fte <- Professional_Wages/(Professional_Employ*1000)
row.names(Professional_Wages_per_fte) <- "Professional"

Other_Wages <- Combined_table3.2["N4111C.Utilities", 2:(1 + length(years))] +
  Combined_table3.2["N4155C.Publishing industries (includes software)", 2:(1 + length(years))] +
  Combined_table3.2["N4165C.Real estate", 2:(1 + length(years))] +
  Combined_table3.2["N4173C.Administrative and support services", 2:(1 + length(years))] +
  Combined_table3.2["N4181C.Arts, entertainment, and recreation", 2:(1 + length(years))] +
  Combined_table3.2["N4187C.Other services, except government", 2:(1 + length(years))]

Other_Wages_per_fte <- Other_Wages/(Other_Employ*1000)
row.names(Other_Wages_per_fte) <- "Other"

Services_wages <- (Distribution_Wages + Electronic_Wages + Financial_Wages + Professional_Wages + Other_Wages)/(
  (Distribution_Employ + Electronic_Employ + Financial_Employ + Professional_Employ + Other_Employ)*1000)
row.names(Services_wages) <- "Services"

Services_GDP <- Distribution_GDP + Electronic_GDP + Financial_GDP + Professional_GDP + Other_GDP
row.names(Services_GDP) <- "Services"

Services_employ <- Distribution_Employ + Electronic_Employ + Financial_Employ + Professional_Employ + Other_Employ
row.names(Services_employ) <- "Services"

Services_wages_per_fte <- Services_wages/(Services_employ*1000)
row.names(Services_wages_per_fte) <- "Services"

Distribution_LP <- Distribution_GDP/(Distribution_Employ*1000)
row.names(Distribution_LP) <- "Distribution"

Electronic_LP <- Electronic_GDP/(Electronic_Employ*1000)
row.names(Electronic_LP) <- "Electronic"

Financial_LP <- Financial_GDP/(Financial_Employ*1000)
row.names(Financial_LP) <- "Financial"

Professional_LP <- Professional_GDP/(Professional_Employ*1000)
row.names(Professional_LP) <- "Professional"

Other_LP <- Other_GDP/(Other_Employ*1000)
row.names(Other_LP) <- "Other"

Goods_LP <- Goods_GDP/(Goods_Employ*1000)
row.names(Other_LP) <- "Other"

Services_LP <- Services_GDP/(Services_employ*1000)
row.names(Services_LP) <- "Services"

Aggregate_GDP <- rbind(Goods_GDP, Services_GDP, Distribution_GDP, Electronic_GDP,
                       Financial_GDP, Professional_GDP, Other_GDP)

Aggregate_Employ <- rbind(Goods_Employ, Services_employ, Distribution_Employ, Electronic_Employ,
                          Financial_Employ, Professional_Employ, Other_Employ)

Aggregate_Wages_per_fte <- rbind(Goods_Wages_per_fte, Services_wages_per_fte, Distribution_Wages_per_fte, 
                                 Electronic_Wages_per_fte, Financial_Wages_per_fte, Professional_Wages_per_fte, 
                                 Other_Wages_per_fte)

Aggregate_LP <- rbind(Goods_LP, Services_LP, Distribution_LP, Electronic_LP, 
                      Financial_LP, Professional_LP, Other_LP)

#perform calculations

function.cagr1 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Aggregate_GDP[[end_date]]/Aggregate_GDP[[start_date]])^(1/periods))-1
  df
}

function.growth1 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Aggregate_GDP[[final_year]]-Aggregate_GDP[[end_date]])/Aggregate_GDP[[end_date]]
}

function.cagr2 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Aggregate_Employ[[end_date]]/Aggregate_Employ[[start_date]])^(1/periods))-1
  df
}

function.growth2 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Aggregate_Employ[[final_year]]-Aggregate_Employ[[end_date]])/Aggregate_Employ[[end_date]]
}

function.cagr3 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Aggregate_Wages_per_fte[[end_date]]/Aggregate_Wages_per_fte[[start_date]])^(1/periods))-1
  df
}

function.growth3 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Aggregate_Wages_per_fte[[final_year]]-Aggregate_Wages_per_fte[[end_date]])/Aggregate_Wages_per_fte[[end_date]]
}

function.cagr4 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Aggregate_LP[[end_date]]/Aggregate_LP[[start_date]])^(1/periods))-1
  df
}

function.growth4 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Aggregate_LP[[final_year]]-Aggregate_LP[[end_date]])/Aggregate_LP[[end_date]]
}

end_date <- paste0("",(years[1]+(length(years)-2)),"",sep="")
beginning_date <- paste0("",years[1],"",sep="")
final_year <- paste0("",(years[1]+(length(years)-1)),"",sep="")

CAGR1 <- function.cagr1(Aggregate_GDP, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year1 <- function.growth1(Aggregate_GDP, "Year-on-Year Growth", final_year, end_date)

Aggregate_GDP <- cbind(CAGR1, Year_on_Year1)

CAGR2 <- function.cagr2(Aggregate_Employ, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year2 <- function.growth2(Aggregate_Employ, "Year-on-Year Growth", final_year, end_date)

Aggregate_Employ <- cbind(CAGR2,Year_on_Year2)

CAGR3 <- function.cagr3(Aggregate_Wages_per_fte, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year3 <- function.growth3(Aggregate_Wages_per_fte, "Year-on-Year Growth", final_year, end_date)

Aggregate_Wages_per_fte <- cbind(CAGR3, Year_on_Year3)

CAGR4 <- function.cagr4(Aggregate_LP, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year4 <- function.growth4(Aggregate_LP, "Year-on-Year Growth", final_year, end_date)

Aggregate_LP <- cbind(CAGR4,Year_on_Year4)

#round digits in CAGR and Year_on_Year
#Create pre-made tables for each industry group

#exports final file to csv
write.csv(Aggregate_GDP, file = file_name1, row.names = TRUE)
write.csv(Aggregate_Employ, file = file_name2, row.names = FALSE)
write.csv(Aggregate_Wages_per_fte, file = file_name3, row.names = TRUE)
write.csv(Aggregate_Employ, file = file_name4, row.names = FALSE)

#----------------old code----------------------------------------------



aggregate_comp <- Combined_table3[c("TransportSea.Exports.AllCountries.AllAffiliations", 
                              "TransportAirFreight.Exports.AllCountries.AllAffiliations",
                              "TransportAirPort.Exports.AllCountries.AllAffiliations",
                              "TransportOth.Exports.AllCountries.AllAffiliations",
                              "TradeRelated.Exports.AllCountries.AllAffiliations",
                              "CipAudVisRelated.Exports.AllCountries.AllAffiliations",
                              "Telecom.Exports.AllCountries.AllAffiliations",
                              "Comp.Exports.AllCountries.AllAffiliations",
                              "Info.Exports.AllCountries.AllAffiliations",
                              "Financial.Exports.AllCountries.AllAffiliations",
                              "Insurance.Exports.AllCountries.AllAffiliations",
                              "Travel.Exports.AllCountries.AllAffiliations",
                              "TransportAirPass.Exports.AllCountries.AllAffiliations",
                              "ChargesForTheUseOfIpNie.Exports.AllCountries.AllAffiliations",
                              "ProfMgmtConsult.Exports.AllCountries.AllAffiliations",
                              "ArchAndEng.Exports.AllCountries.AllAffiliations",
                              "IndEng.Exports.AllCountries.AllAffiliations",
                              "Training.Exports.AllCountries.AllAffiliations",
                              "ResearchAndDev.Exports.AllCountries.AllAffiliations",
                              "MaintenanceAndRepairNie.Exports.AllCountries.AllAffiliations",
                              "Const.Exports.AllCountries.AllAffiliations",
                              "Mining.Exports.AllCountries.AllAffiliations",
                              "SportsPerformArts.Exports.AllCountries.AllAffiliations",
                              "OperatingLeasing.Exports.AllCountries.AllAffiliations",
                              "OperatingLeasing.Exports.AllCountries.AllAffiliations",
                              "OthBusinessNie.Exports.AllCountries.AllAffiliations", 
                              "TransportSea.Imports.AllCountries.AllAffiliations", 
                              "TransportAirFreight.Imports.AllCountries.AllAffiliations",
                              "TransportAirPort.Imports.AllCountries.AllAffiliations",
                              "TransportOth.Imports.AllCountries.AllAffiliations",
                              "TradeRelated.Imports.AllCountries.AllAffiliations",
                              "CipAudVisRelated.Imports.AllCountries.AllAffiliations",
                              "Telecom.Imports.AllCountries.AllAffiliations",
                              "Comp.Imports.AllCountries.AllAffiliations",
                              "Info.Imports.AllCountries.AllAffiliations",
                              "Financial.Imports.AllCountries.AllAffiliations",
                              "Insurance.Imports.AllCountries.AllAffiliations",
                              "Travel.Imports.AllCountries.AllAffiliations",
                              "TransportAirPass.Imports.AllCountries.AllAffiliations",
                              "ChargesForTheUseOfIpNie.Imports.AllCountries.AllAffiliations",
                              "ProfMgmtConsult.Imports.AllCountries.AllAffiliations",
                              "ArchAndEng.Imports.AllCountries.AllAffiliations",
                              "IndEng.Imports.AllCountries.AllAffiliations",
                              "Training.Imports.AllCountries.AllAffiliations",
                              "ResearchAndDev.Imports.AllCountries.AllAffiliations",
                              "MaintenanceAndRepairNie.Imports.AllCountries.AllAffiliations",
                              "Const.Imports.AllCountries.AllAffiliations",
                              "Mining.Imports.AllCountries.AllAffiliations",
                              "SportsPerformArts.Imports.AllCountries.AllAffiliations",
                              "OperatingLeasing.Imports.AllCountries.AllAffiliations",
                              "OperatingLeasing.Imports.AllCountries.AllAffiliations",
                              "OthBusinessNie.Imports.AllCountries.AllAffiliations"),
                            5:(4 + length(years))]

n_a_test <- aggregate_comp[rowSums(is.na(aggregate_comp)) > 0,]

Total_aggregate <- rbind(Aggregate_Exports,Aggregate_Imports)
Total_aggregate$TypeOfService <- NULL


