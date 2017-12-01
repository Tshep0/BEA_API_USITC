# User sets folder where files are written

setwd("Z:/BEA API R/data/GDP, FTE")
#setwd("~/Drive/R")

#user sets file names

file_name1 <- "GDP_FTE_Wages_LP--Table_2.1.csv"
file_name2 <- "GDP_FTE_Wages_LP--Table_2.2_Distribution.csv"
file_name3 <- "GDP_FTE_Wages_LP--Table_2.2_Electronic.csv"
file_name4 <- "GDP_FTE_Wages_LP--Table_2.2_Financial.csv"
file_name5 <- "GDP_FTE_Wages_LP--Table_2.2_Professional.csv"
file_name6 <- "GDP_FTE_Wages_LP--Missing_Data.csv"

#Users input their desired datasets, indicators (table IDs), and years

years <- 2011:2016
Years_for_CAGR <- 2011:2015
dataset2 <- "NIPA" 
dataset3 <- "GDPBYINDUSTRY"
frequency <- "A"
industry <- "ALL"

indicators2 <- c("189","197")
indicators3 <- "10"
                
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

#URL for calling BEA API for NIPA tables - add your own BEA API User ID

url3 <- paste0(
  
  "https://www.bea.gov/api/data/?&UserID=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
  
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
  this.raw.result1 <- readLines(this.call2,encoding="UTF-8", warn=FALSE)
  this.raw.result2 <- fromJSON(this.raw.result1)
  this.result3 <- this.raw.result2$BEAAPI$Results$Data
  
  final.result2 <- rbind.data.frame(final.result2, this.result3)
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
  
  this.raw.result3 <- readLines(this.call3,encoding="UTF-8", warn=FALSE)
  this.raw.result4 <- fromJSON(this.raw.result3)

  this.result5 <- this.raw.result4$BEAAPI$Results$Data
  
  final.result3 <- rbind.data.frame(final.result3, this.result5)
  

  
#formates data tables
Combined_table1.1 <- select(final.result3, Industry, IndustrYDescription, Year, DataValue)
Combined_table1.1 <- Combined_table1.1[,c("Industry", "IndustrYDescription", "DataValue",  "Year")]
Combined_table2.1 <- dcast(Combined_table1.1, Industry + IndustrYDescription ~ Year, value.var = "DataValue", fill=0)
Combined_table2.1[3: (2 + length(years))] = lapply(Combined_table2.1[3:(2 + length(years))], function(x) gsub("[^0-9\\.]", "", x))
Combined_table3.1 <- as.data.frame.matrix(Combined_table2.1, col.names=TRUE)
Combined_table3.1$Unique_ID <- do.call(paste, c(Combined_table3.1[c("Industry", "IndustrYDescription")], sep = "."))
Combined_table3.1 <- as.data.frame.matrix(Combined_table3.1, row.names=Combined_table3.1$Unique_ID)
Combined_table3.1$Unique_ID <- NULL
Combined_table3.1$Industry <- NULL
Combined_table3.1[is.na(Combined_table3.1)] <- 0
Combined_table3.1[2:(1 + length(years))] <- lapply(Combined_table3.1[2:(1 + length(years))], as.numeric)

end_date <- paste0("",(years[1]+(length(years)-2)),"",sep="")
beginning_date <- paste0("",years[1],"",sep="")
final_year <- paste0("",(years[1]+(length(years)-1)),"",sep="")

CAGR_name <- paste0("CAGR ", beginning_date, " to ", end_date)
Year_to_year_name <- paste0("Growth from ", end_date, " to ", final_year)

#aggregates categories for GDP

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
  Combined_table3.1["532RL.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]

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

Private_Sector_GDP <- Combined_table3.1["PVT.Private industries", 2:(1 + length(years))]
row.names(Private_Sector_GDP) <- "Private Sector"

Non_manufacturing_GDP <- Combined_table3.1["11.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] +
  Combined_table3.1["21.Mining", 2:(1 + length(years))] + Combined_table3.1["23.Construction", 2:(1 + length(years))] 

Manufacturing_GDP <- Combined_table3.1["31G.Manufacturing", 2:(1 + length(years))] 
row.names(Manufacturing_GDP) <- "Manufacturing"

Goods_GDP <- Manufacturing_GDP + Non_manufacturing_GDP
row.names(Goods_GDP) <- "Goods"

Services_GDP <- Distribution_GDP + Electronic_GDP + Financial_GDP + Professional_GDP + Other_GDP
row.names (Services_GDP) <- "Services"

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

Private_Sector_Employ <- Combined_table5.2["A4303C.Private industries", 2:(1 + length(years))] 
row.names(Private_Sector_Employ) <- "Private Sector"

Non_manufacturing_employ <- Combined_table5.2["N4304C.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] + 
  Combined_table5.2["N4307C.Mining", 2:(1 + length(years))] + 
  Combined_table5.2["N4312C.Construction", 2:(1 + length(years))] 
row.names(Non_manufacturing_employ) <- "Non Manufacturing"

Manufacturing_Employ <- Combined_table5.2["N4313C.Manufacturing", 2:(1 + length(years))] 
row.names(Manufacturing_Employ) <- "Manufacturing"

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
  Combined_table5.2["N4396C.Accommodation and food services", 2:(1 + length(years))] +
  Combined_table5.2["N4399C.Other services, except government", 2:(1 + length(years))]
row.names(Other_Employ) <- "Other"

Services_employ <- Distribution_Employ + Electronic_Employ + Financial_Employ + Professional_Employ + Other_Employ
row.names(Services_employ) <- "Services"

#aggregates categories for wages (salary accruals  divided by employment by sector)

Non_manufacturing_wages <- Combined_table3.2["N4104C.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] + 
  Combined_table3.2["N4107C.Mining", 2:(1 + length(years))] + 
  Combined_table3.2["N4112C.Construction", 2:(1 + length(years))]

Non_manufacturing_wages_per_fte <- Non_manufacturing_wages/Non_manufacturing_employ*1000
row.names(Non_manufacturing_wages_per_fte) <- "Non Manufacturing"

Goods_Wages <- Combined_table3.2["N4104C.Agriculture, forestry, fishing, and hunting", 2:(1 + length(years))] + 
  Combined_table3.2["N4107C.Mining", 2:(1 + length(years))] + 
  Combined_table3.2["N4112C.Construction", 2:(1 + length(years))] +
  Combined_table3.2["N552RC.Manufacturing", 2:(1 + length(years))] 

Goods_Wages_per_fte <- Goods_Wages/Goods_Employ*1000
row.names(Goods_Wages_per_fte) <- "Goods"

Distribution_Wages <- Combined_table3.2["N4137C.Wholesale trade", 2:(1 + length(years))] +
  Combined_table3.2["N4140C.Retail trade", 2:(1 + length(years))] +
  Combined_table3.2["N4145C.Transportation and warehousing", 2:(1 + length(years))]

Distribution_Wages_per_fte <- Distribution_Wages/Distribution_Employ*1000
row.names(Distribution_Wages_per_fte) <- "Distribution"

Electronic_Wages <- Combined_table3.2["N4156C.Motion picture and sound recording industries", 2:(1 + length(years))] +
  Combined_table3.2["N4157C.Broadcasting and telecommunications", 2:(1 + length(years))] +
  Combined_table3.2["N4158C.Information and data processing services", 2:(1 + length(years))] +
  Combined_table3.2["N4169C.Computer systems design and related services", 2:(1 + length(years))]

Electronic_Wages_per_fte <- Electronic_Wages/Electronic_Employ*1000
row.names(Electronic_Wages_per_fte) <- "Electronic"

Financial_Wages <- Combined_table3.2["N4159C.Finance and insurance", 2:(1 + length(years))] +
  Combined_table3.2["N4166C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]

Financial_Wages_per_fte <- Financial_Wages/Financial_Employ*1000
row.names(Financial_Wages_per_fte) <- "Financial"

Professional_Wages <- Combined_table3.2["N4168C.Legal services", 2:(1 + length(years))] +
  Combined_table3.2["N4170C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))] +
  Combined_table3.2["N4171C.Management of companies and enterprises", 2:(1 + length(years))] +
  Combined_table3.2["N4174C.Waste management and remediation services", 2:(1 + length(years))] +
  Combined_table3.2["N4175C.Educational services", 2:(1 + length(years))] +
  Combined_table3.2["N4176C.Health care and social assistance", 2:(1 + length(years))] 

Professional_Wages_per_fte <- Professional_Wages/Professional_Employ*1000
row.names(Professional_Wages_per_fte) <- "Professional"

Other_Wages <- Combined_table3.2["N4111C.Utilities", 2:(1 + length(years))] +
  Combined_table3.2["N4155C.Publishing industries (includes software)", 2:(1 + length(years))] +
  Combined_table3.2["N4165C.Real estate", 2:(1 + length(years))] +
  Combined_table3.2["N4173C.Administrative and support services", 2:(1 + length(years))] +
  Combined_table3.2["N4181C.Arts, entertainment, and recreation", 2:(1 + length(years))] +
  Combined_table3.2["N4184C.Accommodation and food services", 2:(1 + length(years))] +
  Combined_table3.2["N4187C.Other services, except government", 2:(1 + length(years))]
row.names(Other_Wages) <- "Other"

Other_Wages_per_fte <- Other_Wages/Other_Employ*1000
row.names(Other_Wages_per_fte) <- "Other"

Services_wages <- (Distribution_Wages + Electronic_Wages + Financial_Wages + Professional_Wages + Other_Wages)

Services_wages_per_fte <- Services_wages/Services_employ*1000
row.names(Services_wages_per_fte) <- "Services"

# Creates Labor Productivity numbers (GDP divided by employment by sector)

Distribution_GDP_LP <- round(Distribution_GDP, 1)
Distribution_LP <- Distribution_GDP_LP*1000000/Distribution_Employ
row.names(Distribution_LP) <- "Distribution"

Electronic_GDP_LP <- round(Electronic_GDP, 1)
Electronic_LP <- Electronic_GDP_LP*1000000/Electronic_Employ
row.names(Electronic_LP) <- "Electronic"

Financial_GDP_LP <- round(Financial_GDP, 1)
Financial_LP <- Financial_GDP_LP*1000000/Financial_Employ
row.names(Financial_LP) <- "Financial"

Professional_GDP_LP <- round(Professional_GDP, 1)
Professional_LP <- Professional_GDP_LP*1000000/Professional_Employ
row.names(Professional_LP) <- "Professional"

Other_GDP_LP <- round(Other_GDP, 1)
Other_LP <- Other_GDP_LP*1000000/Other_Employ
row.names(Other_LP) <- "Other"

Goods_GDP_LP <- round(Goods_GDP, 1)
Goods_LP <- Goods_GDP_LP*1000000/Goods_Employ
row.names(Other_LP) <- "Other"

Services_GDP_LP <- round(Services_GDP, 1)
Services_LP <- Services_GDP_LP*1000000/Services_employ
row.names(Services_LP) <- "Services"

Private_Sector_Wages <- Combined_table3.2["A4103C.Private industries", 2:(1 + length(years))]
Private_Sector_Wages_per_fte <- Private_Sector_Wages/Private_Sector_Employ*1000
row.names(Private_Sector_Wages_per_fte) <- "Private Sector"

Manufacturing_Wages <- (Combined_table3.2["N552RC.Manufacturing", 2:(1 + length(years))])
Manufacturing_Wages_per_fte <- Manufacturing_Wages/Manufacturing_Employ*1000
row.names(Manufacturing_Wages_per_fte) <- "Manufacturing"

Private_Sector_GDP_LP <- round(Private_Sector_GDP, 1)
Private_Sector_LP <- Private_Sector_GDP_LP*1000000/Private_Sector_Employ
row.names(Private_Sector_LP) <- "Private Sector"

Manufacturing_GDP_LP <- round(Manufacturing_GDP)
Manufacturing_LP <- Manufacturing_GDP_LP*1000000/Manufacturing_Employ
row.names(Manufacturing_LP) <- "Manufacturing"

Table_2.1 <- rbind(Private_Sector_GDP, Goods_GDP, Manufacturing_GDP, (Goods_GDP - Manufacturing_GDP), 
                   Services_GDP, Distribution_GDP, Electronic_GDP, Financial_GDP, Professional_GDP, Other_GDP,
                   Private_Sector_Employ, Goods_Employ, Manufacturing_Employ, (Goods_Employ - Manufacturing_Employ),
                   Services_employ, Distribution_Employ, Electronic_Employ, Financial_Employ, Professional_Employ, Other_Employ,
                   Private_Sector_Wages_per_fte, Goods_Wages_per_fte,  Manufacturing_Wages_per_fte, 
                   ((Goods_Wages - Manufacturing_Wages)/(Goods_Employ - Manufacturing_Employ)*1000),
                   Services_wages_per_fte, Distribution_Wages_per_fte, Electronic_Wages_per_fte, Financial_Wages_per_fte, 
                   Professional_Wages_per_fte, Other_Wages_per_fte, Private_Sector_LP, Goods_LP, 
                   Manufacturing_LP, (((Goods_GDP - Manufacturing_GDP)*1000000)/(Goods_Employ - Manufacturing_Employ)), Services_LP, Distribution_LP, Electronic_LP, Financial_LP,
                   Professional_LP, Other_LP)

Table_2.1 <- round(Table_2.1, 4)  
                          

Table_2.2_Distribution <- rbind(Combined_table3.1["42.Wholesale trade", 2:(1 + length(years))], 
                                Combined_table3.1["44RT.Retail trade", 2:(1 + length(years))], 
                                Combined_table3.1["48TW.Transportation and warehousing",2:(1 + length(years))],
                                Combined_table5.2["N4335C.Wholesale trade", 2:(1 + length(years))],
                                Combined_table5.2["N4338C.Retail trade", 2:(1 + length(years))],
                                Combined_table5.2["N4343C.Transportation and warehousing", 2:(1 + length(years))],
                                Combined_table3.2["N4137C.Wholesale trade", 2:(1 + length(years))]/Combined_table5.2["N4335C.Wholesale trade", 2:(1 + length(years))]*1000,
                                Combined_table3.2["N4140C.Retail trade", 2:(1 + length(years))]/Combined_table5.2["N4338C.Retail trade", 2:(1 + length(years))]*1000,
                                Combined_table3.2["N4145C.Transportation and warehousing", 2:(1 + length(years))]/Combined_table5.2["N4343C.Transportation and warehousing", 2:(1 + length(years))]*1000,
                                Combined_table3.1["42.Wholesale trade", 2:(1 + length(years))]*1000000/Combined_table5.2["N4335C.Wholesale trade", 2:(1 + length(years))],
                                Combined_table3.1["44RT.Retail trade", 2:(1 + length(years))]*1000000/Combined_table5.2["N4338C.Retail trade", 2:(1 + length(years))],
                                Combined_table3.1["48TW.Transportation and warehousing",2:(1 + length(years))]*1000000/Combined_table5.2["N4343C.Transportation and warehousing", 2:(1 + length(years))])
Table_2.2_Distribution <- round(Table_2.2_Distribution,4)                              

Table_2.2_Electronic <- rbind(Combined_table3.1["512.Motion picture and sound recording industries", 2:(1 + length(years))],
                                Combined_table3.1["513.Broadcasting and telecommunications", 2:(1 + length(years))],
                                Combined_table3.1["514.Data processing, internet publishing, and other information services",2:(1 + length(years))],
                                Combined_table3.1["5415.Computer systems design and related services", 2:(1 + length(years))],
                                Combined_table5.2["N4354C.Motion picture and sound recording industries", 2:(1 + length(years))],
                                Combined_table5.2["N4355C.Broadcasting and telecommunications", 2:(1 + length(years))],
                                Combined_table5.2["N4356C.Information and data processing services", 2:(1 + length(years))],
                                Combined_table5.2["N4367C.Computer systems design and related services", 2:(1 + length(years))],
                                Combined_table3.2["N4156C.Motion picture and sound recording industries", 2:(1 + length(years))]/Combined_table5.2["N4354C.Motion picture and sound recording industries", 2:(1 + length(years))]*1000,
                                Combined_table3.2["N4157C.Broadcasting and telecommunications", 2:(1 + length(years))]/Combined_table5.2["N4355C.Broadcasting and telecommunications", 2:(1 + length(years))]*1000,
                                Combined_table3.2["N4158C.Information and data processing services", 2:(1 + length(years))]/Combined_table5.2["N4356C.Information and data processing services", 2:(1 + length(years))]*1000,
                                Combined_table3.2["N4169C.Computer systems design and related services", 2:(1 + length(years))]/ Combined_table5.2["N4367C.Computer systems design and related services", 2:(1 + length(years))]*1000,
                                Combined_table3.1["512.Motion picture and sound recording industries", 2:(1 + length(years))]*1000000/Combined_table5.2["N4354C.Motion picture and sound recording industries", 2:(1 + length(years))],
                                Combined_table3.1["513.Broadcasting and telecommunications", 2:(1 + length(years))]*1000000/Combined_table5.2["N4355C.Broadcasting and telecommunications", 2:(1 + length(years))],
                                Combined_table3.1["514.Data processing, internet publishing, and other information services",2:(1 + length(years))]*1000000/Combined_table5.2["N4356C.Information and data processing services", 2:(1 + length(years))],
                                Combined_table3.1["5415.Computer systems design and related services", 2:(1 + length(years))]*1000000/Combined_table5.2["N4367C.Computer systems design and related services", 2:(1 + length(years))])
Table_2.2_Electronic <- round(Table_2.2_Electronic,4)

Table_2.2_Financial <- rbind(Combined_table3.1["521CI.Federal Reserve banks, credit intermediation, and related activities", 2:(1 + length(years))],
                            Combined_table3.1["524.Insurance carriers and related activities", 2:(1 + length(years))],
                            Combined_table3.1["532RL.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))],
                            Combined_table3.1["523.Securities, commodity contracts, and investments", 2:(1 + length(years))],
                            Combined_table5.2["N4358C.Federal Reserve banks, credit intermediation, and related activities", 2:(1 + length(years))],
                            Combined_table5.2["N4360C.Insurance carriers and related activities", 2:(1 + length(years))],
                            Combined_table5.2["N4364C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))],
                            Combined_table5.2["N4359C.Securities, commodity contracts, and investments", 2:(1 + length(years))],
                            Combined_table3.2["N4160C.Federal Reserve banks, credit intermediation, and related activities", 2:(1 + length(years))]/Combined_table5.2["N4358C.Federal Reserve banks, credit intermediation, and related activities", 2:(1 + length(years))]*1000,
                            Combined_table3.2["N4162C.Insurance carriers and related activities", 2:(1 + length(years))]/Combined_table5.2["N4360C.Insurance carriers and related activities", 2:(1 + length(years))]*1000,
                            Combined_table3.2["N4166C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]/Combined_table5.2["N4364C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]*1000,
                            Combined_table3.2["N4161C.Securities, commodity contracts, and investments", 2:(1 + length(years))]/Combined_table5.2["N4359C.Securities, commodity contracts, and investments", 2:(1 + length(years))]*1000,
                            Combined_table3.1["521CI.Federal Reserve banks, credit intermediation, and related activities", 2:(1 + length(years))]*1000000/Combined_table5.2["N4358C.Federal Reserve banks, credit intermediation, and related activities", 2:(1 + length(years))],
                            Combined_table3.1["524.Insurance carriers and related activities", 2:(1 + length(years))]*1000000/Combined_table5.2["N4360C.Insurance carriers and related activities", 2:(1 + length(years))],
                            Combined_table3.1["532RL.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))]*1000000/Combined_table5.2["N4364C.Rental and leasing services and lessors of intangible assets", 2:(1 + length(years))],
                            Combined_table3.1["523.Securities, commodity contracts, and investments", 2:(1 + length(years))]*1000000/Combined_table5.2["N4359C.Securities, commodity contracts, and investments", 2:(1 + length(years))])
Table_2.2_Financial <- round(Table_2.2_Financial, 4)

Table_2.2_Professional <- rbind(Combined_table3.1["5411.Legal services", 2:(1 + length(years))],
                                  Combined_table3.1["5412OP.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))],
                                  Combined_table3.1["55.Management of companies and enterprises",2:(1 + length(years))],
                                  Combined_table3.1["562.Waste management and remediation services", 2:(1 + length(years))],
                                  Combined_table3.1["61.Educational services", 2:(1 + length(years))],
                                  Combined_table3.1["62.Health care and social assistance", 2:(1 + length(years))],
                                  Combined_table5.2["N4366C.Legal services", 2:(1 + length(years))],
                                  Combined_table5.2["N4368C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))],
                                  Combined_table5.2["N4369C.Management of companies and enterprises", 2:(1 + length(years))],
                                  Combined_table5.2["N4372C.Waste management and remediation services", 2:(1 + length(years))],
                                  Combined_table5.2["N4373C.Educational services", 2:(1 + length(years))],
                                  Combined_table5.2["N4374C.Health care and social assistance", 2:(1 + length(years))],
                                  Combined_table3.2["N4168C.Legal services", 2:(1 + length(years))]/Combined_table5.2["N4366C.Legal services", 2:(1 + length(years))]*1000,
                                  Combined_table3.2["N4170C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))]/Combined_table5.2["N4368C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))]*1000,
                                  Combined_table3.2["N4171C.Management of companies and enterprises", 2:(1 + length(years))]/Combined_table5.2["N4369C.Management of companies and enterprises", 2:(1 + length(years))]*1000,
                                  Combined_table3.2["N4174C.Waste management and remediation services", 2:(1 + length(years))]/Combined_table5.2["N4372C.Waste management and remediation services", 2:(1 + length(years))]*1000,
                                  Combined_table3.2["N4175C.Educational services", 2:(1 + length(years))]/ Combined_table5.2["N4373C.Educational services", 2:(1 + length(years))]*1000,
                                  Combined_table3.2["N4176C.Health care and social assistance", 2:(1 + length(years))]/Combined_table5.2["N4374C.Health care and social assistance", 2:(1 + length(years))]*1000,
                                  Combined_table3.1["5411.Legal services", 2:(1 + length(years))]*1000000/Combined_table5.2["N4366C.Legal services", 2:(1 + length(years))],
                                  Combined_table3.1["5412OP.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))]*1000000/Combined_table5.2["N4368C.Miscellaneous professional, scientific, and technical services", 2:(1 + length(years))],
                                  Combined_table3.1["55.Management of companies and enterprises",2:(1 + length(years))]*1000000/Combined_table5.2["N4369C.Management of companies and enterprises", 2:(1 + length(years))],
                                  Combined_table3.1["562.Waste management and remediation services", 2:(1 + length(years))]*1000000/Combined_table5.2["N4372C.Waste management and remediation services", 2:(1 + length(years))],
                                  Combined_table3.1["61.Educational services", 2:(1 + length(years))]*1000000/Combined_table5.2["N4373C.Educational services", 2:(1 + length(years))],
                                  Combined_table3.1["62.Health care and social assistance", 2:(1 + length(years))]*1000000/Combined_table5.2["N4374C.Health care and social assistance", 2:(1 + length(years))])
Table_2.2_Professional <- round(Table_2.2_Professional,4)

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
  df[[col_name1]] <- ((Table_2.1[[end_date]]/Table_2.1[[start_date]])^(1/periods))-1
  df
}

function.growth1 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Table_2.1[[final_year]]-Table_2.1[[end_date]])/Table_2.1[[end_date]]
  df
}

function.cagr2 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Table_2.2_Distribution[[end_date]]/Table_2.2_Distribution[[start_date]])^(1/periods))-1
  df
}

function.growth2 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Table_2.2_Distribution[[final_year]]-Table_2.2_Distribution[[end_date]])/Table_2.2_Distribution[[end_date]]
  df
}

function.cagr3 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Table_2.2_Electronic[[end_date]]/Table_2.2_Electronic[[start_date]])^(1/periods))-1
  df
}

function.growth3 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Table_2.2_Electronic[[final_year]]-Table_2.2_Electronic[[end_date]])/Table_2.2_Electronic[[end_date]]
}

function.cagr4 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Table_2.2_Financial[[end_date]]/Table_2.2_Financial[[start_date]])^(1/periods))-1
  df
}

function.growth4 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Table_2.2_Financial[[final_year]]-Table_2.2_Financial[[end_date]])/Table_2.2_Financial[[end_date]]
}

function.cagr5 <- function(df,col_name1,end_date,start_date,periods) {
  df[[col_name1]] <- ((Table_2.2_Professional[[end_date]]/Table_2.2_Professional[[start_date]])^(1/periods))-1
  df
}

function.growth5 <- function(df,col_name2,final_year,end_date) {
  df[[col_name2]] <- (Table_2.2_Professional[[final_year]]-Table_2.2_Professional[[end_date]])/Table_2.2_Professional[[end_date]]
}

CAGR1 <- function.cagr1(Table_2.1, CAGR_name, end_date,beginning_date,length(Years_for_CAGR)-1)
Year_on_Year1 <- function.growth1(Table_2.1, Year_to_year_name, final_year, end_date)

Table_2.1 <- cbind(CAGR1, Year_on_Year1)
Table_2.1 <- Table_2.1 %>% subset(., select=which(!duplicated(names(.)))) 
Table_2.1[,length(years)+1] <- Table_2.1[,length(years)+1]*100
Table_2.1[,length(years)+1] <- round(Table_2.1[,length(years)+1], 4)
Table_2.1[,length(years)+2] <- Table_2.1[,length(years)+2]*100
Table_2.1[,length(years)+2] <- round(Table_2.1[,length(years)+2], 4)

CAGR2 <- function.cagr2(Table_2.2_Distribution, CAGR_name, end_date,beginning_date,length(years)-2)
Year_on_Year2 <- function.growth2(Table_2.2_Distribution, Year_to_year_name, final_year, end_date)

Table_2.2_Distribution <- cbind(CAGR2,Year_on_Year2)
Table_2.2_Distribution <- Table_2.2_Distribution %>% subset(., select=which(!duplicated(names(.)))) 
Table_2.2_Distribution[,length(years)+1] <- Table_2.2_Distribution[,length(years)+1]*100
Table_2.2_Distribution[,length(years)+1] <- round(Table_2.2_Distribution[,length(years)+1], 4)
Table_2.2_Distribution[,length(years)+2] <- Table_2.2_Distribution[,length(years)+2]*100
Table_2.2_Distribution[,length(years)+2] <- round(Table_2.2_Distribution[,length(years)+2], 4)

CAGR3 <- function.cagr3(Table_2.2_Electronic, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year3 <- function.growth3(Table_2.2_Electronic, "Year-on-Year Growth", final_year, end_date)

Table_2.2_Electronic <- cbind(CAGR3, Year_on_Year3)
Table_2.2_Electronic <- Table_2.2_Electronic %>% subset(., select=which(!duplicated(names(.)))) 
Table_2.2_Electronic[,length(years)+1] <- Table_2.2_Electronic[,length(years)+1]*100
Table_2.2_Electronic[,length(years)+1] <- round(Table_2.2_Electronic[,length(years)+1], 4)
Table_2.2_Electronic[,length(years)+2] <- Table_2.2_Electronic[,length(years)+2]*100
Table_2.2_Electronic[,length(years)+2] <- round(Table_2.2_Electronic[,length(years)+2], 4)

CAGR4 <- function.cagr4(Table_2.2_Financial, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year4 <- function.growth4(Table_2.2_Financial, "Year-on-Year Growth", final_year, end_date)

Table_2.2_Financial <- cbind(CAGR4,Year_on_Year4)
Table_2.2_Financial <- Table_2.2_Financial %>% subset(., select=which(!duplicated(names(.)))) 
Table_2.2_Financial[,length(years)+1] <- Table_2.2_Financial[,length(years)+1]*100
Table_2.2_Financial[,length(years)+1] <- round(Table_2.2_Financial[,length(years)+1], 4)
Table_2.2_Financial[,length(years)+2] <- Table_2.2_Financial[,length(years)+2]*100
Table_2.2_Financial[,length(years)+2] <- round(Table_2.2_Financial[,length(years)+2], 4)

CAGR5 <- function.cagr5(Table_2.2_Professional, "CAGR", end_date,beginning_date,length(years)-2)
Year_on_Year5 <- function.growth5(Table_2.2_Professional, "Year-on-Year Growth", final_year, end_date)

Table_2.2_Professional <- cbind(CAGR5,Year_on_Year5)
Table_2.2_Professional <- Table_2.2_Professional %>% subset(., select=which(!duplicated(names(.)))) 
Table_2.2_Professional[,length(years)+1] <- Table_2.2_Professional[,length(years)+1]*100
Table_2.2_Professional[,length(years)+1] <- round(Table_2.2_Professional[,length(years)+1], 4)
Table_2.2_Professional[,length(years)+2] <- Table_2.2_Professional[,length(years)+2]*100
Table_2.2_Professional[,length(years)+2] <- round(Table_2.2_Professional[,length(years)+2], 4)

Combined_table5 <- select(final.result3, Industry, IndustrYDescription, Year, DataValue)
Combined_table5 <- Combined_table5[,c("Industry", "IndustrYDescription", "DataValue",  "Year")]
Combined_table5 <- dcast(Combined_table5, Industry + IndustrYDescription ~ Year, value.var = "DataValue", fill=0)

Combined_table5 <- as.data.frame.matrix(Combined_table5, col.names=TRUE)
Combined_table5$Unique_ID <- do.call(paste, c(Combined_table5[c("Industry", "IndustrYDescription")], sep = "."))
Combined_table5 <- as.data.frame.matrix(Combined_table5, row.names=Combined_table5$Unique_ID)
Combined_table5$Unique_ID <- NULL
Combined_table5$Industry <- NULL
Combined_table5[is.na(Combined_table5)] <- 0
Combined_table5[2:(1 + length(years))] <- lapply(Combined_table5[2:(1 + length(years))], as.numeric)

na_test5 <- Combined_table5[rowSums(is.na(Combined_table5)) > 0,]
na_test5$Identifier <- rownames(na_test5)

BEA_lookup_table <- read_csv("Z:/BEA API R/BEA_lookup_GDP_FTE.csv")

Missing_data <- merge(na_test5, BEA_lookup_table, by.x = "Identifier", by.y = "Sector")

row.names(Table_2.1) <- c("GDP: Private Sector1", "Goods1", "Manufacturing1", "Nonmanufacturing1", "Services1", 
                          "Distribution Services1", "Electronic Services1", "Financial Services1", "Professional Services1","Other Services1",
                          "FTEs (thousands): Private Sector2", "Goods2", "Manufacturing2", "Nonmanufacturing2", "Services2", 
                          "Distribution Services2", "Electronic Services2", "Financial Services2", "Professional Services2","Other Services2",
                          "Wages and salary accruals ($ per FTE): Private Sector3", "Goods3", "Manufacturing3", "Nonmanufacturing3", "Services3", 
                          "Distribution Services3", "Electronic Services3", "Financial Services3", "Professional Services3","Other Services3",
                          "Labor Productivity ($ per FTE): Private Sector4", "Goods4", "Manufacturing4", "Nonmanufacturing4", "Services4", 
                          "Distribution Services4", "Electronic Services4", "Financial Services4", "Professional Services4","Other Services4")

row.names(Table_2.2_Distribution) <- c("GDP: Wholesale Trade1", "Retail Trade1", "Transportation and Warehousing1",
                                       "FTEs (thousands): Wholesale Trade2", "Retail Trade2", "Transportation and Warehousing2",
                                       "Wages and salary accruals ($ per FTE): Wholesale Trade3", "Retail Trade3", "Transportation and Warehousing3",
                                       "Labor Productivity ($ per FTE): Wholesale Trade4", "Retail Trade4", "Transportation and Warehousing4")

row.names(Table_2.2_Electronic) <- c("GDP: Motion picture and sound recording industries1", "Broadcasting and telecommunications1", "Data processing, internet publishing, and other information services1", "Computer systems design and related services1",
                                     "FTEs (thousands): Motion picture and sound recording industries2", "Broadcasting and telecommunications2", "Data processing, internet publishing, and other information services2", "Computer systems design and related services2",
                                     "Wages and salary accruals ($ per FTE): Motion picture and sound recording industries3", "Broadcasting and telecommunications3", "Data processing, internet publishing, and other information services3", "Computer systems design and related services3",
                                     "Labor Productivity ($ per FTE): Motion picture and sound recording industries4", "Broadcasting and telecommunications4", "Data processing, internet publishing, and other information services4", "Computer systems design and related services4")

row.names(Table_2.2_Financial) <- c("GDP: Federal Reserve banks, credit intermediation, and related activities1", "Insurance carriers and related activities1", "Rental and leasing services and lessors of intangible assets1", "Securities, commodity contracts, and investments1",
                                    "FTEs (thousands): Federal Reserve banks, credit intermediation, and related activities2", "Insurance carriers and related activities2", "Rental and leasing services and lessors of intangible assets2", "Securities, commodity contracts, and investments2",
                                    "Wages and salary accruals ($ per FTE): Federal Reserve banks, credit intermediation, and related activities3", "Insurance carriers and related activities3", "Rental and leasing services and lessors of intangible assets3", "Securities, commodity contracts, and investments3",
                                    "Labor Productivity ($ per FTE): Federal Reserve banks, credit intermediation, and related activities4", "Insurance carriers and related activities4", "Rental and leasing services and lessors of intangible assets4", "Securities, commodity contracts, and investments4")

row.names(Table_2.2_Professional) <- c("GDP: Legal services1", "Miscellaneous professional, scientific, and technical services1", "Management of companies and enterprises1", "Waste management and remediation services1", "Educational services1", "Health care and social assistance1",
                                       "FTEs (thousands): Legal services2", "Miscellaneous professional, scientific, and technical services2", "Management of companies and enterprises2", "Waste management and remediation services2", "Educational services2", "Health care and social assistance2",
                                       "Wages and salary accruals ($ per FTE): Legal services3", "Miscellaneous professional, scientific, and technical services3", "Management of companies and enterprises3", "Waste management and remediation services3", "Educational services3", "Health care and social assistance3",
                                       "Labor Productivity ($ per FTE): Legal services4", "Miscellaneous professional, scientific, and technical services4", "Management of companies and enterprises4", "Waste management and remediation services4", "Educational services4", "Health care and social assistance4")

#exports final file to csv
write.csv(Table_2.1, file = file_name1, row.names = TRUE)
write.csv(Table_2.2_Distribution, file = file_name2, row.names = TRUE)
write.csv(Table_2.2_Electronic, file = file_name3, row.names = TRUE)
write.csv(Table_2.2_Financial, file = file_name4, row.names = TRUE)
write.csv(Table_2.2_Professional, file = file_name5, row.names = TRUE)
write.csv(Missing_data, file = file_name6, row.names = TRUE)





