#setwd("Z:/BEA API R/data/Affiliates")
setwd("/Users/Tshepo1/Drive/R work/FAS")

library(wdman)
library(RSelenium)
library(stringr) 
library(data.table) 
library(varhandle)
library(dplyr)

# 363 is Foreign affiliate sales, 369 is foreign affiliate purchases (can get any table in 
# theory if you know the number)

table_1 <- "363"
table_2 <- "369"

file_name1 <- "Affiliate_Data_raw.csv"
start_year <- 2009
end_year <- 2016

years <- end_year-start_year+1
years2 <- 1:years

#_______________________________________________________________________________________

rD <- rsDriver()
remDr <- rD$client

#the numbers 1...8 count back from the current year

url1 <- "https://apps.bea.gov/iTable/iTable.cfm?reqid=62&step=6&isuri=1&tablelist="

url2 <- "&product=4&filter_--5=&filter_--4=&filter_--3=0&thetableflexibleiipita=1&filter_--2=0&filter_--1="

if (years2 == "X") {
  year_list <- "X"
} else {
  year_list <- paste(years2, collapse = ",")
}

url_comb <- paste0(url1,table_1,url2, year_list)

remDr$navigate(url_comb)

Sys.sleep(15)

webElem <- remDr$findElement('css', "#icon_download")
webElem$clickElement()

Sys.sleep(15)

webElem2 <- remDr$findElement('css', "#download_wraper")
webElem2$clickElement()

Sys.sleep(15)

url_comb2 <- paste0(url1,table_2,url2, year_list)

remDr$navigate(url_comb2)

Sys.sleep(15)

webElem <- remDr$findElement('css', "#icon_download")
webElem$clickElement()

Sys.sleep(15)

webElem2 <- remDr$findElement('css', "#download_wraper")
webElem2$clickElement()

Sys.sleep(15)

# clean up
remDr$close()


#________________________________________________________________________________________________

temp1 <- "Foreign_affiliate_sales"
temp2 <- "Foreign_affiliate_purchases"
temp3 <- "Foreign_affiliate_sales 2.csv"
temp4 <- "Foreign_affiliate_purchases 2.csv"
temp5 <- "Foreign affiliate sales detailed.csv"
temp6 <- "Foreign affiliate purchases detailed.csv"

#reads in file - 
#affiliate.data <- read.csv("//hq-fs-1/arthur.chambers$/Downloads/download.csv", header=FALSE,stringsAsFactors=FALSE)
affiliate.data <- read.csv("/Users/Tshepo1/Downloads/download.csv", header=FALSE,stringsAsFactors=FALSE)

#reads in file - 
#affiliate.data10 <- read.csv("//hq-fs-1/arthur.chambers$/Downloads/download (2).csv", header=FALSE,stringsAsFactors=FALSE)
affiliate.data10 <- read.csv("/Users/Tshepo1/Downloads/download (1).csv", header=FALSE,stringsAsFactors=FALSE)


#fixes weird BEA format
affiliate.data <- affiliate.data[-c(1,2,3,4,5,6,7),]
affiliate.data <- affiliate.data[,-1]
affiliate.data[1,1] <- "Country"
affiliate.data[2,1] <- "Year"

#___________________________________________________________________
country_names <- affiliate.data[1,]
country_names <- t(country_names)
country_names <- unique(country_names, incomparables = FALSE)
country_names <- country_names[-1,]
country_ID <- 1:length(country_names)
country_ID <- t(country_ID)
country_names <- t(country_names)
country_lookup <- rbind(country_names, country_ID)
row.names(country_lookup) <- c("Country", "Country_ID")
affiliate.data1.5 <- t(affiliate.data[1,])
country_lookup1.5 <- t(country_lookup)
colnames(affiliate.data1.5) <- "Country"
affiliate.data2.5 <- merge(affiliate.data1.5, country_lookup1.5, by.x="Country", by.y = "Country" )
affiliate.data3.5 <- t(affiliate.data2.5)
affiliate.data3.5 <- cbind(Industry = "", affiliate.data3.5)
# _________________________________________________________________

affiliate.data2 <- t(affiliate.data)
Unique_ID <- paste(affiliate.data2[,1], affiliate.data2[,2], sep=".")
affiliate.data3 <- cbind(affiliate.data2, Unique_ID)
colnames(affiliate.data3) <- affiliate.data3[1,]
affiliate.data4 <- as.data.frame(affiliate.data3, row.names = affiliate.data3)

affiliate.data5 <- t(affiliate.data4)
affiliate.data4 <- as.data.frame(affiliate.data3)

affiliate.data5 <- as.data.frame(affiliate.data5, colnames=affiliate.data3[,1])
affiliate.data5 <- affiliate.data5[-c(1,2),]
affiliate.data6 <- affiliate.data5
start_row <- (nrow(affiliate.data6)-11)
end_row <- (nrow(affiliate.data6))-1
affiliate.data6 <- affiliate.data6[-c(start_row:end_row),]
affiliate.data6 <- as.data.frame(lapply(affiliate.data6, FUN = function(x) gsub("Â","", x )), stringsAsFactors=FALSE)
affiliate.data.na <- affiliate.data6
affiliate.data6[2:(nrow(affiliate.data6)-1),-1] <- as.numeric(gsub("[^0-9\\.]", "", as.matrix(affiliate.data6[2:(nrow(affiliate.data6)-1),-1])))
affiliate.data8 <- is.na(affiliate.data6)
affiliate.data6[affiliate.data8] <- rep(0, sum(affiliate.data8))
affiliate.data6 <- as.data.frame(affiliate.data6, row.names = affiliate.data6[,1], stringsAsFactors=FALSE)
colnames(affiliate.data6) <- affiliate.data6[nrow(affiliate.data6),]
affiliate.data6$Country.Year <- NULL
affiliate.data7 <- affiliate.data6[-nrow(affiliate.data6),]

affiliate.data9 <- as.data.frame(lapply(affiliate.data7, as.numeric))
row.names(affiliate.data9) <- row.names(affiliate.data7)
colnames(affiliate.data9) <- colnames(affiliate.data7)

#to solve numeric problem, convert country to ID # such as...
#df1$Numericcolumnname <- as.numeric(factor(df1$Columnname, 
#                 levels=unique(df1$Columnname)))


#creates industry aggregates

Manufacturing_FAS <- affiliate.data9[5,(1:(ncol(affiliate.data9)))]
row.names(Manufacturing_FAS) <- "Manufacturing Sales"

Distribution_services_FAS <- affiliate.data9[41,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[48,(1:(ncol(affiliate.data9)))] + affiliate.data9[91,(1:(ncol(affiliate.data9)))]
row.names(Distribution_services_FAS) <- "Distribution Sales"

Electronic_services_FAS <- affiliate.data9[58,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[61,(1:(ncol(affiliate.data9)))] + affiliate.data9[65,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[66,(1:(ncol(affiliate.data9)))] + affiliate.data9[67,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[78,(1:(ncol(affiliate.data9)))] + affiliate.data9[57,(1:(ncol(affiliate.data9)))]
row.names(Electronic_services_FAS) <- "Electronic Sales"

Financial_services_FAS <- affiliate.data9[68,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[75,(1:(ncol(affiliate.data9)))]
row.names(Financial_services_FAS) <- "Financial Sales"

Professional_services_sales <- affiliate.data9[77,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[79,(1:(ncol(affiliate.data9)))] + affiliate.data9[81,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[82,(1:(ncol(affiliate.data9)))] + affiliate.data9[83,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[84,(1:(ncol(affiliate.data9)))] + affiliate.data9[85,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[86,(1:(ncol(affiliate.data9)))] + affiliate.data9[98,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[104,(1:(ncol(affiliate.data9)))] + affiliate.data9[105,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[110,(1:(ncol(affiliate.data9)))]
row.names(Professional_services_sales) <- "Professional Sales"

Other_misc_services_sales <- affiliate.data9[109,(1:(ncol(affiliate.data9)))] - (affiliate.data9[110,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[111,(1:(ncol(affiliate.data9)))])
row.names(Other_misc_services_sales) <- "Other misc services"

Other_services_sales <- affiliate.data9[56,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[88,(1:(ncol(affiliate.data9)))] + affiliate.data9[2,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[89,(1:(ncol(affiliate.data9)))] + affiliate.data9[90,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[100,(1:(ncol(affiliate.data9)))] + affiliate.data9[106,(1:(ncol(affiliate.data9)))] +
  affiliate.data9[111,(1:(ncol(affiliate.data9)))] + affiliate.data9[74,(1:(ncol(affiliate.data9)))] +
  Other_misc_services_sales
row.names(Other_services_sales) <- "Other Sales (sum of categories)"

Total_services_sales <- affiliate.data9[1,(1:(ncol(affiliate.data9)))] - affiliate.data9[5,(1:(ncol(affiliate.data9)))]
row.names(Total_services_sales) <- "Total services Sales"

Other_services_sales2 <- Total_services_sales - Distribution_services_FAS - Electronic_services_FAS-
  Financial_services_FAS - Professional_services_sales
row.names(Other_services_sales2) <- "Other Sales (subtraction from total)"

Adjustments_for_suppression_sales <- Other_services_sales2 - Other_services_sales
row.names(Adjustments_for_suppression_sales) <- "Adjustments for suppression services sales"

Foreign_Affiliate_Sales <- rbind(Total_services_sales, Manufacturing_FAS, Distribution_services_FAS,
                                 Electronic_services_FAS, Financial_services_FAS, Professional_services_sales,
                                 Other_services_sales2, Adjustments_for_suppression_sales)

Foreign_Affiliate_Sales_detailed <- rbind(Total_services_sales, Manufacturing_FAS, affiliate.data9[5,(1:(ncol(affiliate.data9)))],
                                          Distribution_services_FAS, affiliate.data9[41,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[48,(1:(ncol(affiliate.data9)))], affiliate.data9[91,(1:(ncol(affiliate.data9)))],
                                          Electronic_services_FAS, affiliate.data9[58,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[61,(1:(ncol(affiliate.data9)))], affiliate.data9[65,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[66,(1:(ncol(affiliate.data9)))], affiliate.data9[67,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[78,(1:(ncol(affiliate.data9)))], affiliate.data9[57,(1:(ncol(affiliate.data9)))],
                                          Financial_services_FAS, affiliate.data9[68,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[75,(1:(ncol(affiliate.data9)))],
                                          Professional_services_sales, affiliate.data9[77,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[79,(1:(ncol(affiliate.data9)))], affiliate.data9[81,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[82,(1:(ncol(affiliate.data9)))], affiliate.data9[83,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[84,(1:(ncol(affiliate.data9)))], affiliate.data9[85,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[86,(1:(ncol(affiliate.data9)))], affiliate.data9[98,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[104,(1:(ncol(affiliate.data9)))], affiliate.data9[105,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[110,(1:(ncol(affiliate.data9)))],
                                          Other_services_sales2, affiliate.data9[56,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[88,(1:(ncol(affiliate.data9)))], affiliate.data9[2,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[89,(1:(ncol(affiliate.data9)))], affiliate.data9[90,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[100,(1:(ncol(affiliate.data9)))], affiliate.data9[106,(1:(ncol(affiliate.data9)))],
                                          affiliate.data9[111,(1:(ncol(affiliate.data9)))], affiliate.data9[74,(1:(ncol(affiliate.data9)))],
                                          Other_misc_services_sales, Adjustments_for_suppression_sales)

Foreign_Affiliate_Sales_detailed2 <- Foreign_Affiliate_Sales_detailed[,1:years]
write.csv(Foreign_Affiliate_Sales_detailed2, file = temp5, row.names = TRUE) 

#creates a country field
Country <- colnames(Foreign_Affiliate_Sales)
Country <- as.character(gsub("[[:digit:]+.+X+//]", "", Country))
Foreign_Affiliate_Sales2 <- rbind(Country, Foreign_Affiliate_Sales)


#creates a year field - need to update
Year <-colnames(Foreign_Affiliate_Sales)
Year <- as.numeric(gsub("[^0-9]", "", Year))
Foreign_Affiliate_Sales2 <- rbind(Year, Foreign_Affiliate_Sales2)


#fixes weird BEA format
affiliate.data10 <- affiliate.data10[-c(1,2,3,4,5,6,7),]
affiliate.data10 <- affiliate.data10[,-1]
affiliate.data10[1,1] <- "Country"
affiliate.data10[2,1] <- "Year"

affiliate.data12 <- t(affiliate.data10)
Unique_ID <- paste(affiliate.data12[,1], affiliate.data12[,2], sep=".")
affiliate.data13 <- cbind(affiliate.data12, Unique_ID)
colnames(affiliate.data13) <- affiliate.data13[1,]
affiliate.data14 <- as.data.frame(affiliate.data13, row.names = affiliate.data13)

affiliate.data15 <- t(affiliate.data14)
affiliate.data14 <- as.data.frame(affiliate.data13)

affiliate.data15 <- as.data.frame(affiliate.data15, colnames=affiliate.data13[,1])
affiliate.data15 <- affiliate.data15[-c(1,2),]
affiliate.data16 <- affiliate.data15
start_row <- (nrow(affiliate.data16)-11)
end_row <- (nrow(affiliate.data16))-1
affiliate.data16 <- affiliate.data16[-c(start_row:end_row),]
affiliate.data16 <- as.data.frame(lapply(affiliate.data16, FUN = function(x) gsub("Â","", x )), stringsAsFactors=FALSE)
affiliate.data.na2 <- affiliate.data16
affiliate.data16[2:(nrow(affiliate.data16)-1),-1] <- as.numeric(gsub("[^0-9\\.]", "", as.matrix(affiliate.data16[2:(nrow(affiliate.data16)-1),-1])))
affiliate.data18 <- is.na(affiliate.data16)
affiliate.data16[affiliate.data18] <- rep(0, sum(affiliate.data18))
affiliate.data16 <- as.data.frame(affiliate.data16, row.names = affiliate.data16[,1], stringsAsFactors=FALSE)
colnames(affiliate.data16) <- affiliate.data16[nrow(affiliate.data16),]
affiliate.data16$Country.Year <- NULL
affiliate.data17 <- affiliate.data16[-nrow(affiliate.data16),]
affiliate.data19 <- as.data.frame(lapply(affiliate.data17, as.numeric))

row.names(affiliate.data19) <- row.names(affiliate.data17)
colnames(affiliate.data19) <- colnames(affiliate.data17)


#creates industry aggregates - right now electronic purchases don't add up

Manufacturing_FAP <- affiliate.data19[5,(1:(ncol(affiliate.data19)))]
row.names(Manufacturing_FAP) <- "Manufacturing Purchases"

Distribution_services_FAP <- affiliate.data19[33,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[40,(1:(ncol(affiliate.data19)))] + affiliate.data19[83,(1:(ncol(affiliate.data19)))]
row.names(Distribution_services_FAP) <- "Distribution Purchases"

Electronic_services_FAP <- affiliate.data19[50,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[53,(1:(ncol(affiliate.data19)))] + affiliate.data19[57,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[58,(1:(ncol(affiliate.data19)))] + affiliate.data19[59,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[70,(1:(ncol(affiliate.data19)))] + affiliate.data19[49,(1:(ncol(affiliate.data19)))]
row.names(Electronic_services_FAP) <- "Electronic Purchases"

Financial_services_FAP <- affiliate.data19[60,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[67,(1:(ncol(affiliate.data19)))]
row.names(Financial_services_FAP) <- "Financial Purchases"

Professional_services_FAP <- affiliate.data19[69,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[71,(1:(ncol(affiliate.data19)))] + affiliate.data19[73,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[74,(1:(ncol(affiliate.data19)))] + affiliate.data19[75,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[76,(1:(ncol(affiliate.data19)))] + affiliate.data19[77,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[78,(1:(ncol(affiliate.data19)))] + affiliate.data19[90,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[96,(1:(ncol(affiliate.data19)))] + affiliate.data19[97,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[102,(1:(ncol(affiliate.data19)))]
row.names(Professional_services_FAP) <- "Professional Purchases"

Other_misc_services_purchases <- affiliate.data19[104,(1:(ncol(affiliate.data19)))]
row.names(Other_misc_services_sales) <- "Other misc services"

Other_services_FAP <- affiliate.data19[48,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[80,(1:(ncol(affiliate.data19)))] + affiliate.data19[2,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[81,(1:(ncol(affiliate.data19)))] + affiliate.data19[82,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[92,(1:(ncol(affiliate.data19)))] + affiliate.data19[98,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[103,(1:(ncol(affiliate.data19)))] + affiliate.data19[104,(1:(ncol(affiliate.data19)))] +
  affiliate.data19[66,(1:(ncol(affiliate.data19)))] 
row.names(Other_services_FAP) <- "Other Purchases (sum of categories)"

Total_services_purchases <- affiliate.data19[1,(1:(ncol(affiliate.data19)))] - affiliate.data19[5,(1:(ncol(affiliate.data19)))]
row.names(Total_services_purchases) <- "Total services purchases"

Other_services_FAP2 <- Total_services_purchases - Distribution_services_FAP - Electronic_services_FAP-
  Financial_services_FAP - Professional_services_FAP
row.names(Other_services_FAP2) <- "Other purchases (subtraction from total)"

Adjustments_for_suppression_purchases <- Other_services_FAP2 - Other_services_FAP
row.names(Adjustments_for_suppression_purchases) <- "Adjustments for suppression services purchases"

Foreign_Affiliate_Purchases <- rbind(Total_services_purchases, Manufacturing_FAP, Distribution_services_FAP,
                                     Electronic_services_FAP, Financial_services_FAP, Professional_services_FAP,
                                     Other_services_FAP2, Adjustments_for_suppression_purchases)

Foreign_Affiliate_Purchases_detailed <- rbind(Total_services_purchases, Manufacturing_FAP, affiliate.data19[5,(1:(ncol(affiliate.data19)))],
                                              Distribution_services_FAP, affiliate.data19[33,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[40,(1:(ncol(affiliate.data19)))], affiliate.data19[83,(1:(ncol(affiliate.data19)))],
                                              Electronic_services_FAP, affiliate.data19[50,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[53,(1:(ncol(affiliate.data19)))], affiliate.data19[57,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[58,(1:(ncol(affiliate.data19)))], affiliate.data19[70,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[49,(1:(ncol(affiliate.data19)))], affiliate.data19[59,(1:(ncol(affiliate.data19)))],
                                              Financial_services_FAP, affiliate.data19[60,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[67,(1:(ncol(affiliate.data19)))],
                                              Professional_services_FAP, affiliate.data19[69,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[71,(1:(ncol(affiliate.data19)))], affiliate.data19[73,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[74,(1:(ncol(affiliate.data19)))], affiliate.data19[75,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[76,(1:(ncol(affiliate.data19)))], affiliate.data19[77,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[78,(1:(ncol(affiliate.data19)))], affiliate.data19[90,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[96,(1:(ncol(affiliate.data19)))], affiliate.data19[97,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[102,(1:(ncol(affiliate.data19)))],
                                              Other_services_FAP2, affiliate.data19[48,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[80,(1:(ncol(affiliate.data19)))], affiliate.data19[2,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[81,(1:(ncol(affiliate.data19)))], affiliate.data19[82,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[92,(1:(ncol(affiliate.data19)))], affiliate.data19[98,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[103,(1:(ncol(affiliate.data19)))], affiliate.data19[104,(1:(ncol(affiliate.data19)))],
                                              affiliate.data19[66,(1:(ncol(affiliate.data19)))], Adjustments_for_suppression_purchases)

Foreign_Affiliate_Purchases_detailed2 <- Foreign_Affiliate_Purchases_detailed [,1:years]
write.csv(Foreign_Affiliate_Purchases_detailed2, file = temp6, row.names = TRUE) 

Country2 <- colnames(Foreign_Affiliate_Purchases)
Country2 <- as.character(gsub("[[:digit:]+.+X+//]", "", Country2))
Foreign_Affiliate_Purchases2 <- rbind(Country2, Foreign_Affiliate_Purchases)


#creates a year field - need to update
Year2 <-colnames(Foreign_Affiliate_Purchases)
Year2 <- as.numeric(gsub("[^0-9]", "", Year2))
Foreign_Affiliate_Purchases2 <- rbind(Year2, Foreign_Affiliate_Purchases2)


row.names(Foreign_Affiliate_Sales2)[1:2] <- c("Year", "Country")
row.names(Foreign_Affiliate_Purchases2)[1:2] <- c("Year", "Country")

gsub("[[:space:]]","", Foreign_Affiliate_Sales2[2,])
Foreign_Affiliate_Sales2[1,] <- gsub("22", "2", Foreign_Affiliate_Sales2[1,])
gsub("[[:space:]]","", Foreign_Affiliate_Purchases2[2,])
Foreign_Affiliate_Purchases2[1,] <- gsub("22", "2", Foreign_Affiliate_Purchases2[1,])

#_______________________________________________________________________________________

# - write to .csv and reimport to fix numeric problem - janky but works
Foreign_Affiliate_Sales3 <- t(Foreign_Affiliate_Sales2)
Foreign_Affiliate_Sales3 <- as.data.frame(Foreign_Affiliate_Sales3)
Foreign_Affiliate_Sales3[,2] <- str_trim(Foreign_Affiliate_Sales3[,2])
Foreign_Affiliate_Sales4 <- Foreign_Affiliate_Sales3[with(Foreign_Affiliate_Sales3, order(Country, Year)),]
Foreign_Affiliate_Sales5 <- t(Foreign_Affiliate_Sales4)
write.csv(Foreign_Affiliate_Sales5, file = temp1, row.names = TRUE) 
#Foreign_affiliate_sales4.5 <- read.csv("Z:/BEA API R/data/Affiliates/Foreign_affiliate_sales", row.names=1)
Foreign_affiliate_sales4.5 <-read.csv("/Users/Tshepo1/Drive/R/FAS/Foreign_affiliate_sales", row.names=1)
#write.csv(Foreign_affiliate_sales4.5, file = temp3, row.names = TRUE)
# use sapply startswith
  
Foreign_Affiliate_Purchases3 <- t(Foreign_Affiliate_Purchases2)
Foreign_Affiliate_Purchases3 <- as.data.frame(Foreign_Affiliate_Purchases3)
Foreign_Affiliate_Purchases3[,2] <- str_trim(Foreign_Affiliate_Purchases3[,2])
Foreign_Affiliate_Purchases4 <- Foreign_Affiliate_Purchases3[with(Foreign_Affiliate_Purchases3, order(Country, Year)),]
Foreign_Affiliate_Purchases5 <- t(Foreign_Affiliate_Purchases4)
write.csv(Foreign_Affiliate_Purchases5, file = temp2, row.names = TRUE)
#Foreign_affiliate_purchases4.5 <- read.csv("Z:/BEA API R/data/Affiliates/Foreign_affiliate_purchases", row.names=1)
Foreign_affiliate_purchases4.5 <- read.csv("/Users/Tshepo1/Drive/R/FAS/Foreign_affiliate_purchases", row.names=1)
#write.csv(Foreign_affiliate_purchases4.5, file = temp4, row.names = TRUE)

#splits countries into dataframes

Foreign_affiliate_sales4.6 <- Foreign_affiliate_sales4.5[,!grepl("X.Total", names(Foreign_affiliate_sales4.5))]
Foreign_affiliate_sales4.7 <- t(Foreign_affiliate_sales4.6)
FAS_by_country <- split.data.frame(Foreign_affiliate_sales4.7, Foreign_affiliate_sales4.7[,2])
Foreign_affiliate_sales4.9 <- cbind(row.names(Foreign_affiliate_sales4.6), Foreign_affiliate_sales4.6)
Industry_names <- row.names(Foreign_affiliate_sales4.6)
# problem here, need to dcast
#FAS_by_industry <- split.data.frame(Foreign_affiliate_sales4.9, Foreign_affiliate_sales4.9[,1])
#names(FAS_by_industry) <- Industry_names
#list2env(FAS_by_industry, envir = .GlobalEnv)
  
Country5.1 <- Foreign_affiliate_sales4.6[2,]
Country5.2 <- t(Country5.1)
Country5.3 <- unique(Country5.2, incomparables = FALSE)
Country5.4 <- t(Country5.3)
Country5.4 <- Country5.4
FAS_by_country <- lapply(FAS_by_country, function(x){
  t(x)
})
Country5.5 <- paste0(Country5.4, ".sales")
names(FAS_by_country) <- Country5.5
list2env(FAS_by_country, envir = .GlobalEnv)

Foreign_affiliate_purchases4.6 <- Foreign_affiliate_purchases4.5[,!grepl("X.Total", names(Foreign_affiliate_purchases4.5))]
Foreign_affiliate_purchases4.7 <- t(Foreign_affiliate_purchases4.6)
FAP_by_country <- split.data.frame(Foreign_affiliate_purchases4.7, Foreign_affiliate_purchases4.7[,2])
Foreign_affiliate_purchases4.9 <- cbind(row.names(Foreign_affiliate_purchases4.6), Foreign_affiliate_purchases4.6)
Industry_names2 <- row.names(Foreign_affiliate_purchases4.6)
#problem here - need to dcast
#Industry_ID <- colnames(Foreign_affiliate_purchases4.9)

#Foreign_affiliate_purchases4.9.1 <- rbind(Industry_ID, Foreign_affiliate_purchases4.9)
#Foreign_affiliate_purchases5.1 <- dcast(Foreign_affiliate_purchases4.9, Country ~ Year, value.var = "row.names(Foreign_affiliate_purchases4.6)", fill=0)
#FAP_by_industry <- split.data.frame(Foreign_affiliate_purchases4.9, Foreign_affiliate_purchases4.9[,1])
#names(FAP_by_industry) <- Industry_names2
#list2env(FAP_by_industry, envir = .GlobalEnv)

Country6.1 <- Foreign_affiliate_purchases4.6[2,]
Country6.2 <- t(Country6.1)
Country6.3 <- unique(Country6.2, incomparables = FALSE)
Country6.4 <- t(Country6.3)
Country6.4 <- Country6.4
FAP_by_country <- lapply(FAP_by_country, function(x){
  t(x)
})
Country6.6 <- paste0(Country6.4, ".purchases")
names(FAP_by_country) <- Country6.6
list2env(FAP_by_country, envir = .GlobalEnv)


# calculate growth and CAGR after split - something wrong with formula - it's the transpose function it changes numbers to factors
periods <- end_year - start_year -1
Cagr_end <- end_year-1

CAGR_name <- paste0("CAGR ", start_year, " to ", Cagr_end)
Year_to_year_name <- paste0("Growth from ", Cagr_end, " to ", end_year)

Countries8 <- c(Country6.6,Country5.5)

function.format.aggregate.calculate <- function(df) {
  function.trans <- function(df) {
    n <- row.names(df)
    n <- gsub("[^0-9]", "", n)
    #df <- as.data.frame(t(df[,-1]))
    df <- transpose(df)
    colnames(df) <- n
    df <- as.data.frame(lapply(df, as.numeric))
    #df$myfactor <- factor(row.names(df))

    
  }
  
  function.cagr <- function(df,col_name1,Cagr_end,start_year,periods) {
    df[[col_name1]] <- ((df[,1]/df[,ncol(df)])^(1/(periods-1)))-1
    df
  }
  
  function.growth <- function(df,col_name2,end_year,Cagr_end) {
    df[[col_name2]] <- (df[,ncol(df)]-df[,(ncol(df)-1)])/df[,(ncol(df)-1)]
    df
  }
  
  
  CAGR <- function.cagr(df, CAGR_name, CAGR_name,start_year,periods)
  Year_on_Year <- function.growth(df, Year_to_year_name, end_year, CAGR_name)
  
  df <- cbind(CAGR, Year_on_Year)
  df <- df %>% subset(., select=which(!duplicated(names(.)))) 
  
  
  final.countries <- data.frame()
}

setwd("/Users/Tshepo1/Drive/R/FAS/Data")
sapply(names(FAP_by_country), 
       function (x) write.csv(FAP_by_country[[x]], file=paste(x, "csv", sep=".")))
sapply(names(FAS_by_country), 
       function (x) write.csv(FAS_by_country[[x]], file=paste(x, "csv", sep=".")))
#___________________________________________________________________________________

sapply(names(FAS_by_industry), 
       function (x) write.csv(FAS_by_industry[[x]], file=paste(x, "csv", sep=".")))
sapply(names(FAP_by_industry), 
       function (x) write.csv(FAP_by_industry[[x]], file=paste(x, "csv", sep=".")))

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

#works to here - saves to folder, still want to run growth 

for (i in 1:length(FAP_by_country)) {
  
  arg1 <- "function.format.aggregate.calculate(" 
  arg2 <- paste0(arg1,FAP_by_country[[i]],")", sep="")
  final.countries1 <- eval(parse(text=arg2))
  final.countries2 <- cbind(FAP_by_country[[i]], final.countries1)
  final.countries <- rbind(final.countries2, final.countries)
}



#for NA - need to split out countries first because every row has an NA somewhere otherwise - affiliate.data.na is copy with NAs

na_test5 <- affiliate.data.na[rowSums(is.na(affiliate.data.na)) > 0,] 
na_test6 <- affiliate.data.na2[rowSums(is.na(affiliate.data.na2)) > 0,] 



