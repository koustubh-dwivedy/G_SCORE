#	FILENAME: code.r
#	DESCRIPTION: Mohanram's G_Score Trading Strategy Implementation for Indian Firms
#	PIPELINE: ProwessIQ (Data Set Download )-> Enter parameters in "USE SPACE" in "code.r" -> Run "code.r"
#	AUTHOR: Koustubh Dwivedy
#	START DATE: 18 Dec 2016


# Before running this code, install the following packages: (uncomment to install)

	# install.packages("readxl")
	# install.packages("quantmod")
	# install.packages("PerformanceAnalytics")
	# install.packages("xts")

# Before running this code, ensure that date downloaded from ProwessDx is in dd-mm-yyyy format and currency is INR million
# Before running this code, ensure that you have the data of [start_year - 4, start_year + 2] (for variance calculation and validation)

########
# NOTE #
########
# I am assuming that in file_1, the pattern of: "Net sales, Total income net of P&E" repeats and that too continuously (difference of 2 between consecutive "Net sales" elements and "Total income net of P&E" elements)

# To run this code, you need to enter names of 2 .xlsx files
#	"file_1" corresponds to the file containing the following (this is got from ProwessIQ):
		# Prowess company code
		# Industry group code
		# Net sales
		# Total income net of P&E (this is quarterly data)
		# Net cash flow from operating activities
		# Research & development expenses
		# Advertising expenses
		# Rent & lease rent
		# Repairs & maintenance
		# Depreciation (net of transfer from revaluation reserves)
		# Adjusted Closing Price
		# BV per Share
#	"file_2" corresponds to the file containing the following (this is got from ProwessDX):
	# This file only contains info about Capital Expenditure which was unavailable in ProwessIQ
		# cas_qsegbrk_cocode
		# cas_company_name
		# cas_qsegbrk_info_type
		# cas_seg_date
		# cas_seg_name
		# cas_seg_cap_exp
		# cas_unallocable_cap_exp_seg
#	"file_3" corresponds to the file containing the following (this is got from ProwessDX):
	# This file only contains info about Annual Average Assets
		# ca_finance1_cocode
		# ca_company_name
		# ca_finance1_year
		# ca_months
		# ca_avg_total_assets_net_of_reval

##############
# PSEUDOCODE #
##############
# 1. Take input of following from user:
#		filename (both files)
#		NSE/BSE price preference
#		start year, end year (= start year + 1) and validation year (= end year + 1)
# 2. Remove top 4 rows AND price columns not containing data of user input preference
# 3. From the 5th column, extract date_start and date_end by creating an ARRAY of all the values of dates in the data
# 4. Handling missing values
# 5. Find all indicators
# how to arrange data?
# how to read excel?
##################
# PSEUDOCODE END #
##################

rm(list=ls())

##############
# USER SPACE #
##############

# your working directory
# dir = "E:/Subjects/Winter_2016/G_SCORE"
dir = "C:/Users/TradingLab15/Desktop/G_SCORE"
# file_1
file_1 = "mohanram_data_prowessIQ.xlsx"
# file_2
file_2 = "mohanram_data_capital_exp.xlsx"
# file_3
file_3 = "mohanram_data_avg_assets.xlsx"
# BSE or NSE stock prices. If NSE = FALSE, then BSE prices will be taken
NSE = TRUE
# start_year (the G_SCORE will be calculated for the fiscal year (start_year<->start_year+1) and validation will be done on (start_year+1<->start_year+2))
start_year = 2014
##################
# USER SPACE END #
##################


setwd(dir)
library(readxl)
library(quantmod)
library(PerformanceAnalytics)
library(xts)

data_file_1<-read_excel(file_1)
num_var_file_1<-dim(data_file_1)[2]

# Indicators: NA: 0, BSE: 1, NSE: 2, Anything else: 4
exchange_file_1<-vector(length = num_var_file_1)
flag<-c()
for (i in 1:num_var_file_1) {
	if(is.na(data_file_1[1, i])){
		exchange_file_1[i]<-0
		} else{
			if(data_file_1[1, i] == "NSE"){
				exchange_file_1[i]<-2
				} else {
					if(data_file_1[1, i] == "BSE"){
						exchange_file_1[i]<-1
					} else{
						exchange_file_1[i]<-4
						flag<-c(flag, "warning line 118")
					}
				}
			}
}

temp<-vector(length = num_var_file_1)
for (i in 1:num_var_file_1) {
	temp[i]<-"date"
}
data_file_1<-read_excel(file_1, col_types = temp)
dates_file_1<-data_file_1[4,]


data_file_1<-read_excel(file_1, skip = 5)

# Collecting G1

# Find the first column containing data of year=start_year
# Extract and sum the "Total income net of P&E" from this + the next 3 (corresponding) columns (because the data is quarterly)
temp = 0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == start_year)){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == start_year) && (names(data_file_1)[i] == "Total income net of P&E")){
			temp = i
			break
		}
	}
}

G_SCORE.table<-data_file_1[,1:3]
G_SCORE.table$Annual_Net_Income<-(data_file_1[, temp+2] + data_file_1[, temp+4] + data_file_1[, temp+6] + data_file_1[, temp+8])

# Collecting G4
# Earnings Variability
G_SCORE.table$Earnings_Variability<-0
for(i in 1:dim(data_file_1)[1]){
	G_SCORE.table$Earnings_Variability[i]=var(c(data_file_1[i, temp-22], data_file_1[i, temp-20], data_file_1[i, temp-18], data_file_1[i, temp-16], data_file_1[i, temp-14], data_file_1[i, temp-12], data_file_1[i, temp-10], data_file_1[i, temp-8], data_file_1[i, temp-6], data_file_1[i, temp-4], data_file_1[i, temp-2], data_file_1[i, temp], data_file_1[i, temp+2], data_file_1[i, temp+4], data_file_1[i, temp+6], data_file_1[i, temp+8]), na.rm = TRUE)
}


# Collecting G5
# Sales Variability
temp = 0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == start_year)){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == start_year) && (names(data_file_1)[i] == "Net sales")){
			temp = i
			break
		}
	}
}

G_SCORE.table$Sales_Variability<-0
for(i in 1:dim(data_file_1)[1]){
	G_SCORE.table$Sales_Variability[i]=var(c(data_file_1[i, temp-20] - data_file_1[i, temp-22], data_file_1[i, temp-18] - data_file_1[i, temp-20], data_file_1[i, temp-16] - data_file_1[i, temp-18], data_file_1[i, temp-14] - data_file_1[i, temp-16], data_file_1[i, temp-12] - data_file_1[i, temp-14], data_file_1[i, temp-10] - data_file_1[i, temp-12], data_file_1[i, temp-8] - data_file_1[i, temp-10], data_file_1[i, temp-6] - data_file_1[i, temp-8], data_file_1[i, temp-4] - data_file_1[i, temp-6], data_file_1[i, temp-2] - data_file_1[i, temp-4], data_file_1[i, temp] - data_file_1[i, temp-2], data_file_1[i, temp+2] - data_file_1[i, temp], data_file_1[i, temp+4] - data_file_1[i, temp+2], data_file_1[i, temp+6] - data_file_1[i, temp+4], data_file_1[i, temp+8] - data_file_1[i, temp+6]), na.rm = TRUE)
}


# Collecting G6
temp = 0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year + 1))){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year + 1)) && (names(data_file_1)[i] == "Research & development expenses")){
			temp = i
			break
		}
	}
}
G_SCORE.table$RnD<-data_file_1[, temp]


# Collecting G8
temp = 0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year + 1))){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year + 1)) && (names(data_file_1)[i] == "Advertising expenses")){
			temp = i
			break
		}
	}
}
G_SCORE.table$AdEx<-data_file_1[, temp]

# Collecting G2
temp = 0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year + 1))){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year + 1)) && (names(data_file_1)[i] == "Net cash flow from operating activities")){
			temp = i
			break
		}
	}
}

G_SCORE.table$Net_Cash_Flow<-(data_file_1[, temp])


# Collecting G1
data_file_3<-read_excel(file_3)

avg_tot_asset_data<-data.frame(cocode=integer(),
	avg_tot_asset=numeric(),
	stringsAsFactors=FALSE)

temp = 1
while(1){
	if(temp <= dim(data_file_3)[1]){
			if((format(as.Date(data_file_3$ca_finance1_year[temp], format = "%d-%m-%Y"), "%Y") == (start_year + 1)) && (data_file_3$ca_months[temp] == 12)){
				avg_tot_asset_data<-rbind(avg_tot_asset_data, c(data_file_3$ca_finance1_cocode[temp], as.numeric(data_file_3$ca_avg_total_assets_net_of_reval[temp])))
			}
			temp = temp + 1
		} else{
			break
		}
}
names(avg_tot_asset_data)[1:2]<-c("Prowess company code", "avg_tot_asset")
G_SCORE.table<-merge(G_SCORE.table, avg_tot_asset_data)

# Collecting G7
data_file_2<-read_excel(file_2)

capex_alloc_data<-data.frame(cocode=integer(),
	capex_alloc=numeric(),
	stringsAsFactors=FALSE)
temp = 1
while(1){
	if(temp <= dim(data_file_2)[1]){
			if((format(as.Date(data_file_2$cas_seg_date[temp], format = "%d-%m-%Y"), "%Y") == (start_year + 1)) && (data_file_2$cas_seg_name[temp] == "ALL SEGMENTS")){
				capex_alloc_data<-rbind(capex_alloc_data, c(data_file_2$cas_qsegbrk_cocode[temp], as.numeric(data_file_2$cas_seg_cap_exp[temp])))
			}
			temp = temp + 1
		} else{
			break
		}
}
names(capex_alloc_data)[1:2]<-c("Prowess company code", "capex_alloc")
G_SCORE.table<-merge(G_SCORE.table, capex_alloc_data)

capex_unalloc_data<-data.frame(cocode=integer(),
	capex_unalloc=numeric(),
	stringsAsFactors=FALSE)
temp = 1
while(1){
	if(temp <= dim(data_file_2)[1]){
			if((format(as.Date(data_file_2$cas_seg_date[temp], format = "%d-%m-%Y"), "%Y") == (start_year + 1)) && (data_file_2$cas_seg_name[temp] == "ALL SEGMENTS")){
				capex_unalloc_data<-rbind(capex_unalloc_data, c(data_file_2$cas_qsegbrk_cocode[temp], as.numeric(data_file_2$cas_unallocable_cap_exp_seg[temp])))
			}
			temp = temp + 1
		} else{
			break
		}
}
names(capex_unalloc_data)[1:2]<-c("Prowess company code", "capex_unalloc")
G_SCORE.table<-merge(G_SCORE.table, capex_unalloc_data)

