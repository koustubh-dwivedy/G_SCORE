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

# Before running this code, ensure that date is in dd-mm-yyyy format and currency is INR million
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
file_1 = "mohanram_data_2.xlsx"
# file_2
file_2 = "mohanram_data_capital_exp.xlsx"
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
						flag<-c(flag, "warning line 101")
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

# Calculating G1

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
G_SCORE.table$Annual_Net_Income<-(data_file_1[, temp] + data_file_1[, temp+2] + data_file_1[, temp+4] + data_file_1[, temp+6])
