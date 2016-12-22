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
# I am assuming that in file_1, "Adjusted Closing Price " and "BV per Share " also form a pattern similar to above

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
NSE = FALSE
# start_year (the G_SCORE will be calculated for the fiscal year (start_year<->start_year+1) and validation will be done on (start_year+1<->start_year+2))
start_year = 2014
##################
# USER SPACE END #
##################


setwd(dir)
library(readxl)
library(quantmod)
library(doBy)
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
						flag<-c(flag, "warning line 120")
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

# Get BM ratio of firms at start_year+1
NSE_or_BSE = 1
if(NSE){
	NSE_or_BSE = 2
}
temp_2 = 0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == start_year)){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == start_year) && (names(data_file_1)[i] == "Adjusted Closing Price ") && (exchange_file_1[i] == NSE_or_BSE)){
			temp_2 = i
			break
		}
	}
}
G_SCORE.table$BM_ratio<-(data_file_1[,temp_2+1]/data_file_1[,temp_2])


num_firms = dim(G_SCORE.table)[1]
num_firms = floor(num_firms)

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









fno_firms = c(23354,5747,86597,5757,98964,8893,11019,12517,86607,15214,18102,18151,21042,22104,22859,24685,256066,33723,374518,28897,30136,30148,31130,33727,384105,33750,33769,33873,34162,373095,36073,149711,38924,40112,41246,371877,41860,42974,43155,43750,47858,48799,49138,50616,52418,52835,59045,53993,54885,59381,2717,61957,62987,66185,68332,71060,72607,79498,76718,82197,83013,83018,85379,85447,88258,88297,91512,92590,18395,93902,94103,94080,94175,95631,95632,96379,100283,512651,100709,96577,373589,97066,36278,98379,370967,369628,98907,99308,99905,100224,100632,109868,109874,106263,108841,109300,109856,62499,400943,122948,117230,118314,122573,385630,125401,127106,130382,132572,136443,136444,140097,140829,70319,146460,149616,153366,155600,348418,155630,155695,163226,163229,97191,164888,384616,177792,168589,174683,175214,177753,177758,183861,196588,369944,196667,27747,196608,201111,204866,225455,226821,228346,231715,236328,236460,237266,239726,239792,245437,246615,248083,265414,164646,248092,248186,248093,248165,248136,136494,251664,252196,372076,371246,215829,125123,257360,320945,142526,216946,269920,272724,272854,354608,275062)
fno<-data.frame(cocode=integer(),
	dummy=numeric(),
	stringsAsFactors=FALSE)
for (i in 1:length(fno_firms)){
	fno<-rbind(fno, c(fno_firms[i], fno_firms[i]))
}
names(fno)[1:2]<-c("Prowess company code", "dummy")
G_SCORE.table<-merge(G_SCORE.table, fno)
G_SCORE.table<-G_SCORE.table[complete.cases(G_SCORE.table$dummy),]
G_SCORE.table<-G_SCORE.table[complete.cases(G_SCORE.table$BM_ratio),]



# Keep bottom 20 percentile firms
G_SCORE.table<-G_SCORE.table[order(G_SCORE.table$BM_ratio, decreasing = FALSE),]
G_SCORE.table<-G_SCORE.table[1:num_firms,]

















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

# Removing data with BM_ratio, Annual_Net_Income, avg_tot_asset and capex_alloc = NA
# Converting NA values of RnD, AdEx and capex_unalloc to 0
# This should give data with no missing values
G_SCORE.table<-G_SCORE.table[complete.cases(G_SCORE.table$BM_ratio),]
G_SCORE.table<-G_SCORE.table[complete.cases(G_SCORE.table$avg_tot_asset),]
G_SCORE.table<-G_SCORE.table[complete.cases(G_SCORE.table$Annual_Net_Income),]
G_SCORE.table<-G_SCORE.table[complete.cases(G_SCORE.table$capex_alloc),]
# The following converts all remaining NA data to 0
G_SCORE.table[is.na(G_SCORE.table)]<-0

G_SCORE.table$ROA1<-G_SCORE.table$Annual_Net_Income/G_SCORE.table$avg_tot_asset
G_SCORE.table$ROA2<-G_SCORE.table$Net_Cash_Flow/G_SCORE.table$avg_tot_asset
G_SCORE.table$G3<-(G_SCORE.table$Net_Cash_Flow > G_SCORE.table$Annual_Net_Income)

# Ordering by BM ratio
G_SCORE.table<-G_SCORE.table[order(G_SCORE.table$`Industry group code`),]


counter = 1
temp = 0
read = 0
ROA1_vec<-c()
ROA2_vec<-c()
earn_var_vec<-c()
sales_var_vec<-c()
rnd_vec<-c()
capex_vec<-c()
advin_vec<-c()

G_SCORE.table$median_ROA1<-0
G_SCORE.table$median_ROA2<-0
G_SCORE.table$median_earn_var<-0
G_SCORE.table$median_sales_var<-0
G_SCORE.table$median_rnd<-0
G_SCORE.table$median_capex<-0
G_SCORE.table$median_advin<-0

while(1){
	if(counter < dim(G_SCORE.table)[1]){
		temp = G_SCORE.table[counter, 3]
		while(1){
			if(!is.na(G_SCORE.table[counter, 3])){
			if(temp == G_SCORE.table[counter, 3]){
				ROA1_vec<-c(ROA1_vec, G_SCORE.table$ROA1[counter])
				ROA2_vec<-c(ROA2_vec, G_SCORE.table$ROA2[counter])
				earn_var_vec<-c(earn_var_vec, G_SCORE.table$Earnings_Variability[counter])
				sales_var_vec<-c(sales_var_vec, G_SCORE.table$Sales_Variability[counter])
				rnd_vec<-c(rnd_vec, G_SCORE.table$RnD[counter])
				capex_vec<-c(capex_vec, (G_SCORE.table$capex_unalloc[counter] + G_SCORE.table$capex_alloc[counter]))
				advin_vec<-c(advin_vec, G_SCORE.table$AdEx[counter])
				counter = counter + 1
				read = read + 1
			} else{
				# Whatever processing you want to do
				m_ROA1 = median(ROA1_vec)
				m_ROA2 = median(ROA2_vec)
				m_earn_var = median(earn_var_vec)
				m_sales_var = median(sales_var_vec)
				m_rnd = median(rnd_vec)
				m_capex = median(capex_vec)
				m_advin = median(advin_vec)
				# Another for loop to save all data
				for(j in 1:read){
					G_SCORE.table$median_ROA1[counter-j] = m_ROA1
					G_SCORE.table$median_ROA2[counter-j] = m_ROA2
					G_SCORE.table$median_earn_var[counter-j] = m_earn_var
					G_SCORE.table$median_sales_var[counter-j] = m_sales_var
					G_SCORE.table$median_rnd[counter-j] = m_rnd
					G_SCORE.table$median_capex[counter-j] = m_capex
					G_SCORE.table$median_advin[counter-j] = m_advin
				}
				# Resetting data
				read = 0
				ROA1_vec<-c()
				ROA2_vec<-c()
				earn_var_vec<-c()
				sales_var_vec<-c()
				rnd_vec<-c()
				capex_vec<-c()
				advin_vec<-c()
				break
			}
		} else{
			break
		}
		}
		} else{
			break
		}
}

G_SCORE.table$G1<-(G_SCORE.table$ROA1 >= G_SCORE.table$median_ROA1)
G_SCORE.table$G2<-(G_SCORE.table$ROA2 >= G_SCORE.table$median_ROA2)
G_SCORE.table$G4<-(G_SCORE.table$Earnings_Variability <= G_SCORE.table$median_earn_var)
G_SCORE.table$G5<-(G_SCORE.table$Sales_Variability <= G_SCORE.table$median_sales_var)
G_SCORE.table$G6<-(G_SCORE.table$RnD >= G_SCORE.table$median_rnd)
G_SCORE.table$G7<-((G_SCORE.table$capex_unalloc + G_SCORE.table$capex_alloc) > G_SCORE.table$median_capex)
G_SCORE.table$G8<-(G_SCORE.table$AdEx >= G_SCORE.table$median_advin)

G_SCORE.table$G_SCORE<-(G_SCORE.table$G8 + G_SCORE.table$G7 + G_SCORE.table$G6 + G_SCORE.table$G5 + G_SCORE.table$G4 + G_SCORE.table$G3 + G_SCORE.table$G2 + G_SCORE.table$G1)



RESULTS<-G_SCORE.table[, 1:2]
RESULTS$G_SCORE<-G_SCORE.table$G_SCORE

temp_2=0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year+1))){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year+1)) && (names(data_file_1)[i] == "Adjusted Closing Price ") && (exchange_file_1[i] == NSE_or_BSE)){
			temp_2 = i
			break
		}
	}
}
price_i<-data_file_1[,temp_2]

temp_2=0
for (i in 1:num_var_file_1){
	if(!is.na(format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year+2))){
		if((format(as.Date(dates_file_1[1, i], format="%y-%m-%d"),"%Y") == (start_year+2)) && (names(data_file_1)[i] == "Adjusted Closing Price ") && (exchange_file_1[i] == NSE_or_BSE)){
			temp_2 = i
			break
		}
	}
}
price_f<-data_file_1[,temp_2]

ret<-data.frame(cocode=integer(),
	RETURNS=numeric(),
	stringsAsFactors=FALSE)

temp<-(price_f/price_i - 1)

for (i in 1:dim(data_file_1)[1]){
	ret<-rbind(ret, c(data_file_1$`Prowess company code`[i], temp[i]))
}
names(ret)[1:2]<-c("Prowess company code", "RETURNS")

RESULTS<-merge(RESULTS, ret)

RESULTS<-RESULTS[complete.cases(RESULTS$RETURNS),]
summaryBy(RETURNS~G_SCORE, data=RESULTS, FUN=c(mean,var,length))

RESULTS$CLASS<-0
for (i in 1:dim(RESULTS)[1]){
	if((RESULTS$G_SCORE[i]  == 0) || (RESULTS$G_SCORE[i]  == 1) || (RESULTS$G_SCORE[i]  == 2)){
		RESULTS$CLASS[i]<-"LOW"
	}
	if((RESULTS$G_SCORE[i]  == 3) || (RESULTS$G_SCORE[i]  == 4) || (RESULTS$G_SCORE[i]  == 5)){
		RESULTS$CLASS[i]<-"MED"
	}
	if((RESULTS$G_SCORE[i]  == 6) || (RESULTS$G_SCORE[i]  == 7) || (RESULTS$G_SCORE[i]  == 8)){
		RESULTS$CLASS[i]<-"HIGH"
	}
}
summaryBy(RETURNS~CLASS, data=RESULTS, FUN=c(mean,var,length))
