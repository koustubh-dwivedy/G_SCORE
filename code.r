#	FILENAME: code.r
#	DESCRIPTION: Mohanram's G_Score Trading Strategy Implementation for Indian Firms
#	PIPELINE: ProwessIQ (Data Set Download )-> Enter parameters in "USE SPACE" in "code.r" -> Run "code.r"
#	AUTHOR: Koustubh Dwivedy
#	START DATE: 18 Dec 2016


# Before running this code, install "quantmod" and "xts"
# Before running this code, create a directory
# To run this code, you need to enter names of 2 .xlsx files
#	"file_1" corresponds to the file containing the following (this is got from ProwessIQ):
		# Prowess company code
		# Industry group code
		# Net sales
		# Total income net of P&E
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


##############
# USER SPACE #
##############

#	User needs to input a .xlsx file and must ensure that date is in dd-mm-yyyy format and currency is INR million

# tick: Ticker of stock
tick = "AMZN"
# start date in yyyy-mm-dd format
date_start = "2010-12-31"
# end date in yyyy-mm-dd format
date_end = "2013-12-31"
# your working directory
dir = "E:/Subjects/Winter_2016/G_SCORE"

##################
# USER SPACE END #
##################