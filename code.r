#	FILENAME: code.r
#	DESCRIPTION: Mohanram's G_Score Trading Strategy Implementation for Indian Firms
#	PIPELINE: ProwessIQ (Data Set Download )-> Enter parameters in "USE SPACE" in "code.r" -> Run "code.r"
#	AUTHOR: Koustubh Dwivedy
#	START DATE: 18 Dec 2016


# Before running this code, install "quantmod" and "xts"
# Before running this code, create a directory

##############
# PSEUDOCODE #
##############
# 1. Take input of filename from user
# 2. Remove top 4 columns from input NO (need to know if the date is from annual or quarterly dataset (this won't be an issue cause variables in annual and variables in quarterly subset are different (non intersecting)))
# 3. From the 5th column, extract date_start and date_end by creating an ARRAY of all the values of dates in the data
# which exchange's  value to take
# handling missing values
# ask user if she wants to take BSE or NSE prices (in second row of data)
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