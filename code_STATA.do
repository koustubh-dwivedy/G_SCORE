clear
import excel "D:\Mohanram\datatest1.xls", sheet("Sheet1") firstrow


local member = 15

if `member' == 15 {
	gen BV0 = BVperShare0614
	gen AP0 = AdjustedClosingPrice0614
	gen TI  = TotalincomenetofPE0315
	gen ATA = Averagetotalassets15
	gen CFO = CFO0315
	gen CP0 = AdjustedClosingPrice0615
	gen CP1 = AdjustedClosingPrice0616
	gen RnD = RnD0315
	gen CPX = Dep0315 + Repairsmaintenance0315 + Rentleaserent0315
	egen d1 = rownonmiss(Netsales0911 Netsales0611 Netsales0311 Netsales1212 Netsales0912 Netsales0612 Netsales0312 Netsales1213 Netsales0913 Netsales0613 Netsales0313 Netsales1214 Netsales0914 Netsales0614 Netsales0314 Netsales0315)
	drop if d1[1] < 6 
	gen sg1 = Netsales0911-Netsales0611
	gen sg2 = Netsales0611-Netsales0311
	gen sg3 = Netsales1212-Netsales0912
	gen sg4 = Netsales0912-Netsales0612
	gen sg5 = Netsales0612-Netsales0312
	gen sg6 = Netsales0312-Netsales1211
	gen sg7 = Netsales1213-Netsales0913
	gen sg8 = Netsales0913-Netsales0613
	gen sg9 = Netsales0613-Netsales0313
	gen sg10 = Netsales0313-Netsales1212
	gen sg11 = Netsales1214-Netsales0914
	gen sg12 = Netsales0914-Netsales0614
	gen sg13 = Netsales0614-Netsales0314
	gen sg14 = Netsales0314-Netsales1213
	gen sg15 = Netsales0315-Netsales1214
	egen SGV = rowsd(sg1 sg2 sg3 sg4 sg5 sg6 sg7 sg8 sg9 sg10 sg11 sg12 sg13 sg14 sg15)
	egen EGV = rowsd(Averagetotalassets15 Averagetotalassets14 Averagetotalassets13 Averagetotalassets12)
	gen ADS = Advertisingexpenses0315
}
 

drop if missing(Industrygroupcode)

gen BM = BV0/AP0
drop if BM<0
pctile m = BM,nq(10)
drop if BM>m[2] 


gen median1 =0
gen median2 =0
gen median4 =0
gen median5 =0
gen median6 =0
gen median7 =0
gen median8 =0
gen ROA2= CFO/ATA
gen ROA1= TI/ATA

drop if missing( ROA1)
drop if missing(ROA2)
drop if missing( CFO)
drop if missing(TI)
drop if missing( SGV)
drop if missing( EGV)
replace RnD = 0 if missing(RnD)
drop if missing( CPX)
replace ADS = 0 if missing(ADS)

//CALCULATION OF G1
egen v1 = count(Prowesscompanycode)
tempname y
scalar `y' = v1[1]
sort Industrygroupcode ROA1

forvalues i = 1(1)`=`y'' {
	local c = 0
	local k = 0
	forvalues j = 1(1)`=`y'' {
		if Industrygroupcode[`i'] == Industrygroupcode[`j'] {
			local c =`c'+1
 			if ROA1[`i'] == ROA1[`j'] {
				local k = `c'
			}
		}
		
	}
	replace median1 = 0.5*(ROA1[`i'-`k'+(1+`c')/2]+ ROA1[`i'-`k'+`c'/2]) in `i'
}
gen g1 = ROA1>=median1 
//CALCULATION OF G2

sort Industrygroupcode ROA2

forval i= 1(1)`=`y'' {
	local c = 0
	local k = 0
	forval j= 1(1)`=`y'' {
		if Industrygroupcode[`i']==Industrygroupcode[`j']{
			local c =`c'+1
			if ROA2[`i'] == ROA2[`j'] {
				local k = `c'
			}
		}		
	}
	replace median2 = 0.5*(ROA2[`i'-`k'+(1+`c')/2]+ ROA2[`i'-`k'+`c'/2]) in `i'
}
gen g2 = ROA2>=median2
//CALCULATION OF G3

gen g3 = CFO > TI

//CALCULAITON OF G5

sort Industrygroupcode SGV
forval i= 1(1)`=`y'' {
	local c = 0
	local k = 0
	forval j= 1(1)`=`y'' {
		if Industrygroupcode[`i']==Industrygroupcode[`j']{
			local c =`c'+1
			if SGV[`i'] == SGV[`j'] {
				local k = `c'
			}
		}
	}
	replace median5 = 0.5*(SGV[`i'-`k'+(1+`c')/2]+ SGV[`i'-`k'+`c'/2]) in `i'
}
gen g5 = SGV <= median5


//CALCULATION OF G4

sort Industrygroupcode EGV
forval i= 1(1)`=`y'' {
	local c = 0
	local k = 0
	forval j= 1(1)`=`y'' {
		if Industrygroupcode[`i']==Industrygroupcode[`j']{
			local c =`c'+1
			if EGV[`i'] == EGV[`j'] {
				local k = `c'
			}
		}
	}
	replace median4 = 0.5*(EGV[`i'-`k'+(1+`c')/2]+ EGV[`i'-`k'+`c'/2]) in `i'
}
gen g4 = EGV<=median4

//CALCULATION OF G6
sort Industrygroupcode RnD

forvalues i = 1(1)`=`y'' {
	local c = 0
	local k = 0
	forvalues j = 1(1)`=`y'' {
		if Industrygroupcode[`i']==Industrygroupcode[`j']{
			local c =`c'+1
			if RnD[`i'] == RnD[`j'] {
				local k = `c'
			}
		}
	}
	replace median6 = 0.5*(RnD[`i'-`k'+(1+`c')/2]+ RnD[`i'-`k'+`c'/2]) in `i'
}
gen g6 = RnD>=median6

//CALCULATION OF G8

sort Industrygroupcode CPX
forval i= 1(1)`=`y'' {
	local c = 0
	local k = 0
	forval j= 1(1)`=`y'' {
		if Industrygroupcode[`i']==Industrygroupcode[`j']{
			local c =`c'+1 
			if CPX[`i'] == CPX[`j'] {
				local k = `c'
			}
			
		}
	}
	di `c'
	replace median8 = 0.5*(CPX[`i'-`k'+(1+`c')/2]+ CPX[`i'-`k'+`c'/2]) in `i'
}
gen g8 = CPX>=median8

//CALCULATION OF G7
sort Industrygroupcode ADS
forval i= 1(1)`=`y'' {
	local c = 0
	local k = 0
	forval j= 1(1)`=`y''{
		if Industrygroupcode[`i']==Industrygroupcode[`j']{
			local c =`c'+1
			if ADS[`i'] == ADS[`j'] {
				local k = `c'
			}
		}
	}
	replace median7 = 0.5*(ADS[`i'-`k'+(1+`c')/2]+ ADS[`i'-`k'+`c'/2]) in `i'
}
gen g7 = ADS > median7

// G_SCORE CALCULATION
drop if missing(median1)
drop if missing(median8)
drop if missing(median2)
drop if missing(median4)
drop if missing(median5)
drop if missing(median6)
drop if missing(median7)

gen g_score= g1+g2+g3+g4+g5+g6+g7+g8

//CALCULATION OF RETURNS

gen returns = CP1/CP0 -1 

keep CompanyName Industrygroupcode  BM g_score g2 g1 g3 g5 g4 g6 g8 g7 returns
sort g_score

gen group1="High" if g_score>=6
replace group1="Others" if g_score<=5
ttest returns, by(group1)

gen group2="High" if g_score>=6
replace group2="Low" if g_score<2
ttest returns, by(group2)
