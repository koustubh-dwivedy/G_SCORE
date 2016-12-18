clear
import excel "D:\Mohanram\Data.xlsx", sheet("Sheet1") firstrow

global member current_year 16

if curremt_year == 16 {
	gen BV0 = 
	gen AP0 = 
	gen TI  = 
	gen ATA =
	gen CFO =
	gen CP0 = 
	gen CP1 = 

}
 

drop if missing(Industrygroupcode)

if Industrygroupcode==Industrygroupcode[_n-1]{

}
else{

}

gen BM = BV0/AP0
drop if BM<0
//CALCULATION OF G1
gen ROA1= TI/ATA
sort Industrygroupcode ROA1
forval i= 1(1)6000 {
	local count = 0
	forval j= 1(1)6000 {
		if ROA1[`i']==ROA1[`j']{
			`count'=`count'+1
		}
	}
	if `count'%2==0{
		median[i] = ROA1[
}
//CALCULATION OF G2
gen ROA2= CFO/ATA
sort Industrygroupcode ROA2

//CALCULATION OF G3
gen G3 = CFO > TI

//CALCULAITON OF G4



//CALCULATION OF G5


//CALCULATION OF G6


//CALCULATION OF G7


//CALCULATION OF G8


// G_SCORE CALCULATION

gen g_score= g1+g2+g3+g3+g4+g5+g6+g7+g8

//CALCULATION OF RETURNS

gen returns = CP1/CP0 -1 
