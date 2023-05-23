library(RcmdrMisc)
library(effects)
library(ggplot2)
library(reshape)
library(plyr)
library(rlang)
library(foreign)
library(Rcmdr)
library(splines)
covid<- read.csv(file.choose(), header= TRUE) 
covid
cases<-covid$CASES
cases

#total number of cases in INDIA
totalcases<- sum(cases)/2
totalcases

#SETTING PARTY VECTOR AS FACTOR
covid$party<- factor(covid$party, levels = c("nda","non nda","union territory","flipped","GOI"))
levels(covid$party)

# ADDING RECOVERY RATE VARIABLE FOR STATES TO DATAFRAME
covid$recoveryrate<- covid$RECOVERIES/covid$positive*100
covid$recoveryrate

# ADDING MORTALITY RATE VARIABLE FOR STATES TO DATA FRAME
covid$mortalityrate<- covid$DEATHS/covid$positive*100
covid$mortalityrate

# ADDING TESTING RATE VARIABLE FOR STATES TO DATAFRAME
covid$testingrate<- covid$tests/covid$population*1000000

# TOTAL POPULATION OF INDIA
indiapopulation<-sum(covid$population/2)
indiapopulation

#ADDING INFECTION RATE VARIABLE FOR STATES TO DATAFRAME
covid$infectionrate<- covid$positive/covid$tests*1000000
covid$infectionrate

#SETTING STATECODE VARIABLE AS FACTOR
covid$statecode<- factor(covid$statecode,levels = covid$statecode)
levels(covid$statecode)

# CALCULATING SHARE OF CASES FOR STATES
covid$sharecases<- covid$CASES/totalcases*100
covid$sharecases

# total deaths in INDIA
totaldeath<- sum(covid$DEATHS)/2
totaldeath

#adding  share of death rates for states variable to dataframe
covid$sharedeath<- covid$DEATHS/totaldeath*100
covid$sharedeath

#sorting data frame acording to caseshare
covidcasesharesorted<-covid[order( covid[ ,grep("sharecases", colnames(covid))] ),]
covidcasesharesorted
covidcasesharesorted$statecode<- factor(covidcasesharesorted$statecode,levels = covidcasesharesorted$statecode)
levels(covidcasesharesorted$statecode)
covidcasesharesorted$party<- factor(covidcasesharesorted$party, levels=c("nda","non nda","union territory","flipped","GOI"))
levels(covidcasesharesorted$party)

# share of cases per state
sharecaseshistogram <- ggplot(covidcasesharesorted,aes(statecode,sharecases, fill=party))
sharecaseshistogram
sharecaseshistogram+stat_summary(geom = "bar")+ scale_fill_discrete(name = "Party in power", labels = c("NDA", "NON-NDA","UNION TERRITORY","flipped", "CENTRAL GOVERNMENT"))+  labs(y="PERCENT SHARE OF TOTAL CASES", x = "STATE/UT CODES")+ggtitle("STATE WISE CASE SHARES(%)")+scale_fill_manual(values=c("Orange", "Green", "Blue","Black","Brown"))


#sorting data fram acording to mortalityrate
covidmortalityratesorted<-covid[order( covid[ ,grep("mortalityrate", colnames(covid))] ),]
covidmortalityratesorted
covidmortalityratesorted$statecode<- factor(covidmortalityratesorted$statecode,levels = covidmortalityratesorted$statecode)
levels(covidmortalityratesorted$statecode)
covidmortalityratesorted$party<- factor(covidmortalityratesorted$party, levels=c("nda","non nda","union territory","flipped","GOI"))
levels(covidmortalityratesorted$party)

#mortality rates of states sorted
mortalityratehistogram<-ggplot(covidmortalityratesorted, aes(statecode,mortalityrate,fill=party))
mortalityratehistogram
mortalityratehistogram+stat_summary(geom = "bar")+scale_fill_discrete(name = "Party in power", labels = c("NDA", "NON-NDA","UNION TERRITORY","flipped", "CENTRAL GOVERNMENT")) + labs(y="MORTALITY RATE AS % OF CASES DETECTED", x = "STATE/UT CODES")+ggtitle("STATE/UT WISE MORTALITY RATES(%)")+scale_fill_manual(values=c("Orange", "Green", "Blue","Black","Brown"))

# share of mortalities sorted
covidmortalitysharesorted<-covid[order( covid[ ,grep("sharedeath", colnames(covid))] ),]
covidmortalitysharesorted
covidmortalitysharesorted$statecode<- factor(covidmortalitysharesorted$statecode,levels = covidmortalitysharesorted$statecode)
levels(covidmortalitysharesorted$statecode)
covidmortalitysharesorted$party<- factor(covidmortalitysharesorted$party, levels=c("nda","non nda","union territory","flipped","GOI"))
levels(covidmortalitysharesorted$party)

#share of mortalities per state
sharedeathhistogram <- ggplot(covidmortalitysharesorted, aes(statecode,sharedeath,fill=party))
sharedeathhistogram
sharedeathhistogram+stat_summary(geom = "bar")+scale_fill_discrete(name = "Party in power", labels = c("NDA", "NON-NDA","UNION TERRITORY","flipped", "CENTRAL GOVERNMENT"))+ labs(y="PERCENT SHARE OF TOTAL DEATHS ", x = "STATE/UT CODES")+ggtitle("STATE WISE SHARES OF TOTAL NATIONAL MORTALITY(%)")+scale_fill_manual(values=c("Orange", "Green", "Blue","Black","Brown"))

# infection rates of states sorted
covidinfectionratesorted<-covid[order( covid[ ,grep("infectionrate", colnames(covid))] ),]
covidinfectionratesorted
covidinfectionratesorted$statecode<- factor(covidinfectionratesorted$statecode,levels = covidinfectionratesorted$statecode)
levels(covidinfectionratesorted$statecode)
covidinfectionratesorted$party<- factor(covidinfectionratesorted$party, levels=c("nda","non nda","union territory","flipped","GOI"))
levels(covidinfectionratesorted$party)

#infection rate of all states where covid affected 
infectionratehistogram<-ggplot(covidinfectionratesorted,aes(statecode,infectionrate,fill=party))
infectionratehistogram
infectionratehistogram+stat_summary(geom = "bar")+stat_summary(geom = "bar")+ scale_fill_discrete(name = "Party in power", labels = c("NDA", "NON-NDA","UNION TERRITORY","flipped", "CENTRAL GOVERNMENT"))+ labs(y="INFECTION RATE AS % OF TESTS CONDUCTED", x = "STATE/UT CODES")+ggtitle("STATE/UT WISE INFECTION RATES")+scale_fill_manual(values=c("Orange", "Green", "Blue","Black","Brown"))

#sorting testing rates of all states
covidtestingratesorted<-covid[order( covid[ ,grep("testingrate", colnames(covid))] ),]
covidtestingratesorted
covidtestingratesorted$statecode<- factor(covidtestingratesorted$statecode,levels = covidtestingratesorted$statecode)
levels(covidtestingratesorted$statecode)
covidtestingratesorted$party<- factor(covidtestingratesorted$party, levels=c("nda","non nda","union territory","flipped","GOI"))
levels(covidtestingratesorted$party)

# testing rate of all states where data is collected
testingratehistogram<-ggplot(covidtestingratesorted, aes(statecode,testingrate,fill=party))
testingratehistogram
testingratehistogram+stat_summary(geom = "bar")+stat_summary(geom = "bar")+ scale_fill_discrete(name = "Party in power", labels = c("NDA", "NON-NDA","UNION TERRITORY","flipped", "CENTRAL GOVERNMENT"))+ labs(y="TESTING RATE PER MILLION", x = "STATE/UT CODES")+ggtitle("STATE/UT WISE TESTING RATES")+scale_fill_manual(values=c("Orange", "Green", "Blue","Black","Brown"))    

#sorting recovery rates of states
covidrecoveryratesorted<-covid[order( covid[ ,grep("recoveryrate", colnames(covid))] ),]
covidrecoveryratesorted
covidrecoveryratesorted$statecode<- factor(covidrecoveryratesorted$statecode,levels = covidrecoveryratesorted$statecode)
levels(covidrecoveryratesorted$statecode)
covidrecoveryratesorted$party<- factor(covidrecoveryratesorted$party, levels=c("nda","non nda","union territory","flipped","GOI"))
levels(covidrecoveryratesorted$party)
#recovery rate sorted
recoveryratehistogram<-ggplot(covidrecoveryratesorted, aes(statecode,recoveryrate,fill=party))
recoveryratehistogram
recoveryratehistogram+stat_summary(geom = "bar")+stat_summary(geom = "bar")+ scale_fill_discrete(name = "Party in power", labels = c("NDA", "NON-NDA","UNION TERRITORY","flipped", "CENTRAL GOVERNMENT"))+ labs(y="RECOVERY RATE AS % OF INFECTIONS", x = "STATE/UT CODES")+ggtitle("STATE WISE RECOVERY RATES AS % OF CASES")+scale_fill_manual(values=c("Orange", "Green", "Blue","Black","Brown"))    
