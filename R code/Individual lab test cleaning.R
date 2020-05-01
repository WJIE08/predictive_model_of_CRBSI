library("rockchalk")

setwd("C:\\Users\\jinfrances\\Desktop\\SSHSPH MSC\\Modules Notes\\SPH5104 Healthcare Data Analytics\\Group Assignment\\csv files\\")

#get unique subject ID with first ICU visit
icustay<-read.csv("clinical_icustays.csv")
#check number of unique values
sum(!duplicated(icustay$ICUSTAY_ID))
sum(!duplicated(icustay$SUBJECT_ID))
#Order rows by ascending CHARTTIME
icustay<-icustay[order(icustay$INTIME),]
#Remove duplicate SUBJECT_IDs keeping row with earliest INTIME (get first icustay)
icustay<-icustay[!duplicated(icustay$SUBJECT_ID),]

setwd("C:\\Users\\jinfrances\\Desktop\\SSHSPH MSC\\Modules Notes\\SPH5104 Healthcare Data Analytics\\Group Assignment\\csv files\\Individual Lab tests\\")

#To get list of unique subject with intubations done at or nearest to the point of ICU admission
#read lab test csv
intubated<-read.csv("50812_Intubated.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(intubated)[names(intubated) == 'VALUE'] <- 'INTUBATION'
as.factor(intubated$INTUBATION)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
intubated<- merge(x = intubated, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
intubated$CHARTTIME <- as.POSIXct(intubated$CHARTTIME)
intubated$CHARTDATE <- as.Date(intubated$CHARTTIME)
intubated$INTIME <- as.POSIXct(intubated$INTIME)
intubated$INDATE <- as.Date(intubated$INTIME)
# create an indicator for if labtesttime<=intime
intubated$indicator<-intubated$CHARTDATE<=intubated$INDATE
# count number that meet the above criteria
table(intubated$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
intub<-subset(intubated, indicator=="TRUE")
sum(!duplicated(intub$SUBJECT_ID))
# intubated order rows by descending CHARTDATE
intub<-intub[rev(order(intub$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
intub<-intub[!duplicated(intub$SUBJECT_ID),]
#output new csv
write.csv(intub,"Cleaned\\intubated.csv", row.names = FALSE)

#Ventilator
#To get list of unique subject with ventilation done at or nearest to the point of ICU admission
#read lab test csv
ventilator<-read.csv("50828_Ventilator.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(ventilator)[names(ventilator) == 'VALUE'] <- 'VENTILATOR'
as.factor(ventilator$VENTILATOR)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
ventilator<- merge(x = ventilator, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
ventilator$CHARTTIME <- as.POSIXct(ventilator$CHARTTIME)
ventilator$CHARTDATE <- as.Date(ventilator$CHARTTIME)
ventilator$INTIME <- as.POSIXct(ventilator$INTIME)
ventilator$INDATE <- as.Date(ventilator$INTIME)
# create an indicator for if labtesttime<=intime
ventilator$indicator<-ventilator$CHARTDATE<=ventilator$INDATE
# count number that meet the above criteria
table(ventilator$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
ven<-subset(ventilator, indicator=="TRUE")
sum(!duplicated(ven$SUBJECT_ID))
# order rows by descending CHARTDATE
ven<-ven[rev(order(ven$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
ven<-ven[!duplicated(ven$SUBJECT_ID),]
#output new csv
write.csv(ven,"Cleaned\\ventilator.csv", row.names = FALSE)

#C3
#To get list of unique subject with C3 tested at or nearest to the point of ICU admission
#read lab test csv
C3<-read.csv("50890_C3.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(C3)[names(C3) == 'VALUE'] <- 'C3'
as.numeric(C3$C3)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
C3<- merge(x = C3, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
C3$CHARTTIME <- as.POSIXct(C3$CHARTTIME)
C3$CHARTDATE <- as.Date(C3$CHARTTIME)
C3$INTIME <- as.POSIXct(C3$INTIME)
C3$INDATE <- as.Date(C3$INTIME)
# create an indicator for if labtesttime<=intime
C3$indicator<- C3$CHARTDATE<=C3$INDATE
# count number that meet the above criteria
table(C3$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
C3N<-subset(C3, indicator=="TRUE")
sum(!duplicated(C3N$SUBJECT_ID))
# order rows by descending CHARTDATE
C3N<-C3N[rev(order(C3N$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
C3N<-C3N[!duplicated(C3N$SUBJECT_ID),]
#output new csv
write.csv(C3,"Cleaned\\C3.csv", row.names = FALSE)

#C4
#To get list of unique subject with C4 tested at or nearest to the point of ICU admission
#read lab test csv
C4<-read.csv("50891_C4.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(C4)[names(C4) == 'VALUE'] <- 'C4'
as.numeric(C4$C4)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
C4<- merge(x = C4, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
C4$CHARTTIME <- as.POSIXct(C4$CHARTTIME)
C4$CHARTDATE <- as.Date(C4$CHARTTIME)
C4$INTIME <- as.POSIXct(C4$INTIME)
C4$INDATE <- as.Date(C4$INTIME)
# create an indicator for if labtesttime<=intime
C4$indicator<-C4$CHARTDATE<=C4$INDATE
# count number that meet the above criteria
table(C4$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
C4N<-subset(C4, indicator=="TRUE")
sum(!duplicated(C4N$SUBJECT_ID))
# order rows by descending CHARTDATE
C4N<-C4N[rev(order(C4N$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
C4N<-C4N[!duplicated(C4N$SUBJECT_ID),]
#output new csv
write.csv(C4,"Cleaned\\C4.csv", row.names = FALSE)

#Cholesterol Ratio
#To get list of unique subject with Cholesterol Ratio tested at or nearest to the point of ICU admission
#read lab test csv
CR<-read.csv("50903_CholesterolRatio.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CR)[names(CR) == 'VALUE'] <- 'CR'
as.numeric(CR$CR)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CR<- merge(x = CR, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CR$CHARTTIME <- as.POSIXct(CR$CHARTTIME)
CR$CHARTDATE <- as.Date(CR$CHARTTIME)
CR$INTIME <- as.POSIXct(CR$INTIME)
CR$INDATE <- as.Date(CR$INTIME)
# create an indicator for if labtesttime<=intime
CR$indicator<-CR$CHARTDATE<=CR$INDATE
# count number that meet the above criteria
table(CR$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CRN<-subset(CR, indicator=="TRUE")
sum(!duplicated(CRN$SUBJECT_ID))
# order rows by descending CHARTDATE
CRN<-CRN[rev(order(CRN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CRN<-CRN[!duplicated(CRN$SUBJECT_ID),]
#output new csv
write.csv(CRN,"Cleaned\\CholesterolRatio.csv", row.names = FALSE)

#Cholesterol HDL
#To get list of unique subject with HDL tested at or nearest to the point of ICU admission
#read lab test csv
HDL<-read.csv("50904_HDL.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(HDL)[names(HDL) == 'VALUE'] <- 'HDL'
as.numeric(HDL$HDL)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
HDL<- merge(x = HDL, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
HDL$CHARTTIME <- as.POSIXct(HDL$CHARTTIME)
HDL$CHARTDATE <- as.Date(HDL$CHARTTIME)
HDL$INTIME <- as.POSIXct(HDL$INTIME)
HDL$INDATE <- as.Date(HDL$INTIME)
# create an indicator for if labtesttime<=intime
HDL$indicator<-HDL$CHARTDATE<=HDL$INDATE
# count number that meet the above criteria
table(HDL$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
HDLN<-subset(HDL, indicator=="TRUE")
sum(!duplicated(HDLN$SUBJECT_ID))
# order rows by descending CHARTDATE
HDLN<-HDLN[rev(order(HDLN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
HDLN<-HDLN[!duplicated(HDLN$SUBJECT_ID),]
#output new csv
write.csv(HDLN,"Cleaned\\HDL.csv", row.names = FALSE)

#Cholesterol LDL, measured
#To get list of unique subject with LDL tested at or nearest to the point of ICU admission
#read lab test csv
LDL<-read.csv("50906_LDL.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(LDL)[names(LDL) == 'VALUE'] <- 'LDL'
as.numeric(LDL$LDL)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
LDL<- merge(x = LDL, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
LDL$CHARTTIME <- as.POSIXct(LDL$CHARTTIME)
LDL$CHARTDATE <- as.Date(LDL$CHARTTIME)
LDL$INTIME <- as.POSIXct(LDL$INTIME)
LDL$INDATE <- as.Date(LDL$INTIME)
# create an indicator for if labtesttime<=intime
LDL$indicator<-LDL$CHARTDATE<=LDL$INDATE
# count number that meet the above criteria
table(LDL$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
LDLN<-subset(LDL, indicator=="TRUE")
sum(!duplicated(LDLN$SUBJECT_ID))
# order rows by descending CHARTDATE
LDLN<-LDLN[rev(order(LDLN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
LDLN<-LDLN[!duplicated(LDLN$SUBJECT_ID),]
#output new csv
write.csv(LDLN,"Cleaned\\LDL.csv", row.names = FALSE)

#Cholesterol Total
#To get list of unique subject with Cholesterol Total tested at or nearest to the point of ICU admission
#read lab test csv
TC<-read.csv("50907_CholesterolTotal.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(TC)[names(TC) == 'VALUE'] <- 'TC'
as.numeric(TC$TC)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
TC<- merge(x = TC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
TC$CHARTTIME <- as.POSIXct(TC$CHARTTIME)
TC$CHARTDATE <- as.Date(TC$CHARTTIME)
TC$INTIME <- as.POSIXct(TC$INTIME)
TC$INDATE <- as.Date(TC$INTIME)
# create an indicator for if labtesttime<=intime
TC$indicator<-TC$CHARTDATE<=TC$INDATE
# count number that meet the above criteria
table(TC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
TCN<-subset(TC, indicator=="TRUE")
sum(!duplicated(TCN$SUBJECT_ID))
# order rows by descending CHARTDATE
TCN<-TCN[rev(order(TCN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
TCN<-TCN[!duplicated(TCN$SUBJECT_ID),]
#output new csv
write.csv(TCN,"Cleaned\\CholesterolTotal.csv", row.names = FALSE)

#Cortisol
#To get list of unique subject with Cortisol tested at or nearest to the point of ICU admission
#read lab test csv
Cortisol<-read.csv("50909_Cortisol.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(Cortisol)[names(Cortisol) == 'VALUE'] <- 'Cortisol'
as.numeric(Cortisol$Cortisol)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
Cortisol<- merge(x = Cortisol, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
Cortisol$CHARTTIME <- as.POSIXct(Cortisol$CHARTTIME)
Cortisol$CHARTDATE <- as.Date(Cortisol$CHARTTIME)
Cortisol$INTIME <- as.POSIXct(Cortisol$INTIME)
Cortisol$INDATE <- as.Date(Cortisol$INTIME)
# create an indicator for if labtesttime<=intime
Cortisol$indicator<-Cortisol$CHARTDATE<=Cortisol$INDATE
# count number that meet the above criteria
table(Cortisol$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
Cor<-subset(Cortisol, indicator=="TRUE")
sum(!duplicated(Cor$SUBJECT_ID))
# order rows by descending CHARTDATE
Cor<-Cor[rev(order(Cor$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
Cor<-Cor[!duplicated(Cor$SUBJECT_ID),]
#output new csv
write.csv(Cor,"Cleaned\\Cortisol.csv", row.names = FALSE)

#Glucose
#To get list of unique subject with Glucose tested at or nearest to the point of ICU admission
#read lab test csv
Glucose<-read.csv("50931_Glucose.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(Glucose)[names(Glucose) == 'VALUE'] <- 'Glucose'
as.numeric(Glucose$Glucose)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
Glucose<- merge(x = Glucose, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
Glucose$CHARTTIME <- as.POSIXct(Glucose$CHARTTIME)
Glucose$CHARTDATE <- as.Date(Glucose$CHARTTIME)
Glucose$INTIME <- as.POSIXct(Glucose$INTIME)
Glucose$INDATE <- as.Date(Glucose$INTIME)
# create an indicator for if labtesttime<=intime
Glucose$indicator<-Glucose$CHARTDATE<=Glucose$INDATE
# count number that meet the above criteria
table(Glucose$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
Glu<-subset(Glucose, indicator=="TRUE")
sum(!duplicated(Glu$SUBJECT_ID))
# order rows by descending CHARTDATE
Glu<-Glu[rev(order(Glu$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
Glu<-Glu[!duplicated(Glu$SUBJECT_ID),]
#output new csv
write.csv(Glu,"Cleaned\\Glucose.csv", row.names = FALSE)

#Hepatitis B Surface Antigen
#To get list of unique subject with Hepatitis B Surface Antigen tested at or nearest to the point of ICU admission
#read lab test csv
HBSAG<-read.csv("50941_Hepatitis B Surface Antigen.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(HBSAG)[names(HBSAG) == 'VALUE'] <- 'HepBSurfaceAg'
as.factor(HBSAG$HepBSurfaceAg)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
HBSAG<- merge(x = HBSAG, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
HBSAG$CHARTTIME <- as.POSIXct(HBSAG$CHARTTIME)
HBSAG$CHARTDATE <- as.Date(HBSAG$CHARTTIME)
HBSAG$INTIME <- as.POSIXct(HBSAG$INTIME)
HBSAG$INDATE <- as.Date(HBSAG$INTIME)
# create an indicator for if labtesttime<=intime
HBSAG$indicator<-HBSAG$CHARTDATE<=HBSAG$INDATE
# count number that meet the above criteria
table(HBSAG$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
HepBAg<-subset(HBSAG, indicator=="TRUE")
sum(!duplicated(HepBAg$SUBJECT_ID))
# order rows by descending CHARTDATE
HepBAg<-HepBAg[rev(order(HepBAg$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
HepBAg<-HepBAg[!duplicated(HepBAg$SUBJECT_ID),]
#output new csv
write.csv(HepBAg,"Cleaned\\Hep B Surface Antigen.csv", row.names = FALSE)


#Hepatitis B Virus Core Antibody
#To get list of unique subject with Hepatitis B Virus Core Antibody tested at or nearest to the point of ICU admission
#read lab test csv
HBCA<-read.csv("50942_Hepatitis B Virus Core Antibody.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(HBCA)[names(HBCA) == 'VALUE'] <- 'HepBCoreAB'
as.factor(HBCA$HepBCoreAB)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
HBCA<- merge(x = HBCA, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
HBCA$CHARTTIME <- as.POSIXct(HBCA$CHARTTIME)
HBCA$CHARTDATE <- as.Date(HBCA$CHARTTIME)
HBCA$INTIME <- as.POSIXct(HBCA$INTIME)
HBCA$INDATE <- as.Date(HBCA$INTIME)
# create an indicator for if labtesttime<=intime
HBCA$indicator<-HBCA$CHARTDATE<=HBCA$INDATE
# count number that meet the above criteria
table(HBCA$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
HBCA1<-subset(HBCA, indicator=="TRUE")
sum(!duplicated(HBCA1$SUBJECT_ID))
# order rows by descending CHARTDATE
HBCA1<-HBCA1[rev(order(HBCA1$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
HBCA1<-HBCA1[!duplicated(HBCA1$SUBJECT_ID),]
#output new csv
write.csv(HBCA1,"Cleaned\\Hep B Virus Core Antibody.csv", row.names = FALSE)

#Hepatitis C Virus Antibody
#To get list of unique subject with Hepatitis C Virus Antibody tested at or nearest to the point of ICU admission
#read lab test csv
HBC<-read.csv("50943_Hepatitis C Virus Antibody.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(HBC)[names(HBC) == 'VALUE'] <- 'HepCAb'
as.factor(HBC$HepCAb)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
HBC<- merge(x = HBC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
HBC$CHARTTIME <- as.POSIXct(HBC$CHARTTIME)
HBC$CHARTDATE <- as.Date(HBC$CHARTTIME)
HBC$INTIME <- as.POSIXct(HBC$INTIME)
HBC$INDATE <- as.Date(HBC$INTIME)
# create an indicator for if labtesttime<=intime
HBC$indicator<-HBC$CHARTDATE<=HBC$INDATE
# count number that meet the above criteria
table(HBC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
HBC1<-subset(HBC, indicator=="TRUE")
sum(!duplicated(HBC1$SUBJECT_ID))
# order rows by descending CHARTDATE
HBC1<-HBC1[rev(order(HBC1$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
HBC1<-HBC1[!duplicated(HBC1$SUBJECT_ID),]
#output new csv
write.csv(HBC1,"Cleaned\\Hep C Virus Antibody.csv", row.names = FALSE)


#Immunoglobulin A
#To get list of unique subject with Immunoglobulin A tested at or nearest to the point of ICU admission
#read lab test csv
IA<-read.csv("50949_Immunoglobulin A.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(IA)[names(IA) == 'VALUE'] <- 'IgA'
as.numeric(IA$IgA)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
IA<- merge(x = IA, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
IA$CHARTTIME <- as.POSIXct(IA$CHARTTIME)
IA$CHARTDATE <- as.Date(IA$CHARTTIME)
IA$INTIME <- as.POSIXct(IA$INTIME)
IA$INDATE <- as.Date(IA$INTIME)
# create an indicator for if labtesttime<=intime
IA$indicator<-IA$CHARTDATE<=IA$INDATE
# count number that meet the above criteria
table(IA$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
IA1<-subset(IA, indicator=="TRUE")
sum(!duplicated(IA1$SUBJECT_ID))
# order rows by descending CHARTDATE
IA1<-IA1[rev(order(IA1$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
IA1<-IA1[!duplicated(IA1$SUBJECT_ID),]
#output new csv
write.csv(IA1,"Cleaned\\Immunglobulin A.csv", row.names = FALSE)

#Immunoglobulin G
#To get list of unique subject with Immunoglobulin A tested at or nearest to the point of ICU admission
#read lab test csv
IG<-read.csv("50950_Immunoglobulin G.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(IG)[names(IG) == 'VALUE'] <- 'IgG'
as.numeric(IG$IgG)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
IG<- merge(x = IG, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
IG$CHARTTIME <- as.POSIXct(IG$CHARTTIME)
IG$CHARTDATE <- as.Date(IG$CHARTTIME)
IG$INTIME <- as.POSIXct(IG$INTIME)
IG$INDATE <- as.Date(IG$INTIME)
# create an indicator for if labtesttime<=intime
IG$indicator<-IG$CHARTDATE<=IG$INDATE
# count number that meet the above criteria
table(IG$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
IG1<-subset(IG, indicator=="TRUE")
sum(!duplicated(IG1$SUBJECT_ID))
# order rows by descending CHARTDATE
IG1<-IG1[rev(order(IG1$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
IG1<-IG1[!duplicated(IG1$SUBJECT_ID),]
#output new csv
write.csv(IG1,"Cleaned\\Immunglobulin G.csv", row.names = FALSE)

#Immunoglobulin M
#To get list of unique subject with Immunoglobulin M tested at or nearest to the point of ICU admission
#read lab test csv
IM<-read.csv("50951_Immunoglobulin M.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(IM)[names(IM) == 'VALUE'] <- 'IgM'
as.numeric(IM$IgM)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
IM<- merge(x = IM, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
IM$CHARTTIME <- as.POSIXct(IM$CHARTTIME)
IM$CHARTDATE <- as.Date(IM$CHARTTIME)
IM$INTIME <- as.POSIXct(IM$INTIME)
IM$INDATE <- as.Date(IM$INTIME)
# create an indicator for if labtesttime<=intime
IM$indicator<-IM$CHARTDATE<=IM$INDATE
# count number that meet the above criteria
table(IM$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
IM1<-subset(IM, indicator=="TRUE")
sum(!duplicated(IM1$SUBJECT_ID))
# order rows by descending CHARTDATE
IM1<-IM1[rev(order(IM1$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
IM1<-IM1[!duplicated(IM1$SUBJECT_ID),]
#output new csv
write.csv(IM1,"Cleaned\\Immunglobulin M.csv", row.names = FALSE)

#Lactate Dehydrogenase
#To get list of unique subject with Lactate Dehydrogenase tested at or nearest to the point of ICU admission
#read lab test csv
LD<-read.csv("50954_Lactate Dehydrogenase.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(LD)[names(LD) == 'VALUE'] <- 'LactateDehydrogenase'
as.numeric(LD$LactateDehydrogenase)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
LD<- merge(x = LD, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
LD$CHARTTIME <- as.POSIXct(LD$CHARTTIME)
LD$CHARTDATE <- as.Date(LD$CHARTTIME)
LD$INTIME <- as.POSIXct(LD$INTIME)
LD$INDATE <- as.Date(LD$INTIME)
# create an indicator for if labtesttime<=intime
LD$indicator<-LD$CHARTDATE<=LD$INDATE
# count number that meet the above criteria
table(LD$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
LD1<-subset(LD, indicator=="TRUE")
sum(!duplicated(LD1$SUBJECT_ID))
# order rows by descending CHARTDATE
LD1<-LD1[rev(order(LD1$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
LD1<-LD1[!duplicated(LD1$SUBJECT_ID),]
#output new csv
write.csv(LD1,"Cleaned\\Lactate Dehydrogenase.csv", row.names = FALSE)

#Absolute CD3 Count
#To get list of unique subject with Absolute CD3 Count tested at or nearest to the point of ICU admission
#read lab test csv
CD3<-read.csv("51130_Absolute CD3 Count.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CD3)[names(CD3) == 'VALUE'] <- 'AbsoluteCD3'
as.numeric(CD3$AbsoluteCD3)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CD3<- merge(x = CD3, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CD3$CHARTTIME <- as.POSIXct(CD3$CHARTTIME)
CD3$CHARTDATE <- as.Date(CD3$CHARTTIME)
CD3$INTIME <- as.POSIXct(CD3$INTIME)
CD3$INDATE <- as.Date(CD3$INTIME)
# create an indicator for if labtesttime<=intime
CD3$indicator<-CD3$CHARTDATE<=CD3$INDATE
# count number that meet the above criteria
table(CD3$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CD3N<-subset(CD3, indicator=="TRUE")
sum(!duplicated(CD3N$SUBJECT_ID))
# order rows by descending CHARTDATE
CD3N<-CD3N[rev(order(CD3N$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CD3N<-CD3N[!duplicated(CD3N$SUBJECT_ID),]
#output new csv
write.csv(CD3N,"Cleaned\\Aboslute CD3 Count.csv", row.names = FALSE)

#Absolute CD4 Count
#To get list of unique subject with Absolute CD4 Count tested at or nearest to the point of ICU admission
#read lab test csv
CD4<-read.csv("51131_Absolute CD4 Count.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CD4)[names(CD4) == 'VALUE'] <- 'AbsoluteCD4'
as.numeric(CD4$AbsoluteCD4)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CD4<- merge(x = CD4, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CD4$CHARTTIME <- as.POSIXct(CD4$CHARTTIME)
CD4$CHARTDATE <- as.Date(CD4$CHARTTIME)
CD4$INTIME <- as.POSIXct(CD4$INTIME)
CD4$INDATE <- as.Date(CD4$INTIME)
# create an indicator for if labtesttime<=intime
CD4$indicator<-CD4$CHARTDATE<=CD4$INDATE
# count number that meet the above criteria
table(CD4$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CD4N<-subset(CD4, indicator=="TRUE")
sum(!duplicated(CD4N$SUBJECT_ID))
# order rows by descending CHARTDATE
CD4N<-CD4N[rev(order(CD4N$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CD4N<-CD4N[!duplicated(CD4N$SUBJECT_ID),]
#output new csv
write.csv(CD4N,"Cleaned\\Aboslute CD4 Count.csv", row.names = FALSE)

#Absolute Lymphocyte Count
#To get list of unique subject with Absolute Lymphocyte Count tested at or nearest to the point of ICU admission
#read lab test csv
ALC<-read.csv("51133_Absolute Lymphocyte Count.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(ALC)[names(ALC) == 'VALUE'] <- 'AbsoluteLymphocyte'
as.numeric(ALC$AbsoluteLymphocyte)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
ALC<- merge(x = ALC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
ALC$CHARTTIME <- as.POSIXct(ALC$CHARTTIME)
ALC$CHARTDATE <- as.Date(ALC$CHARTTIME)
ALC$INTIME <- as.POSIXct(ALC$INTIME)
ALC$INDATE <- as.Date(ALC$INTIME)
# create an indicator for if labtesttime<=intime
ALC$indicator<-ALC$CHARTDATE<=ALC$INDATE
# count number that meet the above criteria
table(ALC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
ALCN<-subset(ALC, indicator=="TRUE")
sum(!duplicated(ALCN$SUBJECT_ID))
# order rows by descending CHARTDATE
ALCN<-ALCN[rev(order(ALCN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
ALCN<-ALCN[!duplicated(ALCN$SUBJECT_ID),]
#output new csv
write.csv(ALCN,"Cleaned\\Aboslute Lymphocyte Count.csv", row.names = FALSE)

#Bands
#To get list of unique subject with Bands tested at or nearest to the point of ICU admission
#read lab test csv
B<-read.csv("51144_Bands.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(B)[names(B) == 'VALUE'] <- 'Bands'
as.numeric(B$Bands)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
B<- merge(x = B, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
B$CHARTTIME <- as.POSIXct(B$CHARTTIME)
B$CHARTDATE <- as.Date(B$CHARTTIME)
B$INTIME <- as.POSIXct(B$INTIME)
B$INDATE <- as.Date(B$INTIME)
# create an indicator for if labtesttime<=intime
B$indicator<-B$CHARTDATE<=B$INDATE
# count number that meet the above criteria
table(B$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
BN<-subset(B, indicator=="TRUE")
sum(!duplicated(BN$SUBJECT_ID))
# order rows by descending CHARTDATE
BN<-BN[rev(order(BN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
BN<-BN[!duplicated(BN$SUBJECT_ID),]
#output new csv
write.csv(BN,"Cleaned\\Bands.csv", row.names = FALSE)

#Basophils
#To get list of unique subject with Basophils tested at or nearest to the point of ICU admission
#read lab test csv
Baso<-read.csv("51146_Basophils.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(Baso)[names(Baso) == 'VALUE'] <- 'Basophils'
as.numeric(Baso$Basophils)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
Baso<- merge(x = Baso, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
Baso$CHARTTIME <- as.POSIXct(Baso$CHARTTIME)
Baso$CHARTDATE <- as.Date(Baso$CHARTTIME)
Baso$INTIME <- as.POSIXct(Baso$INTIME)
Baso$INDATE <- as.Date(Baso$INTIME)
# create an indicator for if labtesttime<=intime
Baso$indicator<-Baso$CHARTDATE<=Baso$INDATE
# count number that meet the above criteria
table(Baso$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
BasoN<-subset(Baso, indicator=="TRUE")
sum(!duplicated(BasoN$SUBJECT_ID))
# order rows by descending CHARTDATE
BasoN<-BasoN[rev(order(BasoN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
BasoN<-BasoN[!duplicated(BasoN$SUBJECT_ID),]
#output new csv
write.csv(BasoN,"Cleaned\\Basophils.csv", row.names = FALSE)

#CD3 Cells %
#To get list of unique subject with CD3 Cells % tested at or nearest to the point of ICU admission
#read lab test csv
CD3P<-read.csv("51176_CD3 CellsPercent.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CD3P)[names(CD3P) == 'VALUE'] <- 'CD3CellsPercent'
as.numeric(CD3P$CD3CellsPercent)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CD3P<- merge(x = CD3P, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CD3P$CHARTTIME <- as.POSIXct(CD3P$CHARTTIME)
CD3P$CHARTDATE <- as.Date(CD3P$CHARTTIME)
CD3P$INTIME <- as.POSIXct(CD3P$INTIME)
CD3P$INDATE <- as.Date(CD3P$INTIME)
# create an indicator for if labtesttime<=intime
CD3P$indicator<-CD3P$CHARTDATE<=CD3P$INDATE
# count number that meet the above criteria
table(CD3P$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CD3PN<-subset(CD3P, indicator=="TRUE")
sum(!duplicated(CD3PN$SUBJECT_ID))
# order rows by descending CHARTDATE
CD3PN<-CD3PN[rev(order(CD3PN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CD3PN<-CD3PN[!duplicated(CD3PN$SUBJECT_ID),]
#output new csv
write.csv(CD3PN,"Cleaned\\CD3 Cell Percent.csv", row.names = FALSE)

#CD4 Cells %
#To get list of unique subject with CD4 Cells % tested at or nearest to the point of ICU admission
#read lab test csv
CD4P<-read.csv("51180_CD4 CellsPercent.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CD4P)[names(CD4P) == 'VALUE'] <- 'CD4CellsPercent'
as.numeric(CD4P$CD4CellsPercent)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CD4P<- merge(x = CD4P, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CD4P$CHARTTIME <- as.POSIXct(CD4P$CHARTTIME)
CD4P$CHARTDATE <- as.Date(CD4P$CHARTTIME)
CD4P$INTIME <- as.POSIXct(CD4P$INTIME)
CD4P$INDATE <- as.Date(CD4P$INTIME)
# create an indicator for if labtesttime<=intime
CD4P$indicator<-CD4P$CHARTDATE<=CD4P$INDATE
# count number that meet the above criteria
table(CD4P$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CD4PN<-subset(CD4P, indicator=="TRUE")
sum(!duplicated(CD4PN$SUBJECT_ID))
# order rows by descending CHARTDATE
CD4PN<-CD4PN[rev(order(CD4PN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CD4PN<-CD4PN[!duplicated(CD4PN$SUBJECT_ID),]
#output new csv
write.csv(CD4PN,"Cleaned\\CD4 Cell Percent.csv", row.names = FALSE)

#CD4/8 Ratio
#To get list of unique subject with CD4/8 Ratio tested at or nearest to the point of ICU admission
#read lab test csv
CD48R<-read.csv("51181_CD4CD8 Ratio.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CD48R)[names(CD48R) == 'VALUE'] <- 'CD4CD8Ratio'
as.numeric(CD48R$CD4CD8Ratio)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CD48R<- merge(x = CD48R, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CD48R$CHARTTIME <- as.POSIXct(CD48R$CHARTTIME)
CD48R$CHARTDATE <- as.Date(CD48R$CHARTTIME)
CD48R$INTIME <- as.POSIXct(CD48R$INTIME)
CD48R$INDATE <- as.Date(CD48R$INTIME)
# create an indicator for if labtesttime<=intime
CD48R$indicator<-CD48R$CHARTDATE<=CD48R$INDATE
# count number that meet the above criteria
table(CD48R$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CD48RN<-subset(CD48R, indicator=="TRUE")
sum(!duplicated(CD48RN$SUBJECT_ID))
# order rows by descending CHARTDATE
CD48RN<-CD48RN[rev(order(CD48RN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CD48RN<-CD48RN[!duplicated(CD48RN$SUBJECT_ID),]
#output new csv
write.csv(CD48RN,"Cleaned\\CD4 CD8 Ratio.csv", row.names = FALSE)


#CD8 Cells Percent 
#To get list of unique subject with CD8 Cells Percent  tested at or nearest to the point of ICU admission
#read lab test csv
CD8P<-read.csv("51194_CD8 CellsPercent.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(CD8P)[names(CD8P) == 'VALUE'] <- 'CD8cellspercent'
as.numeric(CD8P$CD8cellspercent)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
CD8P<- merge(x = CD8P, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
CD8P$CHARTTIME <- as.POSIXct(CD8P$CHARTTIME)
CD8P$CHARTDATE <- as.Date(CD8P$CHARTTIME)
CD8P$INTIME <- as.POSIXct(CD8P$INTIME)
CD8P$INDATE <- as.Date(CD8P$INTIME)
# create an indicator for if labtesttime<=intime
CD8P$indicator<-CD8P$CHARTDATE<=CD8P$INDATE
# count number that meet the above criteria
table(CD8P$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
CD8PN<-subset(CD8P, indicator=="TRUE")
sum(!duplicated(CD8PN$SUBJECT_ID))
# order rows by descending CHARTDATE
CD8PN<-CD8PN[rev(order(CD8PN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
CD8PN<-CD8PN[!duplicated(CD8PN$SUBJECT_ID),]
#output new csv
write.csv(CD8PN,"Cleaned\\CD8 Cells Percent.csv", row.names = FALSE)

#Lymphocytes
#To get list of unique subject with Lymphocytes tested at or nearest to the point of ICU admission
#read lab test csv
L<-read.csv("51244_Lymphocytes.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(L)[names(L) == 'VALUE'] <- 'Lymphocytes'
as.numeric(L$Lymphocytes)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
L<- merge(x = L, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
L$CHARTTIME <- as.POSIXct(L$CHARTTIME)
L$CHARTDATE <- as.Date(L$CHARTTIME)
L$INTIME <- as.POSIXct(L$INTIME)
L$INDATE <- as.Date(L$INTIME)
# create an indicator for if labtesttime<=intime
L$indicator<-L$CHARTDATE<=L$INDATE
# count number that meet the above criteria
table(L$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
LN<-subset(L, indicator=="TRUE")
sum(!duplicated(LN$SUBJECT_ID))
# order rows by descending CHARTDATE
LN<-LN[rev(order(LN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
LN<-LN[!duplicated(LN$SUBJECT_ID),]
#output new csv
write.csv(LN,"Cleaned\\Lymphocytes.csv", row.names = FALSE)

#Lymphocytes Percent
#To get list of unique subject with Lymphocytes Percent tested at or nearest to the point of ICU admission
#read lab test csv
LP<-read.csv("51245_LymphocytesPercent.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(LP)[names(LP) == 'VALUE'] <- 'LymphocytesPercent'
as.numeric(LP$LymphocytesPercent)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
LP<- merge(x = LP, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
LP$CHARTTIME <- as.POSIXct(LP$CHARTTIME)
LP$CHARTDATE <- as.Date(LP$CHARTTIME)
LP$INTIME <- as.POSIXct(LP$INTIME)
LP$INDATE <- as.Date(LP$INTIME)
# create an indicator for if labtesttime<=intime
LP$indicator<-LP$CHARTDATE<=LP$INDATE
# count number that meet the above criteria
table(LP$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
LPN<-subset(LP, indicator=="TRUE")
sum(!duplicated(LPN$SUBJECT_ID))
# order rows by descending CHARTDATE
LPN<-LPN[rev(order(LPN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
LPN<-LPN[!duplicated(LPN$SUBJECT_ID),]
#output new csv
write.csv(LPN,"Cleaned\\Lymphocytes Percent.csv", row.names = FALSE)

#Neutrophils
#To get list of unique subject with Neutrophils tested at or nearest to the point of ICU admission
#read lab test csv
N<-read.csv("51256_Neutrophils.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(N)[names(N) == 'VALUE'] <- 'Neutrophils'
as.numeric(N$Neutrophils)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
N<- merge(x = N, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
N$CHARTTIME <- as.POSIXct(N$CHARTTIME)
N$CHARTDATE <- as.Date(N$CHARTTIME)
N$INTIME <- as.POSIXct(N$INTIME)
N$INDATE <- as.Date(N$INTIME)
# create an indicator for if labtesttime<=intime
N$indicator<-N$CHARTDATE<=N$INDATE
# count number that meet the above criteria
table(N$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
NN<-subset(N, indicator=="TRUE")
sum(!duplicated(NN$SUBJECT_ID))
# order rows by descending CHARTDATE
NN<-NN[rev(order(NN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
NN<-NN[!duplicated(NN$SUBJECT_ID),]
#output new csv
write.csv(NN,"Cleaned\\Neutrophils.csv", row.names = FALSE)

#Platelet Count
#To get list of unique subject with Platelet Count tested at or nearest to the point of ICU admission
#read lab test csv
PC<-read.csv("51265_Platelet Count.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(PC)[names(PC) == 'VALUE'] <- 'Platelet'
as.numeric(PC$Platelet)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
PC<- merge(x = PC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
PC$CHARTTIME <- as.POSIXct(PC$CHARTTIME)
PC$CHARTDATE <- as.Date(PC$CHARTTIME)
PC$INTIME <- as.POSIXct(PC$INTIME)
PC$INDATE <- as.Date(PC$INTIME)
# create an indicator for if labtesttime<=intime
PC$indicator<-PC$CHARTDATE<=PC$INDATE
# count number that meet the above criteria
table(PC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
PCN<-subset(PC, indicator=="TRUE")
sum(!duplicated(PCN$SUBJECT_ID))
# order rows by descending CHARTDATE
PCN<-PCN[rev(order(PCN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
PCN<-PCN[!duplicated(PCN$SUBJECT_ID),]
#output new csv
write.csv(PCN,"Cleaned\\Platelet Count.csv", row.names = FALSE)

#RDW
#To get list of unique subject with RDW tested at or nearest to the point of ICU admission
#read lab test csv
RDW<-read.csv("51277_RDW.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(RDW)[names(RDW) == 'VALUE'] <- 'RDW'
as.numeric(RDW$RDW)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
RDW<- merge(x = RDW, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
RDW$CHARTTIME <- as.POSIXct(RDW$CHARTTIME)
RDW$CHARTDATE <- as.Date(RDW$CHARTTIME)
RDW$INTIME <- as.POSIXct(RDW$INTIME)
RDW$INDATE <- as.Date(RDW$INTIME)
# create an indicator for if labtesttime<=intime
RDW$indicator<-RDW$CHARTDATE<=RDW$INDATE
# count number that meet the above criteria
table(RDW$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
RDWN<-subset(RDW, indicator=="TRUE")
sum(!duplicated(RDWN$SUBJECT_ID))
# order rows by descending CHARTDATE
RDWN<-RDWN[rev(order(RDWN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
RDWN<-RDWN[!duplicated(RDWN$SUBJECT_ID),]
#output new csv
write.csv(RDWN,"Cleaned\\RDW.csv", row.names = FALSE)

#Red Blood Cells
#To get list of unique subject with Red Blood Cells tested at or nearest to the point of ICU admission
#read lab test csv
RBC<-read.csv("51279_Red Blood Cells.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(RBC)[names(RBC) == 'VALUE'] <- 'RBC'
as.numeric(RBC$RBC)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
RBC<- merge(x = RBC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
RBC$CHARTTIME <- as.POSIXct(RBC$CHARTTIME)
RBC$CHARTDATE <- as.Date(RBC$CHARTTIME)
RBC$INTIME <- as.POSIXct(RBC$INTIME)
RBC$INDATE <- as.Date(RBC$INTIME)
# create an indicator for if labtesttime<=intime
RBC$indicator<-RBC$CHARTDATE<=RBC$INDATE
# count number that meet the above criteria
table(RBC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
RBCN<-subset(RBC, indicator=="TRUE")
sum(!duplicated(RBCN$SUBJECT_ID))
# order rows by descending CHARTDATE
RBCN<-RBCN[rev(order(RBCN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
RBCN<-RBCN[!duplicated(RBCN$SUBJECT_ID),]
#output new csv
write.csv(RBCN,"Cleaned\\Red Blood Cells.csv", row.names = FALSE)

#Sedimentation Rate
#To get list of unique subject with Sedimentation Rate tested at or nearest to the point of ICU admission
#read lab test csv
SR<-read.csv("51288_Sedimentation Rate.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(SR)[names(SR) == 'VALUE'] <- 'Sedimentation'
as.numeric(SR$Sedimentation)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
SR<- merge(x = SR, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
SR$CHARTTIME <- as.POSIXct(SR$CHARTTIME)
SR$CHARTDATE <- as.Date(SR$CHARTTIME)
SR$INTIME <- as.POSIXct(SR$INTIME)
SR$INDATE <- as.Date(SR$INTIME)
# create an indicator for if labtesttime<=intime
SR$indicator<-SR$CHARTDATE<=SR$INDATE
# count number that meet the above criteria
table(SR$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
SRN<-subset(SR, indicator=="TRUE")
sum(!duplicated(SRN$SUBJECT_ID))
# order rows by descending CHARTDATE
SRN<-SRN[rev(order(SRN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
SRN<-SRN[!duplicated(SRN$SUBJECT_ID),]
#output new csv
write.csv(SRN,"Cleaned\\Sedimentation Rate.csv", row.names = FALSE)

#White Blood Cell Count
#To get list of unique subject with White Blood Cell Count tested at or nearest to the point of ICU admission
#read lab test csv
WC<-read.csv("51300_WBC Count.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(WC)[names(WC) == 'VALUE'] <- 'WBCCount'
as.numeric(WC$WBCCount)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
WC<- merge(x = WC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
WC$CHARTTIME <- as.POSIXct(WC$CHARTTIME)
WC$CHARTDATE <- as.Date(WC$CHARTTIME)
WC$INTIME <- as.POSIXct(WC$INTIME)
WC$INDATE <- as.Date(WC$INTIME)
# create an indicator for if labtesttime<=intime
WC$indicator<-WC$CHARTDATE<=WC$INDATE
# count number that meet the above criteria
table(WC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
WCN<-subset(WC, indicator=="TRUE")
sum(!duplicated(WCN$SUBJECT_ID))
# order rows by descending CHARTDATE
WCN<-WCN[rev(order(WCN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
WCN<-WCN[!duplicated(WCN$SUBJECT_ID),]
#output new csv
write.csv(WCN,"Cleaned\\WBC Count.csv", row.names = FALSE)

#White Blood Cells
#To get list of unique subject with White Blood Cells tested at or nearest to the point of ICU admission
#read lab test csv
WBC<-read.csv("51301_White Blood Cells.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(WBC)[names(WBC) == 'VALUE'] <- 'WBC'
as.numeric(WBC$WBC)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
WBC<- merge(x = WBC, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
WBC$CHARTTIME <- as.POSIXct(WBC$CHARTTIME)
WBC$CHARTDATE <- as.Date(WBC$CHARTTIME)
WBC$INTIME <- as.POSIXct(WBC$INTIME)
WBC$INDATE <- as.Date(WBC$INTIME)
# create an indicator for if labtesttime<=intime
WBC$indicator<-WBC$CHARTDATE<=WBC$INDATE
# count number that meet the above criteria
table(WBC$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
WBCN<-subset(WBC, indicator=="TRUE")
sum(!duplicated(WBCN$SUBJECT_ID))
# order rows by descending CHARTDATE
WBCN<-WBCN[rev(order(WBCN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
WBCN<-WBCN[!duplicated(WBCN$SUBJECT_ID),]
#output new csv
write.csv(WBCN,"Cleaned\\White Blood Cells.csv", row.names = FALSE)

#Nitrite
#To get list of unique subject with Nitrite tested at or nearest to the point of ICU admission
#read lab test csv
Nitrite<-read.csv("51487_Nitrite.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(Nitrite)[names(Nitrite) == 'VALUE'] <- 'Nitrite'
as.factor(Nitrite$Nitrite)
combineLevels(Nitrite$Nitrite, levs=c(1,2,3,4,5), newLabel= "NA")
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
Nitrite<- merge(x = Nitrite, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
Nitrite$CHARTTIME <- as.POSIXct(Nitrite$CHARTTIME)
Nitrite$CHARTDATE <- as.Date(Nitrite$CHARTTIME)
Nitrite$INTIME <- as.POSIXct(Nitrite$INTIME)
Nitrite$INDATE <- as.Date(Nitrite$INTIME)
# create an indicator for if labtesttime<=intime
Nitrite$indicator<-Nitrite$CHARTDATE<=Nitrite$INDATE
# count number that meet the above criteria
table(Nitrite$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
NitriteN<-subset(Nitrite, indicator=="TRUE")
sum(!duplicated(NitriteN$SUBJECT_ID))
# order rows by descending CHARTDATE
NitriteN<-NitriteN[rev(order(NitriteN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
NitriteN<-NitriteN[!duplicated(NitriteN$SUBJECT_ID),]
#output new csv
write.csv(NitriteN,"Cleaned\\Nitrite.csv", row.names = FALSE)

#pH
#To get list of unique subject with pH tested at or nearest to the point of ICU admission
#read lab test csv
pH<-read.csv("51491_pH.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(pH)[names(pH) == 'VALUE'] <- 'pH'
as.factor(pH$pH)
combineLevels(pH$pH, levs=c(11,12,13), newLabel= "NA")
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
pH<- merge(x = pH, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
pH$CHARTTIME <- as.POSIXct(pH$CHARTTIME)
pH$CHARTDATE <- as.Date(pH$CHARTTIME)
pH$INTIME <- as.POSIXct(pH$INTIME)
pH$INDATE <- as.Date(pH$INTIME)
# create an indicator for if labtesttime<=intime
pH$indicator<-pH$CHARTDATE<=pH$INDATE
# count number that meet the above criteria
table(pH$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
pHN<-subset(pH, indicator=="TRUE")
sum(!duplicated(pHN$SUBJECT_ID))
# order rows by descending CHARTDATE
pHN<-pHN[rev(order(pHN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
pHN<-pHN[!duplicated(pHN$SUBJECT_ID),]
#output new csv
write.csv(pHN,"Cleaned\\pH.csv", row.names = FALSE)

#Specific Gravity
#To get list of unique subject with Specific Gravity tested at or nearest to the point of ICU admission
#read lab test csv
SG<-read.csv("51498_Specific Gravity.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(SG)[names(SG) == 'VALUE'] <- 'SpecificGravity'
as.numeric(SG$SpecificGravity)
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
SG<- merge(x = SG, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
SG$CHARTTIME <- as.POSIXct(SG$CHARTTIME)
SG$CHARTDATE <- as.Date(SG$CHARTTIME)
SG$INTIME <- as.POSIXct(SG$INTIME)
SG$INDATE <- as.Date(SG$INTIME)
# create an indicator for if labtesttime<=intime
SG$indicator<-SG$CHARTDATE<=SG$INDATE
# count number that meet the above criteria
table(SG$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
SGN<-subset(SG, indicator=="TRUE")
sum(!duplicated(SGN$SUBJECT_ID))
# order rows by descending CHARTDATE
SGN<-SGN[rev(order(SGN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
SGN<-SGN[!duplicated(SGN$SUBJECT_ID),]
#output new csv
write.csv(SGN,"Cleaned\\Specific Gravity.csv", row.names = FALSE)


#HIV Antibody
#To get list of unique subject with HIV Antibody tested at or nearest to the point of ICU admission
#read lab test csv
HIV<-read.csv("50944_HIV Antibody.csv", header= TRUE, sep = ",")
#rename column heading=value to respective lab test name
names(HIV)[names(HIV) == 'VALUE'] <- 'HIVAb'
as.factor(HIV$HIVAb)
combineLevels(HIV$HIVAb, levs=c(2,3,4,5), newLabel= "NEGATIVE")
#The following is to take only latest lab test done on patient at 1st day of icu stay or nearest date to icu stay
#join INTIME (ICUSTAY time) to labtest
HIV<- merge(x = HIV, y = icustay[,c("SUBJECT_ID", "INTIME")], by = "SUBJECT_ID", all.x=TRUE)
# convert to date format
HIV$CHARTTIME <- as.POSIXct(HIV$CHARTTIME)
HIV$CHARTDATE <- as.Date(HIV$CHARTTIME)
HIV$INTIME <- as.POSIXct(HIV$INTIME)
HIV$INDATE <- as.Date(HIV$INTIME)
# create an indicator for if labtesttime<=intime
HIV$indicator<-HIV$CHARTDATE<=HIV$INDATE
# count number that meet the above criteria
table(HIV$indicator)["TRUE"]
# Pull out those that meet date criteria (indicator==TRUE)
HIVN<-subset(HIV, indicator=="TRUE")
sum(!duplicated(HIVN$SUBJECT_ID))
# order rows by descending CHARTDATE
HIVN<-HIVN[rev(order(HIVN$CHARTDATE)),]
# remove duplicated subjectid keeping latest CHARTDATE
HIVN<-HIVN[!duplicated(HIVN$SUBJECT_ID),]
#output new csv
write.csv(HIVN,"Cleaned\\HIV Antibody.csv", row.names = FALSE)

