#  import Hensher Data and apply labels
# input : ../raw/Data/SPRP.txt received via Email on 250302
# output: /dat/012a_asm_main_impSPSS_ALLVARS_en.Rds    and ... _ua version
#         /lis/012a_asm_main_impSPSS_ALLVARS_ua.Rds    and ... _en version

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
lisdatu <- paste("lis\\",codefile, "_ua.lis",sep="")   # for naming listing file W UKRAINIAN LABELS
lisdate <- paste("lis\\",codefile, "_en.lis",sep="")   # for naming listing file W ENGLISH LABELS
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=100)  # for A4 landscape 

library(readxl)
#library(readr)
library(tidyverse)
#library(haven)
library(labelled) # var_label
library(summarytools)  # dfSummary
#library(lubridate)  # for seconds_to_period() function

spalnam <- c("ID","CITY","SPRP","SPEXP","ALTISPRP","ALTIJ","CHSNMODE","ALTMODE","SPCHOICE","CHOICE","CSET","RPDA",
             "RPRS","RPBUS","RPTN","RPWALK","RPBIKE","SPCART","SPCARNT","SPBUS","SPTN","SPBW","SPLR","CN","SPMISS",
             "RPMISS","SPRPMISS","RPSPWKBK","RPCAR","BEFORPTR","AFTERPTR","MPTRFARE","OPTRFARE","HOMTOPTR","PTRTOWK",
             "HHLDVEH","WKKMVEH","WALKTIME","MPTRTIME","WAITTIME","OPTRTIME","AUTOPASS","MAXPASS","HLDAUTO","AUTONA",
             "AUTOMAKE","AUTOYEAR","AUTOWKKM","AUTOTIME","AUTOWKTM","AUTWKWLK","AUTWKBUS","AUTWKTRN","AUTWKOTH",
             "VEHPPARK","VEHPRKCT","VEHPTOLL","VEHTOLCT","VEHPOTHE","VEHOTHCT","VEHPNOTH","DRPPTRAN","DRPCCARE",
             "DRPSCHOL","DRPTEDUC","DROPWORK","DROPOTHE","DROPDWEL","NODROPOF","DROPTIME","TRIPTIME","DEPTIME",
             "DISDWCBD","VEHSTATU","CHAWKTR","CHADWTR","SPLENGTH","TIME","TIMEVAR","TOLL","TOLLPRED","FUEL","PARKING",
             "FREQ","FARE","START24","ACCTIME","EGGTIME","HWEIGHT","NUMBVEHS","HLDINCOM","NHLDBCAR","NCOMPCAR",
             "NDRIVLIC","HLDSIZE","NWORKERS","WKREMPLY","WKROCCUP","PERAGE","DRIVLIC","PINCOME","PERSEX","PEREDUC",
             "ACCEGGT","CAN","SYD","MEL","BRS","ADL")
# Read the file from sav file exported from Qualtrics
asm <- read.csv("../raw/Data/SPRP.txt",header=FALSE, sep = "\t",col.names = spalnam) %>%
       filter(!is.na(ID))  %>% # raw data has two records with no data at the end: remove
       mutate(across(everything(), ~ na_if(.x, -999)))  # replace -999 by NA
       
#asm <- read_excel("../raw/Data/SPRP.xls", col_names = TRUE)


#cat("\n\nstr(asm)\n")
#str(asm,list.len = 200)

addlabels <- c(
ID = "Id",
CITY = "City",
SPRP = "1 = RP 2 =SP",
SPEXP = "Experiment number",
ALTISPRP = "Combined SPRP modes 1-12",
ALTIJ = "SP Mode 1 - 6; RP Mode 1 - 6",
CHSNMODE = "Mode chosen in RP choice set",
ALTMODE = "Alternative mode present in RP choice set ",
SPCHOICE = "Travel options to work",
CHOICE = "Chosen mode ",
CSET = "Choice set size",
RPDA = "Mode chosen dummy - drive alone",
RPRS = "Mode chosen dummy - ride share",
RPBUS = "Mode chosen dummy - bus",
RPTN = "Mode chosen dummy - train",
RPWALK = "Mode chosen dummy - walk",
RPBIKE = "Mode chosen dummy - bicycle",
SPCART = "Mode chosen dummy - car with toll",
SPCARNT = "Mode chosen dummy - car with no toll",
SPBUS = "Mode chosen dummy - bus",
SPTN = "Mode chosen dummy - train",
SPBW = "Mode chosen dummy - busway",
SPLR = "Mode chosen dummy - light rail",
CN = "Alternatives present within choice set",
SPMISS = "Decision maker has missing choice sets",
RPMISS = "Decision maker has missing RP data",
SPRPMISS = "Analyst created variable ",
RPSPWKBK = "Walk and/or Bike alternative present in RP choice set ",
RPCAR = "Car alternative present in RP choice set",
BEFORPTR = "Point before main point",
AFTERPTR = "Point after main point",
MPTRFARE = "Cost main form public transport [$]",
OPTRFARE = "Cost other form public transport [$]",
HOMTOPTR = "Mode of travel from home to first point",
PTRTOWK = "Last pt to work",
HHLDVEH = "Household vehicle used to point",
WKKMVEH = "Vehicle km to/from point",
WALKTIME = "Time walking-last trip to work",
MPTRTIME = "Time on main public transport",
WAITTIME = "Time waiting for public transport",
OPTRTIME = "Time on other public transport",
AUTOPASS = "Number & type passengers in car to work",
MAXPASS = "Maximum number of passengers",
HLDAUTO = "Household vehicle driven to work",
AUTONA = "Car not applicable",
AUTOMAKE = "Make/model of vehicle driven to work",
AUTOYEAR = "Year manufacture of car driven to work",
AUTOWKKM = "Distance travelled by car to work",
AUTOTIME = "Time spent travelling by car to work",
AUTOWKTM = "Time in car to work",
AUTWKWLK = "Walk-from car to work",
AUTWKBUS = "Bus-from car to work",
AUTWKTRN = "Train-from car to work",
AUTWKOTH = "Other mode-from car to work",
VEHPPARK = "Paid parking on last trip to work",
VEHPRKCT = "Cost of parking on last trip to work",
VEHPTOLL = "Paid toll on last trip to work",
VEHTOLCT = "Toll costs on last trip to work",
VEHPOTHE = "Paid other on last trip to work",
VEHOTHCT = "Other costs on last car trip to work",
VEHPNOTH = "Paid nothing on last car trip to work",
DRPPTRAN = "Drop passengers in car at public transport",
DRPCCARE = "Drop passengers in car at childcare",
DRPSCHOL = "Drop passengers in car at school",
DRPTEDUC = "Drop passengers in car at tertiary edu",
DROPWORK = "Drop passengers in car at work",
DROPOTHE = "Drop passengers in car at other places",
DROPDWEL = "Drop passengers in car at dwelling",
NODROPOF = "Don’t drop any passengers in car off",
DROPTIME = "Time taken to drop passengers from car",
TRIPTIME = "Trip time walk and bike",
DEPTIME = "Departure time",
DISDWCBD = "Distance from dwelling to CBD",
VEHSTATU = "Vehicle status",
CHAWKTR = "Move work closer to home",
CHADWTR = "Move home closer to work",
SPLENGTH = "SP experiment segment",
TIME = "Travel time to work",
TIMEVAR = "Time variability",
TOLL = "Toll cost",
TOLLPRED = "Times applied to tolls",
FUEL = "Fuel cost",
PARKING = "Parking cost",
FREQ = "Frequency of service",
FARE = "Return fare",
START24 = "Normal depart time",
ACCTIME = "Public transport access time",
EGGTIME = "Public transport egress time",
HWEIGHT = "Household weight",
NUMBVEHS = "Number of vehicles in household",
HLDINCOM = "Households income",
NHLDBCAR = "Number household business cars",
NCOMPCAR = "Number company cars",
NDRIVLIC = "Number licences in household",
HLDSIZE = "Household size",
NWORKERS = "Num workers in household",
WKREMPLY = "Employment type ",
WKROCCUP = "Occupation category",
PERAGE = "Person age",
DRIVLIC = "Person drivers licence ",
PINCOME = "Personal income",
PERSEX = "Person sex",
PEREDUC = "Person highest education",
ACCEGGT = "Access time plus egress time",
CAN = "Canberra",
SYD = "Sydney",
MEL = "Melbourne",
BRS = "Brisbane",
ADL = "Adelaide"
)



asm <- asm %>%   
 mutate( 
  CITY    = factor(CITY, levels = 1:6, labels = c("Canberra","Sydney","Melbourne","Brisbane",
                  "Adelaide","Perth")),
  SPRP	   = factor(SPRP, levels = 1:2, labels = c("RP", "SP")),
  SPEXP	 = factor(SPEXP, levels = 0:3, labels = c("RP","CHOICE SET 1 (SP)","CHOICE SET 2 (SP)","CHOICE SET 3 (SP)")),
  ALTISPRP= factor(ALTISPRP, levels = 1:12, labels = c("DRIVE ALONE (RP)","RIDE SHARE (RP)","BUS (RP)","TRAIN (RP)",
                "WALK (RP)","BICYCLE (RP)","CAR (TOLL) (SP)","CAR (NOT TOLL) (SP)","BUS (SP)",
                "TRAIN (SP)","LIGHT RAIL (SP)","BUSWAY (SP)")),
  ALTIJ	= factor(ALTIJ),
  CHSNMODE	= factor(CHSNMODE, levels = c(0:5,8:16), 
                     labels = c("SP","Train (RP)","Bus (RP)","Tram (RP)","Ferry (RP)","Taxi (RP)","Walk (RP)",
                                "Motorbike (RP)","Bicycle (RP)","Drive alone (RP)",
                                "Drive & household passenger (RP)","Drive + other passenger (RP)",
                                "Drive +household passenger & other passenger (RP)",
                                "Passenger household vehicle (RP)","Passenger other vehicle (RP)")),
  ALTMODE	= factor(ALTMODE, levels = c(0:5,8:16), 
                   labels = c("SP","Train (RP)","Bus (RP)","Tram (RP)","Ferry (RP)","Taxi (RP)","Walk (RP)",
                              "Motorbike (RP)","Bicycle (RP)","Drive alone (RP)",
                              "Drive & household passenger (RP)","Drive + other passenger (RP)",
                              "Drive +household passenger & other passenger (RP)",
                              "Passenger household vehicle (RP)","Passenger other vehicle (RP)")),
  SPCHOICE	= factor(SPCHOICE, levels = c(0:6), 
                     labels = c("RP","car with toll (SP)","car without toll (SP)","bus (SP)",
                                "train (SP)","busway (SP)","light rail (SP)")),
  CHOICE	= factor(CHOICE, labels = c("not chosen","chosen")),
  CSET	= factor(CSET, levels = c(2,4), labels = c("RP","SP")),
  RPDA	= factor(RPDA, levels = c(1,0), labels = c("DRIVE ALONE CHOSEN (RP)","DRIVE ALONE NOT CHOSEN (RP)")),
  RPRS	= factor(RPRS, levels = c(1,0), 
                 labels = c("RIDE SHARE CHOSEN (RP)","RIDE SHARE NOT CHOSEN (RP)")),
  RPBUS	= factor(RPBUS, levels = c(1,0), 
                 labels = c("BUS CHOSEN (RP)","BUS NOT CHOSEN (RP)")),
  RPTN	= factor(RPTN, levels = c(1,0), 
          labels = c("TRAIN CHOSEN (RP)","0 = TRAIN NOT CHOSEN (RP)")),
  RPWALK	= factor(RPWALK, levels = c(1,0), 
                   labels = c("WALK CHOSEN (RP)","WALK NOT CHOSEN (RP)")),
  RPBIKE	= factor(RPBIKE, levels = c(1,0), 
                   labels = c("BICYLCE CHOSEN (RP)","BICYLCE NOT CHOSEN (RP)")),
  SPCART	= factor(SPCART, levels = c(1,0), 
                   labels = c("CAR WITH TOLL CHOSEN (SP)","CAR WITH TOLL NOT CHOSEN (SP)")),
  SPCARNT	= factor(SPCARNT, levels = c(1,0), 
                   labels = c("CAR WITH NO TOLL CHOSEN (SP)","CAR WITH NO TOLL NOT CHOSEN (SP)")),
  SPBUS	= factor(SPBUS, levels = c(1,0), 
                   labels = c("BUS CHOSEN (SP)","BUS NOT CHOSEN (SP)")),
  SPTN	= factor(SPTN, levels = c(1,0), 
                   labels = c("TRAIN CHOSEN (SP)","TRAIN NOT CHOSEN (SP)")),
  SPBW	= factor(SPBW, levels = c(1,0), 
                   labels = c("BUSWAY CHOSEN (SP)","BUSWAY NOT CHOSEN (SP)")),
  SPLR	= factor(SPLR, levels = c(1,0), 
                   labels = c("LIGHT RAIL CHOSEN (SP)","LIGHT RAIL NOT CHOSEN (SP)")),
  CN	= factor(CN, levels = 0:4, 
                   labels = c("Walk and other alternative included (RP)","Bus - Train (SP)",
                              "Bus - Busway (SP)","Train - Light Rail (SP)","Busway - Light Rail (SP)")),
  SPMISS	= factor(SPMISS, levels = 0:1, 
                   labels = c("All choice sets present","One or more choice sets missing")),
  RPMISS	= factor(RPMISS, levels = 0:1, 
                   labels = c("Respondents RP data present","Respondents RP data missing")),
  SPRPMISS	= factor(SPRPMISS, levels = 0:1, labels = c("Use","Reject")),
  RPSPWKBK	= factor(RPSPWKBK, levels = 0:1,
                     labels = c("Walk and/or bike alternatives not present in RP choice set",
                                "Walk and/or bike alternatives are present in RP choice set")),
  RPCAR		= factor(RPCAR, levels = 0:1,
                     labels = c("Ride share and/or drive alone alternatives not present in RP choice set",
                                "Ride share and/or drive alone alternatives are present in RP choice set")),
  BEFORPTR	= factor(BEFORPTR, levels = 1:5,
                     labels = c("TRAIN","BUS","TRAM","FERRY","TAXI")),
  AFTERPTR	= factor(AFTERPTR, levels = 1:5,
                     labels = c("TRAIN","BUS","TRAM","FERRY","TAXI")),
  HOMTOPTR	= factor(HOMTOPTR, levels = 1:6,
                     labels = c("CAR THEN PARK","CAR THEN DROPPED OFF","MOTORBIKE","WALKED","TAXI","BICYCLE")),
  PTRTOWK	= factor(PTRTOWK, levels = 1:6,
                     labels = c("CAR THEN PARK","CAR THEN DROPPED OFF","MOTORBIKE","WALKED","TAXI","BICYCLE")),
  VEHSTATU	= factor(VEHSTATU, levels = 1:3,
                     labels = c("PRIVATE VEHICLE","HOUSEHOLD BUSINESS VEHICLE","COMPANY VEHICLE")),
  SPLENGTH	= factor(SPLENGTH, levels = 0:3,
                     labels = c("RP","Less than 30 Minutes","30 to 45 minutes","over 45 minutes")),
  HLDINCOM	= factor(HLDINCOM, levels = 1:11,
                     labels = c("Less than 5000","5000 - 12000","12001 - 20000","20001 - 30000",
                                "30001 - 40000","40001 - 50000","50001 - 60000","60001 - 70000",
                                "70001 - 80000","80001 - 90000","90001 - 120000")),
  WKREMPLY	= factor(WKREMPLY, levels = 1:3,
                     labels = c("Full Time","Part Time","Self Employed")),
  WKROCCUP	= factor(WKROCCUP, levels = 1:9,
                     labels = c("Managers and Admin","Professionals","Para-professional","Tradespersons",
                                "Clerks","Sales","Plant operators","Laborers","Other")),
  DRIVLIC	= factor(DRIVLIC, levels = 1:3, labels = c("YES","NO","NOT APPLICABLE")),
  PERSEX	= factor(PERSEX, levels = c(1,-1), labels = c("male","female")),
  PEREDUC	= factor(PEREDUC, levels = 1:5, labels = c("PRE-PRIMARYSCHOOL","PRIMARYSCHOOL","SECONDARYSCHOOL",
                                                     "TECH/COLLEGE","UNIVERSITY")),
  CAN	= factor(CAN, levels = c(1,-1,0), labels = c("CANBERRA","PERTH","OTHER")),
  SYD	= factor(SYD, levels = c(1,-1,0), labels = c("SYDNEY","PERTH","OTHER")),
  MEL	= factor(MEL, levels = c(1,-1,0), labels = c("MELBOURNE","PERTH","OTHER")),
  BRS	= factor(BRS, levels = c(1,-1,0), labels = c("BRISBANE","PERTH","OTHER")),
  ADL	= factor(ADL, levels = c(1,-1,0), labels = c("ADELAIDE","PERTH","OTHER"))
   ) 

# Apply variable labels 
for (var in names(addlabels)) {
  var_label(asm[[var]]) <- addlabels[[var]]
}

saveRDS(asm,file=paste("dat/",codefile,".Rds",sep=""))  # saves in R format

cat("\nCase study data of Hensher/Rose/Greene ACA-Primer, 1st ed, 2005 
    (import from SPRP.txt received from Authors on March 2 2025)\n
ToC:
    I)  Codebook ('Data frame summary')
    II) (near) replication of Table 9.11 Breakdown by City for the SP and RP data sets (p. 285)\n\n
I) Codebook\n")
dfSummary(asm)


    
cat("\fII) (near) replication of HRG2005 Table 9.11 - Breakdown by city for the SP and RP data sets (p. 285)\n")
asm$CITY <- factor(asm$CITY, levels = c("Sydney", "Canberra", "Melbourne", "Adelaide", "Brisbane", "Perth"))  # order as in table 9.11 of the book 
asmd <- asm %>% distinct(ID,SPRP, .keep_all = TRUE) # asm w one obs per ID
a <- addmargins(table(asmd$CITY,asmd$SPRP),1)
b <- addmargins(table(asmd$CITY[asmd$SPRP=="SP"],asmd$SPLENGTH[asmd$SPRP=="SP"]),1)[,2:4]
cbind(a,b)

cat("\n\  Data set dimension (16188*109) is as noted on p.287 of the book. The number of RP-respondents (866) conforms with table 9.11.
Total number of SP respondents (1212) exceeds the figure in table 9.11 (1204). Descriptive statistics are very close to
those in 'Appendix 9B - Mode-choice case study data dictionary'.")

#factorvars <- asm %>% select_if(is.factor) %>% names() 
#codebookentries(factorvars,asm)


sink()