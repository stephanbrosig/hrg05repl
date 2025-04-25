#  import HensherRoseGreen Data and apply labels
# input : ../raw/Data/SPRP.txt received via Email on 250302
# output: datdir/XXX.Rds and /lis/XXX.lis  with XXX replaced by the name of this code file  (/cmd/XXX.r)
#         

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
outdat <- paste(datdir,"/",codefile,".Rds",sep="")   # for naming data output file (r-format)
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

spalnam <- c("id","city","sprp","spexp","altisprp","altij","chsnmode","altmode","spchoice","choice","cset","rpda",
             "rprs","rpbus","rptn","rpwalk","rpbike","spcart","spcarnt","spbus","sptn","spbw","splr","cn","spmiss",
             "rpmiss","sprpmiss","rpspwkbk","rpcar","beforptr","afterptr","mptrfare","optrfare","homtoptr","ptrtowk",
             "hhldveh","wkkmveh","walktime","mptrtime","waittime","optrtime","autopass","maxpass","hldauto","autona",
             "automake","autoyear","autowkkm","autotime","autowktm","autwkwlk","autwkbus","autwktrn","autwkoth",
             "vehppark","vehprkct","vehptoll","vehtolct","vehpothe","vehothct","vehpnoth","drpptran","drpccare",
             "drpschol","drpteduc","dropwork","dropothe","dropdwel","nodropof","droptime","triptime","deptime",
             "disdwcbd","vehstatu","chawktr","chadwtr","splength","time","timevar","toll","tollpred","fuel","parking",
             "freq","fare","start24","acctime","eggtime","hweight","numbvehs","hldincom","nhldbcar","ncompcar",
             "ndrivlic","hldsize","nworkers","wkremply","wkroccup","perage","drivlic","pincome","persex","pereduc",
             "acceggt","can","syd","mel","brs","adl")
# Read the file from sav file exported from Qualtrics
asm <- read.csv(paste0(datdir,"/Data/SPRP.txt"),header=FALSE, sep = "\t",col.names = spalnam) %>%
       filter(!is.na(id))  %>% # raw data has two records with no data at the end: remove
       mutate(across(everything(), ~ na_if(.x, -999)))  # replace -999 by NA
       
#asm <- read_excel("../raw/Data/SPRP.xls", col_names = TRUE)



addlabels <- c(
id       = "Id",
city     = "City",
sprp     = "1 = RP 2 =SP",
spexp    = "Experiment number",
altisprp = "Combined SPRP modes 1-12",
altij    = "SP Mode 1 - 6; RP Mode 1 - 6",
chsnmode = "Mode chosen in RP choice set",
altmode  = "Alternative mode present in RP choice set ",
spchoice = "Travel options to work",
choice   = "Chosen mode dummy",
cset     = "Choice set size",
rpda     = "Mode chosen dummy - drive alone",
rprs     = "Mode chosen dummy - ride share",
rpbus    = "Mode chosen dummy - bus",
rptn     = "Mode chosen dummy - train",
rpwalk   = "Mode chosen dummy - walk",
rpbike   = "Mode chosen dummy - bicycle",
spcart   = "Mode chosen dummy - car with toll",
spcarnt  = "Mode chosen dummy - car with no toll",
spbus    = "Mode chosen dummy - bus",
sptn     = "Mode chosen dummy - train",
spbw     = "Mode chosen dummy - busway",
splr     = "Mode chosen dummy - light rail",
cn       = "Alternatives present within choice set",
spmiss   = "Decision maker has missing choice sets",
rpmiss   = "Decision maker has missing RP data",
sprpmiss = "Analyst created variable ",
rpspwkbk = "Walk and/or Bike alternative present in RP choice set ",
rpcar    = "Car alternative present in RP choice set",
beforptr = "Point before main point",
afterptr = "Point after main point",
mptrfare = "Cost main form public transport [$]",
optrfare = "Cost other form public transport [$]",
homtoptr = "Mode of travel from home to first point",
ptrtowk  = "Last pt to work",
hhldveh  = "Household vehicle used to point",
wkkmveh  = "Vehicle km to/from point",
walktime = "Time walking-last trip to work",
mptrtime = "Time on main public transport",
waittime = "Time waiting for public transport",
optrtime = "Time on other public transport",
autopass = "Number & type passengers in car to work",
maxpass  = "Maximum number of passengers",
hldauto  = "Household vehicle driven to work",
autona   = "Car not applicable",
automake = "Make/model of vehicle driven to work",
autoyear = "Year manufacture of car driven to work",
autowkkm = "Distance travelled by car to work",
autotime = "Time spent travelling by car to work",
autowktm = "Time in car to work",
autwkwlk = "Walk-from car to work",
autwkbus = "Bus-from car to work",
autwktrn = "Train-from car to work",
autwkoth = "Other mode-from car to work",
vehppark = "Paid parking on last trip to work",
vehprkct = "Cost of parking on last trip to work",
vehptoll = "Paid toll on last trip to work",
vehtolct = "Toll costs on last trip to work",
vehpothe = "Paid other on last trip to work",
vehothct = "Other costs on last car trip to work",
vehpnoth = "Paid nothing on last car trip to work",
drpptran = "Drop passengers in car at public transport",
drpccare = "Drop passengers in car at childcare",
drpschol = "Drop passengers in car at school",
drpteduc = "Drop passengers in car at tertiary edu",
dropwork = "Drop passengers in car at work",
dropothe = "Drop passengers in car at other places",
dropdwel = "Drop passengers in car at dwelling",
nodropof = "Don’t drop any passengers in car off",
droptime = "Time taken to drop passengers from car",
triptime = "Trip time walk and bike",
deptime  = "Departure time",
disdwcbd = "Distance from dwelling to CBD",
vehstatu = "Vehicle status",
chawktr  = "Move work closer to home",
chadwtr  = "Move home closer to work",
splength = "SP experiment segment",
time     = "Travel time to work",
timevar  = "Time variability",
toll     = "Toll cost",
tollpred = "Times applied to tolls",
fuel     = "Fuel cost",
parking  = "Parking cost",
freq     = "Frequency of service",
fare     = "Return fare",
start24  = "Normal depart time",
acctime  = "Public transport access time",
eggtime  = "Public transport egress time",
hweight  = "Household weight",
numbvehs = "Number of vehicles in household",
hldincom = "Households income",
nhldbcar = "Number household business cars",
ncompcar = "Number company cars",
ndrivlic = "Number licences in household",
hldsize  = "Household size",
nworkers = "Num workers in household",
wkremply = "Employment type ",
wkroccup = "Occupation category",
perage   = "Person age",
drivlic  = "Person drivers licence ",
pincome  = "Personal income",
persex   = "Person sex",
pereduc  = "Person highest education",
acceggt  = "Access time plus egress time",
can      = "Canberra",
syd      = "Sydney",
mel      = "Melbourne",
brs      = "Brisbane",
adl      = "Adelaide"
)



asm <- asm %>%   
 mutate( 
  city    = factor(city, levels = 1:6, labels = c("Canberra","Sydney","Melbourne","Brisbane",
                  "Adelaide","Perth")),
  sprp	   = factor(sprp, levels = 1:2, labels = c("RP", "SP")),
  spexp	 = factor(spexp, levels = c(1:3,0) , labels = c("CHOICE SET 1 (SP)",
                                 "CHOICE SET 2 (SP)","CHOICE SET 3 (SP)","RP")), # recode 0 (for RP) to 4
  altisprp= factor(altisprp, levels = 1:12, labels = c("DRIVE ALONE (RP)","RIDE SHARE (RP)","BUS (RP)","TRAIN (RP)",
                "WALK (RP)","BICYCLE (RP)","CAR (TOLL) (SP)","CAR (NOT TOLL) (SP)","BUS (SP)",
                "TRAIN (SP)","LIGHT RAIL (SP)","BUSWAY (SP)")),
#  altij	= factor(altij),
  chsnmode	= factor(chsnmode, levels = c(0:5,8:16), 
                     labels = c("SP","Train (RP)","Bus (RP)","Tram (RP)","Ferry (RP)","Taxi (RP)","Walk (RP)",
                                "Motorbike (RP)","Bicycle (RP)","Drive alone (RP)",
                                "Drive & household passenger (RP)","Drive + other passenger (RP)",
                                "Drive +household passenger & other passenger (RP)",
                                "Passenger household vehicle (RP)","Passenger other vehicle (RP)")),
  altmode	= factor(altmode, levels = c(0:5,8:16), 
                   labels = c("SP","Train (RP)","Bus (RP)","Tram (RP)","Ferry (RP)","Taxi (RP)","Walk (RP)",
                              "Motorbike (RP)","Bicycle (RP)","Drive alone (RP)",
                              "Drive & household passenger (RP)","Drive + other passenger (RP)",
                              "Drive +household passenger & other passenger (RP)",
                              "Passenger household vehicle (RP)","Passenger other vehicle (RP)")),
  spchoice	= factor(spchoice, levels = c(1:6,0), # recode 0 (for RP) to 7 
                     labels = c("car with toll (SP)","car without toll (SP)","bus (SP)",
                                "train (SP)","busway (SP)","light rail (SP)","RP")),
#  choice	= factor(choice, levels = c(0,1), labels = c("not chosen","chosen")),
#  cset	= factor(cset, levels = c(2,4), labels = c("RP","SP")),
#  rpda	= factor(rpda, levels = c(1,0), labels = c("DRIVE ALONE CHOSEN (RP)","DRIVE ALONE NOT CHOSEN (RP)")),
#  rprs	= factor(rprs, levels = c(1,0), 
#                 labels = c("RIDE SHARE CHOSEN (RP)","RIDE SHARE NOT CHOSEN (RP)")),
#  rpbus	= factor(rpbus, levels = c(1,0), 
#                 labels = c("BUS CHOSEN (RP)","BUS NOT CHOSEN (RP)")),
#  rptn	= factor(rptn, levels = c(1,0), 
#          labels = c("TRAIN CHOSEN (RP)","0 = TRAIN NOT CHOSEN (RP)")),
#  rpwalk	= factor(rpwalk, levels = c(1,0), 
#                   labels = c("WALK CHOSEN (RP)","WALK NOT CHOSEN (RP)")),
#  rpbike	= factor(rpbike, levels = c(1,0), 
#                   labels = c("BICYLCE CHOSEN (RP)","BICYLCE NOT CHOSEN (RP)")),
#  spcart	= factor(spcart, levels = c(1,0), 
#                   labels = c("CAR WITH TOLL CHOSEN (SP)","CAR WITH TOLL NOT CHOSEN (SP)")),
#  spcarnt	= factor(spcarnt, levels = c(1,0), 
#                   labels = c("CAR WITH NO TOLL CHOSEN (SP)","CAR WITH NO TOLL NOT CHOSEN (SP)")),
#  spbus	= factor(spbus, levels = c(1,0), 
#                   labels = c("BUS CHOSEN (SP)","BUS NOT CHOSEN (SP)")),
#  sptn	= factor(sptn, levels = c(1,0), 
#                   labels = c("TRAIN CHOSEN (SP)","TRAIN NOT CHOSEN (SP)")),
#  spbw	= factor(spbw, levels = c(1,0), 
#                   labels = c("BUSWAY CHOSEN (SP)","BUSWAY NOT CHOSEN (SP)")),
#  splr	= factor(splr, levels = c(1,0), 
#                   labels = c("LIGHT RAIL CHOSEN (SP)","LIGHT RAIL NOT CHOSEN (SP)")),
  cn	= factor(cn, levels = c(1:4,0), # recode 0 (for RP/Walk...) to 5
                   labels = c("Bus - Train (SP)","Bus - Busway (SP)","Train - Light Rail (SP)",
                              "Busway - Light Rail (SP)","Walk and other alternative included (RP)")),
#  spmiss	= factor(spmiss, levels = 0:1, 
#                   labels = c("All choice sets present","One or more choice sets missing")),
#  rpmiss	= factor(rpmiss, levels = 0:1, 
#                   labels = c("Respondents RP data present","Respondents RP data missing")),
  sprpmiss	= factor(sprpmiss, levels = 0:1, labels = c("Use","Reject")),
  rpspwkbk	= factor(rpspwkbk, levels = 0:1,
                     labels = c("Walk and/or bike alternatives not present in RP choice set",
                                "Walk and/or bike alternatives are present in RP choice set")),
  rpcar		= factor(rpcar, levels = 0:1,
                     labels = c("Ride share and/or drive alone alternatives not present in RP choice set",
                                "Ride share and/or drive alone alternatives are present in RP choice set")),
  beforptr	= factor(beforptr, levels = 1:5,
                     labels = c("TRAIN","BUS","TRAM","FERRY","TAXI")),
  afterptr	= factor(afterptr, levels = 1:5,
                     labels = c("TRAIN","BUS","TRAM","FERRY","TAXI")),
  homtoptr	= factor(homtoptr, levels = 1:6,
                     labels = c("CAR THEN PARK","CAR THEN DROPPED OFF","MOTORBIKE","WALKED","TAXI","BICYCLE")),
  ptrtowk	= factor(ptrtowk, levels = 1:6,
                     labels = c("CAR THEN PARK","CAR THEN DROPPED OFF","MOTORBIKE","WALKED","TAXI","BICYCLE")),
  vehstatu	= factor(vehstatu, levels = 1:3,
                     labels = c("PRIVATE VEHICLE","HOUSEHOLD BUSINESS VEHICLE","COMPANY VEHICLE")),
  splength	= factor(splength, levels = 0:3,
                     labels = c("RP","Less than 30 Minutes","30 to 45 minutes","over 45 minutes")),
  hldincom	= factor(hldincom, levels = 1:11,
                     labels = c("Less than 5000","5000 - 12000","12001 - 20000","20001 - 30000",
                                "30001 - 40000","40001 - 50000","50001 - 60000","60001 - 70000",
                                "70001 - 80000","80001 - 90000","90001 - 120000")),
  wkremply	= factor(wkremply, levels = 1:3,
                     labels = c("Full Time","Part Time","Self Employed")),
  wkroccup	= factor(wkroccup, levels = 1:9,
                     labels = c("Managers and Admin","Professionals","Para-professional","Tradespersons",
                                "Clerks","Sales","Plant operators","Laborers","Other")),
  drivlic	= factor(drivlic, levels = 1:3, labels = c("YES","NO","NOT APPLICABLE")),
  persex	= factor(persex, levels = c(1,-1), labels = c("male","female")),
  pereduc	= factor(pereduc, levels = 1:5, labels = c("PRE-PRIMARYSCHOOL","PRIMARYSCHOOL","SECONDARYSCHOOL",
                                                     "TECH/COLLEGE","UNIVERSITY")),
  can	= factor(can, levels = c(1,-1,0), labels = c("CANBERRA","PERTH","OTHER")),
  syd	= factor(syd, levels = c(1,-1,0), labels = c("SYDNEY","PERTH","OTHER")),
  mel	= factor(mel, levels = c(1,-1,0), labels = c("MELBOURNE","PERTH","OTHER")),
  brs	= factor(brs, levels = c(1,-1,0), labels = c("BRISBANE","PERTH","OTHER")),
  adl	= factor(adl, levels = c(1,-1,0), labels = c("ADELAIDE","PERTH","OTHER"))
   ) 

# Apply variable labels 
for (var in names(addlabels)) {
  var_label(asm[[var]]) <- addlabels[[var]]
}

cat("\nCase study data of Hensher/Rose/Greene ACA-Primer, 1st ed, 2005 
    (import from SPRP.txt received from Authors on March 2 2025)\n
ToC:
    I) (near) replication of Table 9.11 Breakdown by City for the SP and RP data sets (p. 285)
    II)  Codebook ('Data frame summary')\n\n
I) (near) replication of HRG2005 Table 9.11 - Breakdown by city for the SP and RP data sets (p. 285)\n\n")
asm$city <- factor(asm$city, levels = c("Sydney", "Canberra", "Melbourne", "Adelaide", "Brisbane", "Perth"))  # order as in table 9.11 of the book 

cat(paste0("\nData set dimension: ",dim(asm)[1]," * ",dim(asm)[2],"\n"))
cat("Data set dimension is consistent with p.287 of the book.\n\n")

asmd <- asm %>% distinct(id,sprp, .keep_all = TRUE) # asm w one obs per ID
a <- addmargins(table(asmd$city,asmd$sprp),1)
b <- addmargins(table(asmd$city[asmd$sprp=="SP"],asmd$splength[asmd$sprp=="SP"]),1)[,2:4]
cbind(a,b)
cat("\n\  The number of RP-respondents (866) conforms with table 9.11. Total number 
of SP respondents (1212) exceeds the figure in table 9.11 (1204). Table 9.11 (=Tab 7.9 in the WordDoc)
indicates lower numbers for Canberra in the group of '<30 minutes' (104) and for Adelaide (122/28/12).")

asm$city <- factor(asm$city, levels = c("Canberra", "Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth"))  # order as in table 9.11 of the book 
asm <- asm %>%filter(splength != "RP") # this removes 2 RP records of id 6207, the only TWO records with splength = "RP"

saveRDS(asm,file=outdat)  # saves in R format
cat(paste("\n\nData file saved:",outdat))

cat("\n\n\nII) Codebook of saved data set (i.e. after removal of 2 records with splength='RP' so as 
  to achieve consistency (16186 Obs)  w Appendix 9B(7B): Mode choice Case Study Data Dictionary)\n")
dfSummary(asm, max.distinct.values = 15)
    

#factorvars <- asm %>% select_if(is.factor) %>% names() 
#codebookentries(factorvars,asm)


sink()