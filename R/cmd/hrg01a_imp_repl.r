#  import Hensher Data and apply labels
# input : ../raw/Data/SPRP.txt received via Email on 250302
# output: /dat/XXX.Rds and /lis/XXX.lis  with XXX replaced by the name of this code file  (/cmd/XXX.r)
#         

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
asm <- read.csv("../raw/Data/SPRP.txt",header=FALSE, sep = "\t",col.names = spalnam) %>%
       filter(!is.na(id))  %>% # raw data has two records with no data at the end: remove
       mutate(across(everything(), ~ na_if(.x, -999)))  # replace -999 by NA

#asm <- read_excel("../raw/Data/SPRP.xls", col_names = TRUE)


#cat("\n\nstr(asm)\n")
#str(asm,list.len = 200)


 cat("\nCase study data of Hensher/Rose/Greene ACA-Primer, 1st ed, 2005 
     (import from SPRP.txt received from Authors on March 2 2025)\n
 ToC:
     I) (near) replication of Table 9.11 Breakdown by City for the SP and RP data sets (p. 285)
     II)  Codebook ('Data frame summary')\n\n
 I) (near) replication of HRG2005 Table 9.11 - Breakdown by city for the SP and RP data sets (p. 285)\n")
asm2 <- asm
 asm2$city <- factor(asm$city, levels = c(2,1,3,5,4,6), labels = c("Sydney", "Canberra", "Melbourne", "Adelaide", "Brisbane", "Perth"))  # order as in table 9.11 of the book 
 asm2$splength <- factor(asm$splength, levels = 0:3, labels = c("RP", "<30", "30-45", ">45"))  # order as in table 9.11 of the book 
 asmd <- asm2 %>% distinct(id,sprp, .keep_all = TRUE) # asm w one obs per ID
 a <- addmargins(table(asmd$city,asmd$sprp),1)
 b <- addmargins(table(asmd$city[asmd$sprp==2],asmd$splength[asmd$sprp==2]),1)[,2:4]
 cbind(a,b)
cat(paste0("\nData set dimension: ",dim(asm)[1]," * ",dim(asm)[2],"\n"))
cat("\n\  Data set dimension is consistent with p.287 of the book. The number of RP-respondents (866) conforms
with table 9.11. Total number of SP respondents (1212) exceeds the figure in table 9.11 (1204).")

#asm$city <- factor(asm$city, levels = c("Canberra", "Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth"))  # order as in table 9.11 of the book 
#asm$splength <- as.integer(asm$splength) -1  # order as in table 9.11 of the book but without level 0 ("RP") 


asm <- asm %>%filter(splength != 0) # this removes 2 RP records of id 6207, the only TWO records with splength = "RP"

saveRDS(asm,file=paste("dat/",codefile,".Rds",sep=""))  # saves in R format


cat("\n\n\nII) Codebook of saved data set (i.e. after removal of 2 records with splength='RP' so as 
  to achieve consistency (16186 Obs)  w Appendix 9B(7B): Mode choice Case Study Data Dictionary)\n")
dfSummary(asm, max.distinct.values = 15)
    

##factorvars <- asm %>% select_if(is.factor) %>% names() 
##codebookentries(factorvars,asm)
#

sink()