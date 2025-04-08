# NICHT WEITER GENUTZT clean data from 01401_hensher_imp 
# input : dat/01401_hensher_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
lisdatu <- paste("lis\\",codefile, "_ua.lis",sep="")   # for naming listing file W UKRAINIAN LABELS
lisdate <- paste("lis\\",codefile, "_en.lis",sep="")   # for naming listing file W ENGLISH LABELS
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
#sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=220)  # for A4 landscape 

library(tidyverse)
#library(olsrr) # auxiliary regression 
library(labelled) # var_label
library(summarytools)  # dfSummary
library(apollo)  # for apollo_longToWide


asm <- readRDS(file = "dat/01401_hensher_imp.Rds") %>%
   select(id,spexp,altij,choice,cn,splength,time,timevar,toll,tollpred,fuel,parking,acctime,eggtime)

cat("\n\nhead(asm)\n")
head(asm)

codebookentries("splength",asm)

cat("\n\naddmargins(table(asm$splength,asm$time,useNA='ifany'))\n")
addmargins(table(asm$splength,asm$time,useNA="ifany"))


cat("\n\n(splength)==3 & as.numeric(time) %in% c(10,12,15)) %>%\n")
asm %>% filter(as.numeric(splength)==3 & as.numeric(time) %in% c(10,12,15)) %>%
   head()
cat("\n\naaa <- asm %>% filter(as.numeric(splength)==2 & time >25) \n")
aaa <- asm %>% filter(as.numeric(splength)==2 & time >20) 
   table(aaa$time)
cat("\n\nasm <- asm %>% filter(id %in% c(1007,1017,1048,1056,1070,1071)) \n")
asm <- asm %>% filter(id %in% c(1007,1017,1048,1056,1070,1071)) 

cat("\n\nhead(asm)\n")
head(asm,n=20)


addmargins(table(asm$splength,asm$time,useNA="ifany"))

