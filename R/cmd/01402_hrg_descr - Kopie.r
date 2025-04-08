#  screen Hensher Data 
# input : dat/01401_hensher_imp.Rds
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

# asm <- read_excel("../raw/Data/SPRP.xls", col_names = TRUE)
asm <- readRDS(file = "dat/01401_hensher_imp.Rds")  %>%
#  filter(!(SPMISS == "One or more choice sets missing" & CITY=="Canberra" & SPLENGTH == "Less than 30 Minutes ")) %>% # remove with incomplete choice sets from the Canberra data
###  filter(!(SPMISS == "One or more choice sets missing" & CITY=="Adelaide")) %>% # remove with incomplete choice sets from the adelaide data
###  filter(ID != 5047) %>% # the only Adelaide household with least number of completed choices among short trip 
###  filter(!ID %in% c(1053, 1064)) %>% # the only Canberra households with only one completed choice 
###  filter(!ID %in% c(1209)) %>% # arbitrary choice of one out of three Canberra IDs with only two completed choices
#  filter(SPRP == "SP") %>%  select(-SPRP)  %>% # remove RP records
   select(-CHSNMODE,-ALTMODE,-CSET,-starts_with("RP"),RPMISS,RPDA,-MPTRFARE,-OPTRFARE,-HOMTOPTR,-PTRTOWK,-HHLDVEH,-WKKMVEH,
          -(30:70),-(74:76),-NUMBVEHS,-NHLDBCAR,-NCOMPCAR,-NDRIVLIC) %>%  # remove cols (temporally) not needed
   select(-SPCART,-SPCARNT,-SPBUS,-SPTN,-SPBW,-SPLR)  # remove cols indicating "X chosen"/"X not chosen" for each of six modes X


#table(asm$SPRPMISS)
#table(as.numeric(asm$SPRPMISS))

#asm %>% select(ID, SPRP, SPMISS, RPMISS, SPRPMISS)

#asm <- asm %>% filter(SPMISS == "All choice sets present") # remove incomplete SP records


asm$SPLENGTH <- factor(as.numeric(asm$SPLENGTH) , levels = 1:4,  labels = c("RP","<30", "30-45", ">45"))
asm$PEREDUC <-  factor(as.numeric(asm$PEREDUC)   , levels = 1:5,  labels = c("PP", "Prim", "Sec", "TeCo", "Uni"))
asm$WKROCCUP <-  factor(as.numeric(asm$WKROCCUP) , levels = 1:9,  labels = c("Mgr", "Prof", "PP", "Trd", "Clk", "Sls", "Plt", "Lab", "Oth"))
asm$WKREMPLY <-  factor(as.numeric(asm$WKREMPLY) , levels = 1:3,  labels = c("Full", "Part", "SE"))
asm$HLDINCOM <-  factor(as.numeric(asm$HLDINCOM) , levels = 1:11, labels = c("<5","5-12","12-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-120"))
asm$SPEXP <-  factor(as.numeric(asm$SPEXP) , levels = 1:4, labels = c("RP","CS1","CS2","CS3"))
asm$SPMISS <-  factor(as.numeric(asm$SPMISS) , levels = 1:2, labels = c("cpl","miss"))
asm$RPMISS <-  factor(as.numeric(asm$RPMISS) , levels = 1:2, labels = c("cpl","miss"))
asm$RPDA <-  factor(as.numeric(asm$RPDA) , levels = 1:2, labels = c("DA","nDA"))
asm$DRIVLIC <-  factor(as.numeric(asm$DRIVLIC) , levels = 1:3, labels = c("Y","N","nA"))
options(width=256)  # for A4 landscape 

asm$CITY <- factor(asm$CITY, levels = c("Sydney", "Canberra", "Melbourne", "Adelaide", "Brisbane", "Perth"))  # order as in table 9.11 of the book 

cat("\nBEFORE ANY FILTERING addmargins(table(asm$CITY,asm$SPRP))\n")
cat("\ndim(asm)\n")
dim(asm)


asm$RPDA <- as.numeric(asm$RPDA)

table(asm$RPDA,useNA = "always")


asm %>%
  summarise(across(c(ID, PINCOME, DEPTIME,RPDA), list(
    Mean = ~ mean(.x, na.rm = TRUE),
    SD = ~ sd(.x, na.rm = TRUE),
    Min = ~ min(.x, na.rm = TRUE),
    Max = ~ max(.x, na.rm = TRUE),
    N = ~ sum(!is.na(.x))
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "_") %>%
  pivot_wider(names_from = "Statistic", values_from = "value") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))  # Round numeric values to 3 decimals


asmd <- asm %>% distinct(ID,SPRP, .keep_all = TRUE) # asm w one obs per ID
addmargins(table(asmd$CITY,asmd$SPRP))
addmargins(table(asmd$CITY[asmd$SPRP=="SP"],asmd$SPLENGTH[asmd$SPRP=="SP"]))

cat("\n\nFILTER TO CONCENTRATE ON SUBSETS WITH SUPERFLOUS SP RECORDS \n")
#asm <- asm %>%  # filter((CITY=="Canberra" & SPLENGTH == "<30") ) %>% # | (CITY=="Adelaide"))  %>%
#         #  filter(RPMISS=="miss")  %>%  # dont remove any IDs with RP present because the number of 866 RP IDs is correct
asm <- asm %>%  filter(!is.na(RPDA)  ) %>% 
           select(ID,CITY,SPEXP,SPRP,ALTISPRP,CHOICE,SPLENGTH,SPMISS,RPMISS,SPRPMISS,TIME,TIMEVAR,TOLL,TOLLPRED,FUEL,
                  PARKING,FREQ,FARE, ACCTIME,EGGTIME,HWEIGHT,HLDINCOM,HLDSIZE,NWORKERS,WKREMPLY,WKROCCUP,PERAGE,
                  DRIVLIC,PINCOME,PERSEX,PEREDUC,RPDA) %>%
           print() 

asm %>%  count(ID,SPRP,sort = TRUE)

cat("\n\nasm %>% distinct(ID,SPRP, .keep_all = TRUE)\n")
asmd <- asm %>% distinct(ID,SPRP, .keep_all = TRUE)


cat("\n\n AFTER FILTERING addmargins(table(asmd$CITY,asmd$SPRP))\n")
addmargins(table(asmd$CITY,asmd$SPRP))
addmargins(table(asmd$CITY[asmd$SPRP=="SP"],asmd$SPLENGTH[asmd$SPRP=="SP"]))


#n1 <- 1
#
#d <-  14
#
#table(asm$ID)


#asm <- asm %>% arrange(ID)

#asm[1:14,]




#cat("\n\nasm[1:30,n1:(n1+d)]\n")
#asm[1:30,n1:(n1+d)]
#cat("\n\nasm[1:30,(n1+1*d+1):(n1+2*d)]\n")
#asm[1:30,(n1+1*d+1):(n1+2*d)]
#cat("\n\nasm[1:30,(n1+2*d+1):(n1+3*d)]\n")
#asm[1:30,(n1+2*d+1):(n1+3*d)]
#cat("\n\nasm[1:30,(n1+3*d+1):(n1+4*d)]\n")
#asm[1:30,(n1+3*d+1):(n1+4*d)]
#asm[1:30,(n1+4*d+1):(n1+5*d)]
#asm[1:30,(n1+5*d+1):ncol(asm)]
#asm[1:30,(n1+6*d+1):ncol(asm)]



#asm[16160:16188,(n1+1*d+1):(n1+2*d+1)]
#asm[16160:16188,(n1+1*d+1):(n1+2*d+1)]
#asm[1:50,,n2:n2+d]
#asm[16160:16188,,n2:n2+d]
#asm[1:50,n3:n3+d]
#asm[16160:16188,n3:n3+d]

sink()