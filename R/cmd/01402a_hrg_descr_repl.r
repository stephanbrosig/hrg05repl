#  screen hrg Data 
# input : dat/01401_hrg_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
lisdatu <- paste("lis\\",codefile, "_ua.lis",sep="")   # for naming listing file W UKRAINIAN LABELS
lisdate <- paste("lis\\",codefile, "_en.lis",sep="")   # for naming listing file W ENGLISH LABELS
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=122)  # for A4 landscape 

library(tidyverse)
#library(olsrr) # auxiliary regression 
library(labelled) # var_label
library(summarytools)  # dfSummary


#asm <- as.tibble(readRDS(file = "dat/01401_hrg_imp.Rds"))  
asm <- as_tibble(readRDS(file = "dat/01401a_hrg_imp_repl.Rds"))  

##  ## change factor levels ("labels") to shorter versions
##  asm$splength <- factor(as.numeric(asm$splength) , levels = 1:4,  labels = c("RP","<30", "30-45", ">45"))
##  asm$pereduc  <- factor(as.numeric(asm$pereduc)   , levels = 1:5,  labels = c("PP", "Prim", "Sec", "TeCo", "Uni"))
##  asm$wkroccup <- factor(as.numeric(asm$wkroccup) , levels = 1:9,  labels = c("Mgr", "Prof", "PP", "Trd", "Clk", "Sls", "Plt", "Lab", "Oth"))
##  asm$wkremply <- factor(as.numeric(asm$wkremply) , levels = 1:3,  labels = c("Full", "Part", "SE"))
##  asm$hldincom <- factor(as.numeric(asm$hldincom) , levels = 1:11, labels = c("<5","5-12","12-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-120"))
##  asm$altijc    <- factor(asm$altij , levels = 1:6, labels = c("cart","carnt","bus","train","LR","BW"))
##  asm$spexp    <- factor(as.numeric(asm$spexp) , levels = 1:4, labels = c("CS1","CS2","CS3","RP"))
##  asm$spmiss   <- factor(as.numeric(asm$spmiss) , levels = 1:2, labels = c("cpl","miss"))
##  asm$rpmiss   <- factor(as.numeric(asm$rpmiss) , levels = 1:2, labels = c("cpl","miss"))
##  asm$rpda     <- factor(as.numeric(asm$rpda) , levels = 1:2, labels = c("DA","nDA"))
##  asm$drivlic  <- factor(as.numeric(asm$drivlic) , levels = 1:3, labels = c("Y","N","nA"))

#cat("\n\nFIRST 24 OBSERVATIONS OF SP-RELEVANT COLUMNS IN DATA SAVED BY 01401_hrg_imp.r (w some labels changed)\n")
#asm %>% 
#    filter(sprp==2 & splength==1) %>%    # only SP data of respondents with <30 min commuting
#    select(id,spexp,altij,       choice,time,timevar,toll,tollpred,fuel,parking,freq,fare) %>% print(n=24)

asm <- asm %>%  filter(sprp == 2 & splength == 1 ) %>% # subset using SP data and splength = < 30 min 
   select(-chsnmode,-altmode,-cset,-starts_with("rp"),rpmiss,rpda,-mptrfare,-optrfare,-homtoptr,-ptrtowk,-hhldveh,
   -wkkmveh,-(30:70),-(74:76),-numbvehs,-nhldbcar,-ncompcar,-ndrivlic) %>%  # remove cols (temporally) not needed
   select(-spcart,-spcarnt,-spbus,-sptn,-spbw,-splr) # remove cols indicating "X chosen"/"X not chosen" for each of six modes X

#options(width=256)  # for A4 landscape 

#asm$city <- factor(asm$city, levels = c(2,1,3,5,4,6))  # order as in table 9.11 of the book 

# asms <- asm 
# asmd <- asms %>% distinct(id,sprp, .keep_all = TRUE)
# 
# a <- addmargins(table(asmd$city,asmd$sprp),1)
# b <- addmargins(table(asmd$city[asmd$sprp==2],asmd$splength[asmd$sprp==2]),1)
# cbind(a,b)

##  cat("\nRESULT OF FIRST AUXILIARY REGRESSION, cf HRG2005: 293\n")
##  asms1 <- asms %>%   filter(altij == "1")  
##  mod1 <- lm(time ~ timevar + toll + tollpred + fuel + parking, data = asms1)
##  summary(mod1)
##  
##  cat("\nRi statistic for model w time on LHS as on p. 294\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asms1$time - mean(asms1$time))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(timevar ~ time + toll + tollpred + fuel + parking, data = asms1)
##  cat("\nRi statistic for model w timevar on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asms1$timevar - mean(asms1$timevar))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(toll ~ time + timevar + tollpred + fuel + parking, data = asms1)
##  cat("\nRi statistic for model w toll on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asms1$toll - mean(asms1$toll))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(tollpred ~ time + timevar + toll + fuel + parking, data = asms1)
##  cat("\nRi statistic for model w tollpred on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asms1$tollpred - mean(asms1$tollpred))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(fuel ~ time + timevar + toll + tollpred + parking, data = asms1)
##  cat("\nRi statistic for model w fuel on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asms1$fuel - mean(asms1$fuel))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(parking ~ time + timevar + toll + tollpred + fuel, data = asms1)
##  cat("\nRi statistic for model w parking on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asms1$parking - mean(asms1$parking))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  


dim(asm)


library(apollo)  

longToWide_settings <- list()                                                                                  
longToWide_settings[["altColumn"]] <- "altij"  # use character type (if factor type is used ....
longToWide_settings[["altSpecAtts"]] <- c("time","timevar","toll","tollpred","fuel","parking","freq","fare")
longToWide_settings[["choiceColumn"]] <- "choice"
longToWide_settings[["idColumn"]] <- "id"
longToWide_settings[["obsColumn"]] <- "spexp"

database = apollo_longToWide(asm,  longToWide_settings)


colnames(database)



sink()