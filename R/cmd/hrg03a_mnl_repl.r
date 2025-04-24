#  screen Hensher Data 
# input : dat/hrg01_hensher_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
lisdatu <- paste("lis\\",codefile, "_ua.lis",sep="")   # for naming listing file W UKRAINIAN LABELS
lisdate <- paste("lis\\",codefile, "_en.lis",sep="")   # for naming listing file W ENGLISH LABELS
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
#sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=122)  # for A4 landscape 

library(tidyverse)
#library(olsrr) # auxiliary regression 
library(labelled) # var_label
library(summarytools)  # dfSummary


#asm <- as.tibble(readRDS(file = "dat/hrg01_hensher_imp.Rds"))  
asm <- as_tibble(readRDS(file = "dat/hrg01a_hensher_imp_repl.Rds"))  

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

cat("\n\nFIRST 24 OBSERVATIONS OF SP-RELEVANT COLUMNS IN DATA SAVED BY hrg01_hensher_imp.r (w some labels changed)\n")
asm %>% 
    filter(sprp==2 & splength==1) %>%    # only SP data of respondents with <30 min commuting
    select(id,spexp,altij,       choice,time,timevar,toll,tollpred,fuel,parking,freq,fare) %>% print(n=24)

# CHECK CORRECT LABELLING OF ALTERNATIVES
asm <- asm %>%  filter(sprp == 2 & splength == 1 ) %>% # subset using SP data and splength = < 30 min 
   select(-chsnmode,-altmode,-cset,-starts_with("rp"),rpmiss,rpda,-mptrfare,-optrfare,-homtoptr,-ptrtowk,-hhldveh,
   -wkkmveh,-(30:70),-(74:76),-numbvehs,-nhldbcar,-ncompcar,-ndrivlic) # %>%  # remove cols (temporally) not needed



 asm$cnf <- factor(asm$cn, levels = 1:4, labels = c("1: b+tr (34)","2: b+bw (35)","3: tr+lr (46)","4: bw+lr (56)"))

asm$altij    <-      as.character(factor(asm$altij , levels = 1:6, labels = c("CART","CARNT","BUS","TR","BW","LR"))) # BW and LR swapped to conform with SPLR and SPBW

 asm %>% select(id,spexp,choice,altij,spchoice,cn,cnf,spcart,spcarnt,spbus,sptn,spbw,splr)  %>%  
      print(n=50) 

cat("\n\nasm %>% filter(choice==1)\n")
 asm  %>% select(id,spexp,choice,altij,spchoice,cn,cnf,spcart,spcarnt,spbus,sptn,spbw,splr)  %>%      
      filter(choice==1) %>%
      print(n=12)

## cat("\n\nasm %>% filter(choice==1 & spchoice != altij)\n")
##  asm %>%
##       filter(choice==1 & spchoice != altij) %>%
##       print()
## cat("\n\nasm %>% filter(choice != 1 & spchoice == altij)\n")
##  asm %>%
##       filter(choice != 1 & spchoice == altij) %>%
##       print() %>% dim() %>% print()
## cat("\n\nasm %>% filter(choice == 1 & spchoice == altij)\n")
##  asm %>%
##       filter(choice == 1 & spchoice == altij) %>%
##       print() %>% dim() %>% print()

asm <- asm  %>%  select(-spcart,-spcarnt,-spbus,-sptn,-spbw,-splr) # remove dummies indicating "X chosen"/"X not chosen" for each of six modes X

#options(width=256)  # for A4 landscape 

#asm$city <- factor(asm$city, levels = c(2,1,3,5,4,6))  # order as in table 9.11 of the book 

## AUXILIARY REGRESSIONS 
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


cat("\n\ndim(asm)\n")
dim(asm)
cat("\n\nstr(asm)\n")
str(asm)

library(apollo)  

longToWide_settings <- list()                                                                                  
longToWide_settings[["altColumn"]] <- "altij"  # use character type (if factor type is used ....
longToWide_settings[["altSpecAtts"]] <- c("time","timevar","toll","tollpred","fuel","parking","freq","fare")
longToWide_settings[["choiceColumn"]] <- "choice"
longToWide_settings[["idColumn"]] <- "id"
longToWide_settings[["obsColumn"]] <- "spexp"

database = apollo_longToWide(asm,  longToWide_settings)

cat("\n\ndim(database)\n")
dim(database)
cat("\n\ncolnames(database)\n")

cat("\n\nhead(database)\n")
head(database)
cat("\n\nstr(database$choice_new)\n")
str(database$choice_new)

database$choice_new <- as.vector(database$choice_new)
#database$choice_new <- as.numeric(factor(as.vector(database$choice_new), levels = 1:6, labels = c("CART","CARNT","TR","BW","LR","BUS")))    
#database$choice_new <- as.numeric(factor(as.vector(database$choice_new), levels = 1:6, labels = c("1","2","4","6","5","3")))    


### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "hrg03a_hensher_mnl_repl",
  modelDescr      = "hrg03a_hensher_mnl_repl: Replication of Hensher Ch 10.3(p. 316f) MNL result",
  indivID         = "id", 
  outputDirectory = "hrg03a_hensher_mnl_repl", 
  panelData = TRUE
#  workInLogs=TRUE
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# Define model parameters                                                                    
apollo_beta <- c(                                                                            
 asc_cart  = -.1,
 b_cost     =  -.2,
 asc_carnt =  .4,
 asc_bus    =  .1,
 asc_tr    =  .01,
 asc_bw    = -.04
 )                       

#sink(lisdat, append = TRUE, type="output")
#cat("\n\nSTARTING VALUES AND CONSTRAINED PARAMETERS: rbind(apollo_beta)\n")
#cbind(apollo_beta)                                                                                            
                                                                                             
# Indicate which parameters are fixed                                                        
apollo_fixed <- c() #  c("asc_lr")

#cbind(apollo_fixed)


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant

V <- list()                                                                                  
V[["CART"]]  <- asc_cart   + b_cost * fuel_CART
V[["CARNT"]] <- asc_carnt  + b_cost * fuel_CARNT
V[["BUS"]]   <- asc_bus    + b_cost * fare_BUS
V[["TR"]]    <- asc_tr     + b_cost * fare_TR
V[["BW"]]    <- asc_bw     + b_cost * fare_BW
V[["LR"]]    <-              b_cost * fare_LR

  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(CART=1, CARNT=2, TR=3, LR=4, BW=5, BUS=6), 
    avail         = list(CART=avail_CART, CARNT=avail_CARNT, TR=avail_TR, BW=avail_BW, LR=avail_LR, BUS=avail_BUS), 
    choiceVar     = choice_new,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#sink(lisdat, append = TRUE, type="output")
cat("\n\nmodel = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)\n")
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
#sink()
# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #
sink(lisdat, append = TRUE, type="output")


cat("\n\napollo_modelOutput(model)\n")
apollo_modelOutput(model, modelOutput_settings = list(printDataReport = TRUE))
sink()
##  # ----------------------------------------------------------------- #
##  #---- FORMATTED OUTPUT (TO FILE, using model name)               ----
##  # ----------------------------------------------------------------- #
##  
##  apollo_saveOutput(model, saveOutput_settings = list(saveOld = FALSE, saveEst = FALSE, saveEst = FALSE,
##                      saveModelObject = FALSE, printDataReport = TRUE, printOutliers = FALSE,
##                      saveHBiterations = FALSE, printFunctions = TRUE))
##  
##  
##   
##  ## ################################################################# #
##  ###### POST-PROCESSING                                            ####
##  ## ################################################################# #
##  #
##  #### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
##  #apollo_sink()
##  #
##  #### calculate value and standard error for base of effects coded parameter
##  #
##  #
##  #sink(lisdat, append = TRUE, type="output")
##  #cat("\n\napollo_deltaMethod....\n")
##  #apollo_deltaMethod(model,deltaMethod_settings = list(expression=c(WTX_asc_none  = "-asc_none /  b_AddIncome",
##  #                                                                  WTX_Topic_poll  = "-b_Topic_poll /  b_AddIncome",
##  #                                                                  WTX_Topic_bdiv  = "-b_Topic_bdiv / b_AddIncome",
##  #                                                                  WTX_Audit_sele  = "-b_Audit_sele / b_AddIncome",
##  #                                                                  WTX_Audit_compl = "-b_Audit_compl / b_AddIncome",
##  #                                                                  WTX_ReportPrep_cons  = "-b_ReportPrep_cons / b_AddIncome",
##  #                                                                  WTX_RiskAddI_mod  = "-b_RiskAddI_mod / b_AddIncome",
##  #                                                                  WTX_RiskAddI_high  = "-b_RiskAddI_high / b_AddIncome",
##  #                                                                  WTX_ReputEnh_Slight  = "-b_ReputEnh_Slight / b_AddIncome",
##  #                                                                  WTX_ReputEnh_Substantial  = "-b_ReputEnh_Substantial / b_AddIncome"   )))
##  #
##  ##sink()
##  #
##  #
##  ## ----------------------------------------------------------------- #
##  ##---- switch off writing to file                                 ----
##  ## ----------------------------------------------------------------- #
##  #
##  #apollo_sink()



#cat("\n\nols_vif_tol((mod1)\n")
#ols_vif_tol(mod1)


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


sink()