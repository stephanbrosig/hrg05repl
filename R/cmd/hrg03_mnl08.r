#  hrg 2005 p. 355f: interactions2 (nl: cat)
# input : datdir/hrg01_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=250)  # for A4 landscape 

library(tidyverse)
library(apollo)  
library(modelr)  


asm <- readRDS(file = paste0(datdir,"/hrg01_imp.Rds"))  %>%  # local drive datdir as defined in _hrg_steer.r
  select(-city,-chsnmode,-altmode,-cset,-altisprp,-starts_with("rp"),rpmiss,rpda,-mptrfare,-optrfare,-homtoptr,-ptrtowk,
  -hhldveh,-wkkmveh,-(30:70),-(74:76),-nhldbcar,-ncompcar) %>%  # remove cols (temporally) not needed
  select(-spcart,-spcarnt,-spbus,-sptn,-spbw,-splr)
  
asm <- asm %>%  # subset using SP data and splength = < 30 min 
      filter(sprp == "SP" & splength == "Less than 30 Minutes" )
      asm$spchoice <- droplevels(asm$spchoice) # level 0 "RP" not needed after filtering SP

## change factor levels ("labels") to shorter versions
asm$splength <- factor(as.numeric(asm$splength) , levels = 1:4,  labels = c("RP","<30", "30-45", ">45"))
asm$pereduc  <- factor(as.numeric(asm$pereduc)   , levels = 1:5,  labels = c("PP", "Prim", "Sec", "TeCo", "Uni"))
asm$wkroccup <- factor(as.numeric(asm$wkroccup) , levels = 1:9,  labels = c("Mgr", "Prof", "PP", "Trd", "Clk", "Sls", "Plt", "Lab", "Oth"))
asm$wkremply <- factor(as.numeric(asm$wkremply) , levels = 1:3,  labels = c("Full", "Part", "SE"))
asm$hldincom <- factor(as.numeric(asm$hldincom) , levels = 1:11, labels = c("<5","5-12","12-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-120"))
asm$spexp    <- factor(as.numeric(asm$spexp) , levels = 1:4, labels = c("RP","CS1","CS2","CS3"))
asm$altij    <- factor(asm$altij , levels = 1:6, labels = c("CART","CARNT","BUS","TR","BW","LR")) # BW and LR swapped to conform with SPLR and SPBW IN THE DATA


asm$altijc   <- as.character(asm$altij) # apollo requires character type for alternatives variable

codebookentries("spchoice",asm)

#asm$spchoice <- factor(as.numeric(asm$spchoice) , levels = 1:7, labels = c("1_cart","2_carnt","3_bus","4_tr","5_bw","6_lr","0_7RP"))
asm$spchoicec <- as.character(factor(as.numeric(asm$spchoice) , levels = 1:7, labels = c("1_cart","2_carnt","3_bus","4_tr","5_bw","6_lr","0_7RP")))
#asm$choice <- factor(as.numeric(asm$choice) , levels = 1:2, labels = c("0","1"))
asm$cnf <- factor(as.numeric(asm$cn), levels = 1:5, labels = c("1: b+tr","2: b+bw","3: tr+lr","4: bw+lr","5 (=0): walk... (RP)"))
asm$spmiss   <- factor(as.numeric(asm$spmiss) , levels = 1:2, labels = c("cpl","miss"))
asm$rpmiss   <- factor(as.numeric(asm$rpmiss) , levels = 1:2, labels = c("cpl","miss"))
asm$rpda     <- factor(as.numeric(asm$rpda) , levels = 1:2, labels = c("DA","nDA"))
asm$drivlic  <- factor(as.numeric(asm$drivlic) , levels = 1:3, labels = c("Y","N","nA"))

asm <- as_tibble(asm) %>%
  select(-sprpmiss,-triptime,-deptime,-disdwcbd,-start24,-acctime,-eggtime,-hweight,-hldincom,-hldsize,
         -nworkers,-wkremply,-wkroccup,-perage,-pincome,-persex,-pereduc,-acceggt,-can,-syd,-mel,
         -brs,-adl,-rpmiss,-rpda, -sprp,  -spmiss,
         -splength) 
#         -time,-timevar,-toll,-tollpred,-parking,-freq,-numbvehs,-ndrivlic,-drivlic)

##  asm <- asm %>% 
##     mutate(CART  = 1,
##            CARNT = 1,
##            BUS   = if_else(as.numeric(cn) %in% c(1,2),1,0),
##            TR    = if_else(as.numeric(cn) %in% c(1,3),1,0),
##            BW    = if_else(as.numeric(cn) %in% c(2,4),1,0),
##            LR    = if_else(as.numeric(cn) %in% c(3,4),1,0))
##  cat("\n\nprint(asm)\n")
##  print(asm, n=150)


#asm$altij    <- as.character(asm$altij)
#asm$choice    <- as.logical(asm$choice)
asm$spexp    <- as.numeric(asm$spexp)
#asm$spchoice    <- as.numeric(asm$spchoice)
#asm$choice    <- as.numeric(asm$choice) -1

cat("\n\nprint(asm) for id %in% c(1000,1001,1002,1006,1008 \n")
asm %>% 
        filter(id %in% c(1000,1001,1002,1006,1008)) %>%
        print()

longToWide_settings <- list()                                                                                  
longToWide_settings[["altColumn"]] <- "altijc"  # use character type (if factor type is used ....
longToWide_settings[["altSpecAtts"]] <- c("time","timevar","toll","tollpred","fuel","parking","freq","fare")
longToWide_settings[["choiceColumn"]] <- "choice"
longToWide_settings[["idColumn"]] <- "id"
longToWide_settings[["obsColumn"]] <- "spexp"

#invisible(capture.output(database <- apollo_longToWide(asm,  longToWide_settings))) # saves wide data in database. delete "invisible(capture.output(" for description of reshaping
                          database <- apollo_longToWide(asm,  longToWide_settings)   # saves wide data in database. delete "invisible(capture.output(" for description of reshaping



#cat("\n\nglimpse(database)\n")
#glimpse(database)

database <- database %>% 
#                     mutate(choice_newf =              factor(as.vector(choice_new), levels = 1:6, 
#                                                         labels = c("CART","CARNT","TR","LR","BW","BUS")))  %>%
                     mutate(choice_newc = as.character(factor(as.vector(choice_new), levels = 1:6, 
                                                         labels = c("CART","CARNT","TR","LR","BW","BUS")))) %>%   
                     mutate(choice_newn =   as.numeric(factor(as.vector(choice_new), levels = 1:6, 
                                                        labels = c("1CART","2CARNT","3TR","4LR","5BW","6BUS"))) # this converts a labelled vector to a numeric vector which is necessary for apollo's choiceVar
                     )  

   
### Initialise code
apollo_initialise()
sink(lisdat, append = TRUE, type="output")  # redirect output again after apollo_initialize

cat("\n\n### Set core controls\n")
### Set core controls
apollo_control = list(
  modelName       = "hrg03_mnl8",
  modelDescr      = "hrg03a_mnl8: hrg 2005 p.360ff Choice probabilities",
  indivID         = "id", 
  outputDirectory = "hrg03_mnl", 
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
 asc_bw    = -.04,
 asc_lr    = 0
)                      

#sink(lisdat, append = TRUE, type="output")
#cat("\n\nSTARTING VALUES AND CONSTRAINED PARAMETERS: rbind(apollo_beta)\n")
#cbind(apollo_beta)                                                                                            
                                                                                             
# Indicate which parameters are fixed                                                        
apollo_fixed <- c("asc_lr") # c() 

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
V[["LR"]]    <- asc_lr     + b_cost * fare_LR   

  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(CART=1, CARNT=2, TR=3, LR=4, BW=5, BUS=6), 
    avail         = list(CART=avail_CART, CARNT=avail_CARNT, TR=avail_TR, BW=avail_BW, LR=avail_LR, BUS=avail_BUS), 
    choiceVar     = choice_newn,
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
#sink()
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, saveOutput_settings = list(saveOld = FALSE, saveEst = FALSE, saveEst = FALSE,
                    saveModelObject = TRUE, printDataReport = TRUE, printOutliers = FALSE,
                    saveHBiterations = FALSE, printFunctions = TRUE))


 
# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

### calculate value and standard error for base of effects coded parameter

sink(lisdat, append = TRUE, type="output")

cat("\n\nPREDICTED PROBABILITIES (hrg05: 360) forecast = apollo_prediction(model, apollo_probabilities, apollo_inputs)\n")
forecast = apollo_prediction(model, apollo_probabilities, apollo_inputs)


cols <- c("CART", "CARNT", "BUS", "TR", "BW", "LR")    # in the desired order

cat("\n\nPREDICTED CHOICE PROBABILITIES (hrg05:361 and 368), predicted mode choice (type 2: mode w 
       maximum pred probability, and actually chosen mode)\n")

cbind(forecast,chosen = database[,"choice_newc"])[1:12,]


cat("\n\nAGGREGATES OF PREDICTED CHOICE PROBABILITIES (hrg05: 365f) colSums(forecast[,1:8])\n")
colSums(forecast[,3:8])
 

cat("\n\nUTILITIES (hrg05: 368)(=ASC in cases of alternative not in choice set) \n")
utils <- list()
utils[["CART"]]  <- - model$estimate["asc_cart"] - model$estimate["b_cost"] * database$fuel_CART
utils[["CARNT"]] <- - model$estimate["asc_carnt"] - model$estimate["b_cost"] * database$fuel_CARNT
utils[["BUS"]]   <- - model$estimate["asc_bus"] - model$estimate["b_cost"] * database$fare_BUS
utils[["TR"]]    <- - model$estimate["asc_tr"] - model$estimate["b_cost"] * database$fare_TR
utils[["BW"]]    <- - model$estimate["asc_bw"] - model$estimate["b_cost"] * database$fare_BW
utils[["LR"]]    <- - model$estimate["asc_lr"] - model$estimate["b_cost"] * database$fare_LR

utils %>%
  as.data.frame() %>% 
  head(8) %>%
  print(digits = 6)


# CROSSTABULATION
# TYPE 1 APPROACH (COMPUTE FREQUENCY OF PREDICTED MODE AS SUM OVER OBSERVATIONS OF PREDICTED PROBABILIES)

obs_prd1 <- t(sapply(split(as.matrix(forecast[, 3:8]), database[,"choice_newc"]), function(x) colSums(matrix(x, ncol=6)))) # type 1 aggregation
colnames(obs_prd1) <- colnames(forecast)[3:8]

obs_prd1 <- obs_prd1[cols,cols]

dimnames(obs_prd1) <- list("actual" = rownames(obs_prd1),
                           "predicted" = colnames(obs_prd1))

cat("\n\nCROSSTAB pred_choice1 (type 1- prediction: aggregation by summation of predicted probs,
         Row indicator is 'actual', column indicator is 'predicted'  (cf. table on hrg05: 382)\n")

round(addmargins(obs_prd1))

cat(paste0("\nShare of correct predictions: ",round(sum(diag(obs_prd1)) / sum(obs_prd1),2)*100,"%\n"))


cat("\n\nCorresponding shares of mode predictions within each group defined by actual mode choice\n")
prop.table(obs_prd1,1) %>%
   round(2) %>%
   print(digits =2)

##  transposed: deviation of actual from predicted choice cat("\n\nCROSSTAB pred_choice1 (type 1- prediction: aggregation by summation of predicted probs,
##  transposed: deviation of actual from predicted choice     ~table on hrg05: 382): ...)   Row indicator is 'predicted', column indicator is 'actual'\n")
##  transposed: deviation of actual from predicted choice 
##  transposed: deviation of actual from predicted choice round(addmargins(t(obs_prd1)))
##  transposed: deviation of actual from predicted choice 
##  transposed: deviation of actual from predicted choice cat("\n\nShares of mode choices within each each group of choices defined by predicted mode\n")
##  transposed: deviation of actual from predicted choice prop.table(t(obs_prd1),1) %>%
##  transposed: deviation of actual from predicted choice    round(2) %>%
##  transposed: deviation of actual from predicted choice    print(digits =2)
##  transposed: deviation of actual from predicted choice sink()


# TYPE 2 APPROACH (COMPUTE FREQUENCY OF PREDICTED MODE AS THE NUMBER OF CASES WITH THE MODE HAVING MAXIMUM PREDICTED PROBABILITY)

forecast$pred_choice2 <- apply(forecast[cols], 1, function(row) { # set mode with maximum choice probability to predicted mode
  cols[which.max(as.numeric(row))]
})

vgl <- tibble(database[,c("id","spexp","cnf")],forecast[,c(3:8,10)],CHOICE = database[,c("choice_newc")])

prd2_obs <- table(actual = factor(vgl$CHOICE,  levels = cols),
                 predicted = factor(vgl$pred_choice2,  levels = cols)
                 )
obs_prd2 <- table(predicted = factor(vgl$pred_choice2,  levels = cols),
                 actual = factor(vgl$CHOICE,  levels = cols)
                 )

cat("\fCROSSTAB pred_choice2 (type 2- prediction: aggregation as count of cases where the level has max pred. prob., ~text on hrg05: 382):
  Each cell contains the count of predictions of the column label mode among the mode choices indicated in the row label
  Each cell contains the count of actual choices of the mode indicated in the row label among all predictions of the mode indicated in the column label\n")
addmargins(prd2_obs)

cat("\n\nShare of (type 2-) mode-predictions for each group of actual mode-choice\n")
addmargins(prop.table(prd2_obs,1),2) %>%
   round(2) %>%
   print(digits =1)


##   transposed ... cat("\fCROSSTAB pred_choice2 (type 2- prediction: aggregation as count of cases where the level has max pred. prob., ~text on hrg05: 382):
##   transposed ...    Each cell contains mode prediction counts for the mode choice addressed in each respective row
##   transposed ...    Each row contains mode choice counts for the mode prediction addressed in each respective column\n")
##   transposed ... addmargins(obs_prd2)
##   transposed ... 
##   transposed ... cat("\n\nShare of actual mode-choices (in rows) by (type 2-) mode-predictions (in cols)\n")
##   transposed ... addmargins(prop.table(obs_prd2,1),2) %>%
##   transposed ...    round(2) %>%
##   transposed ...    print(digits =1)
##   transposed ... 

#data.frame(database[,c("id","spexp","fuel_CART","fuel_CARNT","fare_BUS","fare_BW","fare_TR","fare_LR")],utils)[1:15,]






# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()



## #sink(lisdat, append = FALSE, type="output")

#apollo_lrTest("hrg03_mnl/hrg03_mnl01", model)


sink()