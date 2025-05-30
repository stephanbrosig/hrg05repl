#  hrg 2005 p. 355f: interactions2 (nl: cat)
# input : datdir/hrg01_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=250)  # for A4 landscape 

library(tidyverse)
library(apollo)  


asm <- readRDS(file = paste0(datdir,"/hrg01_imp.Rds"))  %>%  # local drive datdir as defined in _hrg_steer.r
  select(-city,-chsnmode,-altmode,-cset,-altisprp,-starts_with("rp"),rpmiss,rpda,-mptrfare,-optrfare,-homtoptr,-ptrtowk,
  -hhldveh,-wkkmveh,-(30:70),-(74:76),-nhldbcar,-ncompcar) %>%  # remove cols (temporally) not needed
  select(-spcart,-spcarnt,-spbus,-sptn,-spbw,-splr)
  
asm <- asm %>%  # subset using SP data and splength = < 30 min 
      filter(sprp == "SP" & splength == "Less than 30 Minutes" )

## change factor levels ("labels") to shorter versions
asm$splength <- factor(as.numeric(asm$splength) , levels = 1:4,  labels = c("RP","<30", "30-45", ">45"))
asm$pereduc  <- factor(as.numeric(asm$pereduc)   , levels = 1:5,  labels = c("PP", "Prim", "Sec", "TeCo", "Uni"))
asm$wkroccup <- factor(as.numeric(asm$wkroccup) , levels = 1:9,  labels = c("Mgr", "Prof", "PP", "Trd", "Clk", "Sls", "Plt", "Lab", "Oth"))
asm$wkremply <- factor(as.numeric(asm$wkremply) , levels = 1:3,  labels = c("Full", "Part", "SE"))
asm$hldincom <- factor(as.numeric(asm$hldincom) , levels = 1:11, labels = c("<5","5-12","12-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-120"))
asm$spexp    <- factor(as.numeric(asm$spexp) , levels = 1:4, labels = c("RP","CS1","CS2","CS3"))
asm$altij    <- factor(asm$altij , levels = 1:6, labels = c("CART","CARNT","BUS","TR","BW","LR")) # BW and LR swapped to conform with SPLR and SPBW


asm$altij   <- as.character(asm$altij) # apollo requires character type for alternatives variable
#asm$spchoice <- factor(as.numeric(asm$spchoice) , levels = 1:7, labels = c("1_1cart","2_2carnt","3_3bus","4_4tr","5_5bw","6_6lr","0_7RP"))
#asm$choice <- factor(as.numeric(asm$choice) , levels = 1:2, labels = c("0","1"))
asm$cns <- factor(as.numeric(asm$cn) , levels = 1:5, labels = c("B-T","B-LR","T-BW","BW-LR","(RP)"))
asm$spmiss   <- factor(as.numeric(asm$spmiss) , levels = 1:2, labels = c("cpl","miss"))
asm$rpmiss   <- factor(as.numeric(asm$rpmiss) , levels = 1:2, labels = c("cpl","miss"))
asm$rpda     <- factor(as.numeric(asm$rpda) , levels = 1:2, labels = c("DA","nDA"))
asm$drivlic  <- factor(as.numeric(asm$drivlic) , levels = 1:3, labels = c("Y","N","nA"))



asm <- as_tibble(asm) %>%
  select(-sprpmiss,-triptime,-deptime,-disdwcbd,-start24,-acctime,-eggtime,-hweight,-hldincom,-hldsize,
         -nworkers,-wkremply,-wkroccup,-perage,-pincome,-persex,-pereduc,-acceggt,-can,-syd,-mel,
         -brs,-adl,-rpmiss,-rpda, -sprp,  -spmiss,
         -spchoice,-cn,-splength,-cns)  

#cat("\n\nprint(asm)\n")
#print(asm, n=50)


#asm$altij    <- as.character(asm$altij)
#asm$choice    <- as.logical(asm$choice)
asm$spexp    <- as.numeric(asm$spexp)
#asm$spchoice    <- as.numeric(asm$spchoice)
#asm$choice    <- as.numeric(asm$choice) -1

#cat("\n\nprint(asm) for id %in% c(1000,1001,1002,1006,1008 \n")
#asm %>% 
#        filter(id %in% c(1000,1001,1002,1006,1008)) %>%
#        print(n=100)

longToWide_settings <- list()                                                                                  
longToWide_settings[["altColumn"]] <- "altij"  # use character type (if factor type is used ....
longToWide_settings[["altSpecAtts"]] <- c("time","timevar","toll","tollpred","fuel","parking","freq","fare")
longToWide_settings[["choiceColumn"]] <- "choice"
longToWide_settings[["idColumn"]] <- "id"
longToWide_settings[["obsColumn"]] <- "spexp"

invisible(capture.output(database <- apollo_longToWide(asm,  longToWide_settings))) # saves wide data in database. delete "invisible(capture.output(" for description of reshaping
#                          database <- apollo_longToWide(asm,  longToWide_settings)   # saves wide data in database. delete "invisible(capture.output(" for description of reshaping


#database <- database %>%  # rename(c2 = choice_new) %>%
#      mutate(
#            c2f = factor(choice_new, levels = 1:6, labels = c("carT","carNT","Train","BW","LR","Bus")),
#            c2 = as.vector(choice_new)
    #         ,
    #         av_B  = as.integer(cn %in% c("Bus - Train (SP)", "Bus - LigtRail!! (SP)")), 
    #         av_T  = as.integer(cn %in% c("Bus - Train (SP)", "Train - Busway!! (SP)")), 
    #         av_BW = as.integer(cn %in% c("Train - Busway!! (SP)", "Busway - Light Rail (SP)")), 
    #         av_LR = as.integer(cn %in% c("Bus - LigtRail!! (SP)", "Busway - Light Rail (SP)"))
    #         ) 

#cat("\n\nglimpse(database)\n")
#glimpse(database)

database <- database %>% 
#             select(id,spexp,starts_with("avail_"),choice_new,ends_with("_CART"),ends_with("CARNT"),ends_with("CAR"),ends_with("BUS"),
#                      ends_with("TR"),ends_with("BW"),ends_with("LR"),-starts_with("av_"),
#                     -starts_with("freq_CAR"),-starts_with("fare_CAR"),
#                     -timevar_BUS,-toll_BUS,-tollpred_BUS,-fuel_BUS,-parking_BUS,
#                     -timevar_TR,-toll_TR,-tollpred_TR,-fuel_TR,-parking_TR,
#                     -timevar_BW,-toll_BW,-tollpred_BW,-fuel_BW,-parking_BW,
#                     -timevar_LR,-toll_LR,-tollpred_LR,-fuel_LR,-parking_LR,
#                     -toll_CARNT,-tollpred_CARNT
#                     ) %>%  
#                     mutate(choice_newf =              factor(as.vector(choice_new), levels = 1:6, 
#                                                         labels = c("CART","CARNT","TR","BW","LR","BUS")))  %>%
                     mutate(choice_newc = as.character(factor(as.vector(choice_new), levels = 1:6, 
                                                         labels = c("CART","CARNT","TR","LR","BW","BUS")))) %>%   
                     mutate(choice_newn =   as.numeric(factor(as.vector(choice_new), levels = 1:6, 
                                                         labels = c("CART","CARNT","TR","LR","BW","BUS")))) %>%   # this converts a labelled vector to a numeric vector which is necessary for apollo's choiceVar  
                     mutate(fare_BUS1  = ifelse(fare_BUS == 1, 1, 0),
                            fare_BUS3  = ifelse(fare_BUS == 3, 1, 0),
                            fare_BUS5  = ifelse(fare_BUS == 5, 1, 0),
                            time_BUS10 = ifelse(time_BUS == 10, 1, 0),
                            time_BUS15 = ifelse(time_BUS == 15, 1, 0),
                            time_BUS20 = ifelse(time_BUS == 20, 1, 0))  %>% # create fare dummies   
                     mutate(vehlic      = ndrivlic * numbvehs,
                            fr1tim10_BUS = fare_BUS1 * time_BUS10,
                            fr1tim15_BUS = fare_BUS1 * time_BUS15,
                            fr3tim10_BUS = fare_BUS3 * time_BUS10,
                            fr3tim15_BUS = fare_BUS3 * time_BUS15,
                            )  # create interactions

      
### Initialise code
apollo_initialise()
sink(lisdat, append = TRUE, type="output")  # redirect output again after apollo_initialize

cat("\n\n### Set core controls\n")
### Set core controls
apollo_control = list(
  modelName       = "hrg03_mnl7",
  modelDescr      = "hrg03a_mnl7: hrg 2005 p. 357ff: WTP",
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
 b_time =  .4,
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
V[["CARNT"]] <- asc_carnt  + b_cost * fuel_CARNT + b_time * time_CARNT
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
cat("\n\nWTP : Value of travel time savings (VTTS) HRG05 p. 359 (via apollo_deltaMethod)\n")
apollo_deltaMethod(model,deltaMethod_settings = list(expression=c(VTTS  = "b_time /  b_cost * 60"  )))

#sink()


# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()






## #sink(lisdat, append = FALSE, type="output")

apollo_lrTest("hrg03_mnl/hrg03_mnl01", model)


sink()