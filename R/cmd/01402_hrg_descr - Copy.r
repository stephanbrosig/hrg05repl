# screen Hensher Data 
# input : dat/01401_hensher_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
lisdatu <- paste("lis\\",codefile, "_ua.lis",sep="")   # for naming listing file W UKRAINIAN LABELS
lisdate <- paste("lis\\",codefile, "_en.lis",sep="")   # for naming listing file W ENGLISH LABELS
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=140)  # for A4 landscape 

library(tidyverse)
#library(olsrr) # auxiliary regression 
library(labelled) # var_label
library(summarytools)  # dfSummary
library(apollo)  # for apollo_longToWide


asm <- as_tibble(readRDS(file = "dat/01401_hensher_imp.Rds"))  

## change factor levels ("labels") to shorter versions
asm$splength <- factor(as.numeric(asm$splength) , levels = 1:4,  labels = c("RP","<30", "30-45", ">45"))
asm$pereduc  <- factor(as.numeric(asm$pereduc)   , levels = 1:5,  labels = c("PP", "Prim", "Sec", "TeCo", "Uni"))
asm$wkroccup <- factor(as.numeric(asm$wkroccup) , levels = 1:9,  labels = c("Mgr", "Prof", "PP", "Trd", "Clk", "Sls", "Plt", "Lab", "Oth"))
asm$wkremply <- factor(as.numeric(asm$wkremply) , levels = 1:3,  labels = c("Full", "Part", "SE"))
asm$hldincom <- factor(as.numeric(asm$hldincom) , levels = 1:11, labels = c("<5","5-12","12-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-120"))
asm$altijc    <- factor(asm$altij , levels = 1:6, labels = c("cart","carnt","bus","train","BW","LR"))
asm$spexp    <- factor(as.numeric(asm$spexp) , levels = 1:4, labels = c("CS1","CS2","CS3","RP"))
asm$spmiss   <- factor(as.numeric(asm$spmiss) , levels = 1:2, labels = c("cpl","miss"))
asm$rpmiss   <- factor(as.numeric(asm$rpmiss) , levels = 1:2, labels = c("cpl","miss"))
asm$rpda     <- factor(as.numeric(asm$rpda) , levels = 1:2, labels = c("DA","nDA"))
asm$drivlic  <- factor(as.numeric(asm$drivlic) , levels = 1:3, labels = c("Y","N","nA"))

# cat("\n\nFIRST 24 OBSERVATIONS OF SP-RELEVANT COLUMNS IN DATA SAVED BY 01401_hensher_imp.r (w some labels changed)\n")
# asm %>% 
# filter(sprp=="SP") %>%
# select(id,spexp,altij,altijc,choice,time,timevar,toll,tollpred,fuel,parking,freq,fare) %>%
#     print(n = 24)



asm <- asm %>% 
#  filter(!(spmiss == "One or more choice sets missing" & CITY=="Canberra" & SPLENGTH == "Less than 30 Minutes ")) %>% # remove with incomplete choice sets from the Canberra data
###  filter(!(spmiss == "One or more choice sets missing" & CITY=="Adelaide")) %>% # remove with incomplete choice sets from the adelaide data
###  filter(id != 5047) %>% # the only Adelaide household with least number of completed choices among short trip 
###  filter(!id %in% c(1053, 1064)) %>% # the only Canberra households with only one completed choice 
###  filter(!id %in% c(1209)) %>% # arbitrary choice of one out of three Canberra IDs with only two completed choices
#  filter(sprp == "SP") %>%  select(-sprp)  %>% # remove RP records
   select(-chsnmode,-altmode,-cset,-starts_with("rp"),rpmiss,rpda,-mptrfare,-optrfare,-homtoptr,-ptrtowk,-hhldveh,
   -wkkmveh,-(30:70),-(74:76),-numbvehs,-nhldbcar,-ncompcar,-ndrivlic) %>%  # remove cols (temporally) not needed
   select(-spcart,-spcarnt,-spbus,-sptn,-spbw,-splr) # remove cols indicating "X chosen"/"X not chosen" for each of six modes X

options(width=122)  # for A4 landscape 

asm$city <- factor(asm$city, levels = c("Sydney", "Canberra", "Melbourne", "Adelaide", "Brisbane", "Perth"))  # order as in table 9.11 of the book 



asm <- asm %>%  
      filter(sprp == "SP" & splength == "<30" ) # subset using SP data and splength = < 30 min 
cat("\n FROM NOW ON FILTERED DATA (sprp == 'SP' & splength == '<30' )")

####    cat("\nRESULT OF FIRST AUXILIARY REGRESSION, cf HRG2005: 293\n")
####    asm1 <- asm %>%   filter(altij == "1")         # only records for the first alternative in each choice set
####    mod1 <- lm(time ~ timevar + toll + tollpred + fuel + parking, data = asm1)
####    summary(mod1)
####    cat("\nRi statistic for model w time on LHS as on p. 294\n")
####    rsq <- 1- (sum(mod1$residuals^2) / sum((asm1$time - mean(asm1$time))^2))
####    r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
####    r_time 

##  mod1 <- lm(timevar ~ time + toll + tollpred + fuel + parking, data = asm1)
##  cat("\nRi statistic for model w timevar on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asm1$timevar - mean(asm1$timevar))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(toll ~ time + timevar + tollpred + fuel + parking, data = asm1)
##  #summary(mod1)
##  cat("\nRi statistic for model w toll on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asm1$toll - mean(asm1$toll))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(tollpred ~ time + timevar + toll + fuel + parking, data = asm1)
##  cat("\nRi statistic for model w tollpred on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asm1$tollpred - mean(asm1$tollpred))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(fuel ~ time + timevar + toll + tollpred + parking, data = asm1)
##  cat("\nRi statistic for model w fuel on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asm1$fuel - mean(asm1$fuel))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 
##  
##  mod1 <- lm(parking ~ time + timevar + toll + tollpred + fuel, data = asm1)
##  cat("\nRi statistic for model w parking on LHS as on p. 295\n")
##  rsq <- 1- (sum(mod1$residuals^2) / sum((asm1$parking - mean(asm1$parking))^2))
##  r_time <- (rsq / (mod1$rank-2)) / ((1 - rsq) / (mod1$df.residual + 1))
##  r_time 

cat("\nTesting for multicollinearity as suggested in HRG2005 8.5.1 (p.245) and 9.5.2 (p. 295)\n")

asm$altij    <- as.character(factor(asm$altij , levels = 1:6, labels = c("CART","CARNT","BUS","TR","BW","LR"))) # apollo requires character type for alternatives variable
asm$spexp    <- as.numeric(asm$spexp) # apollo_longToWide requires numeric type for experiment number (obsColumn)


longToWide_settings <- list()                                                                                  
longToWide_settings[["altColumn"]] <- "altij"  # use character type (if factor type is used ....
longToWide_settings[["altSpecAtts"]] <- c("time","timevar","toll","tollpred","fuel","parking","freq","fare","acctime","eggtime")
longToWide_settings[["choiceColumn"]] <- "choice"
longToWide_settings[["idColumn"]] <- "id"
longToWide_settings[["obsColumn"]] <- "spexp"

invisible(capture.output(database <- apollo_longToWide(asm,  longToWide_settings)))


##  # function for computing Ri statistic of auxiliary regression
##  Ri <- function(formula, data) {
##    mod <- lm(formula, data = data)
##    rsq <- 1 - (sum(mod$residuals^2) / sum((mod$model[[1]] - mean(mod$model[[1]]))^2)) # Compute R-squared
##    Ri <- (rsq / (mod$rank - 2)) / ((1 - rsq) / (mod$df.residual + 1)) # Compute Ri statistic
##    return(Ri)
##  }


#Ri(time_CART     ~              timevar_CART + toll_CART + tollpred_CART + fuel_CART + parking_CART, data = database)
# Ri(time_CART     ~              timevar_CART + toll_CART + tollpred_CART + fuel_CART + parking_CART, data = database)
# Ri(toll_CART     ~ time_CART +  timevar_CART +             tollpred_CART + fuel_CART + parking_CART, data = database)
# Ri(tollpred_CART ~ time_CART +  timevar_CART + toll_CART +                 fuel_CART + parking_CART, data = database)
# Ri(fuel_CART     ~ time_CART +  timevar_CART + toll_CART + tollpred_CART +             parking_CART, data = database)
# Ri(parking_CART  ~ time_CART +  timevar_CART + toll_CART + tollpred_CART + fuel_CART               , data = database)


##  mod <- lm(time_CART     ~              timevar_CART + toll_CART + tollpred_CART + fuel_CART + parking_CART, data = database)
##  rsq <- 1 - (sum(mod$residuals^2) / sum((mod$model[[1]] - mean(mod$model[[1]]))^2)) # Compute R-squared
##  Ri <- (rsq / (mod$rank - 2)) / ((1 - rsq) / (mod$df.residual + 1)) # Compute Ri statistic
##  
##  cat("\n\nsummary(mod)\n")
##  summary(mod)
##  cat("\nRi OF AUXILIARY REGRESSIONS, cf HRG2005: 294f\n")
##  Ri

colnames(database)

compute_all_Ri <- function(data, vars) {
  alt_name <- sub(".*_", "", vars[1])
  cat("\n\nAUXILIARY REGRESSIONS FOR ALTERNATIVE", alt_name, "\n")
  # Create an empty results data frame
  results <- data.frame(Variable = character(), Ri_Statistic = numeric(), stringsAsFactors = FALSE)
  
  for (dep_var in vars) {
    # Define independent variables by excluding the current dependent variable
    indep_vars <- setdiff(vars, dep_var)
    
    # Construct formula dynamically
    formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + ")))
    
    # Fit the linear model
    mod <- lm(formula, data = data)  # Suppress any warnings
    print(summary(mod))
    # Compute R-squared safely
    y_mean <- mean(mod$model[[1]])
    rss <- sum(mod$residuals^2)
    tss <- sum((mod$model[[1]] - y_mean)^2)
    rsq <- 1 - (rss / tss)
   df1 <- mod$rank - 2
   df2 <- mod$df.residual + 1
    # Compute Ri statistic (ensure no printing)
    Ri <- (rsq / (mod$rank - 2)) / ((1 - rsq) / (mod$df.residual + 1))

   F_critical_value <- qf(1 - .05, df1, df2)  # Critical value at the specified alpha level
   Multicoll <- ifelse(Ri >= F_critical_value, "Yes", ".")
    # Store result safely without printing
    results <- suppressMessages(rbind(results, data.frame(Variable = dep_var, Ri_Statistic = Ri, F_CritVal = F_critical_value, Multicoll, stringsAsFactors = FALSE)))
  }
  return(results)  # Ensure only final output is printed
}

compute_all_Ri(database, c("time_CART" , "timevar_CART" , "toll_CART"  , "tollpred_CART", "fuel_CART" , "parking_CART" ))
compute_all_Ri(database, c("time_CARNT", "timevar_CARNT", "fuel_CARNT" , "parking_CARNT"))
compute_all_Ri(database, c("time_BUS"  , "acctime_BUS"  , "eggtime_BUS", "freq_BUS", "fare_BUS"))
compute_all_Ri(database, c("time_TR"   , "acctime_TR"   , "eggtime_TR" , "freq_TR" , "fare_TR"))
compute_all_Ri(database, c("time_BW"   , "acctime_BW"   , "eggtime_BW" , "freq_BW" , "fare_BW"))
compute_all_Ri(database, c("time_LR"   , "acctime_LR"   , "eggtime_LR" , "freq_LR" , "fare_LR"))