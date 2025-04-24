# source("C:/Users/Brosig/Documents/github/hrg05repl/R/cmd/_steer.r", print.eval = TRUE)
### 
### steering file hrg05 replications

rm(list=ls()[!ls()%in%c("")])

pfad0 <- "C:/Users/Brosig/Documents/github/hrg05repl"   # office
pfad <- paste(pfad0,"/R",sep="") 
setwd(pfad)



#######  H e n s h e r  R O S E   G R E E N 2005 case study data  #################

#codefile <- "hrg01_imp"  # import hrg mode choice data and apply labels
#codefile <- "hrg01a_imp_repl"  # import hrg mode choice data without labelling for replication of descriptive book results
########## nicht genutzt: codefile <- "hrg01b_clean"  # clean data from hrg01_imp 
#codefile <- "hrg02_descr"  # screen hrg mode choice data
#codefile <- "hrg02a_descr_repl"  # screen hrg mode choice data
#codefile <- "hrg03_mnl01"  #  hrg 2005 Ch 10: MNL 
#codefile <- "hrg03_mnl02"  #  hrg 2005 p. 329 ASC only model
#codefile <- "hrg03_mnl03"  #  HRG05 LR-Test comparing two models (335ff)
#codefile <- "hrg03_mnl04"  #  hrg 2005 p. 344 dummy coding (nonlinear)
#codefile <- "hrg03_mnl05"  #  hrg 2005 p. 352: interactions1 (lin)
#codefile <- "hrg03_mnl06"  #  hrg 2005 p. 355f: interactions2 (nl: cat)
#codefile <- "hrg03_mnl07"  #  hrg 2005 p. 357ff: WTP
codefile <- "hrg03_mnl08"  #  hrg 2005 p. 360ff Choice probabilities, utilities, predicted/stated-choice crosstabs

#codefile <- "hrg03a_mnl_repl"  #  hrg 2005 Ch 10: MNL without labels

#codefile <- "MNL_SP"   #  from Apollo manual, p. 19ff: stated prefs MNL with covariates applied to travel mode choice data
#codefile <- "MNL_SP_covariates"   #  from Apollo manual, p. 19ff: stated prefs MNL with covariates applied to travel mode choice data



library(haven)

source("cmd/utils.r")
source(paste("cmd/",codefile,".r",sep=""), print.eval = TRUE)

