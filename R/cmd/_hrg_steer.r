# source("C:/Users/Brosig/Documents/github/hrg05repl/R/cmd/_hrg_steer.r", print.eval = TRUE)
### 
### steering file hrg05 replications

rm(list=ls()[!ls()%in%c("")])

pfad0 <- "C:/Users/Brosig/Documents/github/hrg05repl"   # office
pfad <- paste(pfad0,"/R",sep="") 
setwd(pfad)



#######  H e n s h e r  R O S E   G R E E N 2005 case study data  #################

#codefile <- "01401_hrg_imp"  # import hrg mode choice data and apply labels
#codefile <- "01401a_hrg_imp_repl"  # import hrg mode choice data without labelling for replication of book results
########## nicht genutzt: codefile <- "01401b_hrg_clean"  # clean data from 01401_hrg_imp 
#codefile <- "01402_hrg_descr"  # screen hrg mode choice data
#codefile <- "01402a_hrg_descr_repl"  # screen hrg mode choice data
#codefile <- "01403_hrg_mnl01"  #  hrg 2005 Ch 10: MNL 
#codefile <- "01403_hrg_mnl02"  #  hrg 2005 p. 329 ASC only model
#codefile <- "01403_hrg_mnl03"  #  HRG05 LR-Test comparing two models (335ff)
#codefile <- "01403_hrg_mnl04"  #  hrg 2005 p. 344 dummy coding (nonlinear)
#codefile <- "01403_hrg_mnl05"  #  hrg 2005 p. 352: interactions1 (lin)
#codefile <- "01403_hrg_mnl06"  #  hrg 2005 p. 355f: interactions2 (nl: cat)
codefile <- "01403_hrg_mnl07"  #  hrg 2005 p. 357ff: WTP

#codefile <- "01403a_hrg_mnl_repl"  #  hrg 2005 Ch 10: MNL without labels

#codefile <- "MNL_SP"   #  from Apollo manual, p. 19ff: stated prefs MNL with covariates applied to travel mode choice data
#codefile <- "MNL_SP_covariates"   #  from Apollo manual, p. 19ff: stated prefs MNL with covariates applied to travel mode choice data



library(haven)

source("cmd/utils.r")
source(paste("cmd/",codefile,".r",sep=""), print.eval = TRUE)

