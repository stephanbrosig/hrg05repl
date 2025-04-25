# source("C:/Users/Brosig/Documents/github/hrg05repl/R/cmd/_hrg_steer.r", print.eval = TRUE)
### 
### STEERING FILE HRG05 REPLICATIONS
# 1) specify in lines 1 and 12 the directory mirroring the github repo hrg05repl on local computer
# 2) specify in line 13 the directory containing the raw data set Data/SPRP.txt 
# 3) execute the codefiles in lines 20 to 31 one by one: 
#       a) remove the comment character # from the codefile line (starting with hrg01_imp to import raw data)
#       b) executing this steering file by running the command from line 1 ("source(...) from the R prompt
# 
rm(list=ls()[!ls()%in%c("")])

pfad0 <- "C:/Users/Brosig/Documents/github/hrg05repl"   # project working directory
datdir <- "C:/Users/Brosig/Documents/work/hrg05data" # data directory (not synchronized w repo)
pfad <- paste(pfad0,"/R",sep="") 
setwd(pfad)



#######  H e n s h e r  R O S E   G R E E N 2005 case study data  #################

codefile <- "hrg01_imp"  # import hrg mode choice data and apply labels
#codefile <- "hrg01a_imp_repl"  # import hrg mode choice data without labelling for replication of descriptive book results
#codefile <- "hrg02_descr"  # screen hrg mode choice data incl. mulitcollinearity checks
#codefile <- "hrg02a_descr_repl"  # screen hrg mode choice data
#codefile <- "hrg03_mnl01"  #  hrg 2005 Ch 10: MNL 
#codefile <- "hrg03_mnl02"  #  hrg 2005 p. 329 ASC only model
#codefile <- "hrg03_mnl03"  #  HRG05 LR-Test comparing two models (335ff)
#codefile <- "hrg03_mnl04"  #  hrg 2005 p. 344 dummy coding (nonlinear)
#codefile <- "hrg03_mnl05"  #  hrg 2005 p. 352: interactions1 (lin)
#codefile <- "hrg03_mnl06"  #  hrg 2005 p. 355f: interactions2 (nl: cat)
#codefile <- "hrg03_mnl07"  #  hrg 2005 p. 357ff: WTP
#codefile <- "hrg03_mnl08"  #  hrg 2005 p. 360ff Choice probabilities, utilities, predicted/stated-choice crosstabs




library(haven)

source("cmd/utils.r")
source(paste("cmd/",codefile,".r",sep=""), print.eval = TRUE)

