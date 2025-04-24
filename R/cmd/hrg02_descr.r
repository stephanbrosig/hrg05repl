# screen hrg Data 
# input : dat/hrg01_imp.Rds

lisdat <- paste("lis\\",codefile, ".lis",sep="")   # for naming listing file ONLY USED DURING CODING
lisdatu <- paste("lis\\",codefile, "_ua.lis",sep="")   # for naming listing file W UKRAINIAN LABELS
lisdate <- paste("lis\\",codefile, "_en.lis",sep="")   # for naming listing file W ENGLISH LABELS
outdat <- paste("dat\\",codefile,".RData",sep="")   # for naming output file
sink(lisdat, append = FALSE, type="output")

cat(paste("Codefile:",codefile,".r",sep=""),"\n")
options(width=190)  # for A4 landscape 

library(tidyverse)
#library(olsrr) # auxiliary regression 
library(labelled) # var_label
library(summarytools)  # dfSummary
library(apollo)  # for apollo_longToWide


asm <- as_tibble(readRDS(file = "dat/hrg01_imp.Rds"))  

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
asm$cnf <- factor(as.numeric(asm$cn), levels = 1:5, labels = c("1: b+tr","2: b+bw","3: tr+lr","4: bw+lr","5 (=0): walk... (RP)"))


cat("\n\nFIRST 24 OBSERVATIONS OF SP-RELEVANT COLUMNS IN (long form) DATA SAVED BY hrg01_imp.r (w some labels changed)\n")
asm %>% 
filter(sprp=="SP") %>%
select(id,spexp,splength,altij,altijc,choice,spchoice,cn,cnf,time,timevar,toll,tollpred,fuel,parking,freq,fare) %>%
    print(n = 24)



asm <- asm %>% 
   select(-chsnmode,-altmode,-cset,-starts_with("rp"),rpmiss,rpda,-mptrfare,-optrfare,-homtoptr,-ptrtowk,-hhldveh,
   -wkkmveh,-(30:70),-(74:76),-numbvehs,-nhldbcar,-ncompcar,-ndrivlic) %>%  # remove cols (temporally) not needed
   select(-spcart,-spcarnt,-spbus,-sptn,-spbw,-splr) # remove cols indicating "X chosen"/"X not chosen" for each of six modes X

asm$splength <- as.numeric(asm$splength) # so that it can be included in file names 1=<30, 2=30-45, 3=>45min

#triplength <- 3
#asm <- asm %>%  
#      filter(sprp == "SP" & splength == triplength ) # subset using SP data and splength = < 30 min 
#cat(paste0("\n FROM NOW ON FILTERED DATA (sprp == 'SP' & splength == ",triplength,", w 2= <30min...)"))
#
   
# LONG TO WIDE CONVERSION (ONE ROW PER CHOICE SITUATION CONTAINING ATTRIBUTE DATA ON ALL ALTERNATIVES)
asm$altij    <- as.character(factor(asm$altij , levels = 1:6, labels = c("CART","CARNT","BUS","TR","BW","LR"))) # apollo requires character type for alternatives variable
asm$spexp    <- as.numeric(asm$spexp) # apollo_longToWide requires numeric type for experiment number (obsColumn)

longToWide_settings <- list()                                                                                    
longToWide_settings[["altColumn"]] <- "altij" # use character type (if factor type is used ....
longToWide_settings[["altSpecAtts"]] <- c("time","timevar","toll","tollpred","fuel","parking","freq","fare","acctime","eggtime")
longToWide_settings[["choiceColumn"]] <- "choice"
longToWide_settings[["idColumn"]] <- "id"
longToWide_settings[["obsColumn"]] <- "spexp"

#invisible(capture.output(database <- apollo_longToWide(asm, longToWide_settings))) # saves wide data in database. delete "invisible" for descriptives
                         database <- apollo_longToWide(asm, longToWide_settings)   # saves wide data in database. delete "invisible" for descriptives

database <- database %>% 
       filter(sprp == "SP" ) %>% # subset using SP data 
       mutate(cn_own = case_when(
      avail_BUS == 1 & avail_TR == 1 ~ "1: b+tr",
      avail_BUS == 1 & avail_LR == 1 ~ "2: b+lr",
      avail_BUS  == 1 & avail_BW == 1 ~ "5: b+bw",
      avail_TR  == 1 & avail_BW == 1 ~ "3: tr+bw",
      avail_TR  == 1 & avail_LR == 1 ~ "6: tr+lr",
      avail_BW  == 1 & avail_LR == 1 ~ "4: bw+lr", 
      TRUE ~ NA_character_  # optional: assigns NA to all other cases
       ),
       cn_own = factor(cn_own, levels = c("1: b+tr","2: b+lr","3: tr+bw","4: bw+lr","5: b+bw","6: tr+lr")),
       choice_newf =factor(as.vector(choice_new), levels = 1:6, labels = c("1 CART","2 CARNT","3 TR","4 LR","5 BW","6 BUS")) )  

cat("\n\ntable(database(splength))\n")
table(database$splength)

cat("\n\nOVERALL FREQUENCY OF ALTERNATIVE INCLUSION IN CHOICE CARDS (aside from CART and CARNT 
which appear in each choice situation)table(database$cn)\n")
cat("\n\nBEFORE FILTERING: codebookentries('cn',database)\n")
codebookentries("cn",database)
database$cnf <- droplevels(database$cnf)
cat("\n\ncodebookentries('cnf',database)\n")
codebookentries("cnf",database)
cat("\n\ncodebookentries('cn_own',database)\n")
codebookentries("cn_own",database)
codebookentries("choice_newf",database)


triplength <- 2
database <- database %>% filter(splength == triplength ) # subset using SP data and splength = < 30 min 
cat(paste0("\n FROM NOW ON FILTERED DATA (sprp == 'SP' & splength == ",triplength,") [with 2:<30min, 3:30-45min, 4:>45min]\n"))

database$cnf <- droplevels(database$cnf)



as_tibble(database) %>% select(id,spexp,cn,cnf,cn_own,choice_new,choice_newf,
                                    avail_CART,fuel_CART,fare_CART,
                                    avail_CARNT,fuel_CARNT,fare_CARNT,
                                    avail_BUS,fuel_BUS,fare_BUS,
                                    avail_TR,fuel_TR,fare_TR,
                                    avail_BW,fuel_BW,fare_BW,
                                    avail_LR,fuel_LR,fare_LR) %>%
   head(n=20) 


cat("\nTesting for multicollinearity using wide format (all alternatives in same row for each choice situation)   
 as suggested in HRG2005 8.5.1 (p.245) and 9.5.2 (p. 295)\n")


cat("\n\nSPLENGTH-specific FREQUENCY OF ALTERNATIVE INCLUSION IN CHOICE CARDSn)\n")
codebookentries("cn",database)


# AUXILIARY REGRESSIONS
compute_all_Ri <- function(data, vars) {  
 cat("\n\nAUXILIARY REGRESSIONS FOR ALTERNATIVE", sub(".*_", "", vars[1]), "\n"  ) # indicate alternative in title to aux regression results  
 # Create an empty results data frame  
 results <- data.frame(Variable = character(), Ri_Statistic = numeric(), stringsAsFactors = FALSE)  
   
 for (dep_var in vars) {    
 indep_vars <- setdiff(vars, dep_var) # Define independent variables by excluding the current dependent variable    
     
 formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + ")))    
     
 mod <- lm(formula, data = data)    
 #print(summary(mod  ) # for inspecting individual regression output    
 rsq <- 1 - (sum(mod$residuals^2) / (sum((mod$model[[1]] - mean(mod$model[[1]]))^2)))    
 df1 <- mod$rank - 2    
 df2 <- mod$df.residual + 1    
 Ri <- (rsq / (mod$rank - 2)) / ((1 - rsq) / (mod$df.residual + 1)) # Compute Ri statistic    
 F_critical_value <- qf(1 - .05, df1, df2) # Critical value at alpha = 0.05    
 Multicoll <- ifelse(Ri >= F_critical_value, "Yes", "."  ) # indicate aux regressions with F-test of H0 (absence of multicollinearity) rejected   
 results <- suppressMessages(rbind(results, data.frame(Variable = dep_var, Ri_Statistic = Ri, F_CritVal = F_critical_value, Multicoll, stringsAsFactors = FALSE)))  
 }  
 cat("\nRi-STATISTICS AND F-TESTS\n\n") # indicate alternative in title to aux regression results  
 return(results) 
}

compute_all_Ri(database, c("time_CART" , "timevar_CART" , "toll_CART" , "tollpred_CART", "fuel_CART" , "parking_CART" ))
compute_all_Ri(database, c("time_CARNT", "timevar_CARNT", "fuel_CARNT" , "parking_CARNT"))
compute_all_Ri(database, c("time_BUS" , "acctime_BUS" , "eggtime_BUS", "freq_BUS", "fare_BUS"))
compute_all_Ri(database, c("time_TR" , "acctime_TR" , "eggtime_TR" , "freq_TR" , "fare_TR"))
compute_all_Ri(database, c("time_BW" , "acctime_BW" , "eggtime_BW" , "freq_BW" , "fare_BW"))
compute_all_Ri(database, c("time_LR" , "acctime_LR" , "eggtime_LR" , "freq_LR" , "fare_LR"))


cor_matrix <- database %>% select(matches("CART$|CARNT$|BUS$|TR$|BW$|LR$")) %>%             
 select(-starts_with("avail_")) %>%             
 select(-freq_CART,-fare_CART,-acctime_CART,-eggtime_CART,-toll_CARNT,-tollpred_CARNT,-freq_CARNT,                    
 -fare_CARNT,-acctime_CARNT,-eggtime_CARNT,-timevar_TR,-toll_TR,-tollpred_TR,-fuel_TR,-parking_TR,                    
 -timevar_LR,-toll_LR,-tollpred_LR,-fuel_LR,-parking_LR,-timevar_BW,-toll_BW,-tollpred_BW,-fuel_BW,                    
 -parking_BW,-timevar_BUS,-toll_BUS,-tollpred_BUS,-fuel_BUS,-parking_BUS) %>%   
 cor(., use = "pairwise.complete.obs")
cat(paste0("\n\nPEARSON CORRELATION MATRIX for splength = ",triplength," (see also correlation plot 
         hrg02_descr_corrplotX.wmf w X = 2,3,4 acc to splength, also in MSWord file ) \n\n"))
print(round(cor_matrix, 3))

library(corrplot) # for corrplot()

# Plot the correlation matrix
win.metafile(filename = paste("pic/",codefile,"_corrplotX",triplength,".wmf",sep=""))
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)
footer_text <- paste("Correlation Matrix for DCE Attributes -", Sys.Date())
mtext(paste(paste("pic/",codefile,"_corrplot",triplength,".wmf",sep=""),Sys.Date()), side = 1, line = 4, cex = 0.6, col = "black", adj =   0  ) 
dev.off()
