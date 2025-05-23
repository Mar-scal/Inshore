###........................................###
###
###    SPA 1B
###    Model
###
###
###    L.Nasmith
###    September 2016
###    Updated Sept 2017 - J.SAmeoto
###    Updated Oct 2017  - DK
###    Updated June 2020 - JS 
###........................................###

# The October 2017 revision did the following to this file........................
# 1:  Removed the previous years model runs, the results of model runs from 2009-2015 are now stored in the folder ... \BoF\Model_results_2009_2015\.  This was
#     done because there was no consistent file structure before this time for how the model results were stored (if they were even retained).  Now all model results for these
#     years should be easily located.  From 2016 onwards the model results from the previous year are loaded from where they were saved last year
# 2:  Defined a set number of iterations for all the model MCMC runs stored above, niter was 100,000, nburnin was 50,000, nchains = 3, and nthin = 10.  For model year
#     2016 I left these values as whatever they were in 2016 as this model was run and I wanted to use those results for the prediction evaluation to be consistent.
# 3:  While niter and other MCMC parameters can be changed, unless the model structure changes I don't see any reason to use anything other than these values as the 
#     model appears to converge, but we do need to improve our convergence criteria for example we don't return Rhat values which is a good quick convergence criteria check.
# 4:  Made a SPA1B.inits function below, this was more useful when running a bunch of different years, but works and there is no need to rename this each year.  If you
#     change the nchains you'll need to add/remove a row from this (you need 1 list(P=...) for each chain).
# 5:  Using variable NY rather than entering the number of years in all places required.
# 6:  Created/organized some very basic file structure for the results from this based on what we had in place.  
#     Each year we will need to create a folder structure .../'Year' Assement/'Area'....  This is where we should put everything related to at least the modelling for a given
#     area.  We can have a bigger discussion on what exactly should go where whenever we have time.  All model results are being place in a sub-folder called "ModelOutput"
#     So for 2017 the model script for SPA3 is found at Y:\INSHORE SCALLOP\BoF\2017\2017 Assessment\SPA3\SPA3_Model_2017_final.r
#     And the model results (figures, tables, Rdata files) are found inside Y:\INSHORE SCALLOP\BoF\2017\2017 Assessment\SPA3\ModelOutput
#     The larger discussion about file organization may alter this structure but until that happens please look at 2017 file structure to mimic this in subsequent years.
#  7: Options to save the figures and model results are embedded in the code, they are commented out by default so you don't overwrite something by mistake, uncomment these
#     to save.
#  8: The old model run files + an intermediary file I used to transition from old way to this way can be found in the "archive" folder found in each areas Assessment folder
#  9:  Thanks for Freya we now have a function (BoF.model.stats) which produces a nice summary of the model results

# In May/Aug 2018 while doing somethng completely different DK decided to:
# 1:  Remove the ooogly stript which had us inputing data manually 
# 2:  Note that you'll need to manually update the file SPA3_ModelData.xlsx each year.
# In September 2018 the prediction evaluation was overhauled and automated, manual entries are now minimized in this script to the
# as great an extent as possible, I believe you will only need to update catch.next.year in the script.

# In Feb 2020 DK noted a mistake with the LRP and USR, these have been fixed.  The changes had no impact on the values in the decision table (lucky...) but DK
# updated the decision table to be correct for completeness.

###  J.Sameoto June 2020  Modified for running 2019 model run in Summer 2020 and for new folder structure of BOF 
## g and gr were completely recalculated for the full timeseries in 2020 - these values used for this model run 
### J.Sameoto June 2021 modified to define LRP, USR up front, to calculate probabiltiy > USR and > LRP for current year commercial biomass, and save out select result and diagnostics to be used by future Rmd CSAS Update file, make model file name generic so don't update every year,  


### to do - edit SSModel.plot.median;  fix(SSModel.plot.median) line 242 update to y=ref.pts[1]*1.5  (from y=ref.pts[1]*1.1)

rm(list=ls(all=T))
options(stringsAsFactors = FALSE)

#DEFINE:
direct <- "Y:/Inshore/BoF"
assessmentyear <- 2024 #year in which you are conducting the assessment 
surveyyear <- 2024  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 
area <- "1B"  #this would be the SPA, for entries options are to use: 1A, 1B, 3, 4, or 6  

#Reference Points 
LRP <- 880
USR <- 1800

# Set the value for catch next year, this is used in SSModel.plot.median() after the model runs 
# This value should be the interim TAC in the area.
catch.next.year <- 200

#required packages
library(SSModel)#v 1.0-3
library (openxlsx)
library(compareDF)
library(tidyverse)

#### Import Mar-scal functions 
#funcs <- c("https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/CreateExcelModelFile.R",
#           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_plot_median_new.r",
#           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_predict_summary_median.r",
#           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/BoFmodelstats.R")
#dir <- getwd()
#for(fun in funcs) 
#{
#  temp <- dir
#  download.file(fun,destfile = basename(fun))
#  source(paste0(dir,"/",basename(fun)))
#  file.remove(paste0(dir,"/",basename(fun)))
#}


funcs <- c("https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/CreateExcelModelFile.R",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_plot_median_new.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_predict_summary_median.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/BoFmodelstats.R")
dir <- tempdir()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#################################################################
# ---- Build and check the model input file ----

# Steps:
# 1) run the function below with savefile=F
# 2) review the messages in the R console
# 3) review CompareOutputTable_SPAxx.xlsx
# 4) Highlight any problematic values in yellow, and re-save as CompareOutputTable_SPAxx_highlighted.xlsx
# 5) Add manual edits to CreateExcelModelFile_2020.R as needed, in the EDITS section (~line 345). Keep it organized by SPA!
# 6) Review the diagnostics in the Plots and Viewer tabs of Rstudio
# 7) run the function below with savefile=T
# 8) colour code the notes as needed
# 9) when satisfied with the table, re-name it to remove the date. E.g. SPAxx_ModelData_R.xlsx 

CreateExcelModelFile(direct = direct, 
                     assessmentyear=2024, surveyyear = 2024, 
                     area = "1B", LastYearsModelRData = "SPA1B_Model_2023", 
                     savefile = T)

# for testing only (using FK private repo): 
# direct_test <- "C:/Users/keyserf/Documents/Github/BoF/"
# source(paste0(direct_test, "CreateExcelModelFile_2020.R"))

#################################################################

# --- Pre-running Model Prep work ----
# Let's be consistent with our MCMC year over year unless there is a need to change it...
niter = 250000  # default = 100000
nchains = 3    # default =33
nburnin = 50000  # default = 50000
nthin = 10       # default = 10

#Set parameters you want to come back from the winbugs model 
parm = c("B","R","q","K","P","sigma","S","m","kappa.tau","r", "Fmort","mu","Irep","IRrep","Presid","sPresid","Iresid","IRresid","sIresid","sIRresid")
#parm = c("B","R","q","K","P","sigma","S","m","kappa.tau","r", "Fmort","mu","Irep","IRrep")

# Bring in the data, you will need to update this with the latest numbers!
raw.dat <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA1B_ModelData_R_2024-10-14.xlsx"),sheet = "AlignedForModel", cols=1:13) 
str(raw.dat)
raw.dat$C <- as.numeric(raw.dat$C)

# Set the value for catch next year, this is used in SSModel.plot.median() after the model runs and in the final year for the
# prediction evaluation figures DK changed this in 2018
# This value should be the interim TAC in the area.
#catch.next.year <- 613.822 #2019 TAC (with carry over)
raw.dat$C[raw.dat$YearSurvey == max(raw.dat$YearSurvey,na.rm=T)] <- catch.next.year
raw.dat
# Get the years you want to run.
yrs <- min(raw.dat$YearSurvey):max(raw.dat$YearSurvey)
#yrs <- 1992:2019
# Just in case you subset the years to be anything other than all the years in the mod.dat file...
raw.dat <- raw.dat[raw.dat$YearSurvey %in% yrs,]
NY <- length(yrs) # Number of years.
# Create the list for the model.

SPA1B.dat <-  as.list(raw.dat[,c("C","N","I","I.cv","IR","IR.cv","ratiolined","g","gR","clappers")])
SPA1B.dat$NY <- NY


# This makes a function for the SPA4 inits, built this for 3 chains, if you change the number of chains you will need to change the number of lists (each list is inits 
# for 1 chain)
SPA1B.inits <- function(NY)
{
  structure(list(list(P=round(runif(NY,0.01,3),2),K=100,r=round(runif(NY,0.01,3),2),S=0.15,q=0.01,sigma=1,kappa.tau=2), 
                 list(P=round(runif(NY,0.01,3),2),K=1000,r=round(runif(NY,0.01,3),2),S=0.85,q=0.6,sigma=0.1,kappa.tau=1),
                 list(P=round(runif(NY,0.01,3),2),K=500,r=round(runif(NY,0.01,3),2),S=0.5,q=0.4,sigma=0.5,kappa.tau=1.5)))
}


# ---- Run the model ----
Spa1B.model <- SSModel(SPA1B.dat,BoFSPA4.priors,SPA1B.inits(NY),model.file=BoFmodel,Years=yrs,parms = parm,
                    nchains=nchains,niter=niter,nburnin=nburnin,nthin=nthin,debug=F) 
#need to save model as year defined object for prediction evaluations 
assign(paste0("Spa1B.", max(yrs)), Spa1B.model)   

# Save this when you are happy with the results
save(list = paste0("Spa1B.", max(yrs)), file=paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",max(yrs),".RData"))
#save(Spa1B.model,file = paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",max(yrs),".RData"))
# If you are happy with the run you did most recently you can just load it rather than re-running the model.
#load(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",max(yrs),".RData"))

#This is just to save you from wasting time changing the names of a bunch of lines below...
#mod.res <- Spa1B.2024
mod.res <- Spa1B.model

#This gives a print to screen of model results and allows you to save it
temp <- print(mod.res)
write.csv(temp, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/Spa1BModelOutput.csv"))

# You want this to be a minimum of 400, if less than this you should increase your chain length
min.neff <- min(mod.res$summary[,9])
if(min.neff < 400) print(paste("Hold up sport!! Minimum n.eff is",min.neff," You should re-run the model and increase nchains until this is at least 400")) 
if(min.neff >= 400) print(paste("Good modelling friend, your minimum n.eff is",min.neff)) 
# same idea for Rhat, here we want to make sure Rhat is < 1.05 which suggests the chains are well mixed.
Rhat <- signif(max(mod.res$summary[,8]),digits=4)
if(Rhat > 1.05) print(paste("Hold up mes amis!! You have an Rhat of ",Rhat," You should re-run the model and increase nchains until this is below 1.05")) 
if(Rhat <= 1.05) print(paste("Good modelling friend, your max Rhat is",Rhat)) 


# This prints and saves some interesting summary statistics.
summ.Spa1B <- summary(mod.res)
dump('summ.Spa1B', paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/Spa1BModelsummaryObj.R"))

# This plots the time series of survey biomass estimates.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Survey_est_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(Spa1B.2017, type="Survey.est")
SSModel.plot.median(mod.res, type="Survey.est")
dev.off()

# This is our biomass time series with reference points, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_",area,".png"),width=8,height=11,units = "in",res=920)
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,RP.labels = RP.labels,log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY])
dev.off()

# This is our FRENCH biomass time series with reference points, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_",area,"_FR.png"),width=8,height=11,units = "in",res=920)
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,RP.labels = RP.labels,log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY], french = TRUE)
dev.off()


#windows()
jpeg(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Biomass_figure_for_Document_",area,".jpg"),width=11,height=8,units = "in",res=920)
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],log.R = NULL,cex=1.4,RP.labels=RP.labels)
dev.off()

#windows()
jpeg(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Biomass_figure_for_Document_",area,"_FR.jpg"),width=11,height=8,units = "in",res=920)
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],log.R = NULL,cex=1.4,RP.labels=RP.labels, french = TRUE)
dev.off()

# Plot of the posteriors, this is not all of our posteriors it is worth noting, just a selection of them.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Posterior_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(Spa1B.2017, type="Prior.Post")
SSModel.plot.median(mod.res, type="Prior.Post")
dev.off()

# Plot the exploitation and natural mortalities.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Mortality_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(Spa1B.2017, type="Exploit")
SSModel.plot.median(mod.res, type="Exploit")
dev.off()


# ---- RESIDUAL PLOTS (note not "true" residuals.. talk to DK..) ----
#Assuming in your parm call you included pulling the model residuals from winbugs, then you can run the following residual plots
#mod.res <- Spa1B.2019
Presids <- data.frame(mod.res$summary[grepl('^Presid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Presids
Presids$year <- 1997:max(yrs)
# The names for inshore might be different so check those if this breaks
Presids <- Presids %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(Presids) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.Presids <- ggplot(Presids) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
#Based on scale of unstandarized process residuals either in kilotonnes or scaled by K ? Need to talk to Dave 
plot.Presids

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.Presids_",area,".png"),width=11,height=8,units = "in",res=920)
plot.Presids
dev.off()


sPresid <- data.frame(mod.res $summary[grepl('^sPresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Presids
sPresid$year <- 1997:max(yrs)
# The names for inshore might be different so check those if this breaks
sPresid <- sPresid %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(sPresid) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.sPresid <- ggplot(sPresid) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.sPresid

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.sPresid_",area,".png"),width=11,height=8,units = "in",res=920)
plot.sPresid
dev.off()



Iresid <- data.frame(mod.res$summary[grepl('^Iresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Iresid
Iresid$year <- 1997:max(yrs)
# The names for inshore might be different so check those if this breaks
Iresid <- Iresid %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(Iresid) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.Iresid <- ggplot(Iresid) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.Iresid

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.Iresid_",area,".png"),width=11,height=8,units = "in",res=920)
plot.Iresid
dev.off()


IRresid <- data.frame(mod.res$summary[grepl('^IRresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Iresid
IRresid$year <- 1997:max(yrs)
# The names for inshore might be different so check those if this breaks
IRresid <- IRresid %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(IRresid) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.IRresid <- ggplot(IRresid) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.IRresid

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.IRresid_",area,".png"),width=11,height=8,units = "in",res=920)
plot.IRresid
dev.off()


# --- Prediction Evaluations - Condition Assumption ---- 
# Load in the old model results for the prediction evaluation, this will now automatically load all the
# necessary data up to the year you want (whatever you specified in (yrs)
t.yrs <- yrs
pred.yr <- 2009:max(yrs) # Neeed to set this here as these loads will all overwrite the maximum year neeeded below...
num.pred.years <- length(pred.yr)
for(k in 1:num.pred.years)
{
  if(pred.yr[k] < 2016)   load(file = paste0(direct,"/Model_results_2009_2015/SPA1B/SPA1B_",pred.yr[k],".RData"))  
  if(pred.yr[k] %in% c( 2016, 2017, 2018))  load(file = paste0(direct,"/",pred.yr[k],"/",pred.yr[k]," Assessment/SPA1B/ModelOutput/SPA1B_Model_",pred.yr[k],".RData"))  
   #if(pred.yr[k] %in% c(2019, 2020, 2021))   load(file = paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",pred.yr[k],".RData")) 
  if(pred.yr[k] == 2019)   load(file = paste0(direct,"/2020/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",pred.yr[k],".RData")) 
  #Note no model run for 2020 since no survey in 2020 but ran the model with imputed data since needed for prediction evals in 2021 
  if(pred.yr[k] == 2020)   load(file = paste0(direct,"/2021/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",pred.yr[k],".RData"))
  if(pred.yr[k] >= 2021)   load(file = paste0(direct,"/",pred.yr[k],"/Assessment/Data/Model/SPA",area,"/SPA1B_Model_",pred.yr[k],".RData")) 
  
}
yrs <- t.yrs # Just so yrs doesn't get overwritten by what we load here

Combined.runs.predicted <- NULL
for(i in 1:num.pred.years) 
{
  #Grab the correct data...
 if(pred.yr[i] < 2016) dat <- get(paste0("Spa1B.new.",pred.yr[i]))
  if(pred.yr[i] >= 2016) dat <- get(paste0("Spa1B.",pred.yr[i]))
  # These data are missing in the 2015 model object on the ESS so I'm forcing them here.
  if(pred.yr[i] == 2017) 
  {
    # From SPA1B_model_2017_final.R
    dat$data$g[dat$data$NY] <- 1.1707 
    dat$data$gR[dat$data$NY] <- 1.6169
  }
  # The correct Catch data isn't available until the following year, so use this information for all but the most recent year...
  if(i < num.pred.years) 
  {
    # Grab correct data...
   if(pred.yr[i] < 2015) dat.next <- get(paste0("Spa1B.new.",(pred.yr[i]+1)))
   if(pred.yr[i] >= 2015) dat.next <- get(paste0("Spa1B.",(pred.yr[i]+1)))
    
    Combined.runs.predicted[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch =  as.numeric(raw.dat$C[raw.dat$YearSurvey == (pred.yr[i])]),
                                                                      g.parm= dat$data$g[dat$data$NY],
                                                                      gr.parm = dat$data$gR[dat$data$NY])
  } # end if(i < num.pred.years) 
  # Now for the most recent year we need to supply what we expect for the catch next year, this value is supplied above in the catch.next.year obj.
  if(i == num.pred.years)    Combined.runs.predicted[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = catch.next.year,
                                                                                               g.parm= dat$data$g[dat$data$NY],
                                                                                               gr.parm = dat$data$gR[dat$data$NY])
} # end for(i in 1:num.pred.years) 

str.yr.pe <- min(as.numeric(names(Combined.runs.predicted))) # Start year for the prediction eval plots

# Plot this PREDICTED prediction-evaluation plot.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Prediction_evaluation_figure_predicted_",area,".png"),width=11,height=8,units = "in",res=920)
eval.predict(Combined.runs.predicted, Year=str.yr.pe, pred.lim=0.2)
dev.off()

#Runs the prediction evaluation using the ACTUAL growth rates for g and gR parameters.  
Combined.runs.actual <- NULL
for(i in 1:num.pred.years) 
{
  if(pred.yr[i] < 2016) dat <- get(paste0("Spa1B.new.",pred.yr[i]))
  if(pred.yr[i] >= 2016) dat <- get(paste0("Spa1B.",pred.yr[i]))
  # Run the prediction... Note I grab the estimate from the raw.dat object from above, saves a lot of grief!!
  Combined.runs.actual[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = as.numeric(raw.dat$C[raw.dat$YearSurvey == (pred.yr[i])]),
                                                                 g.parm= as.numeric(raw.dat$g[raw.dat$YearSurvey == (pred.yr[i])]),
                                                                 gr.parm = as.numeric(raw.dat$gR[raw.dat$YearSurvey == (pred.yr[i])]))
} # end for(i in 1:num.pred.years) 

str.yr.pe <- min(as.numeric(names(Combined.runs.actual))) # Start year for the prediction eval plots

# Plot this ACTUAL prediction-evaluation plot.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Prediction_evaluation_figure_actual_",area,".png"),width=11,height=8,units = "in",res=920)
eval.predict(Combined.runs.actual, Year=str.yr.pe,pred.lim=0.2)
dev.off()




# --- One Year Projection Boxplot for Plot ----
# Create data object (posterior distribution) associated with one year projection with interm TAC removals -- i.e. 1 year project boxplot data for commercial biomass timeseries figure in FSAR 
# Note B.next is next year predicted biomass having grown up scallop as per g.parm and gr.parm and using mortality - default in function is m.avg = 5 (last 5 years)
pred.1yr.boxplot <- predict(mod.res, Catch=catch.next.year, g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY])
pred.1yr.boxplot$B.next
median((pred.1yr.boxplot$B.next))
write.csv(pred.1yr.boxplot$B.next, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/boxplot.data.1y.predict.catch.of.",catch.next.year,".in.",max(yrs)+1,"_",area,".csv"),row.names = F)


# --- Decision Tables ----
#Finally here we have the decsion table.  This plots the decision table for all catch rates between 0 and 500 increments of 10 tonnes of catch (seq(0,500,10)).
# Note that Feb 2020 DK noted that the incorrect LRP and USR were entered here (used SPA4s), checked against existing decision table and it made
# no difference in our probabilities luckily, but I did update the decision table based on these correct values.
decision <- predict(mod.res, Catch=c(seq(150,500,25)), g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY])
decision.table<-SSModel_predict_summary_median(decision, LRP=LRP, USR=USR, RRP=0.15)
# decision.table<-SSModel_predict_summary_median(decision, LRP=880, USR=1800, RRP=0.12)
decision.table

write.csv(decision.table, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/decisiontable",max(yrs),"_",area,".csv"),row.names = F)


# Finally, use the BoF Model stats function to produce a nice summary of model info that we need for assessment docs text 
#Produces files: summary stats temporal_1B_2019.csv ; summary stats_1B_2019.csv
# Be sure to set assessmentyear and surveyyear and RDatafile appropriately !!
#stats.output <- BoF.model.stats(area = "1B", assessmentyear=2024, surveyyear=2024, direct = "Y:/Inshore/BoF/", RDatafile = "SPA1B_Model_2024")
stats.output <- BoF.model.stats(area = "1B", assessmentyear=assessmentyear, surveyyear=surveyyear, direct = "Y:/Inshore/BoF/", RDatafile = paste0("SPA1B_Model_",surveyyear))


# Probability that current year commercial biomass estimate is in the Healthy zone (i.e. above the USR), and in the cautious zone (ie. interpret as being above the LRP  and below the USR (i.e. 1- prob>USR): 
# Get the biomass posteriors for every year
B.dat <- mod.res$sims.matrix[, is.element(substr(dimnames(mod.res$sims.matrix)[[2]], 1, 2), "B[")]
#colnames(B.dat)
#dim(B.dat)[2]
#select last column of matrix since this is most recent year of biomasses 
B.dat.current.year <- B.dat[,dim(B.dat)[2]]

#probabilty that current year biomass is above the USR and above the LRP 
prob.above.USR <- sum(B.dat.current.year > USR)/length(B.dat.current.year)
prob.above.USR
prob.above.LRP <- sum(B.dat.current.year > LRP)/length(B.dat.current.year)
prob.above.LRP

#Save out key results and diagnostics -- use this workspace to source for .Rmd Update doc (Note can easily take 5 mins or so to save out)
#Objects saved out: LRP, USR, catch.next.year, min.neff, Rhat, temp, summ.Spa1b, decision, decision.table,  prob.above.USR, prob.above.LRP
tt <- Sys.time()
save(LRP, USR, catch.next.year, min.neff, Rhat, temp, stats.output, summ.Spa1B, decision, decision.table,  prob.above.USR, prob.above.LRP,
     file=paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/Model_results_and_diagnostics_",max(yrs),"_",area,".RData"))
tt - Sys.time()

# ... END OF MODEL PLOTS...

############################################################################

## NEW INDICES PLOTS - BIOMASS (scaled to area) BY POPULATION NUMBER ###

raw.dat$wgt.num <- (raw.dat$I*1000000)/raw.dat$N #convert I in tonnes to grams
coeff <- 10^7
options(scipen = 999)
raw.dat.forplot <- raw.dat #|> filter(YearSurvey != 2024)

raw.dat.forplot <- raw.dat.forplot |> 
  rename("Biomass (tonnes)" = I) |> 
  rename("Numbers" = N) |> 
  rename("Average Weight per Scallop (g)" = wgt.num)

raw.dat.forplot.2 <- pivot_longer(raw.dat.forplot, 
                                  cols = c("Average Weight per Scallop (g)", "Numbers", "Biomass (tonnes)"),
                                  names_to = "Indices",
                                  #names_prefix = "X",
                                  values_to = "value",
                                  values_drop_na = FALSE)


I.N.plot.2 <- ggplot(data = raw.dat.forplot.2, aes (x = YearSurvey)) + 
  geom_line(data = raw.dat.forplot.2, aes(y = value), colour = "black") +
  scale_x_continuous(breaks=seq(min(raw.dat.forplot$YearSurvey),max(raw.dat.forplot$YearSurvey), 2))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "grey"))+
  xlab("Year")+
  facet_wrap(Indices~., dir = "v", scales = "free")
I.N.plot.2

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA1B/SPA1B_population_number_panel_indices",surveyyear,".png"), type="cairo", width=20, height=15, units = "cm", res=300)
I.N.plot.2
dev.off() 


#Alternative plot:

raw.dat$wgt.num <- (raw.dat$I*1000000)/raw.dat$N #convert I in tonnes to grams
coeff <- 10^7
options(scipen = 999)
#raw.dat <- raw.dat |> filter(YearSurvey != 2024)


I.N.plot <- ggplot(data = raw.dat, aes (x = YearSurvey)) + 
  geom_line(data = raw.dat, aes(y = wgt.num), colour = "black") +
  geom_line(data = raw.dat, aes(y = N/coeff), colour = "grey", linetype = "dashed") +
  scale_y_continuous(name = "Average weight per scallop (grams)",
                     sec.axis = sec_axis(~.*coeff, name = "Numbers of scallops"))+
  scale_x_continuous(breaks=seq(min(raw.dat.forplot$YearSurvey),max(raw.dat.forplot$YearSurvey), 2))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "grey"))+
  xlab("Year")
I.N.plot

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA1B/SPA1B_population_number_index",surveyyear,".png"), type="cairo", width=20, height=15, units = "cm", res=300)
I.N.plot
dev.off() 

