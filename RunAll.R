###################################################### Load Libraries
 library(rethinking)
 library(MASS)
 library(maps)
 library(maptools)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(xtable)
 library(Cairo)

 rstan_options(auto_write = TRUE)
 options(mc.cores = parallel::detectCores())
 
 setwd("C:\\Users\\cody_ross\\Dropbox\\Completed and Published Projects\\1-papers\\Mushrooming\\PNAS - Revision2\\Workflow")

       
################################################################### Run Analysis
 source("Code/ProjectSupport.R")
 
 source("Code/MakeDataTable.R")
 
 source("Code/PrepareDataForStan.R")
 
 source("Code/FitBasicModel.R")
 source("Code/RegressionPlot.R")
 
 source("Code/TheoryPlot.R")
 source("Code/TheoryToRealPlot.R")
 
 source("Code/FitAR1Model.R")
 source("Code/AR1RegressionPlot.R")
 
 source("Code/FitSexModel.R")
 source("Code/RegressionPlotBySex.R")
 source("Code/TheoryPlotBySex.R")
 
 source("Code/FitVertModel.R")
 source("Code/VertRegressionPlot.R")
 

 
 source("Code/MakeTimeMaps.R")
 

 

  




                                                                                 


 










