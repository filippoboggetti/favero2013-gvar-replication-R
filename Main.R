################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script replicates Favero (2013).
###
### OUTPUT:       Table folder: replicates Table 1 and 2 in pdf. Please ensure 
###               TinyTex (or equivalent) is installed or you are willing to  
###               install it (the code will check existence and otherwise install)
###               Otherwise Run Safe_Replication.
###
###               Dataset: Dataset.rds The main Dataset used in the paper.
###
###               Plots folder: replicates all the graphs in the paper.
###
###               Backup Excel: A series of sanity checks.
###               
### WARNING:      Please remember that to perform bootstrapping for the IRF computation
###               many iterations are needed. Hence un-commenting the GIRFs file
###               will particularly slow down the execution. An Eviews file is pro-
###               vided for a faster and more accurate replication of GIRFs and
###               bootstrapping for confidence intervals (Fig 7.1-7.4).
################################################################################

## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work in every machine
## -----------------------------------------------------------------------------

listofpackages <- c("readxl", "dplyr", "ggplot2", "zoo", "reshape2", "tidyverse",#Import relevant packages, packages are downloaded and loaded
                    "xts", "openxlsx", "lubridate", "tibble", "patchwork", 
                    "purrr", "stringr", "grid", "gridExtra", "gtable","systemfit",
                    "broom", "tidyr", "xtable", "kableExtra", "plm", "MASS","grid")

source("R_scripts/preparePackages.R")
preparePackages(listofpackages)
rm(listofpackages, preparePackages)

## -----------------------------------------------------------------------------

#Exploratory Data Analysis: construct the first and second figure in the paper 
#Main Outcome: Exploratory data analysis reveals immediately that the nature of
#co-movement among bond spreads is very different from that of real variables.

source("R_scripts/Exploratory_Analysis.R")

#Create a full stacked dataset to be converted later in monthly data

source("R_scripts/Dataset.R")

#Create weekly (5-d) dataframe for yields 

source("R_scripts/d_Yields_1990_2011.R")

#Create a monthly full stacked Dataset to be used for the rest of the paper

source("R_scripts/Monthly_Dataset.R")

#Generate Global (in the sense of the GVAR, still European) Monthly Variables

source("R_scripts/Efficient_Global_variables.R")

#Generate the Graph 5 in the paper (pag.348)

source("R_scripts/Graph_5.R")

#Estimate the Traditional Model and Replicate Table 1, store in "Table", estimate 
#the model by also running panel restrictions. Output: 2 pdf (and .tex) file 
#in "tables"-->"table1"-->(No Panel Restrictions, Panel Restrictions)

source("R_scripts/Table_1.R")

#Estimate the GVAR proposed By Favero (2013) to Replicate Table 2, store in "Table",
#estimate the model also with panel restrictions. Output: 2 pdf (and .tex) file 
#in "tables"-->"table2"-->(No Panel Restrictions, Panel Restrictions)

source("R_scripts/Table_2.R")

#Replicate, after the previous step of estimating the two model the figure 6.1-6.3

source("R_scripts/Figure_6.R")

#Replicate Fig.7 (O.O.S.) for the traditional model. This code is deprecated please
#see the Eviews one in "Eviews_graphs". Due to the lack of a P^2 algorithm in R the model 
#is dynamically computed (iterated) and no bootstrapping is applied. To see how to bootstrap
#in R please see the construction of the GIRFs. 

#source("R_scripts/Forecats_Traditional_model.R")



#This code is deprecated please
#see the Eviews one in "Eviews_graphs". Due to the lack of a P^2 algorithm in R the model 
#is dynamically computed (iterated) and no bootstrapping is applied. To see how to bootstrap
#in R please see the construction of the GIRFs. 

source("R_scripts/GVAR_forecasting.R")

#Replicate Figure8 (upper part)

source("R_scripts/Fig8_a.R")

#Replicate the GIRFs: For an accurate replication see the "GIRFs" folder in which 
#it is possible to find and Eviews file. The code here is inefficient and is solved
#by hand. Bootstrapping is applied to compute the confidence bands for the IRFs. 
#Results are comparable with the image provided by Eviews but the necessity of 
#estimating the systemfit multiple times and the fact the the model has to be iterated
#rather then solved makes this code deprecated. The code is left uncommentated, refer 
#to the Eviews one. An frozen image is still provided in "plots". Please also note
#that for graphical purposes replications are set at 5000, if there is the necessity 
#of running this part f code for illustrative reasons, please change the number of 
#replications. 

#source("R_scipts/GIRF.R")

## -----------------------------------------------------------------------------









