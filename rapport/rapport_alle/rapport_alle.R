#############################################
### Rapport for alle habiliteringstjeneste ##
#############################################
rm(list = ls())

##########################
## Valg interval for år ##
##########################

## hvilket år som start punkt
aarFra <- 1996

## hvilket år som end punkt
aarTil <- 2017

####################
##    Filmappe    ## 
####################

filePath <- getwd()

## eg. filePath <- "K:/Sensitivt/Klinikk04/CPRN/Kvartalsrapport/rapport_alle"
## OBS! Forward slash brukes "/" i sti spesifikasjon 


######### Nedenfor denne linjen bør ikke endres ###############

##################################################
## henter pakker eller installere om ikke finnes
###################################################
load <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg))
    install.packages(nypkg, dependencies = TRUE, repos = "http://cran.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

pakker <- c("rreg", "data.table", "ggplot2", "directlabels", "grid", "knitr", "kableExtra")
load(pakker)

#############################################
## DATA kilder - OBS! filtype må være CSV
#############################################


sr01 <- paste0(filePath,"/", "datakilder1.R")
sr02 <- paste0(filePath, "/", "datakilder2.R")
data <- ifelse(file.exists(sr01), sr01, sr02)
source(data)

## DATA
setDT(cphab)  #navn til habiliteringstjenester
setDT(cpdata) #dataset

## kombinere data og hb-navn
##cpdata <- merge(cpdata, cphab, by.x = "HAB_TJENESTE", by.y = "id")
cpdata <- cphab[cpdata, on = c(id = "HAB_TJENESTE")]

## Lage pdf dokumenter
for (hab in unique(cphab$id)){
  knit2pdf("cp_alle.Rnw", output = paste0('hab_', hab, '.tex'))
}
