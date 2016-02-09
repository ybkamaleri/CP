#################################################
##  Ctrl+A (Velge alt) og så Ctrl+R (Kjøre koden)
#################################################

rm(list = ls(all=T))

### OBS!!!
### Valg riktig sti til datasettet i CSV fil
### Bruk Forward Slash "/" og IKKE backslash "\"
setwd("~/OUS/CP/")
libkat <- "~/OUS/CP/"
CPData <- read.csv2("~/OUS/CP/MASTER.csv")

### Inndata til funksjon:
RegData <- CPData
outfile <- "" #paste(valgtVar, ".pdf", sep = "")
source("CPFigAndeler.R", encoding = "UTF-8")

###---Fødselsår---
minFaar <- 1996
maxFaar <- 2008

###---Født i Norge---
FNorge <- 1  # 0=Nei 1=Ja

########################################################
### Valg koden til Habiliteringstjeneste som i kodeboken
########################################################

Habiliteringstjeneste <- 7   #(Her er Habiliteringstjeneste fra kodebok)

#########################
## Skal sammenlignes?
#########################

Resultat <- 1

 #0 - hele landet
 #1 - egen enhet mot resten av landet
 #2 - egen enhet

#############################
## Valg variabel fra listen
##############################

Variabel <- "Svangerskapslengde"  #skrev utvalgte variabel med " "

## Variablene å velge mellom er:-
## Innlagtnyfodt, Respirator, Kramper, Svangerskapslengde, Fodselsvekt, CPDiagnose,
## Kognisjon, Viking, Synshemming, AlvorligSyns, Horselshemming, AlvorligHorsel, Epilepsi,
## Antiepileptika, Spisevanske, Gastrostomi

##########################
## Endre ikke noe herfra
#########################
habT <- Habiliteringstjeneste
valgtVar <- Variabel

FigAndeler(RegData = RegData, valgtVar = valgtVar,
           habT = habT, minFaar = minFaar, maxFaar = maxFaar,
           FNorge = FNorge, libkat = libkat, outfile = outfile)
