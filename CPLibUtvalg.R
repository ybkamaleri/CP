######################
### LibUtvalg
######################

CPLibUtvalg <- function(RegData, minFaar, maxFaar, FNorge, fargepalett='BlaaOff')
{


###Hvis "Variabel" ikke definert
    "%i%" <- intersect
    if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
    Ninn <- dim(RegData)[1]


    ## indVarMed <- intersect(intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')), which(!is.na(RegData$Variabel)))


    indVarMed <- which(RegData$Variabel !='NA') %i% which(!is.na(RegData$Variabel)) %i% which(RegData$Variabel !='NaN')


    ##Fødselsår (minFa og maxFa)
    if ((minFaar > 1995) | (maxFaar < 2011)) {indFaar <- which(RegData$Faar >= minFaar & RegData$Faar <= maxFaar)
    } else {
        indFaar <- 1:Ninn}

    ## Født i Norge
    indFNorge <- if (as.numeric(FNorge) %in% 0:1) {which(RegData$FNorge == as.numeric(FNorge))
                 } else {
                     indFNorge <- 1:Ninn}


    indMed <- indFaar %i% indFNorge %i% indVarMed


    RegData <- RegData[indMed, ]

    N <- dim(RegData)[1]


    utvalgTxt <- c(paste0("Fødselsår fra: ", if ((minFaar > 1995) | (maxFaar < 2011)) {min(RegData$Faar, na.rm = T)
                                             } else {minFaar},' til ', if ((minFaar > 1995) | (maxFaar < 2011)) {max(RegData$Faar, na.rm = T)
                                                                       } else {maxFaar}),
                   if (as.numeric(FNorge) %in% 0:1) {sprintf("Født i %s ", c('Utlandet','Norge')[as.numeric(FNorge)+1])}
                   )




    UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
    return(invisible(UtData))
}
