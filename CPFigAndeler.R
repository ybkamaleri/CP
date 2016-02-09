##################
### FigAndeler
#################

FigAndeler <- function(RegData, valgtVar, libkat, outfile='', minFaar=1996, maxFaar=2099, FNorge='', habT)
{


######################
### Andre funksjoner
######################

    source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")

### Definerer registerspesifikke variable
    habT <- as.numeric(habT)

    RegData$HabT <- as.numeric(RegData$HAB_TJENESTE)
    RegData$Region <- RegData$BOFYLKE
    RegData$Faar <- as.numeric(format(as.Date(RegData$FDATO, format = "%m/%d/%Y"), "%Y"))
    RegData$Alder <- floor(difftime(Sys.Date(), as.Date(RegData$FDATO, format = "%m/%d/%Y"), units = "days")/365)

    RegData$FNorge[!is.na(RegData$FODT_NORGE)] <- 0
    RegData$FNorge[RegData$FODT_NORGE == 1] <- 1

    RegData$Innlagtnyfodt <- as.numeric(RegData$NYFAVD)
    RegData$Respirator <- as.numeric(RegData$RESBEH)
    RegData$Kramper <- as.numeric(RegData$KRAMP)
    RegData$Synshemming <- as.numeric(RegData$SYNSHEM)
    RegData$AlvorligSyns <- as.numeric(RegData$ALV_SYN)
    RegData$Horselshemming <- as.numeric(RegData$HORSELSHEM)
    RegData$AlvorligHorsel <- as.numeric(RegData$ALV_HORSEL)
    RegData$Epilepsi <- as.numeric(RegData$EPILEPSI)
    RegData$Antiepileptika <- as.numeric(RegData$ANTIEPILEPTIKA)
    RegData$Spisevanske <- as.numeric(RegData$SPISEVANSK)
    RegData$Gastrostomi <- as.numeric(RegData$GASTROSTOMI)

### Definere text

    shtxt2 <- c("Sykehuset i Østfold",
                "Akershus univ.sykehus",
                "OUS, Ullevål",
                "Sykehus Innlandet - Hedmark",
                "Sykehus Innlandet - Oppland",
                "Vestre Viken",
                "Sykehuset i Vestfold",
                "Sykehus Telemark",
                "Sørlandet sjukehus-Arendal",
                "Sørlandet sykehus-Kristiansand",
                "Stavanger uni.sykehus-Østerlide",
                "Haukeland uni.sykehus",
                "Helse Førde",
                "St.Olav-Trondsletten",
                "Helse Nord-Trøndelag - Levanger",
                "Nordlandssykehuset",
                "Univ.sykehus Nord-Norge",
                "Helse Finnmark",
                "Svalbard",
                "Helse Fonna - Haugesund",
                "Kristiansund sjukehus",
                "Ælesund sjukehus")

    shtxt3 <- c(1:12, 14, 16:21, 50, 60, 61)
    shtxt4 <- data.frame(shtxt3, shtxt2)

    shtxt <- switch(as.character(Resultat),
                    '0' = 'Hele landet',
                    '1' = with(shtxt4, shtxt2[shtxt3==habT]),
                    '2' = with(shtxt4, shtxt2[shtxt3==habT]))

#######################
### Valge variabler
######################


    if (valgtVar == 'Svangerskapslengde') {
        RegData$GA <- as.numeric(RegData$SVLEN)
        RegData$GA[RegData$GA < 28] <- 1
        RegData$GA[RegData$GA %in% c(28:31)] <- 2
        RegData$GA[RegData$GA %in% c(32:36)] <- 3
        RegData$GA[RegData$GA %in% c(37:41)] <- 4
        RegData$GA[RegData$GA > 41] <- 5
        RegData$Variabel <- RegData$GA
    }

    if (valgtVar == 'Fodselsvekt') {
        RegData$FV <- as.numeric(RegData$VEKT)
        RegData$FV[RegData$FV %in% c(0:999)] <- 1
        RegData$FV[RegData$FV %in% c(1000:1499)] <- 2
        RegData$FV[RegData$FV %in% c(1500:2499)] <- 3
        RegData$FV[RegData$FV %in% c(2500:3499)] <- 4
        RegData$FV[RegData$FV %in% c(3500:4499)] <- 5
        RegData$FV[RegData$FV  >=4500] <- 6
        RegData$Variabel <- RegData$FV
    }

    if (valgtVar == "CPDiagnose") {
        RegData$SDiag <- as.numeric(RegData$CPDIAG)
        RegData$SDiag[match(1:2, RegData$Diag)] <- 1
        RegData$SDiag[match(3:4, RegData$Diag)] <- 2
        RegData$SDiag[match(5:6, RegData$Diag)] <- 3
        RegData$SDiag[match(7, RegData$Diag)] <- 4
        RegData$SDiag[match(8:9, RegData$Diag)] <- 5
        RegData$Variabel <- RegData$SDiag
    }

    ## if (valgtVar == "Kognisjon") {
    ##     RegData$CogGp <- as.numeric(RegData$KOGN_KUN_KLINISK)
    ##     RegData$KogT <- with(RegData, ifelse(KOG_TESTRES %in% c(10,21:23), KOG_TESTRES, NA))
    ##     RegData$CogGp[RegData$CogGp==12 | as.numeric(RegData$KogT) %in% c(10, 21)] <- 1
    ##     RegData$CogGp[RegData$CogGp==11 | as.numeric(RegData$KogT) %in% c(22:23)] <- 2
    ##     RegData$Variabel <- RegData$CogGp
    ## }
    
    if (valgtVar == "Kognisjon") {
        RegData$CogGp <- as.numeric(RegData$KOGN_KUN_KLINISK)
        RegData$CogGp <- with(RegData, ifelse(KOG_TESTRES %in% c(10,21:23), KOG_TESTRES, CogGp))
        RegData$CogGp[RegData$CogGp %in% c(10,12,21)] <- 1
        RegData$CogGp[RegData$CogGp %in% c(11,22,23)] <- 2
        RegData$CogGp[RegData$CogGp==0 | is.na(RegData$CogGp)] <- 3
        RegData$Variabel <- as.numeric(RegData$CogGp)
        tab1 <- table(RegData$CogGp, useNA = "always")
        print(length(RegData$CogGp))
        print(addmargins(tab1))
    }

    if (valgtVar == "Viking") {
        RegData$Viking <- as.numeric(RegData$TALEFNK)
        RegData$Viking <- with(RegData, ifelse(Viking==4, 3, (ifelse(Viking==5, 4, (ifelse(Viking==0, 5, Viking))))))
        RegData$Variabel <- RegData$Viking
    }

    if (valgtVar %in% c('Synshemming', 'AlvorligSyns', 'Horselshemming', 'AlvorligHorsel', 'Epilepsi',
                        'Antiepileptika', 'Spisevanske', 'Gastrostomi', 'Innlagtnyfodt', 'Respirator',
                        'Kramper')) {
        RegData$Variabel <- RegData[ , valgtVar]
        RegData$Variabel[RegData$Variabel==0] <- 3
    }

    if (valgtVar %in% c('...')) {
        RegData$Variabel <- RegData[ ,valgtVar]
    }




########################################################################
###Tar ut de med manglende registrering av valgt variabel og gjør utvalg
    source(paste(libkat, 'CPLibUtvalg.R', sep=''), encoding="UTF-8")

    CPUtvalg <- CPLibUtvalg(RegData=RegData, minFaar=minFaar, maxFaar=maxFaar, FNorge=FNorge)
    RegData <- CPUtvalg$RegData
    utvalgTxt <- CPUtvalg$utvalgTxt

###Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
###if ((sml == 0) & (egenavd == 1)) {RegData <- RegData[which(RegData$ReshId == reshID), ]}     #{indShUt <- which(RegData$ReshId != reshID)}
    if (Resultat == 2) {RegData <- RegData[which(RegData$HabT == habT), ]}



#############################################
###----------- Figurparametre ---------------
#############################################
    cexgr <- 1  #Kan endres for enkeltvariable
    retn <- 'V' #Vertikal som standard. 'H' angis evt. for enkeltvariable
    grtxt <- '' #Spesifiseres for hver enkelt variabel
    grtxt2 <- ''        #Spesifiseres evt. for hver enkelt variabel
    subtxt <- ''        #Benevning
    flerevar <- 0


###Hvis for få observasjoner..
###if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
    if (dim(RegData)[1] < 5 | (length(which(RegData$HabT == habT))<2 & Resultat == 1)) {
###-----------Figur---------------------------------------
        FigTypUt <- figtype(outfile)
        farger <- FigTypUt$farger
        plot.new()
        title(main=paste('variabel: ', valgtVar, sep=''))       #, line=-6)
        legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
        text(0.5, 0.6, 'Færre enn 2 egne registreringer eller færre 5 totalt', cex=1.2)
        if ( outfile != '') {dev.off()}
    } else {




############################################
###----------- Gjøre beregninger ---------
############################################

        medSml <- 0
        utvalg <- c('Sh', 'Rest')       #Sh vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
        Andeler <- list(Sh = 0, Rest =0)

                                        #if (sml == 1) {
                                        #Hvis det skal gjøres sammenligning:
        if (Resultat %in% c(1,3)) {
            indSh <-which(RegData$HabT == habT)
            indRest <- which(RegData$HabT != habT)
            RegDataLand <- RegData
            ind <- list(Sh=indSh, Rest=indRest)
            medSml <- 1
        }

        for (teller in 1:(medSml+1)) {

                                        #for (teller in 1:(sml+1)) {
                                        #       if (sml == 1) {
            if (medSml == 1) {
                RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]
            }

                                        #Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.

            if (valgtVar=='Faar') {
                tittel <- 'Forekomst av CP i Norge'
                gr <- c(1996:2010)      #c(0,16,31,46,61,76,200)
                RegData$VariabelGr <- as.factor(RegData$Variabel)
                grtxt <- as.factor(gr)
                subtxt <- 'Fødselsår'
                retn <- "V"

            }

            if (valgtVar == 'Svangerskapslengde') {
                tittel <- "Svangerskapslengde (i uke)"
                grtxt <- c("<28 prematur", "28-31", "32-36", "37-41 termin", ">41")
                RegData$VariabelGr <- factor(RegData$GA, levels = 1:5, labels = grtxt)
                retn <- "H"
            }

            if(valgtVar == 'Fodselsvekt') {
                tittel <- "Fødselsvekt (i gram)"
                grtxt <- c("<1000", "1000-1499", "1500-2499", "2500-3499", "3500-4499", ">4499")
                RegData$VariabelGr <- factor(RegData$FV, levels = 1:6, labels = grtxt)
                retn <- "H"
            }

            ## if (valgtVar == 'Innlagtnyfodt') {
            ##     tittel <- "Innlagt i nyfødtavdelingen"
            ##     grtxt <- c("Vet ikke", "Ja", "Nei")
            ##     RegData$VariabelGr <- factor(RegData$InnlagtNyfodt, levels = 1:3, labels = grtxt)
            ##     retn <- "H"
            ## }

            if (valgtVar == "CPDiagnose") {
                tittel <- "CP Diagnose"
                grtxt <- c("Spastisk unilateral", "Spastisk bilateral", "Dyskinetisk", "Ataksi", "Uklassifisert")
                RegData$VariabelGr <- factor(RegData$SDiag, levels = 1:5, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Kognisjon") {
                tittel <- "Kognisjon"
                grtxt <- c("PU", "Normal", "Missing")
                RegData$VariabelGr <- factor(RegData$CogGp, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Viking") {
                tittel <- "Viking Taleskala og Grafisk kommunikasjon (ASK)"
                gr <- c(1:5)
                grtxt <- c("1", "2", "3", "4", "Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:5, labels = grtxt)
                retn <- "V"
            }

            if (valgtVar == "Synshemming") {
                tittel <- "Andel med synshemming"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "AlvorligSyns") {
                tittel <- "Andel med alvorlig synshemming"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Horselshemming") {
                tittel <- "Andel med hørselshemming"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "AlvorligHorsel") {
                tittel <- "Andel med alvorlig hørselshemming"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Epilepsi") {
                tittel <- "Andel med Epilepsi"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Antiepileptika") {
                tittel <- "Andel med Antiepileptika"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Spisevanske") {
                tittel <- "Andel med spisevanske"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Gastrostomi") {
                tittel <- "Andel med gastrostomi31 = Sondemates i hovedsak"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Innlagtnyfodt") {
                tittel <- "Andel innlagt i nyfødtavdelingen"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Respirator") {
                tittel <- "Andel med respiratorbehandlet"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Kramper") {
                tittel <- "Andel med kramper innen de første 72 timer"
                gr <- c(1:3)
                grtxt <- c("Ja", "Nei","Vet ikke")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- "H"
            }


            if (teller == 1) {Andeler$Sh <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
                Nsh <- dim(RegData)[1]}

            if (teller == 2) {Andeler$Rest <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
                Nrest <- dim(RegData)[1]}

        }

#############################
                                        #-----------Figur------------
                                        #Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr

                                        #Plottspesifikke parametre:
        FigTypUt <- figtype(outfile)
                                        #Tilpasse marger for å kunne skrive utvalgsteksten
        NutvTxt <- length(utvalgTxt)
        grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Sh)), '%)', sep='')
        vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
                                        #vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
        par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))   #Har alltid datoutvalg med

        farger <- FigTypUt$farger
        fargeSh <- farger[1]
        fargeRest <- farger[3]
        antGr <- length(grtxt)
        lwdRest <- 3    #tykkelse på linja som repr. landet
        cexleg <- 1     #Størrelse på legendtekst

                                        #Horisontale søyler
        if (retn == 'H') {
            xmax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
            pos <- barplot(rev(as.numeric(Andeler$Sh)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel personer (%)", #main=tittel,
                           col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)   #
            mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

            if (medSml == 1) {
                points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
                legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                       border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
                       lwd=lwdRest,     lty=NA, ncol=1, cex=cexleg)
            } else {
                legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
                       border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
            }
        }


        if (retn == 'V' ) {
                                        #Vertikale søyler eller linje
            ymax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
            pos <- barplot(as.numeric(Andeler$Sh), beside=TRUE, las=1, ylab="Andel personer (%)",
                           sub=subtxt,  col=fargeSh, border='white', ylim=c(0, ymax))
            mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
            mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
            if (medSml == 1) {
                points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
                legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                       border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
                       lwd=lwdRest, ncol=2, cex=cexleg)
            } else {
                legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
                       border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
            }
        }


        title(tittel, line=1, font.main=1)

                                        #Tekst som angir hvilket utvalg som er gjort
        avst <- 0.8
        utvpos <- 3     #Startlinje for teksten
        mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

        par('fig'=c(0, 1, 0, 1))
        if ( outfile != '') {dev.off()}


    }

}
