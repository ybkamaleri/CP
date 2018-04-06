#############################
## filnavn
## OBS! filtype må bare CSV
#############################
## filePath <- getwd()
filePath <- "~/avid/cp"

allFiles <- list.files(paste0(filePath, "/data"))
csvFile <- grep(".csv", allFiles, value = TRUE)

## datasettet
dataPath <- paste0(filePath, "/data/", csvFile)
cpdata <- fread(dataPath, header = TRUE, encoding = "Latin-1")

## hab navn
cphab <- fread(paste0(filePath, "/", "hablist.csv"), encoding = "Latin-1")
