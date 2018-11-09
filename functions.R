# Helper scripts to transform data from ./Data

# Loads each file from a specified dir, where regex is in the filename
fileList <- function(dir, regex) {
  file.list <- list.files(path = dir, pattern = regex, full.names=TRUE)
return(file.list)
}

# Returns a datatable from a list of tab delimited files and a list of names
listToDataTable <- function(file.list, names.list, id) {
  DT.list <- lapply(file.list, read.table, fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
  setattr(DT.list, 'names', names.list)
  return(rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol=id)) # creates data.table from list of data.frames
}

# Returns a datatable from a list of comma delimited files and a list of names
csvListToDataTable <- function(file.list, names.list, id) {
  DT.list <- lapply(file.list, read.table, fill=TRUE, na.strings=c("", "NA"), sep =",", quote = "", header=TRUE)
  setattr(DT.list, 'names', names.list)
  return(rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol=id)) # creates data.table from list of data.frames
}

# Replace COUNTY CAPS from '16 with Caps
replaceCAPS <- function(DT) {
  DT[COUNTY == "HUMBOLDT", COUNTY := "Humboldt"]
  DT[COUNTY == "STANISLAUS", COUNTY := "Stanislaus"]
  DT[COUNTY == "TEHAMA", COUNTY := "Tehama"]
  DT[COUNTY == "TULARE", COUNTY := "Tulare"]
  DT[COUNTY == "VENTURA", COUNTY := "Ventura"]
}

# Recode ETHNIC from '92 ch into appropriate code
recodeEthn <- function(DT) {
  # DT %>% .[ETHNIC == " " | ETHNIC == "  ", ETHNIC := NA]
  DT[ETHNIC == "I", ETHNIC := as.integer(1)]
  DT[ETHNIC == "A", ETHNIC := as.integer(2)]
  DT[ETHNIC == "P", ETHNIC := as.integer(3)]
  DT[ETHNIC == "F", ETHNIC := as.integer(4)]
  DT[ETHNIC == "H", ETHNIC := as.integer(5)]
  DT[ETHNIC == "B", ETHNIC := as.integer(6)]
  DT[ETHNIC == "W", ETHNIC := as.integer(7)]
}

# DT1617 c(0: "Not reported", 1: "American Indian", 2: "Asian", 3: "Pacific Islander", 4: "Filipino", 5: "Hispanic", 6: "African American", 7: "White", 9: "Two or more")
# DT0607 c(1: "American Indian", 2: "Asian", 3: "Pacific Islander", 4: "Filipino", 5: "Hispanic", 6: "African American", 7: "White", 8: "Multiple or No Response")
# DT9697 c(1: "American Indian", 2: "Asian", 3: "Pacific Islander", 4: "Filipino", 5: "Hispanic", 6: "African American", 7: "White")
# DT8192 c(" ": NA, "  ": NA, "A": Asian", "B": African American", "F": Filipino", "H": Hispanic", "I": American Indian", "P": Pacific Islander", "W": White")


# Remove ETHNIC and GENDER
removeEthnGen <- function(DT) {
  DT[,ETHNIC:=NULL]
  DT[,GENDER:=NULL]
}

# Recover lost CDS labels from '92 data
recoverLostCDS92 <- function(DT92, sch) {
  cdsna92 <- unique(subset(DT92, is.na(SCHOOL), select=CDS_CODE)) # unlabeled schools
  setkey(cdsna92, CDS_CODE)
  setkey(DT92, CDS_CODE)
  cds92 <- DT92[which(!duplicated(DT92$CDS_CODE)),.(CDS_CODE, DistrictName, SchoolName)][cdsna92] # recovered District/School for NA
  # Recover COUNTY field
  countyDist <- sch[,.(COUNTY, DISTRICT)]
  setkey(countyDist, DISTRICT)
  countyDist <- unique(countyDist)
  # Remove "Elementary" and "High" from DISTRICT and DistrictName to increase matches
  countyDist[,DISTRICT:=sapply(as.character(DISTRICT), removeWords, stopwords = c("Elementary", "High"))]
  cds92[,DistrictName:=sapply(as.character(DistrictName), removeWords, stopwords = c("Elementary", "High"))]
  setkey(countyDist, DISTRICT)
  countyDist <- unique(countyDist) # Remove duplicate districts (used to have elem + high)
  setkey(cds92, DistrictName)
  cds92 <- countyDist[cds92]
  # Remove extra rows
  cds92 <- cds92[-which(cds92$DISTRICT == "Pleasant Valley" & cds92$COUNTY == "Ventura" | cds92$DISTRICT == "Washington Union" & cds92$COUNTY == "Monterey")]
  # Missing Districts 
  missdist <- c("Madera", "Contra Costa", "Orange", "San Diego", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara", "Santa Barbara")
  cds92[which(is.na(COUNTY)), COUNTY:=missdist]
  return(cds92)
}


# Remove words from a list of stopwords from the input string
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

# Joins data.table input with recovered labels table
joinRecoveredCDS <- function(DT, labels) {
  DT[labels, SCHOOL := SchoolName]
  DT[labels, DISTRICT := i.DISTRICT]
  DT[labels, COUNTY := i.COUNTY]
}

# Joins data.table input with Charter factor by CDS_CODE
joinCharters <- function(DT) {
  
  # Load database of California Public Schools into environment
  if(!exists(pubsch)) {
    pubsch <- read.table("./pubschls.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
    pubsch <- data.table(pubsch)
  }
  
  # Isolate the Charter/CDSCode variables in a table to join
  charters <- pubsch[, .(CDSCode, Charter)]
  head(charters)
  # Select unique rows by CDS
  setkey(charters, CDSCode)
  uniqcharter <- subset(unique(charters))
  
  # Join by CDS with data table
  setkey(DT, CDS_CODE)
  setkey(uniqcharter, CDSCode)
  DT <- DT[uniqcharter, nomatch=0]
  return(DT)
}

# Order enrollment columns before merging CDS
reorderCols <- function(DT) {
  setcolorder(DT, c("CDS_CODE", "YEAR", "ETHNIC", "GENDER", "KDGN", "GR_1", "GR_2", "GR_3", "GR_4", "GR_5", "GR_6", "GR_7", "GR_8", "UNGR_ELM", "GR_9", "GR_10", "GR_11", "GR_12", "UNGR_SEC", "ENR_TOTAL", "ADULT"))
}

# Set Language based on Language Code as integers
# https://www.cde.ca.gov/sp/cd/ci/childprimarylanguage.asp
languageFromLC <- function(DT) {
  DT[LC == 1, LANGUAGE := "Spanish"]
  DT[LC == 2, LANGUAGE := "Vietnamese"]
  DT[LC == 3, LANGUAGE := "Cantonese"]
  DT[LC == 4, LANGUAGE := "Korean"]
  DT[LC == 5, LANGUAGE := "Filipino"]
  DT[LC == 6, LANGUAGE := "Portuguese"]
  DT[LC == 7, LANGUAGE := "Mandarin"]
  DT[LC == 8, LANGUAGE := "Japanese"]
  DT[LC == 9, LANGUAGE := "Khmer (Cambodian)"]
  DT[LC == 10, LANGUAGE := "Lao"]
  DT[LC == 11, LANGUAGE := "Arabic"]
  DT[LC == 12, LANGUAGE := "Armenian"]
  DT[LC == 13, LANGUAGE := "Burmese"]
  DT[LC == 14, LANGUAGE := "Croatian"]
  DT[LC == 15, LANGUAGE := "Dutch"]
  DT[LC == 16, LANGUAGE := "Farsi (Persian)"]
  DT[LC == 17, LANGUAGE := "French"]
  DT[LC == 18, LANGUAGE := "German"]
  DT[LC == 19, LANGUAGE := "Greek"]
  DT[LC == 20, LANGUAGE := "Chamarro (Guamanian)"]
  DT[LC == 21, LANGUAGE := "Hebrew"]
  DT[LC == 22, LANGUAGE := "Hindi"]
  DT[LC == 23, LANGUAGE := "Hmong"]
  DT[LC == 24, LANGUAGE := "Hungarian"]
  DT[LC == 25, LANGUAGE := "Ilocano"]
  DT[LC == 26, LANGUAGE := "Indonesian"]
  DT[LC == 27, LANGUAGE := "Italian"]
  DT[LC == 28, LANGUAGE := "Punjabi"]
  DT[LC == 29, LANGUAGE := "Russian"]
  DT[LC == 30, LANGUAGE := "Samoan"]
  DT[LC == 31, LANGUAGE := "Serbian"]
  DT[LC == 32, LANGUAGE := "Thai"]
  DT[LC == 33, LANGUAGE := "Turkish"]
  DT[LC == 34, LANGUAGE := "Tongan"]
  DT[LC == 35, LANGUAGE := "Urdu"]
  DT[LC == 36, LANGUAGE := "Cebuano (Visayan)"]

  DT[LC == 38, LANGUAGE := "Ukrainian"]
  DT[LC == 39, LANGUAGE := "Chaozhou"]
  DT[LC == 40, LANGUAGE := "Pashto"]
  DT[LC == 41, LANGUAGE := "Polish"]
  DT[LC == 42, LANGUAGE := "Assyrian"]
  DT[LC == 43, LANGUAGE := "Gujarati"]
  DT[LC == 44, LANGUAGE := "Mien"]
  DT[LC == 45, LANGUAGE := "Romanian"]
  DT[LC == 46, LANGUAGE := "Taiwanese"]
  DT[LC == 47, LANGUAGE := "Lahu"]
  DT[LC == 48, LANGUAGE := "Marshallese"]
  DT[LC == 49, LANGUAGE := "Mixteco"]
  DT[LC == 50, LANGUAGE := "Khmu"]
  DT[LC == 51, LANGUAGE := "Kurdish"]
  DT[LC == 52, LANGUAGE := "Serbo-Croatian"]
  DT[LC == 53, LANGUAGE := "Toishanese"]
  DT[LC == 54, LANGUAGE := "Chaldean"]
  DT[LC == 55, LANGUAGE := "Other Languages of China"]
  DT[LC == 66, LANGUAGE := "Other Languages of the Philippines"]
  DT[LC == 88, LANGUAGE := "Native American Languages"]
  DT[LC == 99, LANGUAGE := "Other non-English"]
}