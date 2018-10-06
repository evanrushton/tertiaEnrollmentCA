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
recoverLostCDS <- function(DT92, sch) {
  cdsna92 <- unique(subset(DT92, is.na(SCHOOL), select=CDS_CODE)) # unlabeled schools
  setkey(cdsna92, CDS_CODE)
  setkey(DT92, CDS_CODE)
  cds92 <- unique(DT92[,.(CDS_CODE, DistrictName, SchoolName)])[cdsna92] # recovered District/School for NA
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
