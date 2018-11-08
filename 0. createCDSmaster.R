# Create a master CDS list

# ================== Load libraries and scripts ===========================
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(magrittr) # Pipe %>%
source("./functions.R")

# ================== Create data.tables 1981-2017 ============================
# Create data.tables for 1981-1992, 2009-2017, 1998-2008, 1993-1997
# Grouping years by common factors

# Create a list of files to combine
en0917.list <- fileList("./Data/CA", "^rol")
en9806.list <- fileList("./Data/CA", "^men")
en0708.list <- fileList("./Data/CA", "^wat")
en9397.list <- fileList("./Data/CA", "^ent")

# Convert file list into a data.table with an ID row (Using starting year as the year indicator)
DT17 <- listToDataTable(en0917.list, c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), id="YEAR")
DT08 <- listToDataTable(en0708.list, c("2007", "2008"), id="YEAR")
DT06 <- listToDataTable(en9806.list, c("1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006"), id="YEAR")
DT97 <- listToDataTable(en9397.list, c("1993", "1994", "1995", "1996", "1997"), id="YEAR")

# Read in 1981-1992 
if(!exists("DT92")) {
  DT92 <- read.table("./Data/CA/enr8192.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
  setDT(DT92)
}

# Make full CDS_CODE visible
options(scipen = 16) 
# Change CDS_CODE type from double to string for precision
DT17 <- DT17[, CDS_CODE:=as.character(CDS_CODE)]
DT08 <- DT08[, CDS_CODE:=as.character(CDS_CODE)]
DT06 <- DT06[, CDS_CODE:=as.character(CDS_CODE)]
DT97 <- DT97[, CDS_CODE:=as.character(CDS_CODE)]
DT92 <- DT92[, CDS_CODE:=as.character(CDS_CODE)]
# better to use bit64::integer64 for such ids than setNumericRounding(0)

# Load database of California Public Schools
# CDE doesn't keep complete CDS records at https://www.cde.ca.gov/ds/si/ds/pubschls.asp
pubsch <- read.table("./Data/CA/pubschls18.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
pubsch <- data.table(pubsch)
setnames(pubsch, c("CDSCode", "County", "District", "School"), c("CDS_CODE", "COUNTY", "DISTRICT", "SCHOOL"))
pubsch <- pubsch[, CDS_CODE:=as.character(CDS_CODE)]
# Attempted NCES list at https://nces.ed.gov/programs/edge/geographicGeocode.aspx but it was LESS schools

# Create data.table with CDSCode County District School Charter Latitude Longitude
sch <- pubsch[,.(CDS_CODE, COUNTY, DISTRICT, SCHOOL, Charter, Latitude, Longitude)]
setkey(sch, CDS_CODE)
sch <- unique(sch)

# Create data.table with school CDS information only
cds <- sch[,.(CDS_CODE, COUNTY, DISTRICT, SCHOOL)]
setkey(cds, CDS_CODE)
# remove cds without SCHOOL names
cds <- cds[!which(is.na(cds$SCHOOL)),]

# Set key for data tables
setkey(DT92, CDS_CODE); setkey(DT06, CDS_CODE); setkey(DT97, CDS_CODE); setkey(DT17, CDS_CODE); setkey(DT08, CDS_CODE)

# Reorder cols in DT08 and DT16
setcolorder(DT08, c("CDS_CODE", "COUNTY", "DISTRICT", "SCHOOL", "YEAR", "ETHNIC", "GENDER", "KDGN", "GR_1", "GR_2", "GR_3", "GR_4", "GR_5", "GR_6", "GR_7", "GR_8", "UNGR_ELM", "GR_9", "GR_10", "GR_11", "GR_12", "UNGR_SEC", "ENR_TOTAL", "ADULT"))
setcolorder(DT17, c("CDS_CODE", "COUNTY", "DISTRICT", "SCHOOL", "YEAR", "ETHNIC", "GENDER", "KDGN", "GR_1", "GR_2", "GR_3", "GR_4", "GR_5", "GR_6", "GR_7", "GR_8", "UNGR_ELM", "GR_9", "GR_10", "GR_11", "GR_12", "UNGR_SEC", "ENR_TOTAL", "ADULT"))

# Check for missing values
sapply(DT06, function(y) sum(length(which(is.na(y))))) # 0
sapply(DT97, function(y) sum(length(which(is.na(y))))) # 0
sapply(DT17, function(y) sum(length(which(is.na(y))))) # 653 rows 
sapply(DT08, function(y) sum(length(which(is.na(y))))) # 173 rows, 2 CDS
sapply(DT92, function(y) sum(length(which(is.na(y))))) # 0
sapply(cds, function(y) sum(length(which(is.na(y))))) # 0

# Merge County, District, School onto 92, 97 and 06 datatables
DT17 <- cds[DT17] # duplicates existing county, district and school
DT08 <- cds[DT08] # duplicates existing county, district and school
DT92 <- cds[DT92] # duplicates existing district and school

sapply(DT17, function(y) sum(length(which(is.na(y))))) # 653 rows, 22805 newCDS 
sapply(DT08, function(y) sum(length(which(is.na(y))))) # 173 rows, 2 CDS, 6605 newCDS
sapply(DT92, function(y) sum(length(which(is.na(y))))) # 0, 16428 newCDS

## Healing missing CDS codes
cdsna92 <- unique(subset(DT92, is.na(SCHOOL), select=CDS_CODE)) # 226 unlabeled schools
cdsna08 <- unique(subset(DT08, is.na(SCHOOL), select=CDS_CODE)) # 556 unlabeled schools
cdsna17 <- unique(subset(DT17, is.na(SCHOOL), select=CDS_CODE)) # 779 unlabeled schools
length(union(union(cdsna92$CDS_CODE, cdsna08$CDS_CODE), cdsna17$CDS_CODE)) # total 970 unlabeled schools
setkey(cdsna92, CDS_CODE); setkey(cdsna08, CDS_CODE); setkey(cdsna17, CDS_CODE)
setkey(DT92, CDS_CODE); setkey(DT08, CDS_CODE); setkey(DT17, CDS_CODE)

cds92 <-recoverLostCDS92(DT92, sch) # 226 recovered District/School for NA
cds08 <- DT08[which(!duplicated(DT08$CDS_CODE)),.(CDS_CODE, i.COUNTY, i.DISTRICT, i.SCHOOL)][cdsna08] # 556 recovered
cds17 <- DT17[which(!duplicated(DT17$CDS_CODE)),.(CDS_CODE, County, District, School)][cdsna17] # 779 recovered

setcolorder(cds92, c("CDS_CODE", "COUNTY", "DISTRICT", "SchoolName"))
names(cds92)[4] <- "SCHOOL"
names(cds08)[2:4] <- c("COUNTY", "DISTRICT", "SCHOOL")
names(cds17)[2:4] <- c("COUNTY", "DISTRICT", "SCHOOL")
cds_new <- rbind(cds92, cds08, cds17)
setkey(cds_new, CDS_CODE)
cds_new <- cds_new[which(!duplicated(CDS_CODE)),] # 970 unlabeled schools

cds <- rbind(cds, cds_new)

# Write cds_master list
write.csv2(cds, "./Transformed_Data/CA/cds_master.csv", na = "NA")
