# Combining yearly files into year spans based on common structure
# Eventual files are for 1981-1992, 1993-1997, 1998-2008, 2009-2017
# These data and more are available at http://www.cde.ca.gov/ds/sd/sd/

# ================== Load libraries and scripts ===========================
# library(tidyverse)
library(ggplot2) # For data visualization
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(magrittr) # Pipe %<>%
library(scales)
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
cds <- read.table("./Transformed_Data/CA/cds_master.csv", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
cds <- data.table(cds)

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


DT97 <- cds[DT97]
DT06 <- cds[DT06]

# Bind rows
DT08 <- rbind(DT06, DT08)
setkey(DT08, CDS_CODE)


# ================== Check NA values =============================
# Missing rows are likely due to school closures
sapply(DT08, function(y) sum(length(which(is.na(y))))) # 0
sapply(DT08c, function(y) sum(length(which(is.na(y))))) # 79 rows, 24 CDS
sapply(DT17c, function(y) sum(length(which(is.na(y))))) # 0
sapply(DT17, function(y) sum(length(which(is.na(y))))) # 131 rows, 30 CDS
sapply(DTel, function(y) sum(length(which(is.na(y))))) # 210 rows 



# Check Variables
head(DT92)
head(DT97)
head(DT08)
head(DT17)
str(DT92)
str(DT97)
str(DT08)
str(DT17) # should be 58 counties - replace CAPS with Caps         
replaceCAPS(DT17) # "HUMBOLDT", "STANISLAUS", "TEHAMA", "TULARE", "VENTURA"
DT17$COUNTY <- droplevels(DT17$COUNTY)
str(DT17)
# Check dimensions
dim(DT92) # 767338     26
dim(DT97) # 430252     24
dim(DT08) # 1184523      24
dim(DT17) # 11152164      24

# ================== Check NA values =============================
# Note that NA values 92, 97, 08 are mainly explained by unlabeled CDS_CODES with enrollment data 
# poor merging C, D, S by CDS_Code (probably closed schools)

sapply(DT92, function(y) sum(length(which(is.na(y))))) # 16387 CDS_CODEs to merge with C, D, S (not contained in cds of pubschls.txt)
sapply(DT97, function(y) sum(length(which(is.na(y))))) # 9431 CDS_CODEs to merge with C, D, S (not contained in cds of pubschls.txt)
sapply(DT08, function(y) sum(length(which(is.na(y))))) # 20295 CDS_CODEs to merge (not contained in cds of pubschls.txt) and 173 NA rows to inspect/remove, 
sapply(DT17, function(y) sum(length(which(is.na(y))))) # 653 NA rows to inspect/remove

# Recover lost CDS labels from DT92
cds92 <- recoverLostCDS(DT92, sch) # 229 recovered
setkey(cds92, CDS_CODE)
DT92[, SchoolName := NULL]; DT92[, DistrictName := NULL] # Remove S,D cols AFTER recover function above
joinRecoveredCDS(DT92, cds92)
joinRecoveredCDS(DT97, cds92)
joinRecoveredCDS(DT08, cds92)

sapply(DT92, function(y) sum(length(which(is.na(y))))) # All filled
sapply(DT97, function(y) sum(length(which(is.na(y))))) # 268 CDS_CODEs without C, D, S
sapply(DT08, function(y) sum(length(which(is.na(y))))) # 4013 CDS_CODEs without C, D, S

# Schools that opened since 1992 and closed before 2017
un97 <- unique(subset(DT97, is.na(SCHOOL), select=CDS_CODE)) # 15 unlabeled schools (from 199)
un08 <- unique(subset(DT08, is.na(SCHOOL), select=CDS_CODE)) # 322 unlabeled schools (from 510)

# Mysterious NA rows in DT08 and DT17
na08 <- DT08[which(is.na(ETHNIC)),]
unique(na08[,.(CDS_CODE)]) # 13 schools without enrollment for a year or more
na17 <- DT17[which(is.na(ETHNIC)),]
unique(na17[,.(CDS_CODE)]) # 38 schools without enrollment for a year or more

# Collect CDS_CODES for schools with NA enrollment rows
NA_CDS <- rbind(na08, na17) # Schools without enrollment
# Frequency chart of how many NA rows for each NA school by year
xtabs(~ CDS_CODE + YEAR, data=NA_CDS) 
# Looks like NA were removed before 2007
# How were the number of repetitions generated? (eg why 4, 8, 7, 8... for row 3?) This is gender and ethnicity represented at the school when it had/has enrollment.
# A school wih non-zero values may be assumed 'closed' (eg row 1 is a closed school 2007-2017)
# It may be safe to remove NA rows, but I will leave them in

# ================== Write transformed data to csv =============================
# Write data tables to csv for others to use and avoid above work
# Recall the year ranges are kept seperate due to changes in ETHNIC codes across ranges

write.csv2(DT92, "./Transformed_Data/CA/8192.csv", na = "NA")
write.csv2(DT97, "./Transformed_Data/CA/9397.csv", na = "NA")
write.csv2(DT08, "./Transformed_Data/CA/9808.csv", na = "NA")
write.csv2(DT17, "./Transformed_Data/CA/0917.csv", na = "NA")
