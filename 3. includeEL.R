# Combine EL status to relevant yearly data
# These data and more are available at http://www.cde.ca.gov/ds/sd/sd/

# ================== Load libraries and scripts ===========================
library(tidyverse)
library(ggplot2) # For data visualization
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(magrittr) # Pipe %<>%
library(scales)
library(tools)
source("./functions.R")

# ================== Create data.tables  ================== 
# English Learner (EL) data
# EL Data (1980-2018: https://www.cde.ca.gov/ds/sd/sd/fileselsch.asp File structure: https://www.cde.ca.gov/ds/sd/sd/fselsch.asp
# TOTAL variable starting in 95 and changing in 98

# Create a list of files to combine 
el0117.list <- fileList("./Data/CA", "^elsc.*txt$") # 2001, 2007-2017 have headers
el0206.nh.list <- fileList("./Data/CA", "^elsn.*txt$") # 2002-2006 have no header
el8000.list <- fileList("./Data/CA", "^els.*csv$") # 1980 has extra column

# Make consistent EL colnames (21 variables)
options(scipen = 16)  # Make full CDS_CODE visible
colnames <- c("CDS_CODE", "COUNTY", "DISTRICT", "SCHOOL", "LC", "LANGUAGE", "KDGN", "GR_1", "GR_2", "GR_3", "GR_4", "GR_5", "GR_6", "GR_7", "GR_8", "GR_9", "GR_10", "GR_11", "GR_12", "UNGR", "TOTAL")

# Convert file list into a data.table with an ID row (Using starting year as the year indicator)
DT.list <- lapply(el8000.list, read.csv, header=TRUE, check.names=FALSE)
setattr(DT.list, 'names', c("1980", "1981", "1982","1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000"))
DT.list$`1980`$'CDS_CODE,C,14' <- NULL # Remove duplicate CDS col from 1980
DT.list <- lapply(DT.list, setNames, colnames)
DT00 <- rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol='YEAR') # creates data.table from list of data.frames

DT.list <- lapply(el0117.list, read.table, fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE, check.names=FALSE)
setattr(DT.list, 'names', c("2001", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
DT.list <- lapply(DT.list, setNames, colnames)
DT17 <- rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol='YEAR') # creates data.table from list of data.frames

DT.list <- lapply(el0206.nh.list, read.table, fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "\"", header=FALSE, check.names=FALSE)
setattr(DT.list, 'names', c("2002", "2003", "2004", "2005", "2006"))
DT.list <- lapply(DT.list, setNames, colnames)
DT06 <- rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol='YEAR') # creates data.table from list of data.frames

# Remove C, D, S and use LC to unify Language category
DT00[,COUNTY:= NULL]; DT00[,DISTRICT:= NULL]; DT00[,SCHOOL:= NULL]
DT06[,COUNTY:= NULL]; DT06[,DISTRICT:= NULL]; DT06[,SCHOOL:= NULL]
DT17[,COUNTY:= NULL]; DT17[,DISTRICT:= NULL]; DT17[,SCHOOL:= NULL]
languageFromLC(DT00); DT00 <- droplevels(DT00)
languageFromLC(DT06); DT06 <- droplevels(DT06)
convertLC(DT17)
languageFromLC(DT17); DT17 <- droplevels(DT17)

# Load database of California Public Schools
# CDE doesn't keep complete CDS records at https://www.cde.ca.gov/ds/si/ds/pubschls.asp
# Built a more complete one in "0. createCDSmaster.R"
cds <- read.csv2("./Transformed_Data/CA/cds_master.csv", header=TRUE)
cds <- data.table(cds)
cds <- cds[, CDS_CODE:=as.character(CDS_CODE)]

DT00 <- DT00[,CDS_CODE:=as.character(CDS_CODE)]
DT17 <- DT17[,CDS_CODE:=as.character(CDS_CODE)]
DT06 <- DT06[,CDS_CODE:=as.character(CDS_CODE)]

setkey(cds, CDS_CODE); setkey(DT00, CDS_CODE); setkey(DT17, CDS_CODE); setkey(DT06, CDS_CODE)

DT00 <- cds[DT00]
DT17 <- cds[DT17]
DT06 <- cds[DT06]

# Ensure data types are consistent
str(DT00)
str(DT17)
str(DT06)
DTel <- rbind(DT00, DT17, DT06) # combine 3 data tables into one EL table for starting years 1980 - 2017
str(DTel)

# ================== Check NA values =============================
# Missing rows are likely due to school closures

sapply(DT00, function(y) sum(length(which(is.na(y))))) # 0 rows, 449 CDS
sapply(DT06, function(y) sum(length(which(is.na(y))))) # 0 rows, 197 CDS
sapply(DT17, function(y) sum(length(which(is.na(y))))) # 131 rows, 49 CDS (88 unknown languages -> NA)
sapply(DTel, function(y) sum(length(which(is.na(y))))) # 131 rows, 695 CDS
length(unique(DTel$CDS_CODE[which(is.na(DTel$LC))])) 

# ================== Write transformed data to csv =============================
# Write data tables to csv for others to use and avoid above work

write.csv2(DTel, "./Transformed_Data/CA/el.csv", na = "NA")

