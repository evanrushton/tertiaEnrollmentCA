# Combine EL status to relevant yearly data
# These data and more are available at http://www.cde.ca.gov/ds/sd/sd/

# ================== Load libraries and scripts ===========================
# library(tidyverse)
library(ggplot2) # For data visualization
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(magrittr) # Pipe %<>%
library(scales)
source("./functions.R")

# ================== Create data.tables  ================== 
# Free/Reduced Price Lunch (FRPL) data
# FRPL Data (2004-2018): https://www.cde.ca.gov/ds/sd/sd/filessp.asp File structure changes every year until 2014: https://www.cde.ca.gov/ds/sd/sd/fsspfrpm.asp
# FRPL Data (1988-2003): https://www.cde.ca.gov/ds/sh/cw/filesafdc.asp File Structure: https://www.cde.ca.gov/ds/sh/cw/fsafdc.asp

# Create a list of files to combine 
fr8803.list <- fileList("./Data/CA", "^af.*csv$") # 1988-2003 (1988-1997 13 variables, 1998 14 variables, 1999-2003 15 variables)
fr0417.list <- fileList("./Data/CA", "^fr.*csv$") # 2004-2017 (every starting year until 2014 it changed... 2004 20 vars? (14), 2005 13 vars, 2006 14, 2007 14, 2008 14, 2009 14 , 2010 14 , 2011 15, 2012 22, 2013 28, 2014-2017 28 (headers in 2017 first col same as 2013 colnames?))

# Make consistent FRPL colnames (15 variables)
options(scipen = 16)  # Make full CDS_CODE visible
colnames <- c("YEAR", "C", "D", "S", "DISTRICT", "SCHOOL", "GRD_SPAN", "PUBLIC_ENR", "PRIVATE_EN", "CALWORKS", "FREE_MEALS", "RED_MEALS", "TOTAL_MEAL", "CALW_PCT", "MEAL_PCT")

# Convert file list into a data.table with an ID row (year variable is already a column)
DT.list <- lapply(fr8803.list, read.csv, header=TRUE, check.names=FALSE)
DT.list <- lapply(DT.list, setNames, colnames)
# Add year column and district name to 1998 (what is rank?)
DT03 <- rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol='YEAR') # creates data.table from list of data.frames

DT.list <- lapply(fr0417.list, read.csv, header=TRUE, check.names=FALSE)
setattr(DT.list, 'names', c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
DT.list <- lapply(DT.list, setNames, colnames)
DT17 <- rbindlist(DT.list, use.names=TRUE, fill=TRUE, idcol='YEAR') # creates data.table from list of data.frames

# ================== Part 2: Check NA values =============================
# Missing rows are likely due to school closures

sapply(DT00, function(y) sum(length(which(is.na(y))))) # 0
sapply(DT06, function(y) sum(length(which(is.na(y))))) # 79 rows, 24 CDS
sapply(DT17, function(y) sum(length(which(is.na(y))))) # 131 rows, 30 CDS
sapply(DTel, function(y) sum(length(which(is.na(y))))) # 210 rows, 54 CDS
length(unique(DTel$CDS_CODE[which(is.na(DTel$LC))]))


# ================== Write transformed data to csv =============================
# Write data tables to csv for others to use and avoid above work
# Recall the year ranges are kept seperate?

write.csv2(DTfr, "./Transformed_Data/CA/frpl.csv", na = "NA")
