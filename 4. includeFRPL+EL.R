# Combine Free/Reduced Lunch and EL status to relevant yearly data
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
# English Learner (EL) data
# EL Data (1980-2018: https://www.cde.ca.gov/ds/sd/sd/fileselsch.asp File structure: https://www.cde.ca.gov/ds/sd/sd/fselsch.asp
# TOTAL variable starting in 95 and changing in 98
# Free/Reduced Price Lunch (FRPL) data
# FRPL Data (2004-2018): https://www.cde.ca.gov/ds/sd/sd/filessp.asp File structure changes every year until 2014: https://www.cde.ca.gov/ds/sd/sd/fsspfrpm.asp
# FRPL Data (1988-2003): https://www.cde.ca.gov/ds/sh/cw/filesafdc.asp File Structure: https://www.cde.ca.gov/ds/sh/cw/fsafdc.asp

# Create a list of files to combine 
el0117.list <- fileList("./Data/CA", "^els.*txt$") # (years with diff colnames)
el8000.list <- fileList("./Data/CA", "^els.*csv$") # (csv needs different params)
# Convert file list into a data.table with an ID row (Using starting year as the year indicator)
DT17 <- listToDataTable(el0117.list, c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), id="YEAR")
DT00 <- csvListToDataTable(el8000.list, c("1980", "1981", "1982","1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000"), id="YEAR")
 
