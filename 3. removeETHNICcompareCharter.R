# ================================ Total Enrollment ==================================
# Enrollment totals from 1981-2016        ʕ•ᴥ•ʔ
# 

library(ggplot2) 
library(readr) 
library(data.table) 
library(magrittr) 
library(scales)
source("./functions.R")

# Read cleaned data files ***DIRTY***
DT92 <- read.table("./Transformed_Data/CA/8192.csv", fill=TRUE, na.strings=c("", "NA"), sep =",", quote = "", header=TRUE); setDT(DT92)
DT97 <- read.table("./Transformed_Data/CA/9397.csv", fill=TRUE, na.strings=c("", "NA"), sep =",", quote = "", header=TRUE); setDT(DT97)
DT08 <- read.table("./Transformed_Data/CA/9808.csv", fill=TRUE, na.strings=c("", "NA"), sep =",", quote = "", header=TRUE); setDT(DT08)
DT16 <- read.table("./Transformed_Data/CA/0916.csv", fill=TRUE, na.strings=c("", "NA"), sep =",", quote = "", header=TRUE); setDT(DT16)

# Remove GENDER, ETHNIC 
sapply(list(DT92, DT97, DT08, DT16), removeEthnGen)

# Combine 4 tables into one datatable
DT <- rbindlist(list(DT92, DT97, DT08, DT16), fill=TRUE)

# Set columns in correct order (categorical 1-5)
#setcolorder(DT07, c("YEAR", colnames(DT07)[!colnames(DT07) %in% c("YEAR")]))

# Group by CDS_CODE, COUNTY, DISTRICT, SCHOOL, YEAR and SUM over enrollment by grade (What is happeneing to NA?)
DT <- DT[, lapply(.SD,sum), by=eval(colnames(DT)[1:5])]

# Try some individual schools over time
cds1 = 19647331933043 #"Benjamin Franklin Senior High"
cds2 = 19648731936749 #"Paramount High"
ggplot(subset(DT, CDS_CODE %in% c(cds1, cds2)),
       aes(x = YEAR,
           y = ENR_TOTAL,
           color=SCHOOL)) +
  geom_point() +
  labs(y = "Total Enrollment")

# Try an entire district
district = "Stockton Unified"
ggplot(subset(ndDT, DISTRICT %in% district),
       aes(x = YEAR,
           y = ENR_TOTAL,
           group = CDS_CODE,
           color = SCHOOL)) +
  geom_point() + 
  geom_line(alpha = 0.7) +
  theme(legend.position="none")

# Count the number of schools
ndCount <- ndDT[, .(COUNT = uniqueN(CDS_CODE)), by = YEAR]
ggplot(ndCount,
       aes(x = YEAR, y = COUNT)) +
  geom_point() +
  labs(y = "NUMBER OF SCHOOLS")

# ============================ Public v Charter =========================
# Add Charter column from public schools set        ᕙ(⇀‸↼‶)ᕗ
#

# Isolate the Charter/CDSCode variables in a table to join
charters <- pubsch[, .(CDS_CODE, Charter)]
head(charters)
dim(charters)
# Select unique rows by CDS
setkey(charters, CDS_CODE)
uniqcharter <- subset(unique(charters))
dim(uniqcharter)
# Join by CDS with data table
setkey(DT, CDS_CODE)
setkey(uniqcharter, CDSCode)
DTc <- DT[uniqcharter, nomatch=0]


# ============================= Grade Level ========================
# Enrollment by school-type factors     （ ^_^）o自自o（^_^ ）
# Group into HS, MS, ES, other
# 

# ADULT SCHOOL
DTca <- subset(DTc, ADULT > 20)
# HIGH SCHOOL
DTch <- subset(DTc, GR_10 > 20)
# MIDDLE SCHOOL
DTcm <- subset(DTc, GR_7 > 20)
# ELEMENTARY SCHOOL
DTce <- subset(DTc, GR_1 > 20)
# KINDERGARTEN
DTck <- subset(DTc, KDGN > 20)
# Total plot 
ggplot(DTcm,
       aes(x = YEAR,
           y = ENR_TOTAL,
           group = SCHOOL,
           color=Charter)) +
  geom_point() + 
  geom_line(alpha = 0.7) +
  theme(legend.position="none") 

# Try some district-wide high schools over time
district = "Stockton Unified"
ggplot(subset(DTcm, DISTRICT %in% district),
       aes(x = YEAR,
           y = ENR_TOTAL,
           group = SCHOOL,
           color=Charter)) +
  geom_point() + 
  geom_line(alpha = 0.7) +
  theme(legend.position="none")


district = "Los Angeles Unified"
ggplot(subset(DTce, DISTRICT %in% district),
       aes(x = YEAR,
           y = ENR_TOTAL,
           group = SCHOOL,
           color=Charter)) +
  geom_point() + 
  geom_line(alpha = 0.7) +
  theme(legend.position="none")



# ~~~~~~~~~~~~~~~~ Need to sum ETHNIC-GENDER totals by CDS_CODE ~~~~~~~~~~~
#

DT16[,print(.SD),by=c("CDS_CODE", "ETHNIC", "GENDER")]