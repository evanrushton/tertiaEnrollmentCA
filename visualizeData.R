# An exploration of California's Enrollment Data   (╯°□°）╯︵ ┻━┻
# There is other data available http://www.cde.ca.gov/ds/sd/sd/
# Question: How have Charter schools affected Public/Private school enrollments?

# ================== Load libraries and scripts ===========================
# library(tidyverse)
library(ggplot2) # For data visualization
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(magrittr) # Pipe %<>%
library(scales)
source("./functions.R")

# =================== Part 0: Exploration ==================================
# Explore data for 1948-1969, 1970-1976, 1977-1980, 1981-1992, 1996-1997, 2006-2007, and 2016-2017
# File structure to be found http://www.cde.ca.gov/ds/sd/sd/filesenr.asp
# 

# Note 1948-1969, 1970-1976, 1977-1980 only have county-level enrollment (no School-level data, no ethinicities)
# 48-69 seperates grades, 48-76 seperates gender, 77-80 only has total enrollment by county without gender or grades
df4869 <- read.table("./Data/CA/enr4869.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
df7076 <- read.table("./Data/CA/enr7076.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
df7780 <- read.table("./Data/CA/enr7780.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
# School, Grade and Ethnicity factors start in 1981
df8192 <- read.table("./Data/CA/enr8192.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
# Annual data files start in 1993
df9697 <- read.table("./Data/CA/ent1996-1997.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
df0607 <- read.table("./Data/CA/men2006-2007.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
df1617 <- read.table("./Data/CA/rol2016-2017.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
# Make full CDS_CODE visible
options(scipen = 16) 

# Check head, str, dim for each df
head(df8192)
str(df4869)
dim(df4869)

# =================== Part 1a: Plot enrollments by grade ==================================
# with ethnic fill for four decades 8687, 9697, 0607, 1617

df8687 <- df8192[ which(df8192$YEAR == 8687),] # Select single year from 8192 df

# Try an individual school (is)
cds1 = 19647331933043 # "Benjamin Franklin Senior High"
cds2 = 19647331932540 # "Eagle Rock High/Eagle Rock Junior Senior High"
is1617 <- subset(df1617, CDS_CODE == cds2, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
is0607 <- subset(df0607, CDS_CODE == cds2, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
is9697 <- subset(df9697, CDS_CODE == cds2, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
is8687 <- subset(df8192, CDS_CODE == cds2 & YEAR == 8687, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
levels(is8687$ETHNIC) = c(NA, NA, "2", "6", "4", "5", "1", "3", "7")

# Plot enrollments by ethnicity with gender fill
is = is8687 # Set year to visualize (1617, 0607, 9697, 8687)
ggplot(data = is) +
  geom_bar(mapping = aes(x = ETHNIC, y = ENR_TOTAL, fill = GENDER), stat = "identity")

# Melt the grade variables and provide col names
tdyis1617 <- melt(is1617, id=c("ETHNIC", "GENDER"))
tdyis0607 <- melt(is0607, id=c("ETHNIC", "GENDER"))
tdyis9697 <- melt(is9697, id=c("ETHNIC", "GENDER"))
tdyis8687 <- melt(is8687, id=c("ETHNIC", "GENDER"))

# Make ETHNIC a factor and name the levels
tdyis1617$ETHNIC <- as.factor(tdyis1617$ETHNIC)
levels(tdyis1617$ETHNIC) = c("American Indian", "Asian", "Pacific Islander", "Filipino", "Hispanic", "African American", "White", "Two or more")
tdyis0607$ETHNIC <- as.factor(tdyis0607$ETHNIC)
levels(tdyis0607$ETHNIC) = c("American Indian", "Asian", "Pacific Islander", "Filipino", "Hispanic", "African American", "White", "Multiple or No Response") 
tdyis9697$ETHNIC <- as.factor(tdyis9697$ETHNIC)
levels(tdyis9697$ETHNIC) = c("American Indian", "Asian", "Pacific Islander", "Filipino", "Hispanic", "African American", "White") 
tdyis8687$ETHNIC <- as.factor(tdyis8687$ETHNIC)
levels(tdyis8687$ETHNIC) = c(NA, NA, "Asian", "African American", "Filipino", "Hispanic", "American Indian", "Pacific Islander", "White") 

# adjust which decade to visualize 1617, 0607, 9697, 8687
tdyis <- tdyis9697 
setnames(tdyis, c("variable", "value"), c("GRADE", "ENROLLMENT"))
ggplot(data = tdyis) +
  geom_bar(mapping = aes(x = GRADE, y = ENROLLMENT, fill = ETHNIC, order = ETHNIC), stat = "identity") +
  facet_grid(GENDER ~ .) +
  ggtitle("Eagle Rock HS Enrollment by Grade, Ethnicity, and Gender 1986-1987")



; setDT(DT4869); setDT(DT7076); setDT(DT7780); setDT(DT8192); setDT(DT9697); setDT(DT0607); setDT(DT1617)


# Note 1948-1969, 1970-1976, 1977-1980 only have county-level enrollment (no School-level data, no grades, no ethinicities)
# 48-76 seperates gender, 77-80 only has total enrollment by county
df4869 <- read.table("./Data/CA/enr4869.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT4869)
df7076 <- read.table("./Data/CA/enr7076.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT7076)
df7780 <- read.table("./Data/CA/enr7780.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT7780)
# School, Grade and Ethnicity factors start in 1981
df8192 <- read.table("./Data/CA/enr8192.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT8192)
# Annual data files start in 1993
df9697 <- read.table("./Data/CA/ent1996-1997.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT9697)
df0607 <- read.table("./Data/CA/men2006-2007.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT0607)
df1617 <- read.table("./Data/CA/rol2016-2017.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE); setDT(DT1617)
df8687 <- df8192[, YEAR == 8687] # Select single year from 8192 df


# Change CDS_CODE type from double to string for precision
df8192 <- df8192[, CDS_CODE:=as.character(CDS_CODE)]
df9697 <- df9697[, CDS_CODE:=as.character(CDS_CODE)]
df0607 <- df0607[, CDS_CODE:=as.character(CDS_CODE)]
df1617 <- df1617[, CDS_CODE:=as.character(CDS_CODE)]

# Try an individual school (is)
cds1 = 19647331933043 # "Benjamin Franklin Senior High"
cds2 = 19647331932540 # "Eagle Rock High/Eagle Rock Junior Senior High"
is1617 <- subset(df1617, CDS_CODE == cds2, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
is0607 <- subset(df0607, CDS_CODE == cds2, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
is9697 <- subset(df9697, CDS_CODE == cds2, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
is8687 <- subset(df8192, CDS_CODE == cds2 & YEAR == 8687, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
levels(is8687$ETHNIC) = c(NA, NA, "2", "6", "4", "5", "1", "3", "7")

# Plot enrollments by ethnicity with gender fill
is = is1617 # Set year to visualize (1617, 0607, 9697, 8687)
ggplot(data = is) +
  geom_bar(mapping = aes(x = ETHNIC, y = ENR_TOTAL, fill = GENDER), stat = "identity")

listis8616 <- list(is8687, is9697, is0607, is1617)
setattr(listis8616, 'names', c("1986", "1996", "2006", "2016"))
is8616 <- rbindlist(listis8616, use.names=TRUE, fill=TRUE, idcol="YEAR")
levels(is8616$ETHNIC) # "2" "6" "4" "5" "1" "3" "7" "8" "9"
levels(is8616$ETHNIC) = c("Asian", "African American", "Filipino", "Hispanic", "American Indian", "Pacific Islander", "White", "Multiple or No Response", "Two or more") 

# Combine Indian, Islander, Multiple, and No Response to Other for $ETHNIC
setDT(is8616) %>% .[ETHNIC == "American Indian" | ETHNIC == "Pacific Islander" | ETHNIC == "Multiple or No Response" | ETHNIC == "Two or more", ETHNIC := "Other"]



# Plot enrollments by ethnicity stacked across year
ggplot(is8616[order(YEAR, ETHNIC)],aes(x = YEAR, y = ENR_TOTAL, fill = ETHNIC)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Percentage of Total Enrollment") +
  scale_fill_discrete(guide=guide_legend(reverse=T)) +
  ggtitle("Ethnic Diversity of Eagle Rock School by Decade") # Name needs to be substituted

# Plot enrollments by grade with ethnic fill
# Melt the grade variables and provide col names
tdybf1617 <- melt(bf1617, id=c("ETHNIC", "GENDER"))
setnames(tdybf1617, c("variable", "value"), c("GRADE", "ENROLLMENT"))
# Make ETHNIC a factor
tdybf1617$ETHNIC <- as.factor(tdybf1617$ETHNIC)
levels(tdybf1617$ETHNIC) = c("American Indian", "Asian", "Filipino", "Hispanic", "African American", "White", "Two or more")
ggplot(data = tdybf1617) +
  geom_bar(mapping = aes(x = GRADE, y = ENROLLMENT, fill = ETHNIC), stat = "identity") +
  facet_grid(GENDER ~ ETHNIC)

# Visualize same for all of CA
dfHS1617 <- subset(df1617, GR_9 != 0, select = c(ETHNIC, GENDER, GR_9, GR_10, GR_11, GR_12, ENR_TOTAL))
tdydfHS1617 <- melt(dfHS1617, id=c("ETHNIC", "GENDER"))
setnames(tdydfHS1617, c("variable", "value"), c("GRADE", "ENROLLMENT"))
tdydfHS1617$ETHNIC <- as.factor(tdydfHS1617$ETHNIC)
levels(tdydfHS1617$ETHNIC) = c("None", "American Indian", "Pacific Islander", "Asian", "Filipino", "Hispanic", "African American", "White", "Two or more")
ggplot(data = tdydfHS1617) +
  geom_bar(mapping = aes(x = GRADE, y = ENROLLMENT, fill = ETHNIC), stat = "identity") +
  facet_grid(GENDER ~ ETHNIC)



# ================== Part 1c: Combine ETHNIC =============================
# Merge 1981-2016 data by making common levels for the ETHNIC factor
#
require(bit64) # CDS codes read as int64 and no need for change to chr
DT8192 <- fread("./Transformed_Data/CA/8192.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT8192)
DT9397 <- fread("./Transformed_Data/CA/9397.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT9397)
DT9808 <- fread("./Transformed_Data/CA/9808.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT9808)
DT0916 <- fread("./Transformed_Data/CA/0916.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT0916)
DT8192[, ETHNIC:=as.factor(ETHNIC)]
levels(DT8192$ETHNIC) = c("2", "6", "4", "5", "1", "3", "7")
DT9397[, ETHNIC:=as.factor(ETHNIC)]
DT9808[, ETHNIC:=as.factor(ETHNIC)]
DT0916[, ETHNIC:=as.factor(ETHNIC)]
DT8192[, YEAR:=as.factor(YEAR)]# (Using starting year as the year indicator)
levels(DT8192$YEAR) = c(1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992)

listDT8116 <- list(DT8192, DT9397, DT9808, DT0916)
DT8116 <- rbindlist(listDT8116, use.names=TRUE)
levels(DT8116$ETHNIC) # "2" "6" "4" "5" "1" "3" "7" "8" "0" "9"
levels(DT8116$ETHNIC) = c("Asian", "African American", "Filipino", "Hispanic", "American Indian", "Pacific Islander", "White", "Multiple or No Response", "Not Reported", "Two or more") 

write_csv(DT8116, "./Transformed_Data/CA/8116.csv", na = "NA", append = FALSE, col_names = TRUE)


# Combine Indian, Islander, Multiple, and No Response to Other for $ETHNIC
setDT(DT8116) %>% .[ETHNIC == "American Indian" | ETHNIC == "Pacific Islander" | ETHNIC == "Multiple or No Response" | ETHNIC == "Two or more" | ETHNIC == "Not Reported", ETHNIC := "Other"]

# Plot enrollments by ethnicity stacked across year
ggplot(DT8116[CDS_CODE == cds2][order(YEAR, ETHNIC)],aes(x = YEAR, y = ENR_TOTAL, fill = ETHNIC)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Percentage of Total Enrollment") +
  scale_fill_discrete(guide=guide_legend(reverse=T)) +
  ggtitle("Ethnic Diversity of Eagle Rock School by Decade") # Name needs to be substituted


# ================================ Part 2 Total Enrollment ==================================
# Enrollment totals from 1993-2016        ʕ•ᴥ•ʔ
# 

# Remove GENDER, ETHNIC (nd = no demographic)
sapply(list(DT92, DT97, DT08, DT16), removeEthnGen)
ndDT97 <- subset(DT97, select = -c(GENDER, ETHNIC) )
ndDT08 <- subset(DT08, select = -c(GENDER, ETHNIC) )
ndDT16 <- subset(DT16, select = -c(GENDER, ETHNIC) )

# Combine 3 tables into one datatable
ndDT <- rbindlist(list(ndDT97, ndDT08, ndDT16), fill=TRUE)

# Set columns in correct order (categorical 1-5)
#setcolorder(DT07, c("YEAR", colnames(DT07)[!colnames(DT07) %in% c("YEAR")]))

# Group by CDS_CODE, COUNTY, DISTRICT, SCHOOL, YEAR and SUM over enrollment by grade (What is happeneing to NA?)
ndDT <- ndDT[, lapply(.SD,sum), by=eval(colnames(ndDT)[1:5])]

# Try some individual schools over time
cds1 = 19647331933043 #"Benjamin Franklin Senior High"
cds2 = 19648731936749 #"Paramount High"
ggplot(subset(ndDT, CDS_CODE %in% c(cds1, cds2)),
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

# ============================ Part 3 Public v Charter =========================
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


# ============================= Part 4 Grade Level ========================
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

# _crushton 2017-2018