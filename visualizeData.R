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




# _crushton 2017-2018