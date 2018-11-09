# Merge 1981-2016 data by making common levels for the ETHNIC factor
#
library(ggplot2) # For data visualization
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(magrittr) # Pipe %<>%
library(scales)
source("./functions.R")
require(bit64) # CDS codes read as int64 and no need for change to chr

DT8192 <- fread("./Transformed_Data/CA/8192.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT8192)
DT9397 <- fread("./Transformed_Data/CA/9397.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT9397)
DT9808 <- fread("./Transformed_Data/CA/9808.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT9808)
DT0916 <- fread("./Transformed_Data/CA/0916.csv", na.strings=c("", "NA"), header=TRUE); setDT(DT0916)
DT8192[, ETHNIC:=as.factor(ETHNIC)] # "A" "B" "F" "H" "I" "P" "W"
levels(DT8192$ETHNIC) = c(NA, NA, "2", "6", "4", "5", "1", "3", "7")
DT9397[, ETHNIC:=as.factor(ETHNIC)] # "1" "2" "3" "4" "5" "6" "7"
DT9808[, ETHNIC:=as.factor(ETHNIC)] # "1" "2" "3" "4" "5" "6" "7" "8"
DT0916[, ETHNIC:=as.factor(ETHNIC)] # "2" "6" "4" "5" "1" "3" "7"
DT8192[, YEAR:=as.factor(YEAR)] # (Using starting year as the year indicator)
levels(DT8192$YEAR) = c(1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992)

listDT8116 <- list(DT8192, DT9397, DT9808, DT0916) # List of data.tables for 4 year-spans
DT8116 <- rbindlist(listDT8116, use.names=TRUE)
levels(DT8116$ETHNIC) # "2" "6" "4" "5" "1" "3" "7" "8" "0" "9"
levels(DT8116$ETHNIC) = c("Asian", "African American", "Filipino", "Hispanic", "American Indian", "Pacific Islander", "White", "Multiple or No Response", "Not Reported", "Two or more") 

sapply(DT8116, function(y) sum(length(which(is.na(y))))) # 739 CDS, 29715 ETHNIC (mainly 82+83)

# Write combined data.table to csv
write_csv(DT8116, "./Transformed_Data/CA/8116.csv", na = "NA", append = FALSE, col_names = TRUE)

# Combine Indian, Islander, Multiple, and No Response to Other for $ETHNIC
setDT(DT8116) %>% .[ETHNIC == "American Indian" | ETHNIC == "Pacific Islander" | ETHNIC == "Multiple or No Response" | ETHNIC == "Two or more" | ETHNIC == "Not Reported", ETHNIC := "Other"]

# Try an individual school (is)
cds1 = 19647331933043 # "Benjamin Franklin Senior High"
cds2 = 19647331932540 # "Eagle Rock High/Eagle Rock Junior Senior High"

# Plot enrollments by ethnicity stacked across year
ggplot(DT8116[CDS_CODE == cds1][order(YEAR, ETHNIC)],aes(x = YEAR, y = ENR_TOTAL, fill = ETHNIC)) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Percentage of Total Enrollment") +
  scale_fill_discrete(guide=guide_legend(reverse=T)) +
  ggtitle("Ethnic Diversity of Franklin High School by Decade") # Name needs to be substituted

