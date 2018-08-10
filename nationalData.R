# Explore National Data to compare with CA
# Fast Facts: https://nces.ed.gov/fastfacts/display.asp?id=65
# Population: https://www.census.gov/population/www/socdemo/school.html

# Enrollment: https://www.census.gov/data/tables/time-series/demo/school-enrollment/cps-historical-time-series.html
# Enrollmeny by Poverty: https://www.census.gov/data/tables/2017/demo/cps/pov-30.html
# Sample report: https://www.census.gov/content/dam/Census/library/publications/2013/demo/p20-571.pdf
# Private Schools: https://nces.ed.gov/surveys/pss/pssdata.asp

# Note UC admissions are hot: https://www.universityofcalifornia.edu/infocenter/admissions-source-school
# Check out LAUSD Data Nerd: https://www.schooldatanerd.com/2017/04/17/uc-admittance-rates-for-every-public-high-school-in-california/

# ================== Load libraries and scripts ===========================
# library(tidyverse)
library(readr) # For CSV file I/O
library(data.table) # To convert dataframes to datatables
library(readxl) # For xlsx


# =================== Part 0 ==================
# Explore PEP_2016 survey and extract school-aged information for each state by year
#

natpubsch <- read_excel("./Data/National/EDGE_GEOCODE_PUBLICSCH_1516.xlsx")
natpubsch <- setDT(natpubsch)
capubsch <- subset(natpubsch, LSTATE == "CA", select = c(NCESSCH, NAME, LZIP, CNTY15, NMCNTY15, LAT1516, LON1516))

# Check PEP_2016 totals against: https://nces.ed.gov/programs/digest/d09/tables/dt09_017.asp