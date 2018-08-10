# Helper scripts to transform data from ./Data

# Joins input data table with Charter factor by CDS
joinCharters <- function(DT) {
  
  # Load database of California Public Schools
  pubsch <- read.table("./pubschls.txt", fill=TRUE, na.strings=c("", "NA"), sep ="\t", quote = "", header=TRUE)
  pubsch <- data.table(pubsch)
  
  # Isolate the Charter/CDSCode variables in a table to join
  charters <- pubsch[, .(CDSCode, Charter)]
  head(charters)
  # Select unique rows by CDS
  setkey(charters, CDSCode)
  uniqcharter <- subset(unique(charters))
  
  # Join by CDS with data table
  setkey(DT, CDS_CODE)
  setkey(uniqcharter, CDSCode)
  DT <- DT[uniqcharter, nomatch=0]
  return(DT)
}
