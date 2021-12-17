# Load libraries
library("tidyverse")
library("tabulizer")

file <- "Best Mathematics Graduate Programs - US News Rankings.pdf"

# Extract tables (rather rough, we will need to refine)
# raw1 <- extract_areas(file, pages=2:31, output = "data.frame")
# save(raw1,file = "raw1.Rdata")
load("raw1.Rdata")

# Repair tables so that they have consistent headers (and not data as headers)
fixTable <- function(thistab){
  newinstitution <- names(thistab)[1]
  newinstitution <- str_squish(gsub("\\."," ",newinstitution))
  newscore <- names(thistab)[2]
  newrow <- data.frame(institution=newinstitution, score=newscore)
  names(thistab) <- c("institution","score")
  thistab <- rbind(newrow,thistab)
  thistab[,2] <- suppressWarnings(as.numeric(thistab[,2]))
  return(thistab)
}
raw2 <- lapply(raw1, fixTable)

# Merge together into one long table
rankings1 <- bind_rows(raw2)

# Slice up into a list where each element is one school
endrows <- which(!is.na(rankings1[,2]) | grepl("^RNP",rankings1[,1]))
startrows <- c(1,head(endrows,-1)+1)
rankings2 <- lapply(1:length(startrows), function(x) rankings1[startrows[x]:endrows[x],])

# Try to get relevant info for each school
extractInfo <- function(thisblock){
  score <- as.numeric(na.omit(thisblock[,2]))
  score <- ifelse(length(score)==0,NA,score)
  goodrow <- !grepl("(^Save|#)",thisblock[,1]) & thisblock[,1] != "" 
  thisblock <- thisblock[goodrow,]
  institution <- thisblock[1,1]
  data.frame(institution=institution,score=score)
}
rankings <- bind_rows(lapply(rankings2,extractInfo))

# Add rankings
rankings$rank <- rank(5-rankings$score,na.last=TRUE,ties.method="min")
rankings$rank[is.na(rankings$score)] <- NA

# Clean up institution names
rankings$institution <- rankings$institution %>%
  str_replace_all("\\-\\-"," ")

# Write to file
write.csv(rankings,file="mathrankings.csv",row.names=FALSE)