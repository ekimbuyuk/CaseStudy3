data = read.csv("NC_Infant_Death_Merged.csv")

#Convert first letter to Capital
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

data$COUNTY = stri_trans_totitle(data$COUNTY)


#Categorize Race
data$race[data$race == 1] = "White"
data$race[data$race == 2] = "Black or African American"
data$race[data$race == 3] = "American Indian or Alaska Native"
data$race[data$race == 4] = "Other"

write.csv(data,'deathdata.csv')
