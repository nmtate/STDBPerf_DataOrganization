library(dplyr)

#Objective of this code: To compare the data received from the USTA to that which was requested to verify
#that we have all the data.

#This code is messy as it is not "plug-and-play". It will depend on the datasets that are provided
#and will need to be adjusted as necessary.


#Read in merged datasets
stdb_annual <- read.csv("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_annual_ALL_12042023")
stdb_basic <- read.csv("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_basic_ALL_12042023")
stdb_race <- read.csv("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_raceline_ALL_12042023")

#basic horse names and tattoos
stdb_basic_horsenames <- unique(stdb_basic$horse_name)
stdb_basic_horseno <- unique(stdb_basic$horse_no)
stdb_basic_horseno <- gsub('.{1}$', "", stdb_basic_horseno) #removes the extra number from the tattoo, makes them comparable

#annual horse names and tattoos
stdb_annual_horsenames <- unique(stdb_annual$horse_name)
stdb_annual_horseno <- unique(stdb_annual$horse_no)
stdb_annual_horseno <- gsub('.{1}$', "", stdb_annual_horseno) #removes the extra number from the tattoo, makes them comparable

#raceline horse names and tattoos
stdb_race_horsenames <- unique(stdb_race$horse_name)
stdb_race_horseno <- unique(stdb_race$horse_no)
stdb_race_horseno <- gsub('.{1}$', "", stdb_race_horseno) #removes the extra number from the tattoo, makes them comparable




#Read in the requested data
stdb_requested <- read.csv("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/USTA_raceline_request.csv")
stdb_requested <- stdb_requested %>% distinct(Registered.Name, Registration..)


#remove duplicate horses
stdb_request_horsenames <- toupper(unique(stdb_requested$Registered.Name)) #in order to look for names, they need to all be the same case
stdb_request_horseno <- unique(stdb_requested$Registration..)


#add empty columns that will be used to note if horse is included in each dataset
annual <- rep(NA, length(stdb_requested$Registered.Name))
basic <- rep(NA, length(stdb_requested$Registered.Name))
race <- rep(NA, length(stdb_requested$Registered.Name))
stdb_requested <- cbind(stdb_requested, annual, basic, race)


#identify which horse names are in annual
for (x in stdb_requested$Registered.Name) {
  if (is.element(x, stdb_annual_horsenames)) {
    row_pos <- which(stdb_requested$Registered.Name == x)
    stdb_requested$annual[row_pos] <- "YES"
  }
}

#identify which horse tattoos are in annual
for (x in stdb_requested$Registration..) {
  if (is.element(x, stdb_annual_horseno)) {
    row_pos <- which(stdb_requested$Registration.. == x)
    stdb_requested$annual[row_pos] <- "YES"
  }
}

#identify which horse names are in basic
for (x in stdb_requested$Registered.Name) {
  if (is.element(x, stdb_basic_horsenames)) {
    row_pos <- which(stdb_requested$Registered.Name == x)
    stdb_requested$basic[row_pos] <- "YES"
  }
}

#identify which horse tattoos are in basic
for (x in stdb_requested$Registration..) {
  if (is.element(x, stdb_basic_horseno)) {
    row_pos <- which(stdb_requested$Registration.. == x)
    stdb_requested$basic[row_pos] <- "YES"
  }
}

#identify which horse names are in race
for (x in stdb_requested$Registered.Name) {
  if (is.element(x, stdb_race_horsenames)) {
    row_pos <- which(stdb_requested$Registered.Name == x)
    stdb_requested$race[row_pos] <- "YES"
  }
}

#identify which horse tattoos are in race
for (x in stdb_requested$Registration..) {
  if (is.element(x, stdb_race_horseno)) {
    row_pos <- which(stdb_requested$Registration.. == x)
    stdb_requested$race[row_pos] <- "YES"
  }
}

#count yes in each dataset
table(stdb_requested$annual)
table(stdb_requested$basic)
table(stdb_requested$race)

write.csv(stdb_requested, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/StatusOfDataFromUSTA_12042023.csv")


#code to remove horses with missing data and who have never raced and export final datasets

#read in file with horses that have never raced
no_racelines <- read.csv("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/data/STDBs_no_racelines.csv")

#tattoos need to be shortened to 5 values in order to be compared
stdb_annual$horse_no <- substr(stdb_annual$horse_no, 1, nchar(stdb_annual$horse_no)-1)
stdb_basic$horse_no <- substr(stdb_basic$horse_no, 1, nchar(stdb_basic$horse_no)-1)
stdb_race$horse_no <- substr(stdb_race$horse_no, 1, nchar(stdb_race$horse_no)-1)

#compare no racelines to missing data
stdb_annual_missing <- which(is.na(stdb_requested$annual))
stdb_annual_missing <- stdb_requested[stdb_annual_missing, c(1,2)]
stdb_annual_missing <- stdb_annual_missing[!(stdb_annual_missing[,2] %in% no_racelines[,2]),]

stdb_basic_missing <- which(is.na(stdb_requested$basic))
stdb_basic_missing <- stdb_requested[stdb_basic_missing, c(1,2)]
stdb_basic_missing <- stdb_basic_missing[!(stdb_basic_missing[,2] %in% no_racelines[,2]),]

stdb_raceline_missing <- which(is.na(stdb_requested$race))
stdb_raceline_missing <- stdb_requested[stdb_raceline_missing, c(1,2)]
stdb_raceline_missing <- stdb_raceline_missing[!(stdb_raceline_missing[,2] %in% no_racelines[,2]),]

#the only file that needs the horses who have never race is the basic file
stdb_basic_racedonly <- stdb_basic[!(stdb_basic$horse_no %in% no_racelines$Registration..),]


write.csv(stdb_basic_missing, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/data/STDBPerf_Basic_missingdata_12052023.csv")
write.csv(stdb_basic_racedonly, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/data/STDBPerf_Basic_onlyraced_12052023.csv")
write.csv(stdb_annual, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/data/STDBPerf_Annual_onlyraced_12052023.csv")
write.csv(stdb_race, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/data/STDBPerf_Raceline_onlyraced_12052023.csv")
