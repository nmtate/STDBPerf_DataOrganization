library(tidyverse)
library(readxl)
library(tibble)
library(dplyr)

#Objective of this code: Read in the performance data, verify the columns are the same across 
#each sheet and merge. Then export 3 final datasets with all of the Standardbred data.

#This code is messy as it is not "plug-and-play". It will depend on the datasets that are provided
#and will need to be adjusted as necessary.


#Basic Horse Data
stdb_basic_df1 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_miss_basic_11202023.xlsx")
stdb_basic_df2 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_OC450_basic_10062023.xlsx")
stdb_basic_df3 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_2000_basic_10142023.xlsx")

#column names for the dfs
df1_basic_colnames <- colnames(stdb_basic_df1)
df2_basic_colnames <- colnames(stdb_basic_df2)
df3_basic_colnames <- colnames(stdb_basic_df3)


#check if they are the same
df1_basic_colnames == df2_basic_colnames
df1_basic_colnames == df3_basic_colnames
df2_basic_colnames == df3_basic_colnames


#add missing columns
stdb_basic_df2 <- stdb_basic_df2 %>% add_column("mare_disposition" = "NA", .after = "end_date")
stdb_basic_df2 <- stdb_basic_df2 %>% add_column("horse_breed" = "NA", .after = "horse_five")
stdb_basic_df3 <- stdb_basic_df3 %>% add_column("horse_breed" = "NA", .after = "horse_five")
stdb_basic_df3 <- stdb_basic_df3 %>% add_column("non_standard" = "NA", .after = "horse_breed")
stdb_basic_df3 <- stdb_basic_df3 %>% add_column("sire_of_dam_no" = "NA", .after = "sire_of_dam_name")


#verify column names match
#column names for the dfs
df1_basic_colnames <- colnames(stdb_basic_df1)
df2_basic_colnames <- colnames(stdb_basic_df2)
df3_basic_colnames <- colnames(stdb_basic_df3)

#check if they are the same
df1_basic_colnames == df2_basic_colnames
df1_basic_colnames == df3_basic_colnames
df2_basic_colnames == df3_basic_colnames


#merge the dataset
stdb_basic_final <- rbind(stdb_basic_df1, stdb_basic_df2, stdb_basic_df3)

#identify number of unique horses in dataset and pull their names and tattoos
stdb_basic_horsenames <- unique(stdb_basic_final$horse_name)
stdb_basic_horseno <- unique(stdb_basic_final$horse_no)
stdb_basic_horses <- data.frame(stdb_basic_horsenames, stdb_basic_horseno)


#evaluate duplicates
dups <- which(duplicated(stdb_basic_final$horse_no) == TRUE)
dup_horse_no <- stdb_basic_final$horse_no[15]
dup_lines <- which(stdb_basic_final$horse_no == dup_horse_no)
dup_lines <- stdb_basic_final[dup_lines,]


write.csv(stdb_basic_final, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_basic_ALL_12042023")
write.csv(stdb_basic_horses, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_basic_ALL_12042023_horses")



#Annual Data
stdb_annual_df1 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_200_annual_09052023.xlsx")
stdb_annual_df2 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_OC450_annual_10062023.xlsx")
stdb_annual_df3 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_2000_annual_10142023.xlsx")
stdb_annual_df4 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_miss_annual_11202023.xlsx")

#column names for the dfs
df1_annual_colnames <- colnames(stdb_annual_df1)
df2_annual_colnames <- colnames(stdb_annual_df2)
df3_annual_colnames <- colnames(stdb_annual_df3)
df4_annual_colnames <- colnames(stdb_annual_df4)

#check if they are the same
df1_annual_colnames == df2_annual_colnames
df1_annual_colnames == df3_annual_colnames
df1_annual_colnames == df4_annual_colnames
df2_annual_colnames == df3_annual_colnames
df2_annual_colnames == df4_annual_colnames
df3_annual_colnames == df4_annual_colnames

#update column names to match
colnames(stdb_annual_df1)[15] <- "rec_type"
colnames(stdb_annual_df2)[7] <- "race_year"
colnames(stdb_annual_df2)[14] <- "annual_earnings"
colnames(stdb_annual_df3)[7] <- "race_year"
colnames(stdb_annual_df3)[14] <- "annual_earnings"
colnames(stdb_annual_df4)[15] <- "rec_type"


#merge the dataset
stdb_annual_final <- rbind(stdb_annual_df1, stdb_annual_df2, stdb_annual_df3, stdb_annual_df4)

#identify number of unique horses in dataset and pull their names and tattoos
stdb_annual_horsenames <- unique(stdb_annual_final$horse_name)
stdb_annual_horseno <- unique(stdb_annual_final$horse_no)
stdb_annual_horses <- data.frame(stdb_annual_horsenames, stdb_annual_horseno)

write.csv(stdb_annual_final, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_annual_ALL_12042023")
write.csv(stdb_annual_horses, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_annual_ALL_12042023_horses")




#Raceline Data
stdb_raceline_df1 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_200_raceline_09052023.xlsx")
stdb_raceline_df2 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_OC450_raceline_10062023.xlsx")
stdb_raceline_df3 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_2000_raceline_10142023.xlsx")
stdb_raceline_df4 <- read_excel("/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/USTA_raw/stdbperf_miss_raceline_11202023.xlsx")


#column names for the dfs
df1_raceline_colnames <- colnames(stdb_raceline_df1)
df2_raceline_colnames <- colnames(stdb_raceline_df2)
df3_raceline_colnames <- colnames(stdb_raceline_df3)
df4_raceline_colnames <- colnames(stdb_raceline_df4)

#check if they are the same
df1_raceline_colnames == df2_raceline_colnames
df1_raceline_colnames == df3_raceline_colnames
df1_raceline_colnames == df4_raceline_colnames
df2_raceline_colnames == df3_raceline_colnames
df2_raceline_colnames == df4_raceline_colnames
df3_raceline_colnames == df4_raceline_colnames

#merge the dataset
stdb_raceline_final <- rbind(stdb_raceline_df1, stdb_raceline_df2, stdb_raceline_df3, stdb_raceline_df4)

#identify number of unique horses in dataset and pull their names and tattoos
stdb_raceline_horsenames <- unique(stdb_raceline_final$horse_name)
stdb_raceline_horseno <- unique(stdb_raceline_final$horse_no)
stdb_raceline_horses <- data.frame(stdb_raceline_horsenames, stdb_raceline_horseno)

write.csv(stdb_raceline_final, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_raceline_ALL_12042023")
write.csv(stdb_raceline_horses, "/Users/nmtate/Library/CloudStorage/Dropbox/1EquineProjects/STDB_Projects/Data/PerformanceData/merged/STDBperf_raceline_ALL_12042023_horses")


