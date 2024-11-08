#load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)

#set working directory
setwd("~/Documents/Uni/Masterarbeit/Data_Analysis_Endalen")


dat_dir209 <- list.files("/Users/ms/Documents/Uni/Masterarbeit/Data_Analysis_Endalen/data/Weather/Adventdalen 1993-2009", full.names = TRUE)
datadir <- dat_dir209[1:9] #lists all files/folders in the folder Adventdalen 1993-2009


# Define a function to read either CSV or XLS files
readxls_csv_folderstr <- function(file) {
  if (grepl("\\.csv", file, ignore.case = TRUE)) {
    read.csv(file, header = F)
  } else if (grepl("\\.xls", file, ignore.case = TRUE) | grepl("\\.xlsx", file, ignore.case = TRUE)) {
    readxl::read_xls(file)
  } else {
    NULL
  }
}


datlist209 <- list() #creates empty list for the files
datlist_out <- list()
for (i in 1:length(datadir)){ #runs for each folder/file
  datlist209[[i]] <- list.files(datadir[i], full.names = TRUE)  #lists all files/folders in the subfolder i
  if (any(grepl("\\.csv|\\.xls", datlist209[[i]]))) { #checks if the subfolder contains a csv or xls file
    # If datlist209 contains at least one .csv or .xls file, read them
    datlist_out[[i]] <- sapply(datlist209[[i]], readxls_csv_folderstr,simplify = FALSE)
  } else {
    # Use the file names as directories and read the .csv files from that directory
    test <- list()
    filesv <- list()
   # browser()
    for (j in 1:length(datlist209[[i]])) { 
      newdir <- datlist209[[i]][j]
      filesv <- list.files(newdir,pattern = ("*.csv|\\.xlsx|\\.xls"), full.names = TRUE)
      # Check if there are any files in the directory
      print(paste(i,filesv))
      if(length(filesv)>2){browser()}
      if (any(grepl("\\.csv|\\.xls", filesv))){
        test[[j]] <- lapply(filesv, readxls_csv_folderstr)
      }
    datlist_out[[i]] <- unlist(test, recursive = FALSE)
    #names(datlist_out) <- datadir
  }
}
}
datlist_out[[9]] #thats one list with xls files

# Print the structure of the resulting list
str(datlist_out)


####make the list nice####
# Flatten the list if needed
flat_datlist <- unlist(datlist209, recursive = FALSE)
names(flat_datlist)
str(flat_datlist)
head(flat_datlist)
