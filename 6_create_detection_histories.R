
##CREATING DETECTION HISTORIES FROM THE CAMERA TRAP RECORDS ALREADY IN A FILE 

#(e.g. manually extracted, from digikam or snapshot)

## this is using camtrapR, similarly to the 4_RAI_from reports scripts, so we need two spreadsheets: 
# 1) the sampling effort with sites covars 2) the cameratrap records (spp detections)

## OBJ: 1) extract the detection histories for all species in all sites,
##      2) stack together species for multispecies occupancy format

#rm(list = ls())
#functional script

#Lain E. Pardo


library(camtrapR)
library(tidyverse)
library(readr)


# basically the matrix of the design (cameras and days..), in this part we tell R if station has more than one cam 


---------------------------
# 1. camera operation matrix ----------------------------------------
----------------------------

#load sp records
  
sp_rec <- read_csv("data_in/+sp_records_only_mammals_updated.csv")
head(sp_rec)

# put correct format
#change the date format if needed, we need yyyy-mm-dd 

sp_rec$Photo.Date <- lubridate::parse_date_time(x = sp_rec$Photo.Date,
                                                       order = c("dmY", "Ymd","dmy"))

# create camera operation table which contains the station names, days, location etc.

survey_lenght <- plyr::ddply(sp_rec, c("Reserve.Location", "Camera.Site"), # 
                          summarise,  
                          First.Photo.Date = min(as.Date(Photo.Date)),
                          Last.Photo.Date = max(as.Date(Photo.Date)),
                          Cam.Days = Last.Photo.Date-First.Photo.Date)
survey_lenght

cameras <- survey_lenght
str(cameras)
-----------------------------------------
# we can also use the file created in 2_species exploration

#cameras <- read.csv("data_out/survey_lenght.csv", header = T, sep = ",", stringsAsFactors = FALSE)
-----------------------------------------

  
# #put correct format for dates
cameras$First.Photo.Date <- as.POSIXct(cameras$First.Photo.Date) #if file is in other fomat include e.g., format="%d/%m/%Y")
cameras$Last.Photo.Date  <- as.POSIXct(cameras$Last.Photo.Date)#
cameras$Camera.Site <- as.factor(cameras$Camera.Site)

# create camera operation

cam_op <- cameraOperation(CTtable = cameras,    
                          setupCol = "First.Photo.Date",
                          retrievalCol = "Last.Photo.Date",
                          stationCol = "Camera.Site",
                          writecsv = T,
                          hasProblems  = FALSE,
                          outDir = "data_out")

str(cam_op) # 

# name of stations
unique(cameras$Camera.Site)  #
class(cameras$Camera.Site)

####  3. let´s see the operation time (effort) of all cams 


par(mfrow= c (1,1))

camopPlot <- function(camOp){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = c("red", "grey70"))
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}

camopPlot(camOp = cam_op)  #need to be tidier, see other options in 2_species_exploration

# if one wants to read this use: cam_operation_csv <- read.csv(...CameraOperationMatrix_by_station_with_problems_2019-03-01.csv",
#row.names = 1, check.names = F


---------------------------
#  2. built detection history --------------------
----------------------------

                         
names(sp_rec)
#remember column DateTimeOriginal must be included with proper format

# Add DateTimeOriginal if not in original species records (I did this in other scripts)

sp_rec <- sp_rec %>% mutate(DateTimeOriginal = paste(Photo.Date, Photo.Time))


sp_rec$DateTimeOriginal <- as.POSIXlt(sp_rec$DateTimeOriginal) # must be format="%Y-%m-%d:%H:%M:%S"
                                                                        

unique(sp_rec$Common.Name)  #


# DETECTION HISTORY -------------------------------------------------------

#one by one: e.g. just lion for now

DH_lion_10 <- detectionHistory (recordTable = sp_rec, 
                                            camOp = cam_op,                      #
                                            stationCol = "Camera.Site", 
                                            speciesCol = "Common.Name",
                                            recordDateTimeCol = "DateTimeOriginal",
                                            species = "lion", 
                                            occasionLength = 10,         # 
                                            day1 = "station",  #CHECK IF ITS BETTER "survey"
                                            datesAsOccasionNames = F, 
                                            includeEffort = F, #careful if trapping effort is thought to influence detection probability, it can be returned by setting includeEffort = TRUE. 
                                            scaleEffort = F)            #maybe wise using T, explore later 
#recordDateTimeFormat  = "%Y-%m-%d %H:%M:%S"
#check warnings in case useful, depends on objectives

str(DH_lion_10)  ## 

DH_lion_10$detection_history  #

#write.csv(DH_lion_10 , "Data_out/det_hist_all/DH_lion_10.csv")


------------------------
# 3. DH for all other spp simultaneously ----------
------------------------

##Split recordTable by species 
  
records.split <- split(sp_rec, sp_rec$Common.Name)


##Build a function for applying to all species

det.hist.apply <- function(x){
  detectionHistory(recordTable = x,
                   species = unique(x$Common.Name),
                   camOp                 = cam_op, ##Camera operation matrix
                   stationCol            = "Camera.Site",
                   speciesCol            = "Common.Name",
                   output                = "binary",
                   recordDateTimeCol     = "DateTimeOriginal",
                   recordDateTimeFormat  = "%Y-%m-%d %H:%M:%S",
                   occasionLength        = 10,
                   day1                  = "station",
                   datesAsOccasionNames = TRUE,
                   includeEffort         = FALSE,
                   scaleEffort          = FALSE)
}



##Apply the detection history function above to all individual dataframes that are part of the list of species
community.det.hist <- lapply(records.split, det.hist.apply)

## extract species of interest manually

lion <-as.data.frame(community.det.hist$lion)
lion

#write.csv(lion, "Data_out/Detection_hist_all/lion_from_loop.csv") #same as previous!!


# Get ALL species at once in separate files------------------------------------

path = "data_out/separate_DH"


# Create path if it doesn't exist already --

if(!exists(path)){
  dir.create(path)
}

for(i in 1:length(community.det.hist)){
  
  # Add "species" column ---
  # Extract detection history in a variable (not modifying the original list)
  df <- community.det.hist[[i]]$detection_history
  df <- as.data.frame(df) # convert matrix to dataframe
  
  
  # Change column names ---
  
  suffix <-  1:length(colnames(df)) # create suffix of length of the columns
  new_names <- paste0("occ_", suffix) # create new names by pasting custom root "occ_" with suffix
  colnames(df) <- new_names
  
  # Camera name from rowname to new column "camera" (will be easier to stack files together after)
  df <- df %>% rownames_to_column("camera")
  
  # Get species' name
  name <- names(community.det.hist)[i] 
  
  # Print species
  message(paste("Writing file for species", name))
  
  df$species <- rep(name, nrow(df)) # Add "species" column to the dataframe
  
  # Create a name without spaces (for file name)
  name_no_spaces <- gsub(pattern = " ", replacement = "_", name) 
  
  # Save file in "path"
  write.csv(df, file = paste0(path, "/", name_no_spaces, '.csv'), 
            row.names = FALSE) # write file
  
  # write.csv(data.frame(community.detection.history[[i]]), 
  # file = paste0(path, names(community.detection.history)[i], '.csv'))
} 

# stack all species DH in a single csv----------------------------------------

getwd()


#set the working directory from which the files will be read from
# change according to your system

setwd("~/GitHub/Snapshot_Safari/data_out/separate_DH")

#create a list of the files from your target directory
file_list <- list.files(path= "~/GitHub/Snapshot_Safari/data_out/separate_DH")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i])#, range = cell_cols("A:P")) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$Class <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[2]}) #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
}

str(dataset)

# put dataset in the PROPER order for multi species occupancy analysis following Kery´s book------

# 1) add sp_id to sp_recs ---
dataset1 <- dataset %>% 
        mutate(sp_id =  as.integer(as.factor(species))) %>% 
        select(-Class)

#check if you might also need to  delete  rowcol 

# 2) reorder df
dataset2 <- select(dataset1, camera, sp_id, species, everything())
names(dataset2)
write.csv(dataset2, "DH_all_species.csv", row.names = FALSE)


# end ---------------------------------------------------------------------














