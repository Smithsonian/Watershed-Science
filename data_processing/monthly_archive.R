###Step one -- Conversion of .DAT files to .CSV and aggregation into monthly or yearly chunks. 
#This step is meant to be run as an automated pre-processing step before normalization.
#It takes a while to run this on a lot of data because of the aggregation function. 

library(tidyverse)
library(data.table)

#Load functions, directories, and unprocessed file names-------------------------------------------------------------

#relevant directories
rawData_dir <- Sys.getenv("weirs_unprocessed_archive") 
rawDataArchive_dir <- Sys.getenv("weirs_processed_archive")
rawCSVData_dir <- Sys.getenv("weirs_csv") 
archive_dat_as_csv <- Sys.getenv("weirs_dat_to_csv") 

#list files in the raw data folder to be converted. 
files <- list.files(rawData_dir, full.names = T)%>%
  str_subset(pattern = 'backup',negate = TRUE) #We are not processing backup tables

#Processing occurs one file at a time
for (file in files){
  
  #Load the .DAT file into R with this special function (from Ben Bond-Lamberty)--------------------------------------------------------------------------- 
  dt <- read_datalogger_file_weirs(file)
  #write reformatted .dat as .csv
  fwrite(dt,paste0(archive_dat_as_csv,basename(file),".csv")
 
  #Make sure timestamps are stored properly
  dt$TIMESTAMP <- ifelse(nchar(dt$TIMESTAMP) == 10, paste0(dt$TIMESTAMP, " 00:00:00"), dt$TIMESTAMP)
  dt$TIMESTAMP <- as.character(format(as.POSIXct(dt$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S"))
  
  tablename <- substr(basename(file),1,(nchar(basename(file))-19)) 
  
  
  #Aggregate the data into monthly chunks and write them out to the correct place---------------------------------------------------------------------------
  write_monthly_data_weirs(dt, rawCSVData_dir, tablename)
  
  
  #Move the Raw Loggernet data file into the archive----------------------------------------------------------------------------------
  file.rename(file, paste0(rawDataArchive_dir, basename(file)))
  
}

######################################
#######(from Ben Bond-Lamberty)#######
######################################

read_datalogger_file_weirs <- function(filename, quiet = FALSE, ...) {
  
  # Parse line one to extract logger and table names
  dat <- read_lines(filename)
  header_split <- strsplit(dat[1], ",")[[1]]
  header_split <- gsub("\"", "", header_split) # remove quotation marks
  format_name <- header_split[1] # first field of row 1
  logger_name <- header_split[2] # second field of row 1
  table_name <- header_split[length(header_split)]
  
  # We have no time zone information, so read the timestamp as character
  if(length(list(...))) {
    x <- read_csv(I(dat[-c(1, 3, 4)]), ...)
  } else {
    x <- read_csv(I(dat[-c(1, 3, 4)]), show_col_types = FALSE)%>%
      #This ensures that ALL variables interpreted as timestamps are read in as characters. 
      mutate(across(where(lubridate::is.POSIXt), as.character))
  }
  info <- tibble(Logger = rep(logger_name, nrow(x)),
                 Table = rep(table_name, nrow(x)),
                 Format = rep(format_name, nrow(x)))
  as_tibble(cbind(info, x))
  
}

############################################
###########(from Liz Westbrook)#############
############################################
write_monthly_data_weirs <- function(dt, rawCSVData_dir, filename) {
  
  dt$year_month <- substr(dt$TIMESTAMP,1,7)
  months <- unique(dt$year_month)
  
  for (m in months){
    #Filter the current dataset down to just the month in question
    dt_m <- dt %>%
      filter(year_month == m)%>%
      select(!year_month)#once this has been used for filtering, immediately get rid of it so it does not create an aggregation issue
    
    #define a name for this monthly file
    file_path <- paste0(rawCSVData_dir,filename,"/",filename,"_",m,".csv")
    
    #This is here just in case this is a brand new table. Makes a folder for this table in the rawCSV folder for neatness. 
    if (!dir.exists(paste0(rawCSVData_dir,filename,"/"))){
      dir.create(paste0(rawCSVData_dir,filename,"/"), recursive = TRUE)
    }
    
    #determine if any data for that month has already been processed. 
    if(!file.exists(file_path)){
      
      #if the file does not already exist, we do not need to run duplicate removal. 
      write.csv(dt_m, file_path, row.names = FALSE)
      
    }else{
      
      #if there is data already for this month, append the new data to that file and remove any duplicates 
      existing_data <- read.csv(file_path)
      
      #Combine the existing data and the new data -- could be more robust 
      combined_data <- rbindlist(list(dt_m, existing_data), use.names=F)
      
      #run this function which handles the possibility of duplicate timestamps 
      aggregated_combined_data <- aggregate_data_weirs(combined_data)
      
      aggregated_combined_data <- aggregated_combined_data %>%
        mutate(.TIMESTAMP_SORT = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
        arrange(.TIMESTAMP_SORT) %>%
        select(-.TIMESTAMP_SORT)
      
      write.csv(aggregated_combined_data, file_path, row.names = FALSE)
      
    }
  }
}


aggregate_data_weirs <- function(dt) {
  setDT(dt)
  
  # Save original column order. This is important for aggregating later with potential straggling data from the following month's files 
  original_order <- names(dt)
  
  # Identify numeric and character columns. these need to be aggregated separately because we want to take the mean of the numeric 
  #columns and the first of the two values for character columns 
  numeric_cols <- names(dt)[sapply(dt, is.numeric) & names(dt) != "rms"]
  char_cols <- setdiff(original_order, c(numeric_cols, "TIMESTAMP"))
  
  
  #This section will go through and actually aggregate the data. Handling character columns and numerical columns seperately. 
  aggregated_data <- dt[, {
    result <- vector("list", length(char_cols) + length(numeric_cols))
    names(result) <- c(char_cols, numeric_cols)
    for (j in seq_along(char_cols)) {
      result[[char_cols[j]]] <- .SD[[char_cols[j]]][1]
    }
    for (j in seq_along(numeric_cols)) {
      result[[numeric_cols[j]]] <- mean(.SD[[numeric_cols[j]]], na.rm = TRUE)
    }
    result
  }, by = TIMESTAMP, .SDcols = c(char_cols, numeric_cols)]
  
  # Reorder columns to match original using the order that we saved. 
  setcolorder(aggregated_data, intersect(original_order, names(aggregated_data)))
  
  return(aggregated_data)
  
}
