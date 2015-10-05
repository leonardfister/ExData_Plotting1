#file has two purposes:
#   0. set working directory: this has to be uncommented and adjusted if not set externally
#   1. read and subset the data that is to be plotted (only days 2007-02-01 & 2007-02-02)
#   2. plot the data

###########################################################################################
###########################################################################################
#0. set working directory
#working directory
#  -> set to be in the same directory as the file "household_power_consumption.txt"
#setwd("/Users/leo/github/ExData_Plotting1/")

###########################################################################################
###########################################################################################
#1. read and subset the data that is to be plotted
#
#file "household_power_consumption.txt"
#   taken from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#       (accessed 5 Okt 2015)
#   for codebook and details see https://class.coursera.org/exdata-033/human_grading/view/courses/975128/assessments/3/submissions
#       (accessed 5 Okt 2015)
#   or original data on http://archive.ics.uci.edu/ml/
#
#   file has
#      ordering in chronological order in the first column in date
#      a header
#      9 columns
#      2075259 rows
#      NA-strings that are given by "?"
#      the sep=";"
#      the first column containing date in format day/months/four-digit-year
#      the second column containing time in format 0-23-format-hour:minutes:seconds
file <- "household_power_consumption.txt"
if(!file.exists(file)){
    stop("file household_power_consumption.txt not found, 
         maybe working directory not set properly, or file not downloaded and unpacked?")
    }

#estimate file size of household_power_consumption.txt
#   note that the estimate is based on the assumpt. 
#   that all entries are numeric with 8 bytes/object
numberofrows <- 2075259
numberofcolumns <- 9
bytespernumeric <- 8 # bytes/numeric object
#estimated filesize in bytes
filesize <- numberofrows * numberofcolumns * bytespernumeric
# -"- in MB
filesizeMB <- filesize / 2^20
filesizeMB
# -> estimated file size around 143MB (actual size on system is 127MB)

dateformat <- "%d/%m/%Y" # format of 1st column in input file
date1 <- as.Date("2007-02-01") # begin of time-interval under consideration
date2 <- as.Date("2007-02-02") # end   - " -
fileclasses <- c("character","character",rep("numeric",7))
dat <- read.table(file, nrows=1, colClasses=fileclasses, header=TRUE, sep=";",na.strings = "?") # extract first line and header
dat <- dat[as.Date(dat$"Date",dateformat)==date1 | as.Date(dat$"Date",dateformat)==date2, ] # match to time interval



#read NChunks (~50) of size SizeChunk determined by 
#   NChunk*SizeChunk (slightly) larger than the number of lines in the file
#   imports stops automatically if end of file is reached
NChunk <- 50
SizeChunk <- ceiling(numberofrows/NChunk)
#50*SizeChunk gives 2075300

#for-loop:
#  1. if time-interval not exceeded yet (cf. chronological ordering)
#  2. read chunks in temporary table tmp
#  3. subset to dates in the desired range
#  4. append to table dat -> will contain all data in given range after loop
time_frame_exceeded <- FALSE
for(i in 0:(NChunk-1)){
    if( !time_frame_exceeded ){ #test if the interval of interest is still to or has come
        tmp <- read.table(file, 
                      skip =1+1+i*SizeChunk, # 1st/2nd skip is header/import above
                      nrows=SizeChunk, 
                      colClasses=fileclasses, 
                      header=FALSE, sep=";", na.strings = "?") 
        names(tmp) <- names(dat) #header from "to-be-final" frame
    
        end   <- as.Date(tail(tmp,n=1)[[1]],dateformat)# last date of the chunk (all other dates are later than this one)
        if(end<date1) next # TRUE: time interval [d1,d2] not reached yet
        if(end>date2) time_frame_exceeded <- TRUE # TRUE: [d1,d2] has passed -> rest is ignored
        
        tmp <- tmp[as.Date(tmp$"Date",dateformat)==date1 | as.Date(tmp$"Date",dateformat)==date2 , ]
        dat <- rbind(dat,tmp)
    }
}

#cleanup
rm(tmp,date1,date2,dateformat,NChunk,SizeChunk,time_frame_exceeded)
#put date & time in one column
dat <- cbind("Date"=strptime(paste(dat$"Date",dat$"Time"), "%d/%m/%Y %H:%M:%S"), dat[,3:9])

###########################################################################################
###########################################################################################
#2. plot the data
#   here: histogram for output file plot1.png
#   480x480 pixels is default option of png and does not need to be set
png("plot1.png")
hist(dat$"Global_active_power" ,xlab="Global Active Power (kilowatts)",main="Global Active Power",col="red")
dev.off()
