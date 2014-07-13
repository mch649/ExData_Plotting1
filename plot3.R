
## plot3.R - script for assessment 1, Coursera MOOC: Exploratory Data Analysis, July 2014


rm(list=ls())

###################
## script variables
###################

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip" # ~20.1Mb
filename <- "household_power_consumption.zip"

## set this variable for your use; note: script was only tested on a Windows machine
#working_dir <- "c:/junk/r"  # give a workspace; THIS IS MY TEST CASE; THE FOLDER ALREADY EXISTS
working_dir <- "" 

dependencies <- c("tools","grDevices")

#######################
## end script variables
#######################


####################
## script functions
####################

exit_script <- function(){
    cat("Pressing ENTER will execute the script, 'Q' will quit.\n")
    response <- toupper(readline("Press ENTER to continue, Q to quit: "))
    if(response=="Q"){cat("Goodbye");stop()}#if    
}#exit_script()



load_dependencies <- function(packs){
    # load / install required packages
    # hacked together to support automatic execution. mch
    
    list.of.packages <- packs
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages) 
    
    lapply(list.of.packages,require,character.only=TRUE)
    
}#load_dependencies



download_data <- function(filename,url){
    # download
    if(!file.exists(filename)){
        cat(paste("\ndownloading file:",filename), " ... this will take a few minutes.\n")
        download.file(url,destfile=filename,method="auto")#curl,wget,lynx
    }else{
        cat(paste("\nfile:",filename," , already exists."))
    }#if
}#download_data()



unzip_data <- function(filename){
    # unzip
    if(!paste(basename(file_path_sans_ext(filename)),".txt",sep="") %in% dir()){
        unzip(filename)
    }else{
        cat(paste("\nunzipped:",filename," , already exists."))
    }#if
}#unzip_data()



prepare_data <- function(filename){
    cat("reading and preparing data will take a few moments... please be patient.\n")
    
    data <- read.csv2(
        paste(basename(file_path_sans_ext(filename)),".txt",sep=""),
        header=TRUE,
        sep=";",
        na.strings="?",
        stringsAsFactors=FALSE,
        skip=66636,
        nrow=2880,
        as.is=TRUE
    )#read.csv2()
    
    column_names <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
    names(data) <- column_names
    
    # combine Date and Time columns
    data$Date <- paste(data$Date,data$Time,sep=" ")
    data$Date <- strptime(data$Date,format="%d/%m/%Y %H:%M:%S")#OK
    data$Time <- NULL
    
    #not combinedassign types to date & time columns
    #data$Date <- strptime(data$Date,format="%d/%m/%Y")#OK
    #data$Time <- strftime(data$Date,format="%H:%M:%S")#OK
    
    #assign types to remaining columns
    data$Global_active_power <- as.numeric(data$Global_active_power)
    data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
    data$Voltage <- as.numeric(data$Voltage)
    data$Global_intensity <- as.numeric(data$Global_intensity)
    data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
    data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
    data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
    
    return(data)
    
}#prepare_data()



write_datafile <- function(s,datafilename){
    cat("Would you like a TAB, COMMA, or SPACE delimited output file?")
    response <- toupper(readline("Enter a 'T', 'C' or just press ENTER [T/C/ENTER]: "))
    
    if(response=='T'){
        write.table(s,file=paste(datafilename,".tab",sep=""),sep="\t",row.names=FALSE)
        extension <- "tab"
    }else if(response=='C'){
        write.table(s,file=paste(datafilename,".csv",sep=""),sep=",",row.names=FALSE)
        extension <- "csv"
    }else{
        write.table(s,file=paste(datafilename,".txt",sep=""),sep=" ",row.names=FALSE)
        extension <- "txt"
    }#if
    
    cat("\n\nData preparation completed. Prepared data file in working folder named: ",
        paste(datafilename,extension,sep='.'))
}#write_datafile()



get_working_folder <- function(){
    x <- readline("\nEnter a working folder in which to save output, i.e.(\"c:/junk/r\"): ")
    return(x)
}#get_working_folder



create_plot <- function(working_dir){
    
    ## 0) Introduction
    cat("\n\nplot3.R - \n\nIntroduction:\n 
        This script is used to create 'plot3.png' saved into the user-selected working 
        folder. The script assumes execution on a Windows machine. The script will require
        a folder to download data and create files. Edit the script variable, 'working_dir'
        near the top of the source code to set this, or enter a working folder when prompted
        by the script.\n
        The script will require a few minutes to run on first use to download the data 
        set from its internet host. If the required .zip data source file is present,
        the download will not be necessary and speedier execution will result.\n
        Upon completing the download, execution requires only a few moments to complete.\n
        There are a few prompts requiring a response - the script is not entirely 
        automated, so please stand by until the script runs to completion.\n
        The prompts are for whether or not to run this script, and for choosing a data
        file format for saving the source data, as '.tab', '.csv', or as '.txt'.\n
        
        \nDependencies:\n
        packages: tools, grDevices
        
        These packages may be installed, if not already available by detection. I have 
        hacked together a few lines of code to attempt a feature function. It seems to 
        work - although I have only tested it installing a few packages. The packages are 
        required to run the script.
        
        \nReferences:\n
            - http://stackoverflow.com/questions/7156770/in-r-how-can-read-lines-by-number-from-a-large-file
            - http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
            - http://stackoverflow.com/questions/14359546/converting-two-columns-of-date-and-time-data-to-one
            - http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r

        \nNotes:\n
        The script was developed on Windows 7 x64 using R 3.0.2 32-bit. I have also
        installed the appropriate Rtools version.
        
        Github site: https://github.com/mch649
        
        mch649
        July 2014
        
        Good luck.
        \n\n")
    
    
    ## 1) Execute or exit?
    exit_script()
    
    
    ## 2) Prompt for working folder if missing
    if(working_dir==""){
        cat("\n\nWorking folder selection.\nRemember this script was meant to run on Windows, so choose an appropriate folder name.")
        working_dir <- get_working_folder()
    }#if
    
    
    setwd(working_dir)
    
    
    ## 3) Load/install dependencies
    cat("\n\nChecking for required packages...\n")
    load_dependencies(dependencies)
    
    
    ## 4) Download and unzip dataset from internet source into workspace
    cat("\n\nStep 4. Download and unzip dataset to selected working folder.\n")
    download_data(filename,url)
    unzip_data(filename)
    
    
    ## 5) Read and subset data file to create one data set for all 4 scripts.
    cat("\n\nStep 5. Read and subset data file to create one data set for all 4 scripts.\n")
    data <- prepare_data(filename)
    write_datafile(data,"assessment_1")
    
    
    ## 6) Plot data
    cat("\n\n Step 6. Plotting data to file, 'plot3.png' in the working folder.\n")
    png(file="plot3.png",width=480,height=480,units="px",res=96)
    
    plot(data$Date,data$Sub_metering_1
         ,type="l"
         ,col="black"
         ,xlab=""
         ,ylab="Energy sub metering"
         ,ylim=c(0,40)
         ,cex.lab=0.75
    )  
    par(new = T)
    plot(data$Date,data$Sub_metering_2
         ,type="l"
         ,col="red"
         ,xlab=""
         ,ylab=""
         ,axes=F
         ,ylim=c(0,40)
    )
    par(new = T)
    plot(data$Date,data$Sub_metering_3
         ,type="l"
         ,col="blue"
         ,xlab=""
         ,ylab=""
         ,axes=F
         ,ylim=c(0,40)
    )
    legend("topright",lwd=1,seg.len=2,cex=0.75,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    dev.off()
    
    
    ## 7) Exit
    cat("\n\n Script complete. Look for the plot3.png in the working folder.\n")
    dir(working_dir)
    
}#create_plot()

#######################
## end script functions
#######################



############
## main
############

### choose a test case

# case 1.
if(working_dir==""){
    cat("Please edit the script and enter a working folder name in the variable 'working_dir'.")
}else{
    setwd(working_dir)
    create_plot(working_dir)
}#if

### OR

# case 2.
create_plot(working_dir)
