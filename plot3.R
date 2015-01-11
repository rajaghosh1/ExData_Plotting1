extractData <- function(
  indir= ".", #input dir
  infile = "household_power_consumption.txt", #input file 
  start = "2007-02-01",
  end = "2007-02-02") {
  
  fileName  <- paste(indir, infile, sep = "/")
  cat("reading data from ", fileName, "...\n" )
  data <- read.table(fileName, sep=";", header=TRUE, na.strings="?")
  
  cat("converting dates...\n" )
  data$ConvertedDate <- as.Date(data$Date, format = "%d/%m/%Y")
  startdate <- as.Date(start)
  enddate <- as.Date(end)
  cat("filtering rows between ", start, " and ", end, "...\n" )
  filteredData <- data[
    (data$ConvertedDate >= startdate) & 
      (data$ConvertedDate <= enddate) & 
      !is.na(data$Global_active_power),]
  
  cat("generating datetime field...\n" )  
  filteredData$DateTime <- strptime(paste(filteredData$Date, filteredData$Time), format="%d/%m/%Y %H:%M:%S")
  filteredData
}

plot3 <- function(
    indir= ".", #input dir
    infile = "household_power_consumption.txt", #input file 
    outdir = ".", #output dir
    outfile= "plot3.png", #output file
    start = "2007-02-01",
    end = "2007-02-02") {
  
  filteredData <- extractData(indir = indir,
                              infile = infile,
                              start = start,
                              end = end)
  
  cat("generating plot...\n" )
  with(filteredData, 
       plot(DateTime, Sub_metering_1, type="l", col = "black", xlab="", ylab="Energy sub metering"))
  with(filteredData, lines(DateTime, Sub_metering_2, type="l", col = "red"))
  with(filteredData, lines(DateTime, Sub_metering_3, type="l", col = "blue"))
  legend("topright", 
         lty = 1,
         #bty = 'o',
         col=c("black", "red", "blue"), 
         legend= c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  fileName  <- paste(outdir, outfile, sep = "/")
  cat("outputting plot to png file ", fileName, "...\n" )
  dev.copy(png, file = fileName, 
           width = 510, 
           #height = 480, 
           units = "px")
  dev.off()
}