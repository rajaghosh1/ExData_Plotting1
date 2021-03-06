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

plot1 <- function(
    indir= ".", #input dir
    infile = "household_power_consumption.txt", #input file 
    outdir = ".", #output dir
    outfile= "plot1.png", #output file
    start = "2007-02-01",
    end = "2007-02-02") {
  
  filteredData <- extractData(indir = indir,
                              infile = infile,
                              start = start,
                              end = end)

  cat("generating plot...\n" )
  hist(filteredData$Global_active_power, 
       main="Global Active Power",
       xlab="Global Active Power (kilowatts)",
       col="red")

  fileName  <- paste(outdir, outfile, sep = "/")
  cat("outputting plot to png file ", fileName, "...\n" )
  dev.copy(png, file = fileName, 
           width = 480, height = 480, 
           units = "px")
  dev.off()
}