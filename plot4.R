## Exploratory Data Analysis course project 1
## plot power consumptions
## for the two day period of 2007-02-01 and 2007-02-02

## please note that I also program in many other languages
## in which the assignment operator is =,
## so I am used to use = in stead of <- for assignments

# encapsulate the activities in a function
plot4 = function() {
  # file = "household_power_consumption.txt" # data file
  file = "test.txt"

  con = file(file)
  open(con)

  # read the header line
  header = readLines(con, n = 1, ok = T)
  # Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3

  num = 0;
  lines = character(0)

  repeat {  # read in lines one by one

    line = readLines(con, n = 1, ok = T)

    if(length(line) < 1) break  # no more lines to read

    # collect data for Feb. 1 and Feb. 2 of 2007
    ok = grep("^0?[12]/0?2/2007", line, perl=T) # date in the format of d/m/yyyy
    if(length(ok)){
      num = num + 1
      lines[num] = line  # much faster to collect lines first
    }
  }
  close(con)

  # initiate the data frame with proper number of rows and columns
  data = data.frame("", "", 1:num, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, stringsAsFactors=F)
  for(i in 1:length(lines)){ # build the data frame
    vals = unlist(strsplit(lines[i], ";"))
    data[i,1:2] = vals[1:2]
    data[i,3:9] = as.numeric(vals[3:9])
  }

  names(data) = unlist(strsplit(header, ";")) # use original column names

  # data already ordered by data/time in the input file
  # otherwise sort the data by data/time ascending
  # data = data[order(data[1],data[2]),]

  png("plot4.png") # default size is 480 x 480

  par(mfrow=c(2,2))  # plot 4 graphes; only affect the current device
  plot(data$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)", xaxt="n")
  axis(1, at=c(1, num/2, num), labels=c("Thur", "Fri", "Sat"))

  plot(data$Voltage, type="l", xlab="datetime", ylab="Voltage", xaxt="n")
  axis(1, at=c(1, num/2, num), labels=c("Thur", "Fri", "Sat"))

  plot(data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering", xaxt="n")
  lines(data$Sub_metering_2, col="red")
  lines(data$Sub_metering_3, col="blue")
  axis(1, at=c(1, num/2, num), labels=c("Thur", "Fri", "Sat"))
  legend(x="topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red","blue"), lty=c(1,1,1), bty="n")

  plot(data$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power", xaxt="n")
  axis(1, at=c(1, num/2, num), labels=c("Thur", "Fri", "Sat"))

  dev.off()

  # data   # if want to return the data
}

plot4()
