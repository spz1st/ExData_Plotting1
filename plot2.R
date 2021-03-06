## Exploratory Data Analysis course project 1
## plot time series of global active power consumption
## for the two day period of 2007-02-01 and 2007-02-02

## please note that I also program in many other languages
## in which the assignment operator is =,
## so I am used to use = in stead of <- for assignments

# encapsulate the activities in a function
plot2 = function(){
  file = "household_power_consumption.txt" # data file

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
    else if(num > 0){
      # if num is not 0 any more, that means the two days data have been read
      # because the data are ordered by time,
      # we can quit once 2007-02-01 and 2007-02-02 have passed
      break
    }
  }
  close(con)

  # initiate the data frame with needed columns
  df = data.frame("", "", 1:num, stringsAsFactors=F)
  for(i in 1:length(lines)){ # build the data frame
    vals = unlist(strsplit(lines[i], ";"))
    df[i,1:2] = vals[1:2]
    df[i,3] = as.numeric(vals[3])
  }

  names(df) = unlist(strsplit(header, ";"))[1:3] # use original column names

  # data already ordered by data/time in the input file
  # otherwise sort the data by data/time ascending
  # df$date <- as.POSIXct(paste(df$Date,df$Time, sep=" "), format="%d/%m/%Y %H:%M:%S", tz="EST")
  df$Date <- strptime(paste(df$Date,df$Time, sep=" "), format="%d/%m/%Y %H:%M:%S")

  png("plot2.png") # default size is 480 x 480
  plot(df$Date, df$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.off()

  # df   # if want to return the data
}

plot2()

