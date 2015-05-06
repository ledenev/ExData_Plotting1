plot4 <- function(print2file=FALSE) {

    url <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

        # Read original .zip file from Internet

    zipfile <- tempfile()
    download.file(url=url, destfile=zipfile, quiet=TRUE)
    src <- "household_power_consumption.txt"

        # Define 'myDate' to convert date to 'as.Date' format
        # while reading table from source file

    setClass("myDate")
    setAs("character", "myDate",
          function(from) as.Date(from, format="%d/%m/%Y"))

    classes=c("myDate", "character", "numeric",
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric")

    table_all <- read.table(unz(zipfile, src),
                            header = TRUE,
                            sep=";",
                            na.strings = "?",
                            colClasses=classes)
    unlink(zipfile)
    rm(zipfile)

        # Filter dates from 2007-02-01 to 2007-02-02

    dates <- c(as.Date("2007-02-01"),
               as.Date("2007-02-02"))
    DF <- with(table_all, table_all[Date %in% dates, ])
    rm(table_all)

        # Add new column DateTime to data frame DF

    DF$DateTime <- strptime(paste(DF$Date, DF$Time), "%Y-%m-%d %H:%M")

        # Multi-plot (2x2)

    if (print2file)
        png(file="plot4.png")

    par(mfrow=c(2,2))

    plot(DF$DateTime, DF$Global_active_power,
         type="l",
         xlab="",
         ylab="Global Active Power")

    plot(DF$DateTime, DF$Voltage,
         type="l",
         xlab="datetime",
         ylab="Voltage")

    plot(DF$DateTime, DF$Sub_metering_1, type="l",
         xlab="",
         ylab="Energy sub metering")
    lines(DF$DateTime, DF$Sub_metering_2, col="red")
    lines(DF$DateTime, DF$Sub_metering_3, col="blue")

    legend("topright", bty = "n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lwd=c(1,1,1), col=c("black", "red", "blue"))

    plot(DF$DateTime, DF$Global_reactive_power,
         type="l",
         xlab="datetime",
         ylab="Global_reactive_power")

    if (print2file)
        dev.off()
}
