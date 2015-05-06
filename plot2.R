plot2 <- function(print2file=FALSE) {

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

        # Plot Global Active Power

    if (print2file)
        png(file = "plot2.png")

    plot(DF$DateTime, DF$Global_active_power,
         type="l",
         xlab="",
         ylab="Global Active Power (kilowatts)")

    if (print2file)
        dev.off()
}
