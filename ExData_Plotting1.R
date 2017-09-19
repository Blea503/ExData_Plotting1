if(!file.exists("exdata-data-household_power_consumption.zip")){
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
        file <- unzip(temp)
        unlink(temp)
}
power <- read.table(text = grep("^[1,2]/2/2007", readLines(file), value=TRUE), header=TRUE, 
                    sep=";", na.strings = "?", col.names = c("Date", "Time", "Global_active_power", 
                                                             "Global_reactive_power", "Voltage", "Global_intensity", 
                                                             "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
datetime <- paste(as.Date(power$Date, format = "%d/%m/%Y"), power$Time)
power$Datetime <- as.POSIXct(datetime)

plot1 <- hist(power$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.copy(png, file = "plot1.png", height=480, width=480)
dev.off()
plot2 <- with(power, plot(Global_active_power ~ Datetime, type = "l", ylab="Global Active Power (kilowatts)", xlab = ""))
dev.copy(png, file = "plot2.png",height=480, width=480)    
dev.off()
plot3 <- with(power, {
        plot(Sub_metering_1 ~ Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
        lines(Sub_metering_2~Datetime, col = 'Red')
        lines(Sub_metering_3 ~ Datetime, col = 'Blue')
        legend("topright", inset=c(0.1,0),bty="n",col = c("black", "red", "blue"), lty = 1, lwd = 2, 
        legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))})       
dev.copy(png, file = "plot3.png",height=480, width=480)
dev.off()
par(mfrow = c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(power, plot(Global_active_power ~ Datetime, type = "l", ylab="Global Active Power (kilowatts)", xlab = ""))
with(power, plot(Voltage~Datetime, type="l", ylab="Voltage",xlab="datetime"))
with(power, {
        plot(Sub_metering_1 ~ Datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
        lines(Sub_metering_2~Datetime, col = 'Red')
        lines(Sub_metering_3 ~ Datetime, col = 'Blue')
        legend("topright", inset=c(0.1,0), bty="n",col = c("black", "red", "blue"), lty = 1, lwd = 2, 
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))})  
with(power, plot(Global_reactive_power ~ Datetime, type = "l", ylab = "Global_rective_power", xlab = "datetime"))
dev.copy(png, file = "plot4.png",height=480, width=480)     
dev.off()