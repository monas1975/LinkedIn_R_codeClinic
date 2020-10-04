
#biblioteki
library(magrittr)
library(lubridate)

#wczytywanie danych
mytempfile <- tempfile()

#funkcja wczytujaca
readOneFile <- function(dataPath){
  read.table(dataPath,
             header = TRUE,
             stringsAsFactors = FALSE)
}

#progress bar

myProgressBar <- txtProgressBar(min = 2012,max = 2015,style = 3)

for (dataYear in 2012:2015) {
  
  dataPath <- paste0(
    "https://raw.githubusercontent.com/lyndadotcom/LPO_weatherdata/master/Environmental_Data_Deep_Moor_",
    dataYear,
    ".txt"
  )
  
  if (exists("LPO_weather_data")){
    mytempfile <-readOneFile(dataPath)
    LPO_weather_data <- rbind(LPO_weather_data,mytempfile)
  }else {
    LPO_weather_data <- readOneFile(dataPath)
  }
  setTxtProgressBar(myProgressBar,value = dataYear)
  
}

#sprawdz reslutaty importu
head(LPO_weather_data,n=3)
tail(LPO_weather_data,n=3)

print(paste("ilosc zaimportowanych wierszy", nrow(LPO_weather_data) ))



# Calculate the Coefficient of Barometric Pressure

startDateTime <-"2014-01-02 12:03:34"
endDateTime <- "2014-01-04 12:03:34"

getBaromPressures <- function(dateTimeInterval) {
  subset(
    LPO_weather_data,
    ymd_hms(paste(date, time)) %within% dateTimeInterval,
    select = c(Barometric_Press, date, time)
  )
}

calculateBaroPress <- function(startDateTime, endDateTime) {
  dateTimeInterval <- interval(ymd_hms(startDateTime),
                               ymd_hms(endDateTime))
  
  baroPress <- getBaromPressures(dateTimeInterval)
  
  slope <- ymd_hms(paste(baroPress$date, baroPress$time))
  
  lm(Barometric_Press ~ slope, data = baroPress)
  
}


calculateBaroPress(startDateTime, endDateTime)

graphBaroPressure <- function(startDateTime, endDateTime ) {
  
  dateTimeInterval <- interval(ymd_hms(startDateTime),
                               ymd_hms(endDateTime))
  
  baroPress <- getBaromPressures(dateTimeInterval)
  
  thisDateTime <- ymd_hms(paste(baroPress$date, baroPress$time))
  
  plot(
    x = thisDateTime,
    y = baroPress$Barometric_Press,
    xlab = "Date and Time",
    ylab = "Barometric Pressure",
    main = paste(
      "Barometric Pressure from ",
      ymd_hms(startDateTime),
      "to",
      ymd_hms(endDateTime)
    )
  )
  abline(calculateBaroPress(startDateTime, endDateTime), col = "red")
}

graphBaroPressure(startDateTime, endDateTime)

