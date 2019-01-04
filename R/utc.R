#'
#'@title toUTC
#'
#'@description Returns UTC time from local time
#'
#'@param localtime POSIXct local time
#'
#'@param timeformat string format of local time, by default it will be "\%Y-\%m-\%d \%H:\%M:\%S"
#'
#'@return utctime POSIXct UTC time
#'
#'@example toUTC(Sys.time())
#'
toUTC <- function(localtime, timeformat = "%Y-%m-%d %H:%M:%S"){
  utctime <- as.POSIXct( localtime, format = timeformat)
  attr(utctime,"tzone") <- "UTC"
  utctime
}

#'
#'@title fromUTC
#'
#'@description Returns local time from UTC time
#'
#'@param utctime POSIXct UTC time
#'
#'@param timeformat string format of local time, by default it will be "\%Y-\%m-\%d \%H:\%M:\%S"
#'
#'@return localtime POSIXct local time
#'
#'@examples  fromUTC( as.POSIXct("1991-06-29 05:27:25", tz = "UTC") )
#'
fromUTC <- function(utctime, timeformat = "%Y-%m-%d %H:%M:%S"){
  localtime <- as.POSIXct( utctime, format = timeformat)
  attr(localtime,"tzone") <- Sys.timezone()
  localtime
}

#'
#'@title hoursUTC
#'
#'@description This function returns time difference between UTC and Local
#'
#'@param to_test_time only for testing UTC Sys.time()
#'
#'@param timeformat string format of local time, by default it will be "\%Y-\%m-\%d \%H:\%M:\%S"
#'
#'@return number of hours
#'
#'@example  hoursUTC()
#'
hoursUTC <- function(to_test_time = Sys.time(), timeformat = "%Y-%m-%d %H:%M:%S"){
  localtime <- to_test_time
  difftime(
    format(localtime, timeformat)
    ,
    format(toUTC( localtime ), timeformat)
    , units ="hours"
  )

}

#'
#'@title add hours
#'
#'@description This function adds h hours to a POSIXct
#'
#'@param h number of hours
#'
#'@param datetime POSIXct
#'
#'@return  POSIXct and h added
#'
#'@example 2 %h+% Sys.time()
#'

#'
'%h+%' <- function(h,datetime){
  h * 60 *60 + datetime
}

