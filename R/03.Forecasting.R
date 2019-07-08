# TO_ADD Forecasting Codebase

#' customer_forecast_ets
#'
#' Forecast cutomers monthly consumption for the next 6 months. This function uses ETS model and require at least 2 years of consumption histor
#' to generate forecast.
#' @param object Contract timeseries object or list of contracts timeseries
#' @param plot_type Type of plot used to dispaly the forecast results, line or bar charts
#' @param pdf_file name of pdf file to output the pltos. If kept empty no pdf will be generated
#' @param out_csv boolean to indicate  if forecast need to be outputed in csv files
#' @param mID meter ID for the case wheere a single contract is passed, used to plot the contract id in the plot title
#'
#' @return list of forecast results
#' @export
#'
#' @examples \dontrun{customer_forecast_ets(contractss, plot_type= 'bar', pdf_file= 'forecast.pdf', out_csv = TRUE)}
customer_forecast_ets <- function (object, plot_type= c('bar', 'line'), pdf_file = NA, out_csv = TRUE, mID='000'){
  #check if the input is a list of xts if not create a list of one xts object, to make code concise!
  if (typeof(object)!='list'){
    object <- list("meter00" = object)
    if (!is.na(mID)){
      names(object) <-  mID
    }
  }

  forecast.list <- list()
  forecast.plots <- list()
  for (id in names(object)){
    myts <- object[[id]]
    myts<-apply.monthly(myts,FUN = sum, na.rm = T)

    # Generate plots only for the contracts that contains more than two years worth of data.
    if (nrow(myts) < 24){
      next
    }else{
      # Applying ETS function for monthly data (i.e hence the frequency is 12, forecasting 6 months ahead).
      # Lamda is set to TRUE to avoid nagative forecasts.
      model <- ets(ts(myts, start = c(year(myts[1]), month(myts[1])), frequency = 12), lambda = TRUE)
      f.cast <- forecast(model, h = 6)
      forecast.list [[id]] <- f.cast

      # Generating forecasting plots.
      Title <- paste0("ETS Forecast Customer ID:", id)

      if (plot_type=='line'){
        forecast.plots[[id]]<-
          autoplot(f.cast, main =  Title, ylab = "Water Consumption")
      }else{
        forecast.plots[[id]] <-
          myBarPlot.forecast(f.cast, main =  Title, ylab = "Water Consumption")
      }
    }

  }
  # CSV OUTPUT
  if (out_csv == TRUE){
    for (id in names(forecast.list)){
      write.csv( forecast.list[[id]], paste0(id,'_ets_forecat.csv'),row.names = TRUE)
    }

  }

  # PDF OUTPUT
  if (!is.na(pdf_file)){
    filePath <- file.path(getwd(),paste0(pdf_file,'.pdf'))
    pdf(filePath)
    print(forecast.plots)
    dev.off()
    print(paste0('Plots are plotted in the following file: ',filePath))
    system(paste0('open ', filePath))

  }

  return(forecast.list)

}


#customer_forecast_ets(meter_sample,plot_type = 'bar',pdf_file = 'fcast')

#function to plot forecasts as barplots
myBarPlot.forecast <- function(object, fcol="#596DD5", main="", ylab="", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)

  if (!is.forecast(object)) {
    stop("autoplot.forecast requires a forecast object, use object=object")
  }

  p <- ggplot2::ggplot()

  #object <- forecast_model
  data <- data.frame(yvar = c(object$x))

  vars <- c(yvar = object$series)

  # Data points
  timex <- time(object$x)
  data <- data.frame(yvar = as.numeric(data$yvar), datetime = as.numeric(timex))
  p <- p + ggplot2::scale_x_continuous()

  p <- p + ggplot2::geom_bar(ggplot2::aes_(x = ~datetime, y = ~yvar), data = data, stat="identity") +
    ggplot2::labs(y = vars["yvar"], x = "Time")

  # Forecasted points
  predicted <- data.frame(xvar = time(object$mean), yvar = object$mean)
  colnames(predicted) <- c("datetime", "ypred")
  p <- p + ggplot2::geom_bar(ggplot2::aes_(x = ~datetime, y = ~ypred), data = predicted, stat="identity", fill = fcol)
  p <- p + ggtitle(main) + ylab(ylab)
  p
}



############################## ARIMA FORECAST ###########################

#' hourly_forecasst
#'
#' Conduct 24 hour consumption forecast for aggregatre consumption from the list of contracts passed to the function
#'
#' @param object Contract timeseries object or list of contracts timeseries
#' @param plot_type Type of plot used to dispaly the forecast results, line or bar charts
#' @param pdf_file name of pdf file to output the pltos. If kept empty no pdf will be generated
#' @param out_csv boolean to indicate  if forecast need to be outputed in csv files
#' @temp_file temperature record file from BOM
#' @return forecasted series
#' @export
#'
#' @examples \dontrun{hourly_forecast(contractss, plot_type= 'line', pdf_file= 'forecast.pdf', out_csv = TRUE)}
hourly_forecasst <- function (object, plot_type= c('bar', 'line'), temp_file,  pdf_file = NA, out_csv = TRUE){
  # Concat all the contracts in object into a single xts timeseries
  masterxts <- (do.call(cbind,object))

  # Sum consumptions with same time index
  masterxts <- xts(x = rowSums(coredata(masterxts), na.rm = T),index(masterxts) )
  # Sum to hourly consumption
  masterxts <- period.apply( masterxts,endpoints(masterxts, "hours"), sum, na.rm=T)
  # Approximate to the nearest hour
  index(masterxts) <-  round_date(index(masterxts), '60 mins')

  # reomve entries with duplicate index
  masterxts <- masterxts[ ! duplicated( index(masterxts), fromLast = TRUE ),  ]

  ix.history= length(masterxts)

  weather <- fetch_wather_data(start(masterxts), end(masterxts)+86400 , temp_file )
  masterxts <- merge(masterxts, weather)

  masterxts <- masterxts %>%  fortify()
  colnames(masterxts) <- c('Date','Consumption', 'Temperature')#, 'Rainfall')
  masterxts <- creat_master_df(masterxts) %>%  na.locf()

  #Create Regressors
  ix.history <- (1:(nrow(masterxts)-24))
  xreg <- masterxts %>%  mutate( Weekend = model.matrix(~Weekend)[,2], Public.Holiday = model.matrix(~Public.Holiday)[,2] )%>%
    dplyr::select(Temperature, Weekend, Public.Holiday)
  xreg.train <- xreg[ix.history,]

  xreg.test <-  xreg[-ix.history,]

  #myts <- xts(masterxts$Consumption[ix.history], order.by = masterxts$Date[ix.history],frequency = 24)
  myts <- ts(masterxts$Consumption[ix.history], frequency = 24)
  # Fit model
  model <- auto.arima(myts, xreg = xreg.train,seasonal = TRUE)
  # Forecast fit one day ahead
  f.cast <- forecast(model, xreg = xreg.test,h=24)


  Title <- paste0(" Hourly Forecast with ARIMA")
  # Generating forecasting plots.
  if (plot_type=='line'){
    forecast.plots<-
      autoplot(f.cast,main=Title,ylab="Water consumption",include = 24*7)
  }else{
    forecast.plots <-
      myBarPlot.forecast(f.cast, main =  Title, ylab = "Water Consumption")
  }

  # CSV OUTPUT
  if (out_csv == TRUE){
    write.csv( f.cast, ('Hourly_Waterconsumption_ARIMA_Forecasts.csv'),row.names = TRUE)

  }

  # PDF OUTPUT
  if (!is.na(pdf_file)){
    filePath <- file.path(getwd(),paste0(pdf_file,'.pdf'))
    pdf(filePath)
    print(forecast.plots)
    dev.off()
    print(paste0('Plots are plotted in the following file: ',filePath))
    system(paste0('open ', filePath))

  }

  return(f.cast)

}

 #hourly_forecasst(meter_sample,temp_file = 'IDCJAC0010_086282_1800_Data.csv', plot_type = 'line',pdf_file = 'TestArima')





# Utility function to fetch weather data from BOM Website
fetch_wather_data <- function(start_date=NA, end_date=NA, temp_file){

  rainfall_url <- "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=086282&p_nccObsCode=136&p_c=-1489039397&p_startYear=1970"
  temp_url <-     "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=086282&p_nccObsCode=122&p_c=-1489036507&p_startYear=1970"
  rainfall_file <- 'IDCJAC0009_086282_1800_Data.csv'
  #temp_file <- 'IDCJAC0010_086282_1800_Data.csv'


  # while(TRUE){
  #
  #   if(!file.exists(rainfall_file)){
  #     tryCatch({
  #       print('Fetching weather data')
  #       download.file(rainfall_url,destfile = 'rainfall.zip', method = 'curl')
  #       download.file(temp_url,destfile = 'temp.zip', method = 'curl')
  #       unzip('rainfall.zip')
  #       unzip('temp.zip')
  #     },error=function(e) 1)
  #   }
  #   # Rainfall
  #   df_rainfall <- read_csv(rainfall_file) %>%  dplyr::select(Year,Month, Day, 'Rainfall amount (millimetres)') %>%
  #     dplyr::rename('rainfall'= `Rainfall amount (millimetres)` ) %>% mutate(Date = (as.POSIXct(paste(Year,Month,Day, '00:00:00', sep='-'),"%Y-%m-%d-%H:%M:%S",tz = 'Australia/Victoria'))) #mutate(Date = as.Date(paste(Year,Month,Day, sep = '-')))
  #   # check that the weather files are upto date compared to consumption file
  #   if (tail(df_rainfall$Date,1)< end_date){
  #     print('removing old files')
  #     file.remove(rainfall_file)
  #     file.remove(temp_file)
  #   } else{
  #     break;
  #   }
  # }
  # rainfal_ts <- xts(x = df_rainfall$rainfall, as.POSIXct(df_rainfall$Date))
  # equi.dist.time <-seq.POSIXt(from = start(rainfal_ts),to=end(rainfal_ts),by = '60 mins' )
  # rainfal_ts <- merge(rainfal_ts, equi.dist.time,join='right') %>%  na.locf( na.rm = T)

  #Temperature
  df_temp <- read_csv(temp_file) %>%  dplyr::select(Year,Month, Day, 'Maximum temperature (Degree C)') %>%
    dplyr::rename('temp'= 'Maximum temperature (Degree C)' ) %>%
    mutate(Date = (as.POSIXct(paste(Year,Month,Day, '00:00:00', sep='-'),"%Y-%m-%d-%H:%M:%S",tz = 'Australia/Victoria'))) #mutate(Date = as.Date(paste(Year,Month,Day, sep = '-')))

  temp_ts <- xts(x = df_temp$temp, as.POSIXct(df_temp$Date))
  equi.dist.time <-seq.POSIXt(from = start(temp_ts),to=end(temp_ts),by = '60 mins' )
  temp_ts <- merge(temp_ts, equi.dist.time,join='right') %>%  na.locf( na.rm = T)
  res <- temp_ts
  #res <- merge(temp_ts, rainfal_ts)
  #names <- c('Temp', 'Rainfall')
  if (!is.na(start_date)){
    res <- res[index(res) > start_date &  index(res) < end_date]
  }

  return(res)
}



#------------- Utility Function to create DataFrame Out of XTS object---------------------------
creat_master_df <- function (df){
  #create dataframe
  #Note:  #Summer months below are as per BOM classification for Spring/Summer in Australia.
  #Refer to this for details: http://www.bom.gov.au/climate/glossary/seasons.shtml
  #Define Holidays
  hlist <- c("USChristmasDay","USGoodFriday",
             "USNewYearsDay","USThanksgivingDay")
  myholidays  <- dates(as.character(holiday(2000:2020,hlist)),format="Y-M-D")
  is.holiday(lubridate::as_date(index(df)), myholidays)

  df<-df%>% mutate(Day = factor(weekdays(Date), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
           Time= hour(Date), Weekend= (if_else(Day %in% c("Saturday", "Sunday"), TRUE, FALSE)),
           Month =lubridate::month(Date), Public.Holiday =is.holiday(lubridate::as_date(Date), myholidays) )
}


