
#------------- Date Preparation ---------------------------

#' Data Preparation
#'
#' This function takes rds with a list of contracts and returns them in standerdized equidistant form
#' @param rds a list of xts object returned from read.csv function
#' @param align_interval the timing between subsequent consumption measurments in minutes
#' @param nas.action flag to decide on the action with NA points: Interpolate conducts linear interpoaltion from last observation
#' LOCF Last observation carried forward
#'None: leave NAs as they are
#' @return Standerdizedd list of temseries per input  contract
#' @export
#'
#' @examples \dontrun{data_prep(contracts, align_interval = 30, nas.action = LOCF)}
data_prep<- function(rds,align_interval = NULL, nas.action=c('interpolate','LOCF', 'None')){

  # This function convert the timeseries into equidstant series.
  # Input: RDS file for raw data
  # Output: list of XTS object standerdized
  ix<-1
  con_tot <- length(names(rds))
  for (id in names(rds)){
    prgrs <- round(ix/con_tot*100,0)
    if ( prgrs %% 2 ==0 ){print(paste0(prgrs, '%'))}

    if (!is.null(align_interval)){
      #align the datapoints to the nearest align_interval
      #rds[[id]]<-rds[[id]] %>% align.time(n=60*align_interval)
      # Using round_date from lubridate instead of xts align.time
      # the letter seems to round to the next half an hour always
      index(rds[[id]]) <- lubridate::round_date(index(rds[[id]]),align_interval)
      rds[[id]] <- make.index.unique(rds[[id]])

      # generate timestep from start to end date with align_interval spacing
      equi.dist.time<-seq.POSIXt(from = start(rds[[id]]),to=end(rds[[id]]),by = align_interval )
      # merge with original time stamp
      rds[[id]]<-merge(rds[[id]],equi.dist.time,join='right')

      #Last Observation Carried Forward to fill NAs
      if (nas.action=='LOCF'){
        rds[[id]] <- na.locf(rds[[id]], fromLast = TRUE,na.rm = TRUE)

      }else if (nas.action=='interpolate'){
        rds[[id]] <- na.approx(rds[[id]])
      }else{
        print('No action on NAs')
      }


    }
    ix = ix+1
    #take diff of the timeseries
    rds[[id]]<- diff(rds[[id]])
  }
  rds
}
#------------- daily minimum ---------------------------
#'  daily_min
#'
#' Utility function to get the minimum daily ocnsumption per contract
#' @param rds list of xts objects
#'
#' @return list of xts objects with daily minimum
#'
#'
daily_min <- function(rds){

  daily.min.list <- list()
  for (id in names(rds)){
    print(id)
    myts <- rds[[id]]
    myts<-apply.daily(myts,FUN = min)
    daily.min.list[[id]] <- myts
  }
  daily.min.list
}


#--------------- read_from_csv -----------------------------
#' read_from_csv
#'
#' This function receives a csv file with three columns (ID, Time, Litres) which contains meter data
#' for one or more service agreement. "ID" is a unique identifier for a service agreement, "Time" is
#' the timestamp for a certain reading in %Y-%m-%d %H:%M:%S format, and "Litres" is the meter reading at
#' that timestamp. (in units of litres)
#'
#' @param filename       The CSV filename containing the three columns as described above
#' @param align_interval The spacing between reading to be used for alignments
#' @param na.action      How to handle NAs
#' @param align_and_pad  Set to True by default so all the time series' will be aligned to the neareast 30 minutes and all data gaps will be padded with "NA"
#'
#' @return  List of XTS objects, each object represents the time series for a specific service agreement
#' @export
#'
#' @examples \dontrun{read_from_csv("north_melbourne_data.csv")}
read_from_csv <- function(filename, align_and_pad = TRUE, align_interval = '30 min', na.action= "None"){


  data <- read.csv(filename)

  if (ncol(data) != 3) stop(simpleError("The CSV file must contain exactly three columns"))

  names(data) <- c("ID", "Time", "Litres")

  #Convert time column to a standard time format
  print("Stripping Time")
  data$Time<- strptime(data$Time,"%Y-%m-%d %H:%M:%S")
  print("Done")

  data$ID <-as.factor(data$ID)

  # split the csv by ID (every ID represents a unique service agreement)
  print("Splittig by Service Agreement ID")
  df1<- split(data,data$ID)
  print("Done")
  # get rid of any Nulls introduced during time standardization
  df1 <- lapply(df1, function(x) {na_idx = which(is.na(x$Time))
                                  if (length(na_idx) > 0) return (x[-na_idx,])
                                  else return (x)
                                  }
                  )

  dfxt<-list()

  print("Converting into XTS objects")
  # loop on each service agreement and convert it into XTS format
  for (m in df1){
    id=as.character(m$ID[1])
    print(id)
    dfxt[[id]] <- xts(m[,c('Litres')],order.by = as.POSIXct(m[,'Time']))
  }
  print("Done")
  if (align_and_pad) dfxt <- data_prep(dfxt, align_interval= align_interval, nas.action = na.action)
  return(dfxt)
}


generate_csv <- function( input_rds,num_contracts =2){
  print('reading input RDS file')
  df <- data.frame(matrix(nrow =0, ncol = 3 ))
  colnames(df) <-c('ID','Time','Liters')
  rds<-input_rds#readRDS(input_rds)
  for (id in names(rds[1:num_contracts])){
    print(id)
    tm <- index(rds[[id]])
    readings <- coredata(rds[[id]])
    num_rows <- length(tm)
    temp_df <- data.frame(matrix(nrow =num_rows, ncol = 3 ))
    colnames(temp_df) <-c('ID','Time','Liters')
    temp_df<- temp_df %>% mutate(ID = rep(id,num_rows), Time = tm, Liters = readings )
    df <- rbind(df,temp_df)

  }
  return (df)
}


