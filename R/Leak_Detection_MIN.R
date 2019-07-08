

#------------- daily minimum ---------------------------
#' daily_min
#'
#' Utility function to get the minimum daily ocnsumption per contract
#' @param input : contract timeseries object or list of contracts timeseries
#'
#' @return list or Single timeseries object of daily minimum consumption
#'
#' @examples \dontrun{daily_min(contract)}
daily_min <- function(input){
  # print(typeof(input))
  # if (class(input)!='list'){
  #   input <- list(meter = input)
  # }

  daily.min.list <- list()
  for (id in names(input)){
    myts <- input[[id]]
    myts<-apply.daily(myts,FUN = min, na.rm = T)
    myts [coredata(myts)==Inf] = 0.1
    #myt <- apply(myts,FUN= function(x) ifelse(is.finite(x), x, 0))
    daily.min.list[[id]] <- myts
  }
  daily.min.list
}


#' leak_detection_min
#'
#' Utility function to highlight leaks based on Min Method, flag as a leak if daily minimum consumption is above 0 for n consequetive days
#' @param input contract timeseries object or list of contracts timeseries
#' @param mID contract ID if a single contract is passed to the function
#' @param days  Number of days to be considered for Analysis
#'
#' @return list of dataframes with data and a booolean flag weather a leak has occured or not
#' @export
#'
#' @examples \dontrun{leak_detection_min(contract, days =2)}
leak_detection_min <- function(input, mID ="000", days =2){
  # a list to collect the dataframes per contract
  leak.df <- list()

  #check if the input is a list of xts if not create a list of one xts object, to make code concise!
  if (typeof(input)!='list'){
    input <- list("meter00" = input)
    if (!is.na(mID)){
      names(input) <-  mID
    }
  }

  input <- daily_min(input)
  for (id in names(input)){
    # get xts series from the list
    myts <- input[[id]]
    myts %>%  fortify() -> mydf
    colnames(mydf) <- c('Date','Min.Con')
    mydf$Date <- as.Date(mydf$Date)
    # rle counts the length of consequetive inputpoints where consumption > 0 (TRUE) <= 0 (FALSE)
    r <- with(with(mydf, rle(Min.Con >1)),rep(lengths,lengths))
    mydf <- mydf %>% mutate(streak = r , leak_flag =with(mydf,Min.Con >1) & (r>=days) )
    # subset to Data and Leak Flag
    leak.df[[id]] <- mydf %>%  dplyr::select( Date, leak_flag) %>% filter( leak_flag == T)

  }
  leak.df
}

######################################



#Leak Detection
leak_plot<- function(input, mID="000"){
  # a list to collect the visualisations
  plt <- list()
  print(class(input))
  #check if the input is a list of xts if not create a list of one xts object, to make code concise!
  if (class(input)!='list'){
    print('converted')
    input <- list(id = input)
    names(input) <- mID
  }

  for (id in names(input)){
    mydf <- input[[id]]
    tryCatch({
      mydf <- mydf %>% filter(leak_flag ==T)
      p <- ggcal(mydf$Date,mydf$leak_flag)+ggtitle(paste0(year(mydf$Date), '\t \t Contract: ', id))

      plt[[id]] <- p

    },error=function(e){
      print('No leakage')
      p <- ggplot() +
        annotate('text', x = 4, y = 25, size=8, label = paste0('Contract ID ',id,' has no leakage')) +
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
      plt[[id]] <- p
    })

  }
  return (plt)
}


