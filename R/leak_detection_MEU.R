library(xts)
library(tidyverse)
library(dplyr)



leak_detection_MEU <- function(timeseries, model_params = NA, id = NA){


  # if timseries sent is a single xts
  if (class(timeseries) %in% c("xts", "zoo")){
    if(is.na(id))id <-  gsub("-|:|\\s","",paste0(Sys.time()))
    # wrap the outcome in a list to avoid redundent checks going forward
    res <- list(id0 = leak_detection_MEU_single(timeseries, id, model_params))
    return (res)
  }

  # if timeseries is a list of xts
  else if (class(timeseries) == 'list'){
    res <- list()
    if(!is.na(model_params)){
      if (length(timeseries) != length(model_params))stop("The model_params must be a list of the same length as timeseries list")
      id = names(timeseries)
      for (i in 1:length(timeseries)){
        res[[i]] <- leak_detection_MEU_single(timeseries[[i]], id[i], model_params[[i]])
      }
    }
    else{
      id = names(timeseries)
      for (i in 1:length(timeseries)){
        res[[i]] <- leak_detection_MEU_single(timeseries[[i]], id[i])

      }

    }
    return(res)
  }
}



leak_detection_MEU_single <- function(xts_object, id = NA, model_params = NA){


  # if model_params are sent, use it to detect the leak flags directly
  if(!is.na(model_params))return(learn_and_predict(contract_ts = xts_object, model_params = model_params))

  # otherwise, look on disk for previously saved models for this id
  else if (!is.na(id)){
    model_params = tryCatch({
      readRDS(paste0("MEU_models/",id,".rds"))
    }, error = function(e) {
      print("No saved Model Params")
      return (NA)
    })
  }

  if(!is.na(model_params))print("Model Params Loaded from Disk")
  # proceed with main function call
  return (learn_and_predict(contract_ts = xts_object, id = id, model_params = model_params))
}


learn_and_predict <- function(contract_ts, id = NA, model_params = NA, leak_start = NA, leak_end = NA, z_crit = 1.95, leak_threshold = 0.8,
                              daily_threshold = 0.5, skip_leak = TRUE){


  # extract hourly markers and use them to calculate hourly usage
  ep <- endpoints(contract_ts, on = "hours")
  usagePerhour_all <- period.apply(contract_ts, INDEX = ep ,FUN = sum, na.rm = T)

  # if model_params are sent, use it and skip the training phase
  if(!is.na(model_params))model_params_df <- model_params

  # otherwise, proceed with training
  else {
    print("Training model")
    model_params_df <- data.frame(mean = numeric(0) , sd = numeric(0), n = numeric(0), meu = numeric(0))


    mon_idx <- 0:11             # define months and hour notation (as per xts)
    hr_idx <- 0:23



    # remove hourly usage data during the leak
    if(skip_leak)usagePerhour <- usagePerhour_all
    else{
      leak_idx <- usagePerhour_all[paste0(leak_start,"/",leak_end), which.i = T]
      usagePerhour <- usagePerhour_all[-leak_idx]
    }
    # loop on each month and each hour to calculate the average hourly usage once for weekdays and another for weekends
    # the notation of the obtained dictionary is as follows: "Month.Wkday/Wkend.Hour"
    # f.ex  "0.0.13" is the entry for month of Jan (0), Wkday (0), 1P.m. (13)
    # and "5.1.23" is the entry for month of "june (5), wkend (1), 11P.M. (23)
    for (mon in mon_idx){
      for(hr in hr_idx){
        # mark the weekday index for a specific hour and a specific month
        wkday_idx <- which(.indexhour(usagePerhour) == hr & .indexmon(usagePerhour) == mon &
                             .indexwday(usagePerhour) !=0 & .indexwday(usagePerhour) !=6)
        # calculate mean, sd, number of observations and Maximum Expected Usage for a specific hour and a specific month
        MEAN <- mean(usagePerhour[wkday_idx], na.rm = T)
        SD <- sd(usagePerhour[wkday_idx], na.rm = T)
        n <- n <- sum(!is.na(usagePerhour[wkday_idx]))
        MEU <- MEAN + z_crit * SD / sqrt(n)
        model_params_df[paste0(mon,".",0,".",hr),] <- c(MEAN,SD,n,MEU)

        # do the same as above for weekend index
        wkend_idx <- which(.indexhour(usagePerhour) == hr & .indexmon(usagePerhour) == mon &
                             (.indexwday(usagePerhour) ==0 | .indexwday(usagePerhour) ==6))
        MEAN <- mean(usagePerhour[wkend_idx], na.rm = T)
        SD <- sd(usagePerhour[wkend_idx], na.rm = T)
        n <- sum(!is.na(usagePerhour[wkend_idx]))
        MEU <- MEAN + z_crit * SD / sqrt(n)
        model_params_df[paste0(mon,".",1,".",hr),] <- c(MEAN,SD,n,MEU)

      }
    }
    if(!is.na(id))tryCatch({dir.create("MEU_models", showWarnings = FALSE)
                            saveRDS(model_params_df, paste0("MEU_models/",id,".rds"))
                            },
                            error = function(e)print("Couldn't Write Model to Disk"))
  }

  # Using the learned dictionary to loop on all the contract and identify potential leak intervals
  m <- month(index(usagePerhour_all))-1
  w <- ifelse(weekdays(index(usagePerhour_all)) %in% c("Saturday","Sunday"),1,0)
  h <- hour(index(usagePerhour_all))

  # lookup the maximum estimated usage (MEU) corresponding to each hour in the original ts
  values <- as.vector(sapply(paste0(m,".",w,".",h),  FUN = function(x)return(model_params_df[x,"meu"])))
  meu_xts <- xts(values, order.by = index(usagePerhour_all))

  # concatenate the actual usage to the MEU, convert to dataframe and create a flag column
  comp_xts <- data.frame(coredata(usagePerhour_all),coredata(meu_xts))
  names(comp_xts) <- c("usage", "meu")
  df <- comp_xts %>% mutate(flag = ifelse(usage > meu, TRUE,FALSE))
  flag_xts <- xts(df$flag, order.by = index(usagePerhour_all))

  # define an aux function to check if a certain day should be marked as a leak (depending on leak_threshold and daily_threshold)
  isLeak <- function(x){
    if(sum(is.na(coredata(x))) == length(coredata(x))) return (NaN)
    if(mean(x, na.rm = T) > leak_threshold & sum(is.na(coredata(x))) < 24*daily_threshold)return(TRUE)
    else return(FALSE)
  }
  # apply the aux function on our flag_xts to obtain the leak_xts
  leak_xts <- period.apply(flag_xts, endpoints(flag_xts, on = "days"), FUN = isLeak)
  leak_flag <- data.frame(Date = as.Date(index(leak_xts)), leak_flag = coredata(leak_xts)) %>% filter(leak_flag == TRUE)

  return (list(leak_xts = leak_xts, leak_flag = leak_flag, model_params = model_params_df))
}



