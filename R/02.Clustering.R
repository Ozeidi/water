##########################################  Dependencies #######################################################
# -----  extract_features dependencies -----
# library(tsfeatures) # Install from github devtools::install_github("robjhyndman/tsfeatures")
# library(forecast)
# library(xts)
# library(ggplot2)
# library(lubridate)
# library(parallel)
# library(pbapply) # for progress bar in parlapply

# -----  calculate_clusters dependencies -----
# library(clusterSim)
# library(reshape)

# -----  calculate_consumption dependencies -----
# library(dplyr)

# -----  benchmark dependencies -----
# library(plotly)    -- need to install from github version 4.7.1.9   devtools::install_github("ropensci/plotly")
# also need 'scales', 'digest', 'lazyeval', 'rlang', 'data.table' libraries as dependencies to plotly
#################################################################################################################
##########################################  Feature Extraction #################################################
# AUX functions -------------------------------------------------------------------------------------------------

# Append Seasonality to Feature Name
nameAddSeas <- function(features, freq){
  names(features) <- sapply(names(features), function(x) paste0(x,"_", freq))
  features
}

# Run Features with error catch
runFeatures <- function(FUN, series, featurenames = NULL, freq = NULL, inds = NULL){
  features <- tryCatch(FUN(series),
                       error = function(e){
                         err <- rep(NA, length(featurenames))
                       })
  if(!is.null(inds)) features <- features[inds]
  if(!is.null(featurenames)) names(features) <- featurenames
  if(!is.null(freq)) features <- nameAddSeas(features, freq)
  features
}

# Get Features
extractFeatures <- function(meter, id, seasonal_periods = c(24, 168, 366*24), preferred_freq = 24){

  results <- c("id" = id)
  #cat(paste(c("service_id:", id, "\n", length(meter), "readings", "\n")))

  # Extract Seasonally independent/reconciled features
  meter_msts <- msts(data = meter, seasonal.periods = seasonal_periods, ts.frequency = preferred_freq)

  results <- append(results, runFeatures(acf_features, meter_msts, c('x_acf1', 'x_acf10', 'diff1_acf1', 'diff1_acf10',  'diff2_acf1', 'diff2_acf10'), inds = 1:6))
  results <- append(results, runFeatures(arch_stat, meter_msts, c("ARCH.LM")))
  results <- append(results, runFeatures(crossing_points, meter_msts, c("crossing_points")))
  results <- append(results, runFeatures(entropy, meter_msts, c("entropy")))
  results <- append(results, runFeatures(flat_spots, meter_msts, c("flat_spots")))
  results <- append(results, runFeatures(heterogeneity, meter_msts, c("arch_acf", "garch_acf", "arch_r2", "garch_r2")))
  results <- append(results, runFeatures(holt_parameters, meter_msts, c("alpha", "beta")))
  results <- append(results, runFeatures(hurst, meter_msts, c("hurst")))
  results <- append(results, runFeatures(nonlinearity, meter_msts, c("nonlinearity")))
  results <- append(results, runFeatures(pacf_features, meter_msts, c("x_pacf5", "diff1x_pacf5", "diff2x_pacf5"), inds = 1:3))
  results <- append(results, runFeatures(stl_features, meter_msts,
                                         c("trend", "spike", "linearity", "curvature", "e_acf1", "e_acf10",
                                           paste0("seasonal_strength_", seasonal_periods[1]), paste0("seasonal_strength_", seasonal_periods[2]), paste0("seasonal_strength_", seasonal_periods[3]),
                                           paste0("peak_", seasonal_periods[1]), paste0("peak_", seasonal_periods[2]), paste0("peak_", seasonal_periods[3]),
                                           paste0("trough_", seasonal_periods[1]), paste0("trough_", seasonal_periods[2]), paste0("trough_", seasonal_periods[3])), inds = 5:19))
  results <- append(results, runFeatures(unitroot_pp, meter_msts, c("unitroot_pp_pp")))
  results <- append(results, runFeatures(unitroot_kpss, meter_msts, c("unitroot_pp_kpss")))


  # Extract Features that work for a single seasonality for each period individually
  for(freq in seasonal_periods){
    meter_ts <- ts(meter, frequency = freq)

    results <- append(results, runFeatures(acf_features, meter_msts, c("seas_acf1"), freq, 7))
    results <- append(results, runFeatures(hw_parameters, meter_msts, c("alpha", "beta", "gamma"), freq))
    results <- append(results, runFeatures(lumpiness, meter_msts, c("lumpiness"), freq))
    results <- append(results, runFeatures(max_kl_shift, meter_msts, c("max_kl_shift", "time_kl_shift"), freq))
    results <- append(results, runFeatures(max_level_shift, meter_msts, c("max_level_shift", "time_level_shift"), freq))
    results <- append(results, runFeatures(max_var_shift, meter_msts, c("max_var_shift", "time_var_shift"), freq))
    results <- append(results, runFeatures(pacf_features, meter_msts, c("seas_pacf"), freq, 4))
    results <- append(results, runFeatures(stability, meter_msts, c("stability"), freq))
  }

  # Extract Consumption Time features
  results["mean"] <- mean(meter_msts, na.rm = T)
  results["median"] <- median(meter_msts, na.rm = T)
  results["sd"] <- sd(meter_msts, na.rm = T)

  meter_df <- fortify(meter)
  colnames(meter_df) <- c("date", "volume")
  meter_df$hour <- hour(meter_df$date)
  meter_df$month <- month(meter_df$date)

  meter_df$day <- weekdays(meter_df$date)
  meter_df$weekday <- ifelse(meter_df$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  meter_df$timeofday <- sapply(meter_df$hour, function(x){
    if(x %in% 6:9) return("Morning")
    if(x %in% 10:17) return("Day")
    if(x %in% 18:21) return("Evening")
    if(x %in% 22:23 || x %in% 0:5)  return("Night")
  })
  meter_df$daynight <- sapply(meter_df$hour, function(x){
    if(x %in% 9:17) return("Day")
    if(x %in% 18:23 || x %in% 0:8)  return("Night")
  })
  meter_df$season <- sapply(meter_df$month, function(x){
    if(x %in% 6:8) return("Winter")
    if(x %in% 12 || x %in% 1:2)  return("Summer")
    if(x %in% 3:5)  return("Autumn")
    if(x %in% 9:11)  return("Spring")
  })

  results["weekday"] <- 100*sum(meter_df$volume[meter_df$weekday == "Weekday"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["weekend"] <- 100*sum(meter_df$volume[meter_df$weekday == "Weekend"], na.rm = T)/sum(meter_df$volume, na.rm = T)

  results["Morning"] <- 100*sum(meter_df$volume[meter_df$timeofday == "Morning"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Day"] <- 100*sum(meter_df$volume[meter_df$timeofday == "Day"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Evening"] <- 100*sum(meter_df$volume[meter_df$timeofday == "Evening"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Night"] <- 100*sum(meter_df$volume[meter_df$timeofday == "Night"], na.rm = T)/sum(meter_df$volume, na.rm = T)

  results["BusinessHrs"] <- 100*sum(meter_df$volume[meter_df$daynight == "Day"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Notbusinesshrs"] <- 100*sum(meter_df$volume[meter_df$daynight == "Night"], na.rm = T)/sum(meter_df$volume, na.rm = T)

  results["Winter"] <- 100*sum(meter_df$volume[meter_df$season == "Winter"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Summer"] <- 100*sum(meter_df$volume[meter_df$season == "Summer"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Autumn"] <- 100*sum(meter_df$volume[meter_df$season == "Autumn"], na.rm = T)/sum(meter_df$volume, na.rm = T)
  results["Spring"] <- 100*sum(meter_df$volume[meter_df$season == "Spring"], na.rm = T)/sum(meter_df$volume, na.rm = T)


  return(results)
}




# ---------------------------------Main Function ----------------------------------------------
#' extract_features
#' This function extract Time Series features from a list of time series objects. The features extracted are based on
#' tsfeatuers package and suppport multiple seasonalities. The purpose of the feature extraction is to use the features
#' (or part thereof) for clustering. Please see the example markdown for a complete workflow.
#'
#' @param timeseries_list : a list of timeseries XTS objects named with the service_id of each customer contract.
#'
#' @return : a features dataframe where the first column is the service_id and the remaining columns are the features
#' (refer to documentation for more details about the features descritpion)
#' @export
#'
#' @examples
extract_features <- function(timeseries_list){
  # Creat local environment for parrallelizng the feature extraction
  water.env <- new.env()
  # Add the function needed in feature extraction to the local env
  assign('nameAddSeas',nameAddSeas,envir = water.env)
  assign('runFeatures',runFeatures,envir = water.env)
  assign('extractFeatures',extractFeatures,envir = water.env)

  meters <- timeseries_list
  meters <- meters[which(sapply(meters, nrow) != 0)]
  # Parallel
  print("Initializing Parallel Computing")
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, varlist = c('nameAddSeas', 'runFeatures', 'extractFeatures'),envir = water.env)
  clusterEvalQ(cl,{
    library(forecast)
    library(tsfeatures)
    library(lubridate)
    library(ggplot2)
    library(xts)
  })
  print("Done")
  # calculate features of all service ids (i.e. meters)
  print("Starting Feature Extraction")
  # pblapply is a wrapper for parlappy which shows a progress bar
  features <- pblapply(1:length(meters), cl = cl, FUN = function(contract){
    extractFeatures(meters[[contract]], names(meters)[contract])
  })
  stopCluster(cl)

  # Bind to a data.frame
  series_features <- do.call(rbind, features)
  series_features_num <- apply(series_features, 2, as.numeric)
  features_df <- data.frame(series_features_num)
  names(features_df) <- c("service_id", names(features_df)[2:ncol(features_df)])

  return(features_df)
}


#-------------------------------------- End of extract_features -------------------------------------







####################################### Cluster Calculation #########################################
#####################################################################################################
#' calculate_clusters
#' This function receives a features dataframe (custom as per YVW requirements or obtained from one of the feature extraction functions in this package)
#' Based on the features provided and the maximum allowed number of clusters, the function will calculate the best cluster assignment possible and will
#' in an iterative manner while removing any outliers found in each iteration. Finally, the function will return the cluster assignment and some other
#' useful plots. The output of this function can be used as an input for the customer_benchmarking function which benchmarks a customer based on his/her
#' cluster.
#'
#' @param features : is a dataframe with service_id as the first column and the features as the remaining columns
#' @param k_override : can be set to an integer number to override the number of clusters, if k is left NULL, best K will be automatically chosen based on lowest DB index
#' @param k_max : is the maximum number of clusters allowed
#'
#' @return : return a list containing the chosen number of clusters (k_chosen), the cluster centers scaled and unscaled, the cluster_assignment data frame, and two plot objects
#' for a PCA representation, a cluster barplot representation, and a vector of outlier service IDs.
#' @export
#'
#' @examples
calculate_clusters <- function(features, k_override=NA, k_max = 10){

  if (nrow(features) < 20)stop("Very few service_id's, please use at least 20 service_id")
  names(features) <- c("service_id", names(features)[2:length(features)])
  # scale the data but first keep the mean and sd to unscale at the end
  print("Scaling the features data")
  features_mean <- data.frame(mean = features %>% dplyr::select(-c(service_id)) %>% apply(MARGIN = 2, FUN = mean))
  features_sd <- data.frame(sd = features %>% dplyr::select(-c(service_id)) %>% apply(MARGIN = 2, FUN = sd))
  features_scaled <- features %>% dplyr::select(-c(service_id)) %>% apply(MARGIN = 2, FUN = function(x){(x-mean(x))/sd(x)}) %>%
    data.frame(service_id = features$service_id) %>% dplyr::select(ncol(features), 2:ncol(features) - 1)
  print("Scaling the features data completed")
  outliers <- integer()
  # check if k_override is sent or not
  if(is.na(k_override)){
    print("Selecting Best Cluster Assignment")
    tryagain = TRUE
    while(tryagain){
      SSE <- NULL   # Initialize Sum of Square Error vector
      DB <- NULL    # Initialize Davies-Bouldin index vector
      KC <- list()  # Initialize kc k-means container
      for (k in 3:k_max){
        KC[[k]] <- kmeans(features_scaled[,-1],k,nstart = 50,iter.max = 50)
        SSE[k] <- KC[[k]]$tot.withinss
        DB[k] <- index.DB(features_scaled[,-1],KC[[k]]$cluster,centrotypes = "centroids")$DB
      }
      # Plot SSE and DB index to decide
      par(mfrow = c(2,1))
      plot(SSE,type = "o",xlab = "K values", ylab = "Sum of Square Error")
      plot(DB, type = "o", xlab = "K Values", ylab = "Davies-Bouldin Index")
      par(mfrow = c(1,1))
      k_chosen <- which.min(DB)
      #k_chosen <- which.max(SSE - c(SSE[2:length(SSE)],SSE[length(SSE)])) + 1
      kc <- KC[[k_chosen]]
      print(paste("Best Clusters are ", k_chosen, ", checking outliers"))
      # after each iteration check if any cluster contains few number of members (i.e. outlier cluster)
      outlier_clusts <- which(kc$size <= 0.03/ k_chosen * nrow(features_scaled))
      if (length(outlier_clusts) != 0){
        # remove the service IDs belonging to the outlier clusters and go to next iteration
        outlier_service_ids <- features_scaled$service_id[which(kc$cluster %in% outlier_clusts)]
        print(paste(length(outlier_service_ids), "identified as outlier services IDs, excluding and trying again"))
        outliers <- append(outliers, outlier_service_ids)
        features_scaled <- features_scaled %>% filter(!service_id %in% outlier_service_ids)
      }
      else {
        print(paste("Final Best Clusters are ", k_chosen, ", preparing function returns"))
        tryagain = FALSE
      }
    }
  }
  else{
    # in case k_override was sent, use it directly
    print("Starting Clustering")
    k_chosen <- k_override
    kc <- kmeans(features_scaled[,-1],k_chosen,nstart = 50, iter.max = 50)
    print("Clustering complete, preparing function returns")
  }

  # unscale the centroids
  centers <- data.frame(kc$centers)
  centers_unscaled <- centers
  for (name in names(centers)){
    centers_unscaled[,name] <- (centers[,name] * features_sd[name,1]) + features_mean[name,1]
  }

  # prepare cluster assignment data frame
  clusters <- data.frame("service_id" = features_scaled$service_id, "cluster" = kc$cluster)

  # preparing centroids plot object
  centers <- data.frame(k=1:k_chosen, centers)
  plot_data <- data.frame(melt(centers,id.vars = "k"), dummy = "Centroids")
  plot_object <-
    ggplot(plot_data, aes(variable,value)) + geom_col()+
    facet_grid(k~dummy, switch = "y") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # preparing PCA plot object
  p.comp <- prcomp(features_scaled, scale. = TRUE, center = TRUE)
  colpal <- factor(kc$cluster)
  PCA <- ggplot(data.frame(p.comp$x[,1:2]), aes(PC1,PC2,col=colpal)) +
    geom_point() + labs(color = "Cluster")

  # return values
  return(list("centers_scaled"=centers, "centers_unscaled" = centers_unscaled,
              "k"=k_chosen, "cluster_assignment" = clusters, "plot_object" = plot_object,
              "PCA.plot" = PCA, "outliers" = outliers))

}

# Aux functions to be used to find nearest IDs to cluster centroids (for verification purposes)
# define a function to calculate the euclidean distance between a matrix and a centroid
myEuclid <- function(points1, points2) {
  distanceMatrix <- matrix(double(), nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

# define function to retrieve contracts near the centroids
get_contracts_near_centroids <- function(clust_obj, clust_data = clust_data, howMany){
  ## This function takes a yvw_cluster object and the original clustering data and the number of contracts
  ## to be retrieved (howMany). Then it returns a matrix where each row represents the contract numbers
  ## nearest to the centroids of each cluster.
  clust_data_scaled <- as.matrix(scale(clust_data[,-1]))
  centers_scaled <- matrix(unlist(clust_obj$centers_scaled[,-1]), nrow = clust_obj$k)
  dist <- myEuclid(clust_data_scaled , centers_scaled)
  sorted <- sapply(as.data.frame(dist), function(x){sort(x,index.return = T)$ix})
  return (t(matrix(sapply(sorted[1:howMany,], function(x){clust_data[unlist(x),1]}), ncol = clust_obj$k)))
}
#----------------------------------- End of calculate clusters ---------------------------------------








#####################################  Calculate Consumption aggregates ################################
#' calculate_consumption_aggregates
#' Receives a timeseries list and calculates consumption aggregate values for three modalities (daily, weekly, seasonally)
#' The output can be saved to disk to be used at a later time for benchmarking using radar charts.
#' @param timeseries_list : a list of xts timeseries objects containing customer meter data
#' @param filename : optional argument with the filename to save to disk, if left NA, the aggregates won't be saved
#'
#' @return the function returns a list of three dataframes: daily_aggregate, weekly_aggregate, and sesonal_aggregate
#' @export
#'
#' @examples
calculate_consumption_aggregates <- function(timeseries_list, filename = NA){

  # Contract Series Data
  meters <- timeseries_list # xts list
  meters <- meters[which(sapply(meters, nrow) != 0)]

  meters_df <- lapply(1:length(meters), function(contract){
    series <- fortify(meters[[contract]])
    colnames(series) <- c("time", "volume")
    series$contract_id <- rep(names(meters)[contract])
    series
  })


  # Aggregate and extract time usage patterns
  print("Aggregating time usage patterns")
  meters_df <- bind_rows(meters_df)
  meters_df$DayofWeek <- weekdays(meters_df$time)
  meters_df$Hour <- hour(meters_df$time)
  meters_df$Month <- month(meters_df$time)
  meters_df$weekday <- ifelse(meters_df$DayofWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  meters_df$timeofday <- sapply(meters_df$Hour, function(x){
    if(x %in% 6:9) return("Morning")
    if(x %in% 10:17) return("Day")
    if(x %in% 18:21) return("Evening")
    if(x %in% 22:23 || x %in% 0:5)  return("Night")
  })

  meters_df$season <- sapply(meters_df$Month, function(x){
    if(x %in% 6:8) return("Winter")
    if(x %in% 12 || x %in% 1:2)  return("Summer")
    if(x %in% 3:5)  return("Autumn")
    if(x %in% 9:11)  return("Spring")
  })

  print("Calculating Weekly Aggregates")
  meters_weekly_agg <- group_by(meters_df, contract_id, DayofWeek) %>%
    summarise(Mean_con = mean(volume, na.rm = T)) %>%
    arrange(contract_id, DayofWeek) %>% data.frame()

  print("Calculating Daily Aggregates")
  meters_daily_agg <- group_by(meters_df, contract_id, timeofday) %>%
    summarise(Mean_con = mean(volume, na.rm = T)) %>%
    arrange((contract_id), timeofday) %>% data.frame()

  print("Calculating Seasonal Aggregates")
  meters_season_agg <- group_by(meters_df, contract_id, season) %>%
    summarise(Mean_con = mean(volume, na.rm = T)) %>%
    arrange((contract_id), (season)) %>% data.frame()

  aggregates_list <- list("meters_weekly_agg" = meters_weekly_agg, "meters_daily_agg" = meters_daily_agg,
                          "meters_season_agg" = meters_season_agg)

  if(!is.na(filename))saveRDS(aggregates_list, filename)

  return(aggregates_list)

}
#----------------------------------- End of calculate_consumption_aggregates --------------------------------








#####################################  Benchmarking with Radar Charts ############################################
#########################################################################################################
#' Benchmarking
#' This function plots a group of radar charts to benchmark a customer against his/her cluster or to compare the different clusters
#' The comparison/benchmarking is done in terms of consumption averages for three different modalities (daily, weekly, seasonlly)
#' For this function to work, two steps must be completed beforehand:
#' 1) a cluster assignment to be calculated using calculate_clusters function
#' 2) consumption aggregates to be calculated using calculate_consumption_aggregates
#'
#' @param clust_obj : a cluster object returned from calculate_clusters function
#' @param aggregates_list : a list of aggregate consumptions calculated using calculate_consumption_aggregates function
#' @param service_id : ID of the customer to be benchmarked against his/her cluster, if left None, all clusters will be plotted
#' @param plot_title : desired title of the generated plot
#'
#' @return
#' @export
#'
#' @examples
benchmark <- function(clust_obj, aggregates_list, service_id = NULL, plot_title = ""){


  # set cluster assignment based on clust_obj (retrieved from calculate_clusters fucntion)
  clust_assign <- clust_obj$cluster_assignment

  # extract different modalities of aggregates
  meters_weekly_agg <- aggregates_list$meters_weekly_agg
  meters_daily_agg <- aggregates_list$meters_daily_agg
  meters_season_agg<- aggregates_list$meters_season_agg

  if(is.null(service_id)) clust_id <- NULL
  else clust_id <- clust_assign[clust_assign[,1] == service_id,2]



  # Call Functions to plot charts
  p_daily <- dailyCompareClusters(clust_assign, meters_daily_agg, clust_id, service_id, title = plot_title)
  p_weekly <- weeklyCompareClusters(clust_assign, meters_weekly_agg, clust_id, service_id, title = plot_title)
  p_seas <- seasonalCompareClusters(clust_assign, meters_season_agg, clust_id, service_id, title = plot_title)

  return(list("daily" = p_daily, "weekly" = p_weekly, "seasonal" = p_seas))


}
############################  Aux functions for benchmark function #######################################
# Daily
dailyCompareClusters <- function(clusts, meters_daily_agg, cluster_ids = NULL, contracts = NULL, title = ""){

  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    layout(annotations = list(yref='paper',xref="paper",y=1,x=0.5, text=title,showarrow=F, font = list(size = 36)),
           legend = list(
             font = list(size = 18)),
           polar = list(
             radialaxis = list(
               visible = T
             ),
             angularaxis = list(
               tickfont = list(
                 size = 18
               ),
               rotation = 90,
               direction = 'clockwise'
             )
           )
    )

  clust_list <- unique(clusts$cluster)
  if(!is.null(cluster_ids)) clust_list <- cluster_ids

  for(cluster in clust_list){
    cluster_dayuse <- filter(meters_daily_agg, contract_id %in% clusts$service_id[which(clusts$cluster == cluster)]) %>%
      group_by(timeofday) %>%
      summarise(MeanUse = mean(Mean_con))
    cluster_dayuse$timeofday<- factor(cluster_dayuse$timeofday, levels = c('Morning','Day', 'Evening', 'Night'))
    cluster_dayuse <- cluster_dayuse[order(cluster_dayuse$timeofday),]
    print(cluster_dayuse)
    p <-  add_trace(p,
                    r = cluster_dayuse$MeanUse,
                    theta = c('Morning','Day', 'Evening', 'Night'),
                    name = paste0("Cluster ", cluster)
    )
  }

  if(is.null(contracts)) return(p)

  for(contract in contracts){
    contract_dayuse <- group_by(meters_daily_agg, contract_id, timeofday) %>%
      summarise(MeanUse = mean(Mean_con)) %>% filter(contract_id == contract)
    contract_dayuse$timeofday<- factor(contract_dayuse$timeofday, levels = c('Morning','Day', 'Evening', 'Night'))
    contract_dayuse <- contract_dayuse[order(contract_dayuse$timeofday),]

    p <-  add_trace(p,
                    r = contract_dayuse$MeanUse,
                    theta = c('Morning','Day', 'Evening', 'Night'),
                    name = paste0("Contract ", contract)
    )
  }

  return(p)
}
# Weekly
weeklyCompareClusters <- function(clusts, meters_weekly_agg, cluster_ids = NULL, contracts = NULL, title = ""){

  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    layout(annotations = list(yref='paper',xref="paper",y=1,x=0.5, text=title,showarrow=F, font = list(size = 36)),
           legend = list(
             font = list(size = 18)),
           polar = list(
             radialaxis = list(
               visible = T
             ),
             angularaxis = list(
               tickfont = list(
                 size = 18
               ),
               rotation = 90,
               direction = 'clockwise'
             )
           )
    )

  clust_list <- unique(clusts$cluster)
  if(!is.null(cluster_ids)) clust_list <- cluster_ids

  for(cluster in clust_list){
    cluster_dayuse <- filter(meters_weekly_agg, contract_id %in% clusts$service_id[which(clusts$cluster == cluster)]) %>%
      group_by(DayofWeek) %>%
      summarise(MeanUse = mean(Mean_con))
    cluster_dayuse$DayofWeek<- factor(cluster_dayuse$DayofWeek, levels = c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    cluster_dayuse <- cluster_dayuse[order(cluster_dayuse$DayofWeek),]
    print(cluster_dayuse)
    p <-  add_trace(p,
                    r = cluster_dayuse$MeanUse,
                    theta = c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                    name = paste0("Cluster ", cluster)
    )
  }

  if(is.null(contracts)) return(p)

  for(contract in contracts){
    contract_dayuse <- group_by(meters_weekly_agg, contract_id, DayofWeek) %>%
      summarise(MeanUse = mean(Mean_con)) %>% filter(contract_id == contract)
    contract_dayuse$DayofWeek<- factor(contract_dayuse$DayofWeek, levels = c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    contract_dayuse <- contract_dayuse[order(contract_dayuse$DayofWeek),]

    p <-  add_trace(p,
                    r = contract_dayuse$MeanUse,
                    theta = c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                    name = paste0("Contract ", contract)
    )
  }

  return(p)
}
# Seasons
seasonalCompareClusters <- function(clusts, meters_season_agg, cluster_ids = NULL, contracts = NULL, title = ""){

  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    layout(annotations = list(yref='paper',xref="paper",y=1,x=0.5, text=title,showarrow=F, font = list(size = 36)),
           legend = list(
             font = list(size = 18)),
           polar = list(
             radialaxis = list(
               visible = T
             ),
             angularaxis = list(
               tickfont = list(
                 size = 16
               ),
               rotation = 90,
               direction = 'clockwise'
             )
           )
    )

  clust_list <- unique(clusts$cluster)
  if(!is.null(cluster_ids)) clust_list <- cluster_ids

  for(cluster in clust_list){
    cluster_dayuse <- filter(meters_season_agg, contract_id %in% clusts$service_id[which(clusts$cluster == cluster)]) %>%
      group_by(season) %>%
      summarise(MeanUse = mean(Mean_con))
    cluster_dayuse$season<- factor(cluster_dayuse$season, levels = c('Summer','Autumn', 'Winter', 'Spring'))
    cluster_dayuse <- cluster_dayuse[order(cluster_dayuse$season),]
    print(cluster_dayuse)
    p <-  add_trace(p,
                    r = cluster_dayuse$MeanUse,
                    theta = c('Summer','Autumn', 'Winter', 'Spring'),
                    name = paste0("Cluster ", cluster)
    )
  }

  if(is.null(contracts)) return(p)

  for(contract in contracts){
    contract_dayuse <- group_by(meters_season_agg, contract_id, season) %>%
      summarise(MeanUse = mean(Mean_con)) %>% filter(contract_id == contract)
    contract_dayuse$season<- factor(contract_dayuse$season, levels = c('Summer','Autumn', 'Winter', 'Spring'))
    contract_dayuse <- contract_dayuse[order(contract_dayuse$season),]

    p <-  add_trace(p,
                    r = contract_dayuse$MeanUse,
                    theta =  c('Summer','Autumn', 'Winter', 'Spring'),
                    name = paste0("Contract ", contract)
    )
  }

  return(p)
}
#################################### End of Benchmarking with Radar Charts ##################################
