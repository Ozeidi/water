#------------- Utility Function to create DataFrame Out of XTS object---------------------------
create_df <- function (df,year=NA){
  #create dataframe
  #Note:  #Summer months below are as per BOM classification for Spring/Summer in Australia.
  #Refer to this for details: http://www.bom.gov.au/climate/glossary/seasons.shtml
  df<-tibble(Date_Time= index(df), Hourly_Consumption =coredata(df)[,])%>%
    mutate(Date= lubridate::date(Date_Time),Year=lubridate::year(Date_Time), Month=lubridate::month(Date_Time,label=T,abb=F),
           Mdate=lubridate::day(Date_Time),Day = factor(weekdays(Date), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), Time= hour(Date_Time)) %>%
           {if (!is.na(year))filter(Year==year) else .} %>% na.omit() %>%
    mutate (Weekend= if_else(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%
    mutate(Season= if_else(Month %in% c('Sep','Oct', 'Nov','Dec','Jan',
                                                                 'Feb'), 'Summer', 'Winter'))
}



#------------- Calendar Plot---------------------------
#' consumption_cal
#'
#'Plots  consumption as a calendar plot. This can help to see the patterns in consumption at multiple granularity levels
#' @param df  xts object of the meter consumption
#' @param mID meterID to be used in the plot title
#' @param year the year to be plotted. default NA which plots all years available for the contract
#'
#' @return calendar plot
#'
#' @examples \dontrun{ consumption_cal(xts, mID='1234', year=2016)}
consumption_cal<- function(df,mID='', year=NA){
  #if the object given is not dataframe then create a dataframe
  if (typeof(df)!='list'){
    df<-create_df(df)
  }
  df<-df %>% {if (!is.na(year)) filter(.,Year==year) else .} %>% na.omit()
  if (nrow(df)!=0){
    df<- df %>%  frame_calendar( x = Time, y = Hourly_Consumption, date = Date, calendar = "monthly")
  }else{
    return('No consumption in this year')
  }

  p <- df%>%
    ggplot(aes(x = .Time, y = .Hourly_Consumption, group = Date, colour = Weekend)) +
    geom_line(size=1) +
    ggtitle(paste0("ContractID ",mID,", ",year, ' Consumption Tile'))+
    theme(legend.position = "bottom")
  plt <-prettify(p, label.padding = unit(0.001, "lines"))
  plt

}



#------------- Calendar Heatmap---------------------------
#' consumption_cal_HM
#'
#' plots the consumption as a heat map over calendar plot.
#' @param df xts object of the meter consumption
#' @param mID meterID to be used in the plot title
#' @param year the year to be plotted. default NA which plots all years available for the contract
#' @param scale type of scale used for the heatmap, continues or discrete
#' @param fixed_scale Boolean, to indicate whether the scale should be fixed for all the plots or change dyanmically per plot. We recommend keeping the scale fixed to enable comparison over different contracts. Default is TRUE
#'
#' @return consumption heatmap over calendar plot
#'
#' @examples \dontrun{consumption_cal_HM(contract1, mID='1254', year=2016, scale= 'continuous',fixed_scale=TRUE )}
consumption_cal_HM<- function(df, mID='', year=NA, scale= 'continuous',fixed_scale=TRUE){
  #if the object give is not dataframe then create a dataframe
  if (typeof(df)!='list'){
    df<-create_df(df)
  }
  df <- df %>% {if (!is.na(year)) filter(.,Year==year) else .} %>% na.omit()
  df <- df %>% group_by(Date) %>%summarise(Daily_Con=sum(Hourly_Consumption)) %>% ungroup()

  if (nrow(df)!=0){
    df<-df %>% frame_calendar(x=1,y=1, date=Date, calendar="monthly")
  }else{
    return('No consumption in this year')
  }

  #create a new variable from incidence
  df$ConsumptionFactor <- cut(df$Daily_Con,
                              breaks = c(-1,0,10,100,300,500,1000,max(df$Daily_Con,1001,na.rm=T)),
                              labels=c("0","0-10","10-100","100-300","300-500","500-1000",">1000"))
  df$ConsumptionFactor <- factor(as.character(df$ConsumptionFactor),
                                 levels=rev(levels(df$ConsumptionFactor)))
  #Create the base of the plot
  p <- df%>%ggplot(aes(x = .x, y = .y)) +
    ggtitle(paste0("ContractID ",mID,", ",year, ' Consumption Tile'))+
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5,vjust = 0.5))

  # Create the heatmap based on continuous or discrete scale
  if (scale=='continuous'){

    p<- p + geom_tile(aes(fill = Daily_Con), colour = "grey50") +
      scale_fill_gradient2(low='green',mid='yellow', high='red')

  }else if(scale =='discrete'){
    # p <- p+ geom_tile(aes(fill = (ConsumptionFactor)), colour = "grey50") +
    #  scale_fill_manual(name = 'Consumption',values=rev(brewer.pal(7,"YlOrRd")),na.value="grey90")

    p <- p+ geom_tile(aes(fill = (ConsumptionFactor)), colour = "grey50")
    if (fixed_scale==TRUE){
      p <- p + scale_fill_manual(name = 'Consumption',values=rev(brewer.pal(7,"YlOrRd")),
                                 limits=levels(df$ConsumptionFactor))
    }else{
      p <- p + scale_fill_manual(name = 'Consumption',values=rev(brewer.pal(7,"YlOrRd")))
    }
  }

  plt <-prettify(p, label.padding = unit(0.001, "lines"))
  plt

}


#------------- Consumption Heat Plot (Time Vs Day Tiled Plot)---------------------------
#' consumption_heat
#'
#' THis plot generates consumption heat tile, with date on the x-axis, hour of the day on the y-axis and consumption depicted as coloured tile.
#' @param df xts object of the meter consumption
#' @param mID meterID to be used in the plot title
#' @param year the year to be plotted. default NA which plots all years available for the contract
#' @param scale type of scale used for the heatmap, continues or discrete
#' @param fixed_scale Boolean, to indicate whether the scale should be fixed for all the plots or change dyanmically per plot. We recommend keeping the scale fixed to enable comparison over different contracts. Default is TRUE
#'
#'
#' @return Consumption heat plot
#'
#' @examples \dontrun{Consumption_Heat(rds$`1_101510`, mID= " ",year=NA,scale = 'discrete',fixed_scale = TRUE)}
consumption_heat<- function(df,mID='', year=NA,scale = 'continuous', fixed_scale=TRUE){
  #if the object give is not dataframe then create a dataframe
  if (typeof(df)!='list'){
    df<-create_df(df)
  }

  df<-df %>% {if (!is.na(year)) filter(.,Year==year) else .} %>% na.omit()
  #create a new variable from incidence
  df$ConsumptionFactor <- cut(df$Hourly_Consumption,
                              breaks = c(-1,0,10,100,300,500,1000,max(df$Hourly_Consumption,1001,na.rm=T)),
                              labels=c("0","0-10","10-100","100-300","300-500","500-1000",">1000"))
  df$ConsumptionFactor <- factor(as.character(df$ConsumptionFactor),
                                 levels=rev(levels(df$ConsumptionFactor)))
  # geom_tile(aes(fill = Hourly_Consumption)) +
  #   scale_fill_manual(values=rev(brewer.pal(7,"YlGnBu")),na.value="grey90")+
  p <- df%>%
    ggplot(aes(x = Date, y =Time)) +
    ggtitle(paste0("ContractID ",mID,", ",{if (!is.na(year)) year else 'All years'}, ' Consumption Heat Plot'))+
    theme(legend.position = "bottom")+ ylim(0,24)

  # Create the heatmap based on continuous or discrete scale
  if (scale=='continuous'){
    p<- p + geom_tile(aes(fill = Hourly_Consumption)) +
      scale_fill_gradient2(low='green',mid='yellow', high='red')

  }else if(scale =='discrete'){

    p <- p+ geom_tile(aes(fill = (ConsumptionFactor)))
    if (fixed_scale==TRUE){
      p <- p + scale_fill_manual(name = 'Consumption',values=rev(brewer.pal(7,"YlOrRd")),
                                 limits=levels(df$ConsumptionFactor))
    }else{
      p <- p + scale_fill_manual(name = 'Consumption',values=rev(brewer.pal(7,"YlOrRd")))
    }

  }

  p+scale_y_continuous(expand = c(0, 0)) + scale_x_date(date_labels="%b %y",expand = c(0,0))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

}


#------------- Daily Consumption on facet Plot---------------------------
#' consumption_day
#'
#' return faceted line chart of consumption per hour of the day for every weekday on summer and winter
#' @param df xts object of the meter consumption
#' @param mID meterID to be used in the plot title
#' @param year the year to be plotted. default NA which plots all years available for the contract
#'
#' @return faceted line chart plot
#'
#' @examples \dontrun{consumption_day(rds$'11', mID='11', year=2014)}
consumption_day<- function(df,mID='', year=NA){
  #if the object give is not dataframe then create a dataframe
  if (typeof(df)!='list'){
    df<-create_df(df)
  }
  df <- df %>% {if (!is.na(year)) filter(.,Year==year) else .} %>% na.omit()
  #Plotting
  df %>% ggplot( aes(x=Time, y=Hourly_Consumption))+facet_grid(Season~Day)+
    stat_summary(geom="ribbon", fun.data=mean_cl_normal,
                 fun.args=list(conf.int=0.95), fill="darkgrey")+
    stat_summary(geom="line", fun.y=mean, linetype="solid",color="red", size=1)

}
#------------- Diagnose Consumption : Wraper for all functions above---------------------------
#---------------------Plotting-----------------------------
#--------------------------

#' consumption_vis
#'
#' Generate diagnostic plots to analyse the consumption patterns
#' @param df xts object of the meter consumption
#' @param contracts_list list of contracts to be investigated
#' @param scale type of scale used for the heatmap, continues or discrete
#' @param fixed_scale Boolean, to indicate whether the scale should be fixed for all the plots or change dyanmically per plot. We recommend keeping the scale fixed to enable comparison over different contracts. Default is TRUE.
#' @param file_name name of the file to be used for outputing the plotting results
#' @param path name of the path where plots will be outputed
#' @param output_format png or pdf format
#'
#' @return pdf of png output with the results
#' @export
#'
#' @examples \dontrun{consumption_vis(contract_lst, contract_list= NA, scale= 'discrete', fixed_scale= TRUE)}
consumption_vis <- function(df, contracts_list= NA, scale='discrete', fixed_scale =TRUE,
                            file_name='p', path=getwd(), output_format=c("png", "pdf")){

  plt <- list()
  if (is.na(contracts_list)){
    contracts_list <- names(df)
  }else{
    contracts_list <- as.character(contracts_list)
  }

  for (i in contracts_list){
    dfi <- create_df(df[[i]])
    print(i)
    plt[[paste0(i,'S')]] <- autoplot(df[[i]], main=paste0('Contract ID: ',i, ', Water consumption time series'))+xlab('Time')+
      ylab('Consumption (lt)')
    yrs <- unique(dfi$Year)
    for (y in yrs){
      plt[[paste0(i,'_Cal_',y)]]<-consumption_cal(dfi, mID =i, year = y)
      plt[[paste0(i,'_Day_',y)]]<-consumption_day(dfi, mID =i, year = y)
      plt[[paste0(i,'_Heat_',y)]]<-consumption_heat(dfi, mID =i, year = y,scale=scale,fixed_scale)
      plt[[paste0(i,'_HM_',y)]]<-consumption_cal_HM(dfi, mID =i, year = y,scale=scale,fixed_scale)

      if(output_format=="png") {
        for(name in names(plt)) {
          filePath <- file.path(path,paste0(file_name,name,'.png'))
          ggsave(filePath, plot=plt[[name]])
        }
        plt <- list()
      }

    }
  }


  if(output_format=="pdf") {
    filePath <- file.path(path,paste0(file_name,'.pdf'))
    pdf(filePath)
    print(plt)
    dev.off()
  }

  print(paste0('Plot are plotted in the following file: ',filePath))
  system(paste0('open ', filePath))
}


#TO_ADD Report generator function
#No Longer Necessary
