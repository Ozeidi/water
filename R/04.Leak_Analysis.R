

#' leak_detection
#'
#' This function conducts leak detection on a series or list of series using Min or MEU  method and return results into csv files and/or pdf
#' @param timeseries a single or a list of xts objects to be investigated for leak detection
#' @param method argument for the detection method to be conducted
#' @param model_params optional list of model parameters to be used for MEU method, if not provided the model will calculate the parameters and use them for detection
#' @param days number of days to be considered for Min method
#' @param id optional name of contract id, to be passed in case of conducting detection on single contract
#' @param pdf_file name of the pdf file to be used for ouptuing the leak results in a visual form. Defaults to NA, where no pdf will be outputed
#' @param out_csv boolean flag indicates whether results should be outputed into csv. Defaults to TRUE
#' @param return_params boolean to indicate if the MEU parameters should be returned or not. Defaults to FALSE
#'
#' @return CSV and/or PDF with leak results
#' @export
#'
#' @examples \dontrun{leak_detection( contracts, method= 'meu', pdf = 'Vis', out_csv = TRUE, return_params =FALSE)}
leak_detection <- function(timeseries, method = c("min", "meu"), model_params = NA, days=2, id = NA,
                           pdf_file = NA, out_csv = TRUE, return_params = FALSE){
  if(method == 'min'){
    output <- leak_detection_min(timeseries, mID=id, days=days)

  }
  else if (method == 'meu'){
    output <- leak_detection_MEU(timeseries, model_params = model_params, id = id)
  }

  # MIN METHOD
  if (out_csv & method == 'min'){
    for (id in names(output)){
      if (nrow(output[[id]]) >0 ){
        write.csv(output[[id]], paste0(id,'_min_method_leaks.csv'),row.names = FALSE)
      }else{
        print(paste0('There is no leak in contract: ', id))
      }
    }
    print(paste0('All csv files outputed in: ', getwd()))
  }

  if (!is.na(pdf_file) & method == 'min'){
    plt <- leak_plot(output)
    filePath <- file.path(getwd(),paste0(pdf_file,'.pdf'))
    pdf(filePath)
    print(plt)
    dev.off()
    print(paste0('Plots are plotted in the following file: ',filePath))
    system(paste0('open ', filePath))
  }
  # MEU METHOD

  if (out_csv & method == 'meu'){
    for (id in 1:length(output)){
      leak_flag <- output[[id]]$leak_flag
      if (nrow(leak_flag)>0){
        write.csv( leak_flag, paste0(id,'_meu_method_leaks.csv'),row.names = FALSE)
      } else{
        print(paste0('There is no leak in contract: ', id))
      }

    }
    print(paste0('All csv files ouputed in: ', getwd()))
  }

  if (!is.na(pdf_file) & method == 'meu'){
    leak_flag <- list()
    for (id in 1:length(output)){
      leak_flag[[as.character(id)]] <- output[[id]]$leak_flag

    }
    plt <- leak_plot(leak_flag)
    filePath <- file.path(getwd(),paste0(pdf_file,'.pdf'))
    pdf(filePath)
    print(plt)
    dev.off()
    print(paste0('Plots are plotted in the following file: ',filePath))
    system(paste0('open ', filePath))
  }

}

