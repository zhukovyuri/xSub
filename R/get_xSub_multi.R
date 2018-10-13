#' Get xSub files for multiple countries
#'
#' This function downloads and merges mutiple country files from \code{www.x-sub.org}. Syntax is similar to \code{get_xSub()}.
#'
#' @param data_source Name of data source. Character string. See \code{info_xSub()} for full list.
#' @param sources_type Type of data sources ("individual" or "multiple"). Character string.
#' @param data_type Type of dataset ("event" or "panel"). Character string.
#' @param country_iso3 Country codes (ISO3). Character string or vector. See \code{info_xSub()} for full list. If left blank, function will download all available countries for selected data source.
#' @param space_unit Geographic level of analysis. Character string. Can be one of \code{"adm0"} (country), \code{"adm1"} (province), \code{"adm2"} (district), \code{"priogrid"} (grid cell), \code{"clea"} (electoral constituency). See \code{info_xSub(details=TRUE)} for availability by country.
#' @param time_unit Temporal level of analysis. Character string. Can be one of \code{"year"}, \code{"month"}, \code{"week"}, \code{"day"}. See \code{info_xSub(details=TRUE)} for availability by country.
#' @param geo_window Geographic window (if source_type="multiple"). Could be either of "1 km" or "5 km". Character string or vector.
#' @param time_window Time window (if source_type="multiple"). Could be either of "1 day" or "2 day". Character string or vector.
#' @param dyad_type Time window (if source_type="multiple"). Could be either of "undirected" or "directed". Character string or vector.
#' @param merge_files Logical. If \code{merge_files=TRUE} (default), function will combine individual country files into single data.frame, and write single file to disk. If \code{merge_files=FALSE}, function produces a list, and writes individual country files to disk separately.
#' @param out_dir Path to directory where files will be saved. Character string.
#' @param write_file Logical. If \code{write_file=TRUE}, selected file will be written to disk, at location specified by \code{out_dir}.
#' @param write_format Output file format. Character string. Can be one of \code{"csv"} (comma-separated values, default), \code{"R"} (RData format, compatible with R statistical programming language), \code{"STATA"} (dta format, compatible with Stata 14).
#' @param verbose Logical. When \code{verbose=TRUE}, file download progress is printed to console..
#' @export
#' @seealso \code{\link{info_xSub}}, \code{\link{get_xSub}}
#' @examples
#' @import haven RCurl countrycode
#' @importFrom utils data download.file read.csv unzip write.csv globalVariables
#' @export
#' @seealso \code{\link{info_xSub}}, \code{\link{get_xSub}}
#' @examples
#' # Check which countries are available for GED
#' info_xSub(data_source="GED")
#'
#' # Example with two countries
#' my_file <- get_xSub_multi(data_source = "PITF",country_iso3 = c("ALB","ARM"),
#'            space_unit = "adm0",time_unit = "year")
#'
#' # Example with two countries
#' \dontrun{
#' my_file <- get_xSub_multi(data_source = "GED",country_iso3 = c("EGY","AGO"),
#'            space_unit = "adm1",time_unit = "month")
#' }
#'
#' # Example with two countries, multiple sources, event-level
#' \dontrun{
#' my_file <- get_xSub_multi(sources_type = "multiple",data_type="event",country_iso3 = c("EGY","AGO"))
#' }
#'
#' # Example with all countries (WARNING: this can take a long time to run)
#' \dontrun{
#' my_file <- get_xSub_multi(data_source = "BeissingerProtest",country_iso3 = NULL,
#'            space_unit = "adm0",time_unit = "year")
#' }

get_xSub_multi <- function(data_source,sources_type="individual",data_type="spatial panel",
                           country_iso3=NULL,space_unit,time_unit,
                           geo_window="1 km",time_window="1 day",dyad_type="undirected",
                           merge_files=TRUE,out_dir=getwd(),write_file=FALSE,
                           write_format="csv",verbose=FALSE){

  # # Load dependencies
  # list.of.packages <- c("RCurl","countrycode"); new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}; lapply(list.of.packages, require, character.only = TRUE)


  print("Assembling file list...")
  # Check which files exist
  url_dir <- "http://cross-sub.org/api/data/"
  if(length(country_iso3)>0){cntz <- country_iso3}
  if(length(country_iso3)==0){
    cntz <- sort(unique(countrycode_data$iso3c))
  }

  if((grepl("^i|^I",sources_type)&grepl("^s|^S|^p|^P",data_type))){
    file_urlz <- paste0(url_dir,"xSub_",data_source,"_",cntz,"_",space_unit,"_",time_unit,".zip")
  }

  if(!(grepl("^i|^I",sources_type)&grepl("^s|^S|^p|^P",data_type))){
    if((grepl("^i|^I",sources_type)&grepl("^e|^E",data_type))){
      file_urlz <- paste0(url_dir,"xSub_",data_source,"_",cntz,"_event.zip")
    }
    if((grepl("^m|^M",sources_type)&grepl("^e|^E",data_type))){
      if(length(geo_window)==0|length(time_window)==0|length(dyad_type)==0){stop('Please specify geo_window (\"1 km\" or \"5 km\"), time_window (\"1 day\" or \"2 day\") and dyad_type (\"undirected\" or \"directed\").')}
      if(length(geo_window)>0&length(time_window)>0&length(dyad_type)>0){
        gwz <- ifelse(grepl("^1",geo_window),"1km",ifelse(grepl("^5",geo_window),"5km",stop("geo_window can only be \"1 km\" or \"5 km\"")))
        twz <- ifelse(grepl("^1",time_window),"1d",ifelse(grepl("^2",time_window),"2d",stop("time_window can only be \"1 day\" or \"2 day\"")))
        dyz <- ifelse(grepl("^u|^U",dyad_type),"B",ifelse(grepl("^d|^D",dyad_type),"A",stop("dyad_type can only be \"undirected\" or \"directed\"")))
        data_source <- paste0("MELTT",gwz,twz,dyz)
        file_urlz <- paste0(url_dir,"xSub_",data_source,"_",cntz,"_event.zip")
      }
    }
    if((grepl("^m|^M",sources_type)&grepl("^s|^S|^p|^P",data_type))){
      if(length(geo_window)==0|length(time_window)==0|length(dyad_type)==0){stop('Please specify geo_window (\"1 km\" or \"5 km\"), time_window (\"1 day\" or \"2 day\") and dyad_type (\"undirected\" or \"directed\").')}
      if(length(geo_window)>0&length(time_window)>0&length(dyad_type)>0){
        gwz <- ifelse(grepl("^1",geo_window),"1km",ifelse(grepl("^5",geo_window),"5km",stop("geo_window can only be \"1 km\" or \"5 km\"")))
        twz <- ifelse(grepl("^1",time_window),"1d",ifelse(grepl("^2",time_window),"2d",stop("time_window can only be \"1 day\" or \"2 day\"")))
        dyz <- ifelse(grepl("^u|^U",dyad_type),"B",ifelse(grepl("^d|^D",dyad_type),"A",stop("dyad_type can only be \"undirected\" or \"directed\"")))
        data_source <- paste0("MELTT",gwz,twz,dyz)
        file_urlz <- paste0(url_dir,"xSub_",data_source,"_",cntz,"_",space_unit,"_",time_unit,".zip")
      }
    }
  }


  exist_urlz <- c()
  for (n in seq_along(file_urlz)){
    z <- ""
    try(z <- getBinaryURL(file_urlz[n], failonerror = TRUE))
    if(length(z) > 1){exist_urlz[n] <- TRUE}
    else{exist_urlz[n] <- FALSE}
  }
  country_iso3 <- cntz[exist_urlz]

  # Loop
  print("Downloading files...")
  xSub_list <- lapply(seq_along(country_iso3),function(j){
    get_xSub(data_source = data_source,data_type = data_type,country_iso3 = country_iso3[j],space_unit = space_unit,time_unit = time_unit,out_dir = out_dir,write_file = (!merge_files),write_format = write_format,verbose = verbose)
  })


  # Merge
  if(merge_files){
    print("Merging files...")
    xSub_file <- do.call(rbind,xSub_list)

    # Write file
    if(write_file){
      print("Writing to disk...")

      if(grepl("^s|^S|^p|^P",data_type)){
      file_name2 <- paste0("xSub_",data_source,"_",space_unit,"_",time_unit,".csv")
      }
      if(grepl("^e|^E",data_type)){
        file_name2 <- paste0("xSub_",data_source,"_event.csv")
      }

      if(write_format=="csv"){
        write.csv(xSub_file,file=paste0(out_dir,"/",file_name2),fileEncoding = "UTF-8",row.names = FALSE)
      }
      if(write_format%in%c("R","RData")){
        save(xSub_file,file=paste0(out_dir,"/",gsub("csv$","RData",file_name2)))
      }
      if(write_format%in%c("STATA","stata","dta")){
        # Load (or install) dependencies
        # list.of.packages <- c("haven"); new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}; lapply(list.of.packages, require, character.only = TRUE)
        write_dta(data=xSub_file,path = paste0(out_dir,"/",gsub("csv$","dta",file_name2)),version = 14)
      }
    }

    # Return object
    return(xSub_file)
  }
  else(
    return(xSub_list)
  )


}
