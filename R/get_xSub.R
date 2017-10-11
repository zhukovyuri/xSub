#' Get xSub file
#'
#' This function downloads individual files from \code{www.x-sub.org}. Function produces a data.frame, for the user's choice of data source, country, spatial and temporal units, and (optionally) writes this data.frame to disk, in multiple formats.
#'
#' @param data_source Name of data source. See \code{info_xSub()} for full list.
#' @param country_iso3 Country code (ISO3). See \code{info_xSub()} for full list.
#' @param country_name Country name. See \code{info_xSub()} for full list.
#' @param space_unit Geographic level of analysis. Character string. Can be one of \code{"adm0"} (country), \code{"adm1"} (province), \code{"adm2"} (district), \code{"priogrid"} (grid cell), \code{"clea"} (electoral constituency). See \code{info_xSub(details=TRUE)} for availability by country.
#' @param time_unit Temporal level of analysis. Character string. Can be one of \code{"year"}, \code{"month"}, \code{"week"}, \code{"day"}. See \code{info_xSub(details=TRUE)} for availability by country.
#' @param out_dir Path to directory where files will be saved. Character string. Default is R session’s temporary directory, \code{tempdir()}.
#' @param write_file Logical. If \code{write_file=TRUE}, selected file will be written to disk, at location specified by \code{out_dir}.
#' @param write_format Output file format. Can be one of \code{"csv"} (comma-separated values, default), \code{"R"} (RData format, compatible with R statistical programming language), \code{"STATA"} (dta format, compatible with Stata 14).
#' @param verbose Logical. When \code{verbose=TRUE}, file download progress is printed to console.
#' @import haven RCurl countrycode
#' @importFrom utils data download.file read.csv unzip write.csv globalVariables
#' @export
#' @seealso \code{\link{info_xSub}}, \code{\link{get_xSub_multi}}
#' @examples
#' # Check which countries are available for ACLED
#' info_xSub(data_source="ACLED")
#'
#' # Download ACLED data for Egypt, at country-year level
#' my_file <- get_xSub(data_source = "ACLED",country_iso3 = "EGY",
#'            space_unit = "adm0",time_unit = "year")
#'
#' # Download ACLED data for Egypt, at district-month level
#' \dontrun{
#' my_file <- get_xSub(data_source = "ACLED",country_iso3 = "EGY",
#'            space_unit = "adm2",time_unit = "month")
#' }
#'
#' # With country name instead of ISO3 code
#' \dontrun{
#' my_file <- get_xSub(data_source = "ACLED",country_name = "Egypt",
#'            space_unit = "adm2",time_unit = "month")
#' }

get_xSub <- function(data_source,country_iso3=NULL,country_name=NULL,space_unit,time_unit,out_dir=tempdir(),write_file=TRUE,write_format="csv",verbose=FALSE){

  # Load dependencies
  sapply(list("countrycode","haven","RCurl"),function(x){library(x,character.only = TRUE)})

  # Units of analysis
  space.agg <- c("adm0","adm1","adm2","priogrid","clea")
  time.agg <- c("year","month","week","day")
  space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
  time.ix <- c("YEAR","YRMO","WID","TID")

  # Parent directory
  url_dir <- "http://cross-sub.org/download/file/"

  # Country name
  if(length(country_iso3)==0&length(country_name)>0){
    country_iso3 <- countrycode::countrycode(country_name,"country.name","iso3c")
  }

  # File name
  file_name <- paste0("xSub_",data_source,"_",country_iso3,"_",space_unit,"_",time_unit,".csv")
  file_url <- paste0(url_dir,file_name)

  # Download and unzip
  if(Sys.info()['sysname']!="Windows"){
    temp <- tempfile()
    download.file(file_url,temp,cacheOK = TRUE,quiet = (!verbose))
    xSub_file <- read.csv(unzip(temp, files=file_name))
    unlink(temp)
    if(file.exists(file_name)){file.remove(file_name)}
  }
  if(Sys.info()['sysname']=="Windows"){
    temp <- tempfile()
    # gsub("csv$","RData",file_name)
    # download.file(file_url,destfile="TEMP.zip")
    # unz("TEMP.zip")
    download.file(file_url,temp,cacheOK = TRUE,quiet = (!verbose),mode="wb")
    xSub_file <- read.csv(unzip(temp, files=file_name))
    unlink(temp)
    if(file.exists(file_name)){file.remove(file_name)}
  }

  # Add source column
  xSub_file$SOURCE <- data_source[1]
  xSub_file <- xSub_file[,c(ncol(xSub_file),1:(ncol(xSub_file)-1))]

  # Write file
  if(write_file){
    if(write_format=="csv"){
      write.csv(xSub_file,file=paste0(out_dir,"/",file_name),fileEncoding = "UTF-8",row.names = FALSE)
    }
    if(write_format%in%c("R","RData")){
      save(xSub_file,file=paste0(out_dir,"/",gsub("csv$","RData",file_name)))
    }
    if(write_format%in%c("STATA","stata","dta")){
      write_dta(data=xSub_file,path = paste0(out_dir,"/",gsub("csv$","dta",file_name)),version = 14)
    }
  }

  # Return object
  return(xSub_file)

}
