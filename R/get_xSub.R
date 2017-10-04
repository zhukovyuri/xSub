#' Get xSub file
#'
#' This function downloads individual files from \code{www.x-sub.org}. Function produces a data.frame, for the user's choice of data source, country, spatial and temporal units, and (optionally) writes this data.frame to disk, in multiple formats.
#'
#' @param data_source Name of data source. See \code{info_xSub()} for full list.
#' @param country_iso3 Country code (ISO3). See \code{info_xSub()} for full list.
#' @param country_name Country name. See \code{info_xSub()} for full list.
#' @param space_unit Geographic level of analysis. Character string. Can be one of \code{"adm0"} (country), \code{"adm1"} (province), \code{"adm2"} (district), \code{"priogrid"} (grid cell), \code{"clea"} (electoral constituency). See \code{info_xSub(details=TRUE)} for availability by country.
#' @param time_unit Temporal level of analysis. Character string. Can be one of \code{"year"}, \code{"month"}, \code{"week"}, \code{"day"}. See \code{info_xSub(details=TRUE)} for availability by country.
#' @param out_dir Path to directory where files will be saved.
#' @param write_file Logical. If \code{write_file=TRUE}, selected file will be written to disk, at location specified by \code{out_dir}.
#' @param write_format Output file format. Can be one of \code{"csv"} (comma-separated values, default), \code{"R"} (RData format, compatible with R statistical programming language), \code{"STATA"} (dta format, compatible with Stata 14).
#' @param verbose Logical. When \code{verbose=TRUE}, file download progress is printed to console.
#' @export
#' @seealso \code{\link{info_xSub}}, \code{\link{get_xSub_multi}}
#' @examples
#' # Check which countries are available for ACLED
#' info_xSub(data_source="ACLED")
#'
#' # Download ACLED data for Egypt, at province-month level
#' my_file <- get_xSub(data_source = "ACLED",country_iso3 = "EGY",
#'            space_unit = "adm1",time_unit = "month")
#'
#' # With country name instead of ISO3 code
#' my_file <- get_xSub(data_source = "ACLED",country_name = "Egypt",
#'            space_unit = "adm1",time_unit = "month")

get_xSub <- function(data_source,country_iso3=NULL,country_name=NULL,space_unit,time_unit,out_dir=getwd(),write_file=TRUE,write_format="csv",verbose=FALSE){

  # Units of analysis
  space.agg <- c("adm0","adm1","adm2","priogrid","clea")
  time.agg <- c("year","month","week","day")
  space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
  time.ix <- c("YEAR","YRMO","WID","TID")

  # Parent directory
  url_dir <- "http://cross-sub.org/download/file/"

  # Country name
  if(length(country_iso3)==0&length(country_name)>0){
    list.of.packages <- c("countrycode"); new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}; lapply(list.of.packages, require, character.only = TRUE)
    country_iso3 <- countrycode::countrycode(country_name,"country.name","iso3c")
  }

  # File name
  file_name <- paste0("xSub_",data_source,"_",country_iso3,"_",space_unit,"_",time_unit,".csv")
  file_url <- paste0(url_dir,file_name)

  # Download and unzip
  temp <- tempfile()
  download.file(file_url,temp,cacheOK = TRUE,quiet = (!verbose))
  xSub_file <- read.csv(unzip(temp, files=file_name))
  unlink(temp)
  if(file.exists(file_name)){file.remove(file_name)}

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
      # Load (or install) dependencies
      list.of.packages <- c("haven"); new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}; lapply(list.of.packages, require, character.only = TRUE)
      write_dta(data=xSub_file,path = paste0(out_dir,"/",gsub("csv$","dta",file_name)),version = 14)
    }
  }

  # Return object
  return(xSub_file)

}
