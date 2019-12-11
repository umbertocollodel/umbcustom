#' Download IMF Data
#'
#' This function downloads a single serie for all countries available from
#' the IMF API and tidies it into dataframe format with country-id, time-id
#' and numeric value.
#'
#' @param source,start,end,freq,serie Character strings.
#' @return Dataframe with country-id in iso2c, time-id and numeric value.
#'
#' @example
#' download_imf("IFS", "1980-01-01", "2019-01-01", "Q", "ENDA_XDC_USD_RATE")
#'
#' @export

download_imf <- function(source, start, end, freq, seriename){
  # set the criteria for the query
  databaseID <- source
  startdate = start
  enddate = end
  checkquery = FALSE
  # queryfilter
  queryfilter <- list(CL_FREA = freq, CL_AREA_IFS = "", CL_INDICATOR_IFS = c(seriename))
  # download
  x <- IMFData::CompactDataMethod(databaseID, queryfilter, startdate, enddate,
                         checkquery)
  # calculate number rows
  x.number_rows <- 0
  for(i in 1:nrow(x)) {
    x.number_rows[i] <- nrow(x$Obs[[i]])
    print(x.number_rows[i])
  }
  print(i)
  # sum of the vector elements
  x.number_rows <- sum(x.number_rows)
  # transform into dataframe
  IMFData::head(CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery,
                               tidy = TRUE),x.number_rows) %>%
    select("@REF_AREA","@TIME_PERIOD","@OBS_VALUE") %>% # select country-id, time-id and value
    mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`)) # time series not numeric
}
