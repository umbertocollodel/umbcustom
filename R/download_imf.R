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
  data_list <- IMFData::CompactDataMethod(databaseID, queryfilter, startdate, enddate,
                         checkquery)

  # Need to count the number of rows for each dataframe of the list and sum
  # the total.

  number_rows <- data_list$Obs %>%
           purrr::map_dbl(nrow) %>%
           sum()

  # Check unit of measurement difference: stops if there are countries with
  # different units of measure.

  if (data_list %>%
        select("@UNIT_MULT") %>%
        unique() %>%
        length() != 1) {
    stop("Different units of mesurement for countries.")
  }

  # Transform into dataframe.

  head(IMFData::CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery,
                               tidy = TRUE), number_rows) %>%
    select("@REF_AREA","@TIME_PERIOD","@OBS_VALUE") %>% # select country-id, time-id and value
    mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`)) # time series not numeric
}
