#' HP-filter for panel data.
#'
#' Filters a single serie for different IDs in panel individually.
#'
#' @param data Dataframe.
#' @param var Variable of interest, object.
#' @param group Individuals ID panel, object.
#' @param time Time ID panel, object (character,numeric & date).
#' @param freq Frequency of HP filter, integer. Default is 1600.
#'
#' @return List with three elements. First element, original dataframe with
#' cyclical and trend component of variable, second element, list of graphs with
#' trend and variable and third element, list of ids not de-trended.
#' Warning message if some ids not de-trended.
#'
#' @examples
#' panel_hpfilter(chinn_ito,ka_open,ccode,year, 1600)
#'
#' @export

panel_hpfilter <- function(data, var, group, time, freq = 1600){

  # Create a list of countries in the panel:
  country.list <- unique(data[[deparse(substitute(group))]]) # deparse/substitute useful when
                                                              # in the same function object and character string

  # Split the dataframe into different subsets, one for each country,
  # return a list.
  list_individual <- split(data, data[[deparse(substitute(group))]])

  # Quosurize the column to use in dplyr setting:
  var_quosurize <- enquo(var)

  # For each list, retrieve only the interested column, convert
  # to time series object (compatible) and apply the filter:

  list_hpfilter <- lapply(list_individual, function(x) x %>%
                       select(!!var_quosurize) %>%
                       as.ts() %>%
                       mFilter::hpfilter(freq = freq)
                     )

  # The problem is that the objects in the list are of class 'mFilter'.
  # Want 'dataframe' to then rbind them together and join to the original
  # one.

   # Create an empty list with the same length as original one and same names:
  list_hpfilter.dataframes <- vector(mode = "list", length = length(list_hpfilter))
  names(list_hpfilter.dataframes) <- names(list_hpfilter)

  # Each element of the list is a dataframe with two columns: cycle and trend
  # Start loop:
  for (i in 1:length(list_hpfilter)) {
   list_hpfilter.dataframes[[i]] <- data.frame(list_hpfilter[[i]][c("cycle","trend")])
  }

  # Convert each column of each element of the list to numeric and change trend name:
  list_hpfilter.dataframes <- lapply(list_hpfilter.dataframes, function(x) x %>%
           mutate_all(as.numeric) %>%
           setNames(c("cycle","trend"))
  )

  # Row bind all dataframes (list elements) together:
  list_binded <- bind_rows(list_hpfilter.dataframes)

  if (length(list_binded$cycle) != length(data[[deparse(substitute(var))]])){
    stop("Hp-filtered data are not the same length as original dataframe", length(list_binded$cycle), "vs",
         count(data[[deparse(substitute(var))]]))
  }

  # Column bind with original dataframe:
  # elements of list were in alphabetical order, need to arrange the original
  # dataframe in the same way.

  group_quosurize <- enquo(group)

  final_data <- data %>%
    arrange(!!group_quosurize) %>%
    cbind(.,list_binded)

  # Want to return a list with graph of variable and trend for every
  # individual: easy to check the filter.

  # Split final dataframe into list of dataframes:
  list_graphs <- split(final_data,final_data[,deparse(substitute(group))])

  # Plot:
  # quosurize to allow for general time-id.
  time_quosurize <- enquo(time)

  list_graphs <- lapply(list_graphs, function(x) x %>%
           ggplot(aes(!!time_quosurize, group = 1)) +
           geom_line(aes(y=trend,col = "trend" )) +
            geom_line(aes(y=!!var_quosurize,col = deparse(substitute(var)))) +
            ylab("") +
            theme(axis.text.x = element_text(angle = 270))
            )

  # Individual id for which filter not performed: count number of observations
  # and number of NAs by id and return "no" if equal

  missing_filter <- final_data %>%
    group_by(!!group_quosurize) %>%
    summarise(n = length(cycle), n_nas = sum(is.na(cycle))) %>%
    mutate(filtered = case_when(n != n_nas ~ "yes",
                                TRUE ~ "no")) %>%
    dplyr::filter(filtered == "no") %>%
    select(!!group_quosurize) %>%
    as.list()

  # Message of warning if some countries not filtered:
  if (length(missing_filter[[1]]) != 0) {
    warning(length(missing_filter[[1]])," ids were not filtered. Check third element
    of return list for details.")
  }

  # Return the final dataframe, list of all graphs inside
  # a list and list of ids not filtered without printing:
  invisible(list(data_hpfilter = final_data, graphs = list_graphs,
                 missing_filter = missing_filter))

}

# To fix: the function does not take incomplete time series.
