#' Check outliers and subsequent winsorization.
#'
#' @param data Dataframe.
#' @param var Variable of interest, object.
#' @param id Individual id for panel data, object.
#' @param time Time id for panel data, object.
#' @param probs Probability cut-off for winsorization. Default is 5\%.
#' @param na.rm Default is TRUE.
#'
#' @return List with three elements: graph with comparison pre/after
#' winsorization, dataframe with outliers and character vector with
#' individual ids outliers.
#'
#' @example check_winsorize(gross_inflows, inflows_gdp, iso2, quarter)
#'
#' @export

check_winsorize <- function (data, var, id, time, probs = c(0.05,0.95), na.rm = TRUE){

  # Quosurize variables required for tidy evaluation.

  var_quosurize <- enquo(var)
  id_quosurize <- enquo(id)
  time_quosurize <- enquo(time)

  # Boxplot variable of interest before and after outliers removal (winsorization).
  # Arrange them togheter.

  original_boxplot <- data %>%
    ggplot(aes(y = !!var_quosurize)) +
    geom_boxplot()

  corrected_boxplot <- data %>%
    mutate(winsor_serie = DescTools::Winsorize(!!var_quosurize, na.rm = na.rm)) %>%
    ggplot(aes(y = winsor_serie)) +
    geom_boxplot() +
    ylab("")

  correction.graph <- gridExtra::grid.arrange(original_boxplot, corrected_boxplot, ncol = 2,
                                              top = grid::textGrob("Winsorize",gp=grid::gpar(fontsize=20,font=1)))

  # Create dataframe with only outliers.
  # We create a numeric vector with quantiles:

  quantiles <- stats::quantile(data[[deparse(substitute(var))]], probs = probs, na.rm = na.rm)

  # And filter the original data:

  outliers_dataframe <- data %>%
    dplyr::filter(!!var_quosurize < quantiles[1] | !!var_quosurize > quantiles[2]) %>%
    select(!!id_quosurize, !!time_quosurize, !!var_quosurize)

  # We save a character vector with name of countries with outliers:

  list_countries <- unique(outliers_dataframe[[deparse(substitute(id))]])


  return(list(correction.graph = correction.graph,
              outliers_dataframe = outliers_dataframe,
              list_countries = list_countries))
}

