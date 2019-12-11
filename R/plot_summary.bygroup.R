#' Plot cross sectional statistics in a panel.
#'
#' Plot average/median/cumulative value (and number of
#' cross-sectional units over which is performed the calculation) over time.
#'
#' @param data Dataframe.
#' @param variable_to_summ Variable of interest, numeric vector.
#' @param group Grouping variable, character or date vector.
#' @param n Cross-sectional unit, character vector.
#' @param sum.stat Summary statistic to plot, character. Default set to mean.
#' @param na.rm Default TRUE.
#'
#' @return List with two elements: plot and dataframe with values.
#'
#' @example
#' plot_summary.bygroup(nominal_exchange, ner, quarter, iso2)
#'
#' @export

plot_summary.bygroup <- function(data, variable_to_summ, group, n, sum.stat = c("mean","median","sum","n"), na.rm = TRUE){

  # Default categorical:
  sum.stat <- match.arg(sum.stat)

  # Quosurize column variables for tidyeval:
  variable_quosure <- enquo(variable_to_summ)
  group_quosure <- enquo(group)
  n_quosure <- enquo(n)

  # In alternative, if many parameters, we can quosurize as list with quos().

  # Grouped summary statistics: cross-sectional mean, median, sum and count of unique units
  # per year.
  df.stat <- data %>%
    group_by(!!group_quosure) %>%
    summarise(mean = mean(!!variable_quosure, na.rm = na.rm),
              median = median(!!variable_quosure, na.rm = na.rm),
              sum = sum(!!variable_quosure, na.rm = na.rm),
              n = length(unique(!!n_quosure))
              )

    # Plot conditional:
  if (sum.stat == "mean") {
    plot.mean <- ggplot(df.stat, aes(!!group_quosure, mean, group = 1)) +
      geom_line(color = "darkblue") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 270))
    return(list(plot.mean=plot.mean, summary.group = df.stat))
  }

  if (sum.stat == "median") {
    plot.median <- ggplot(df.stat, aes(!!group_quosure, median, group = 1)) +
      geom_line(color = "darkblue") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 270))
    return(list(plot.median=plot.median, summary.group = df.stat))
  }

  if (sum.stat == "sum") {
    plot.sum <- ggplot(df.stat, aes(!!group_quosure, sum, group = 1)) +
      geom_line(color = "darkblue") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 270))
    return(list(plot.sum=plot.sum, summary.group = df.stat))

  }

  if (sum.stat == "n") {
  plot.n <- ggplot(df.stat, aes(!!group_quosure, n, group = 1)) +
    geom_line(color = "darkblue") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 270))
  return(list(plot.n=plot.n, summary.group = df.stat))
  }
}




