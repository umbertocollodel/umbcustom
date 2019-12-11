#' Event study of variable of interest.
#'
#' Performs an event study of a variable around the occurence of a certain
#' episode. Mean/median value before and after. CAUTION: build dummies before.
#'
#' @param data Dataframe object.
#' @param variable_of_interest Character string. Name of indicator to track.
#' @param n_before Numeric (negative). Periods before the event.
#' @param n_after Numeric (positive). Periods after the event.
#' @param type Character. Summary static to track: default is "mean".
#' @param level Numeric. Default is 0.90.
#'
#' @return List with two elements - graph and dataframe on which latter based upon.
#'
#' @example
#' event_study(df, "reserves",-6,6)
#'
#' @export

run_event.study <- function(data, variable_of_interest, n_before, n_after, type = c("mean","median"),
                            level = 0.90) {
  # Set categorical default equal to mean
  type <- match.arg(type)
  # First step: running the regression
  # colnames must be in order: from the furthest lead to furthest lag
  # create a character vector with the explanatory variables
  event_dummies <- grep("event", colnames(data), value=TRUE)

  if (length(event_dummies) != length(n_before:n_after)){
    stop("The dummy columns created do not correspond to the
          criteria set for length of the
         event study")
  }
  # check if the variable of interest is a character string:
  if (!is.character(variable_of_interest)) {
    stop("variable_of_interest is not a character; it has class'", class(variable_of_interest), "'.")
  }
  # create custom formula to generalise:
  event_formula <- reformulate(termlabels=event_dummies, response=variable_of_interest)
  # fit model:
  if (type == "mean") {
    sequence <- lm(event_formula, data)
  # Second step: construction graph
    sequence.confint <- confint(sequence, level = level) # building CI - parameter to specify
    sequence.confint[,1] <- sequence.confint[,1] + sequence.confint[1,1] # add intercept ones - lower
    sequence.confint[,2] <- sequence.confint[,2] + sequence.confint[1,2] # add intercept ones - upper
    sequence.confint <- sequence.confint[-c(1),]
    time <- n_before:n_after # create time sequence
    sequence.confint <- data.frame(cbind(sequence.confint, time))
    # store coefficients
    coefs <- coef(sequence)
    # array into dataframe and indexing: n-1 to include also the intercept
    df.coefs <- data.frame(coefs,time = (n_before-1):n_after)
    # create a column just for intercept value
    df.coefs$intercept <- df.coefs[1,1]
    # add constant to estimates of dummies (i.e. average in tranquil times)
    df.coefs$coefs <- df.coefs$coefs + df.coefs[1,1]
    df.coefs <- df.coefs[-c(1),]

    # merge the two (estimates and confidence bands values)
    # plot the graph
    # renaming columns by position because name changes with the level of CIs specified.

    event.study <- df.coefs %>%
      inner_join(.,sequence.confint,by=c("time")) %>%
      rename(lower = 4,upper = 5) %>%
      ggplot(aes(time, coefs)) +
      geom_line(col = "red") +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey60",alpha = 0.4) +
      geom_vline(aes(xintercept = 0)) +
      geom_hline(aes(yintercept = intercept), linetype = "dashed") +
      xlab("") +
      ylab("") +
      scale_x_continuous(breaks=c(time))
    # Output
    return(list(event.study = event.study, data_event.study = df.coefs))
  }
  if (type == "median") {
    sequence <- suppressWarnings(quantreg::rq(event_formula, data = data)) # must specify data, second argument in rq is tau
    # obtain coefficients and CI in dataframe format
    median <- jtools::summ(sequence, confint = TRUE, ci.width = level)
    median <- data.frame(median[["coeftable"]]) %>%
      select(1:3) %>%
      setNames(c("coefs", "lower", "upper"))
    # create time vector (n_before - 1 for the intercept)
    time <- (n_before-1):n_after
    df.coefs <- cbind(median, time)
    # create a column just for intercept value
    df.coefs$intercept <- df.coefs[1,1]

    # # create a column just for intercept value and standard errors
    df.coefs$intercept <- df.coefs[1,1]
    df.coefs$intercept_lower <- df.coefs[1,2]
    df.coefs$intercept_upper <- df.coefs[1,3]

    # # add constant to estimates of dummies and SE (i.e. average in tranquil times)
    df.coefs$coefs <- df.coefs$coefs + df.coefs[,c("intercept")]
    df.coefs$lower <- df.coefs$lower + df.coefs[,c("intercept_lower")]
    df.coefs$upper <- df.coefs$upper + df.coefs[,c("intercept_upper")]
    # remove intercept CI columns and intercept first row
    df.coefs <- df.coefs %>%
          select(-c(intercept_lower,intercept_upper)) %>%
          slice(-1)
    # plot
    event.study <- df.coefs %>%
      ggplot(aes(time, coefs)) +
      geom_line(col = "red") +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey60",alpha = 0.4) +
      geom_vline(aes(xintercept = 0)) +
      geom_hline(aes(yintercept = intercept), linetype = "dashed") +
      xlab("") +
      ylab("") +
      scale_x_continuous(breaks=c(time))
    # Output
    return(list(event.study = event.study, data_event.study = df.coefs))
  }
}


# Before running the function
# library(data.table)
# try <- nominal_exchange %>%
#   mutate(SS = case_when(quarter == "2008-Q4"~ 1,
#                         TRUE ~ 0))
#
# with_dummies <- setDT(try)[, sprintf("eventT_%0d", 6:1) := shift(SS, c(6:1), type = 'lead'), by = iso2]
# with_dummies <- with_dummies %>%
#   mutate(event = SS)
# with_dummies <- setDT(with_dummies)[, sprintf("eventT%0d", 1:3) := shift(SS, c(1:3), type = 'lag'), by = iso2]

