#' A logistic regression function
#'
#' This function allows you to compute model
#' significance (model chi-squared), model fit (percent
#' correctly predicted), and odds ratios with 95 percent confidence
#' intervalS for
#' a glm object from a logistic regression analysis.
#' @param x is a glm object
#' @keywords glm logistic odds fit
#' @export
#' @examples
#' sick <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
#' age <- c(23, 25, 26, 34, 54, 46, 48, 95, 81, 42, 62, 25, 31, 49, 57, 52, 54, 63, 61, 50)
#' logisticModel <- glm(sick ~ age, na.action = na.exclude, family = binomial(logit))
#' odds.n.ends(logisticModel)

odds.n.ends <- function(x) {
  if(class(x)[1] != "glm") stop("x must be a glm object") |
  if(range(x$fitted.values)[2] < .5) warning("specificity is 100%, sensitivity is 0%, check model") |
  if(range(x$fitted.values)[1] >= .5) warning("sensitivity is 100%, specificity is 0%, check model")
  # model significance
  modelsig <- round(c(x$null.deviance - x$deviance,
                      x$df.null - x$df.residual,
                      pchisq(x$null.deviance - x$deviance,
                             x$df.null - x$df.residual,
                             lower.tail = FALSE)), 3)
  names(modelsig) <- c("Chi-squared", "d.f.", "p")

  # odds ratio calculate
  oddsRatios <- exp(cbind(OR = coef(x), stats::confint(x)))

  # model fit contingency tables
  # error if na.action = na.exclude not used
  # observed and predicted values percents
  if(max(x$fitted.values)>=.5)
          {percTable <- addmargins(prop.table(table("Percent predicted" = as.numeric(x$fitted.values>=0.5),
                                           "Percent observed" = x$y)[2:1, 2:1]))}
  else if(max(x$fitted.values) < .5)
  {percTable <- matrix(c(0, 0, sum(x$y == 1), sum(x$y == 0)), ncol = 2, byrow = TRUE)
  colnames(percTable) <- c("observed 1", "observed 0")
  rownames(percTable) <- c("predicted 1", "predicted 0")
  percTable <- addmargins(prop.table(as.table(percTable)))
  percTable}

  # observed and predicted values frequencies
  if(max(x$fitted.values)>=.5)
          {freqTable <- addmargins(table("Number predicted" = as.numeric(x$fitted.values>=0.5),
                                 "Number observed" = x$y)[2:1, 2:1])}
  else if(max(x$fitted.values) < .5) {freqTable <- matrix(c(0, 0, sum(x$y == 1), sum(x$y == 0)), ncol = 2, byrow = TRUE)
                                 colnames(freqTable) <- c("observed 1", "observed 0")
                                 rownames(freqTable) <- c("predicted 1", "predicted 0")
                                 freqTable <- addmargins(as.table(freqTable))
                                 freqTable }

  # sensitivity and specificity
  sens <- freqTable[1,1]/(freqTable[1,1] + freqTable[2,1])
  spec <- freqTable[2,2]/(freqTable[2,2] + freqTable[1,2])

  # consolidate
  resultList <- list(modelsig, percTable, freqTable, oddsRatios, sens, spec)
  names(resultList) <- c("Logistic regression model significance",
                         "Contingency tables (model fit): percent predicted",
                         "Contingency tables (model fit): frequency predicted",
                         "Predictor odds ratios and 95% CI",
                         "Model sensitivity",
                         "Model specificity")
  return(resultList)

}


