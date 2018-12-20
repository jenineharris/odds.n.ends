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
  if(class(x)[1] != "glm") stop("x must be a glm object")
    # model significance
    modelsig <- round(c(x$null.deviance - x$deviance,
                        x$df.null - x$df.residual,
                        pchisq(x$null.deviance - x$deviance,
                               x$df.null - x$df.residual,
                               lower.tail = FALSE)), 3)
    names(modelsig) <- c("Chi-squared", "d.f.", "p")
    print("Logistic regression model significance")
    print(modelsig)
    cat("\n")

    # model fit contingency tables
    # error if na.action = na.exclude not used
    print("Contingency tables (model fit): percent predicted")
    print(addmargins(prop.table(table("% observed" = x$y,
                                      "% predicted" = as.numeric(x$fitted.values>=0.5)))))
    cat("\n")
    print("Contingency tables (model fit): frequency predicted")
    print(addmargins(table("n observed" = x$y,
                           "n predicted" = as.numeric(x$fitted.values>=0.5))))
    cat("\n")

    # odds ratio calculate and print
    oddsRatios <- exp(cbind(OR = coef(x), confint(x)))
    print("Predictor odds ratios and 95% CI")
    print(oddsRatios)
}



