#' A binary logistic regression function
#'
#' This function allows you to compute model
#' significance (model chi-squared), model fit (percent
#' correctly predicted, sensitivity, specificity),
#' ROC plot, predicted probability plot, and
#' odds ratios with 95 percent confidence intervals for
#' a glm object from a binary logistic regression analysis.
#' @param mod is a glm object
#' @param thresh is the threshold between 0-1 for predicted prob to be considered a case
#' @param rocPlot is TRUE or FALSE to display an ROC plot
#' @param predProbPlot is TRUE or FALSE to display predicted prob histogram by outcome value
#' @param color1 choose color for plot
#' @param color2 choose 2nd color for plot
#' @keywords glm logistic odds fit
#' @export
#' @importFrom graphics abline hist legend mtext par
#' @importFrom stats addmargins coef confint pchisq
#' @examples
#' sick <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
#' age <- c(23, 25, 26, 34, 54, 46, 48, 95, 81, 42, 62, 25, 31, 49, 57, 52, 54, 63, 61, 50)
#' logisticModel <- glm(sick ~ age, na.action = na.exclude, family = binomial(logit))
#' odds.n.ends(mod = logisticModel)

odds.n.ends <- function(mod,
                        thresh = 0.5,
                        rocPlot = FALSE,
                        predProbPlot = FALSE,
                        color1 = "#7463AC",
                        color2 = "deeppink") {
  if(class(mod)[1] != "glm") stop("mod must be a glm object")

  # model significance
  modChiSq <- round(mod$null.deviance - mod$deviance, 3)
  modDf <- round(mod$df.null - mod$df.residual, 3)
  modPval <- ifelse(pchisq(mod$null.deviance - mod$deviance,
                           mod$df.null - mod$df.residual,
                           lower.tail = FALSE) >= .0005,
                    round(pchisq(mod$null.deviance - mod$deviance,
                                 mod$df.null - mod$df.residual,
                                 lower.tail = FALSE), 3),
                    '<.001')
  modelsig <- noquote(c(modChiSq,
                        modDf,
                        modPval))
  names(modelsig) <- c("Chi-squared", "d.f.", "p")

  # odds ratio calculate
  oddsRatios <- exp(cbind(OR = coef(mod), confint(mod)))

  # model fit contingency tables
  # error if na.action = na.exclude not used
  # observed and predicted values frequencies
  freqTable <- addmargins(table("Number predicted" = factor(as.numeric(mod$fitted.values>=thresh), levels = 0:1),
                                "Number observed" = mod$y))

  # sensitivity and specificity
  spec <- freqTable[1,1]/(freqTable[1,1] + freqTable[2,1])
  sens <- freqTable[2,2]/(freqTable[2,2] + freqTable[1,2])

  # count-R-squared
  countrsquared <- 100*(freqTable[1,1] + freqTable[2,2])/(freqTable[1,1] + freqTable[2,1] +
                                                            freqTable[2,2] + freqTable[1,2])

  # ROC
  # Code adapted from http://rstudio-pubs-static.s3.amazonaws.com/220197_7131fb0b2455404cb95ea8f788d45828.html
  # Calculate sensitivity and false positive measures for logit model
  fity_ypos <- mod$fitted[mod$y == 1]
  fity_yneg <- mod$fitted[mod$y == 0]

  sort_fity <- sort(mod$fitted.values)

  sensRoc <- 0
  spec_c <- 0

  for (i in length(sort_fity):1){
    sensRoc <- c(sensRoc, mean(fity_ypos >= sort_fity[i]))
    spec_c <- c(spec_c, mean(fity_yneg >= sort_fity[i]))

  }

  npoints <- length(sensRoc)

  # Discrete approximation area under the curve, using Trapezoidal Rule
  area <- sum(0.5 * (sensRoc[-1] + sensRoc[-npoints]) * (spec_c[-1] -
                                                           spec_c[-npoints]))
  # plot ROC curves
  if(rocPlot == TRUE)
  {par(mfrow = c(1,1))
    plot(spec_c, sensRoc, xlim = c(0, 1), ylim = c(0, 1), type = "l", lwd=3,
         xlab = "False positive rate", ylab = "True positive rate", col = color2)
    abline(0, 1, col= color1, lty=2, lwd=3)
    legend("topleft", legend = c(paste("AUC =", round(area, 2)), "AUC = 0.50") ,
           pch = 15, bty = 'n', col = c(color2, color1))
    mytitle = "Receiver Operating Characteristic (ROC) Curve"
    mysubtitle = "Area under curve (AUC) computed using trapezoidal rule"
    mtext(side=3, line=2, at=-.2, adj=-0.1, cex=1.2, mytitle)
    mtext(side=3, line=1, at=-.2, adj=-0.1, cex=1.0, mysubtitle)}

  # plot predicted probabilities by observed y
  if(predProbPlot == TRUE)
  {par(mfrow = c(2,1))
    casePlot <- hist(mod$fitted.values[mod$y == 1],
                     xlab = "Probability of being a case",
                     main = "Model predicted probabilities for\nobserved cases",
                     xlim = c(0,1), col = color1)
    refPlot <- hist(mod$fitted.values[mod$y == 0],
                    xlab = "Probability of being a case",
                    main = "Model predicted probabilities for\nobserved reference group members",
                    xlim = c(0,1), col = color1)
    casePlot
    refPlot}

  # consolidate
  resultList <- list(modelsig, freqTable[c(2,1,3), c(2,1,3)], countrsquared, sens, spec, oddsRatios)
  names(resultList) <- c("Logistic regression model significance",
                         "Contingency tables (model fit): frequency predicted",
                         "Count R-squared (model fit): percent correctly predicted",
                         "Model sensitivity",
                         "Model specificity",
                         "Predictor odds ratios and 95% CI")
  return(resultList)
}


