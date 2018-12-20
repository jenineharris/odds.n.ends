# odds.n.ends
# computes odds ratios
# computes model significance
# computes contingency table for fit assessment
# from glm binary logistic model
# check to see if glm object
# if glm object get odds ratios
# chi-squared with df and pvalue
# percent correctly predicted
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



