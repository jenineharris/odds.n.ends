% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/or_function.R
\name{odds.n.ends}
\alias{odds.n.ends}
\title{A binary logistic regression function}
\usage{
odds.n.ends(
  mod,
  thresh = 0.5,
  rocPlot = FALSE,
  predProbPlot = FALSE,
  color1 = "#7463AC",
  color2 = "deeppink"
)
}
\arguments{
\item{mod}{is a glm object}

\item{thresh}{is the threshold between 0-1 for predicted prob to be considered a case}

\item{rocPlot}{is TRUE or FALSE to display an ROC plot}

\item{predProbPlot}{is TRUE or FALSE to display predicted prob histogram by outcome value}

\item{color1}{choose color for plot}

\item{color2}{choose 2nd color for plot}
}
\description{
This function allows you to compute model
significance (model chi-squared), model fit (percent
correctly predicted, sensitivity, specificity),
ROC plot, predicted probability plot, and
odds ratios with 95 percent confidence intervals for
a glm object from a binary logistic regression analysis.
}
\examples{
sick <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
age <- c(23, 25, 26, 34, 54, 46, 48, 95, 81, 42, 62, 25, 31, 49, 57, 52, 54, 63, 61, 50)
logisticModel <- glm(sick ~ age, na.action = na.exclude, family = binomial(logit))
odds.n.ends(mod = logisticModel)
}
\keyword{fit}
\keyword{glm}
\keyword{logistic}
\keyword{odds}
