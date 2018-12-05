#' @title Clean up logistic regression model
#'
#' @description Strips logistic regression model object of extraneous data that
#' are not required in order to calculate the probability of death within
#' 30 days of admission. This is a necessary step as the object will likely be
#' quite large and take up significant amounts of memory when producing the
#' probabilities.
#'
#'
#' @details \code{clean_model} expects an object of class \code{"glm"}.
#'
#'
#' @param cm Input \code{glm}.
#'
#'
#' @examples
#'
#'
#' @export

clean_model <- function(cm) {
  # just in case we forgot to set
  # y=FALSE and model=FALSE

  cm$residuals <- c()
  cm$fitted.values <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()
  cm$data <- c()

  cm$family$variance <- c()
  cm$family$dev.resids <- c()
  cm$family$aic <- c()
  cm$family$validmu <- c()
  cm$family$simulate <- c()
  attr(cm$terms,".Environment") <- c()
  attr(cm$formula,".Environment") <- c()

  cm
}
