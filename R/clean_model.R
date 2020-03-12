#' @title Clean up logistic regression model
#'
#' @description Strips logistic regression model object of extraneous data that
#' are not required in order to calculate the probability of death within
#' 30 days of admission. This is a necessary step as the object will likely be
#' quite large and take up significant amounts of memory when producing the
#' probabilities.
#'
#'
#' @details \code{clean_model} expects an object of class \code{glm}.
#'
#'
#' @param model Input \code{glm}.
#'
#'
#' @examples
#'
#'
#' @export

clean_model <- function(model) {

  model$residuals <- c()
  model$fitted.values <- c()
  model$effects <- c()
  model$qr$qr <- c()
  model$linear.predictors <- c()
  model$weights <- c()
  model$prior.weights <- c()
  model$data <- c()
  model$family$variance <- c()
  model$family$dev.resids <- c()
  model$family$aic <- c()
  model$family$validmu <- c()
  model$family$simulate <- c()
  attr(model$terms,".Environment") <- c()
  attr(model$formula,".Environment") <- c()

  model
}
