#########################################################################
# Name of file - data_prep_functions.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - February 2018
#
# Type - Functions
# Written/run on - RStudio server
# Version of R - 3.2.3
#
# Description - Functions sourced and used in create_hsmr_data.R
#########################################################################

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

