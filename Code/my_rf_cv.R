#' My version of Random Forest Cross-Validation
#'
#' This function predict \code{body_mass_g} using covariates
#'   \code{bill_length_mm}, \code{bill_depth_mm}, and \code{flipper_length_mm}.
#'
#' @param k a integer number of folds
#'
#' @return a numeric with the cross-validation error
#'
#' @keywords prediction
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # remove NAs in the penguins data
  penguins_data <- na.omit(my_penguins)
  # select columns for predicting
  penguins_df <- penguins_data %>%
    select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm)

  # create fold variable within the penguins data
  penguins_df$fold <- sample(rep(1:k, length = nrow(penguins_df)))
  # create a varible for record the cv errors
  cv_errors <- c()
  #iterate through k folds
  for (i in 1:k) {
    # update the train data and test data
    data_train <- penguins_df %>% dplyr::filter(fold != i)
    data_test <- penguins_df %>% dplyr::filter(fold == i)

    # record the true body mass of the test data
    body_mass_test <- data_test$body_mass_g

    # build a random forest model with 100 trees
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)

    # predict the body mass based on the model
    pr <- predict(model, data_test[, -1])

    # calculate the mean squared error
    err <- mean((pr - body_mass_test)^2)
    cv_errors <- append(cv_errors, err)
  }
  # return the mean of cv errors
  return(mean(cv_errors))
}
