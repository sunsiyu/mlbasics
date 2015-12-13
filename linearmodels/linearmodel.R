
cclass <- c(rep("character", 2), rep("numeric", 4), "integer", "character", rep("integer", 8), 
            "character", rep("numeric", 4))
training <- read.table("kc_house_train_data.csv", header=T, sep=",", colClasses = cclass)
testing <- read.table("kc_house_test_data.csv", header=T, sep=",", colClasses = cclass)

training$bedrooms_squared <- training$bedrooms * training$bedrooms
training$bed_bath_rooms <- training$bedrooms * training$bathrooms
training$log_sqft_living <- log(training$sqft_living)
training$lat_plus_long <- training$lat * training$long
testing$bedrooms_squared <- testing$bedrooms * testing$bedrooms
testing$bed_bath_rooms <- testing$bedrooms * testing$bathrooms
testing$log_sqft_living <- log(testing$sqft_living)
testing$lat_plus_long <- testing$lat * testing$long




lm1 <- lm(price ~ sqft_living + bedrooms + bathrooms + lat + long, data = training)
lm2 <- lm(price ~ sqft_living + bedrooms + bathrooms + lat + long + bed_bath_rooms, data = training)
lm3 <- lm(price ~ sqft_living + bedrooms + bathrooms + lat + long + 
            bed_bath_rooms + bedrooms_squared + log_sqft_living + lat_plus_long, data = training)
res1 <- predict(lm1, testing) - testing$price
sum(res1*res1)
res2 <- predict(lm2, testing) - testing$price
sum(res2*res2)
res3 <- predict(lm3, testing) - testing$price
sum(res3*res3)


fm <- data.frame(constants = rep(1, nrow(training)), sqft_living = training$sqft_living)
output <- training$price

initial_weights = c(-47000, 1)
step_size = 7e-12
tolerance = 2.5e7



regression_gradient_descent  <- function(feature_matrix, output, initial_weights, 
                                         step_size, tolerance)
{
  converged <- F
  weights <- initial_weights
  features <- feature_matrix
  while (converged != T) {
    # compute the predictions based on feature_matrix and weights:
    for (i in 1:length(initial_weights))
      features[,i] <-feature_matrix[,i]*initial_weights[i]
    # compute the errors as predictions - output:
    errors <- rowsum(features) - output
    gradient_sum_squares = 0 # initialize the gradient
    
    for (i in 1:length(initial_weights)) {
      # Recall that feature_matrix[:, i] is the feature column associated with weights[i]
      # compute the derivative for weight[i]:
      derivatives <- -2*feature_matrix[,i]*errors
      # add the squared derivative to the gradient magnitude
      gradient_sum_squares <- gradient_sum_squares + derivatives*derivatives
      # update the weight based on step size and derivative:
      weights <- weights + step_size * derivatives
    }
    gradient_magnitude <- sqrt(gradient_sum_squares)
    if (gradient_magnitude < tolerance)
      converged <- T
  }
  return(weights)
}
  
  

