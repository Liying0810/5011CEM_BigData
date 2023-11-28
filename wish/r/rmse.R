# Function to calculate model performance metrics (Root Mean Squared Error)
performance <- function(test_df, model, model_name){
  # Predict units_sold using the provided model
  model.pred <- predict(model, test_df)
  
  # Calculate the error between predicted and actual units_sold
  error = model.pred - test_df$units_sold
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean(error^2)
  
  # Calculate Root Mean Squared Error (RMSE)
  rmse <- sqrt(mse)
  
  # Generate a string summarizing the RMSE for the model
  result <- paste("RMSE of ", model_name, ":", round(rmse,2))
  
  # Return the RMSE result
  result
}

# Calculate and print RMSE for each model
performance(test_df, fit.lm, "Simple Linear Regression")
performance(test_df, fit.lm2, "Polynomial Linear Regression")
performance(test_df, fit.lm3, "Multiple Linear Regression")
performance(test_df, fit.ireg, "MLR with Interaction")

