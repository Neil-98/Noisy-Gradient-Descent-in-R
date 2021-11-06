library(ggplot2)

get_cost <- function(W, Y, X) {
  cost <- norm(x = (X %*% matrix(t(W)) - Y) ^ 2, type = "2")
  return(cost)
}
  
get_random_error <- function(row_dim, col_dim, delta) {
  error <- matrix(rnorm(row_dim * col_dim, mean = 0, sd = delta), row_dim, col_dim)
  return(error)
}

get_noisy_gradient <- function(W, Y, X, delta) {
  error <- get_random_error(dim(W)[1], dim(W)[2], delta)
  gradient <- (t(X) %*% X %*% W - t(X) %*% Y) - error
  return(gradient)
}

# Descent step
stepwise_descent <- function(W, Y, X, learning_rate, delta) {
  gradient <- get_noisy_gradient(W, Y, X, delta)
  subtrahend <-  learning_rate * gradient
  W <- W - subtrahend
  return(W)
}

# Looping stepwise_descent, with constant learning rate
gradient_descent <- function(W, Y, X, learning_rate, no_of_steps, delta) { # cost_table: A detailed history of costs at each step of gradient descent
  
  ### Code required just for record-keeping, not for gradient descent!
  cost_table <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(cost_table) <- c("Step_No", "Cost")
  ###
  
  
  prev_cost <- get_cost(W, Y, X)
  # sprintf("Initial Cost = ", prev_cost)
  i <- 1
  for (i in 1: no_of_steps) {
    
    cost <- prev_cost
    
    ### Code required just for record-keeping, not for gradient descent!
    cost_record <- cbind(i, cost)
    colnames(cost_record) <- c("Step_No", "Cost")
    cost_table <- rbind(cost_table, cost_record)
    ###
    
    W <- stepwise_descent(W, Y, X, learning_rate, delta)
    
    cost <- get_cost(W, Y, X)
    
    prev_cost <- cost
  }
  
  ### Code required just for record-keeping, not for gradient descent!
  final_cost <- get_cost(W, Y, X)
  final_cost_record <- cbind(i + 1, final_cost)
  colnames(final_cost_record) <- c("Step_No", "Cost")
  cost_table <- rbind(cost_table, final_cost_record)
  ### 
  
  weights_and_record <- list(W, cost_table)
  names(weights_and_record) <- c("Final_Weights", "Record")
  
  return(weights_and_record)
}

# Looping stepwise_descent, reducing learning_rate (learning_rate = learning_rate/ step_no)
variable_gradient_descent <- function(W, Y, X, learning_rate, no_of_steps, delta) { # cost_record: A detailed history of costs at each step of gradient descent
  
  ### Code required just for record-keeping, not for gradient descent!
  cost_table <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(cost_table) <- c("Step_No", "Cost")
  ###
  
  
  prev_cost <- get_cost(W, Y, X)
  i <- 1
  for (i in 1: no_of_steps) {
    
    cost <- prev_cost
    
    ### Code required just for record-keeping, not for gradient descent!
    cost_record <- cbind(i, cost)
    colnames(cost_record) <- c("Step_No", "Cost")
    cost_table <- rbind(cost_table, cost_record)
    ###
    
    W <- stepwise_descent(W, Y, X, learning_rate, delta)
    
    cost <- get_cost(W, Y, X)
    
    if (prev_cost != 0) {
      learning_rate <- learning_rate/ i # Update learning_rate
    }
    
    prev_cost <- cost
  }
  
  ### Code required just for record-keeping, not for gradient descent!
  final_cost <- get_cost(W, Y, X)
  final_cost_record <- cbind(i + 1, final_cost)
  colnames(final_cost_record) <- c("Step_No", "Cost")
  cost_table <- rbind(cost_table, final_cost_record)
  ### 
  
  weights_and_record <- list(W, cost_table)
  names(weights_and_record) <- c("Final_Weights", "Record")
  
  return(weights_and_record)
}

delta_plot1 <- data.frame(matrix(ncol = 2, nrow = 0)) # Average Final Cost per Delta. For constant learning rate
colnames(plot1) <- c("Delta", "Average_Cost")

delta_plot2 <- data.frame(matrix(ncol = 2, nrow = 0)) # Average Final Cost per Delta. For changing learning rate
colnames(plot2) <- c("Delta", "Average_Cost")

D <- 10

for(delta in seq(1, 22, 3)) {
  
  plot1 <- data.frame(matrix(ncol = 2, nrow = 0)) # For constant learning rate
  colnames(plot1) <- c("Step_No", "Average_Cost")
  
  plot2 <- data.frame(matrix(ncol = 2, nrow = 0)) # For changing learning rate
  colnames(plot2) <- c("Step_No", "Average_Cost")
  
  for(i in 1:10) {
    A <- matrix(rnorm(D * D, mean = 0, sd = 1), D, D)
    
    vk <- matrix(rnorm(D, mean = 0, sd = 1), D, 1)
    
    k <- 1
    ek <- matrix(0, D, 1)
    ek[k] <- 1
    
    avg_cost_by_delta1 <- 0
    avg_cost_by_delta2 <- 0
    
    weights_and_record1 <- gradient_descent(vk, ek, A, learning_rate =  0.01, no_of_steps = 500, delta = delta)
    final_weights1 <- weights_and_record1$Final_Weights
    record1 <- weights_and_record1$Record
    
    if(dim(plot1)[1] == 0) { # dataframe has no observations
      # print("Dataframe has no observations")
      steps_and_cost1 <- cbind(record1$Step_No, record1$Cost)
      colnames(steps_and_cost1) <- c("Step_No", "Average_Cost")
      plot1 <- rbind(plot1, steps_and_cost1) # Initialize all rows with values of the first ek's cost history 
    } else { # dataframe has rows
      # print(D)
      plot1$Average_Cost <- (plot1$Average_Cost + record1$Cost) / 2
    }
    
    weights_and_record2 <- variable_gradient_descent(vk, ek, A, learning_rate =  0.01, no_of_steps = 500, delta = delta)
    final_weights2 <- weights_and_record2$Final_Weights
    record2 <- weights_and_record2$Record
    
    if(dim(plot2)[1] == 0) { # dataframe has no observations
      # print("Dataframe has no observations")
      steps_and_cost2 <- cbind(record2$Step_No, record2$Cost)
      colnames(steps_and_cost2) <- c("Step_No", "Average_Cost")
      plot2 <- rbind(plot2, steps_and_cost2) # Initialize all rows with values of the first ek's cost history 
    } else { # dataframe has rows
      # print(D)
      plot2$Average_Cost <- (plot2$Average_Cost + record2$Cost) / 2
    }
    
    
    cost1 <- get_cost(final_weights1, ek, A)
    avg_cost_by_delta1 <- (avg_cost_by_delta1 + cost1) / 2
    
    cost2 <- get_cost(final_weights2, ek, A)
    avg_cost_by_delta2 <- (avg_cost_by_delta2 + cost2) / 2
  }
  
  # Graphs displaying variation of average costs with respect to time-step for each experimental value of delta tested
  
  print(ggplot(plot1, aes(x = Step_No, y = Average_Cost)) +
          geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) +
          ggtitle(paste("Cost vs Time for Delta ", delta, " with Constant Learning Rate")))
  
  print(ggplot(plot2, aes(x = Step_No, y = Average_Cost)) +
          geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) +
          ggtitle(paste("Cost vs Time for Delta ", delta, " with Varying Learning Rate")))
  
  delta_plot1_record <- cbind(delta, avg_cost_by_delta1)
  colnames(delta_plot1_record) <- c("Delta", "Average_Cost")
  delta_plot1 <- rbind(delta_plot1, delta_plot1_record)
  
  delta_plot2_record <- cbind(delta, avg_cost_by_delta2)
  colnames(delta_plot2_record) <- c("Delta", "Average_Cost")
  delta_plot2 <- rbind(delta_plot2, delta_plot2_record)
  
  print("Executing...") # Print loading screen
}

# Computations completed. Print status update
print("Computation completed. Display graphs...")


# Graphs displaying variation of average final costs with delta  
ggplot(delta_plot1, aes(x = Delta, y = Average_Cost)) +
  geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  ggtitle("Cost vs Delta for Constant Learning Rate")

ggplot(delta_plot2, aes(x = Delta, y = Average_Cost)) +
  geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  ggtitle("Cost vs Delta for Varying Learning Rate")

# Execution completed. Print status update
print("Execution completed.")