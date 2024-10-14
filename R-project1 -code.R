



my_function <- function(number_of_weekends,
                        order_when_out,
                        fixed_delivery,
                        delivery_to_home) {
  trials <- 1000 #here we set number of trials
  #number_of_weekends <- This is the timeframe we want to check
  
  #order_when_out   : Flag used for orders in case we are out of stock
  #(If value is 1 then whenever our stock is 0 we make an order)
  
  #fixed_delivery: This parameter is used for deliveries that are on a fixed schedule.
  #If it's 0 the parameter is disabled
  
  #delivery_to_home: This parameter is set for strategy 4, where we have a delivery option to customer's location
  
  lambda <- 1 / 7   #Lambda parameter for Poisson
  
  #To get our final result I implemented a for loop of 10.000 trials and gathered each trial data into vectors
  profit_trials <- c() #this vector saves profit values
  customers_trials <- c()
  lost_customers_trials <- c()
  storage_cost_trials <- c()
  overstock_trials <- c()
  scooters_sold_trials <- c()
  
  
  
  for (k in 1:trials) {
    #I used a for loop to run trials
    
    customers <- 0  #number of customers
    scooter <- 1    #number of scooters on stock
    scooter_sold <- 0   #number of scooters sold
    scooter_sold_with_delivery <- 0 #number of scooters that were sold with delivery option
    storage_cost <- 0   #summary of storage cost
    lost_customer <- 0   #customers that we lost due to lack of scooters
    
    orders <- 0 #this is a counting number used when we make an order and we want to check when it will be delivered
    order_underway_flag <- 0 #this flag is used in order to know if we have made an order already
    #(so we don't make extra orders daily when we are out of stock)
    profit_loss_function <- 0 #This is our PnL function which calculates our total profit
    delivery_day <- 0 #this is an index used in order to organize deliveries
    
    for (j in 1:number_of_weekends) {
      poisson_data <- rpois(7, lambda)
      poisson_array <- array(poisson_data)
      #print(poisson_array)
      
      for (i in 1:7) {
        delivery_day <- delivery_day + 1
        
        #the following if statement is used to add scooters to the inventory when our orders reach maturity(date of delivery)
        if (orders == delivery_day - 5 &
            order_when_out == 1 & order_underway_flag == 1) {
          scooter <- scooter + 1
          orders = 0
          order_underway_flag = 0
        }
        
        #This if statement is used to add scooters on inventory on a Fixed Delivery base
        if (fixed_delivery == 0) {
          #I used this if statement because in case fixed_delivery is disabled
          #and we get an exemption on the next statement *division with 0*
        } else if (delivery_day %% fixed_delivery == 0) {
          #finding mod of delivery day and fixed delivery number
          scooter = scooter + 1
        }
        
        #The following if statement is used to calculate sales and lost customers
        if (poisson_data[i] > 0) {
          customers <- customers + poisson_data[i]
          if (scooter >= poisson_data[i]) {
            scooter <- scooter - poisson_data[i]
            scooter_sold <- scooter_sold + poisson_data[i]
          } else if (scooter < poisson_data[i]) {
            dif <- abs(scooter - poisson_data[i])
            scooter_sold <- scooter_sold + scooter
            scooter <- 0
            if (delivery_to_home == 0) {
              lost_customer <- lost_customer + dif
            } else if (delivery_to_home == 1) {
              for (i in 1:dif) {
                customer_answer = rbinom(1, 1, 0.5)
                if (customer_answer == 0) {
                  lost_customer <- lost_customer + 1
                } else if (customer_answer == 1) {
                  scooter_sold_with_delivery = scooter_sold_with_delivery + 1
                }
              }
            }
          }
        }
        
        #following if statement is used to create orders when we are out of stock
        if (scooter == 0 &
            order_when_out == 1 & order_underway_flag == 0) {
          orders <- delivery_day
          order_underway_flag = 1
        }
        
        if (scooter > 0) {
          storage_cost <- storage_cost + scooter * 5
        }
      }
    }
    
    if (delivery_to_home == 0) {
      profit_loss_function <- scooter_sold * 200 - lost_customer * 100 - storage_cost
      #print(profit_loss_function)
    } else if (delivery_to_home == 1) {
      profit_loss_function <- scooter_sold * 200 + scooter_sold_with_delivery *
        125 - lost_customer * 100 - storage_cost
      scooter_sold <- scooter_sold + scooter_sold_with_delivery
    }
    
    #gathering trial data
    profit_trials[k] <- profit_loss_function
    customers_trials[k] <- customers
    lost_customers_trials[k] <- lost_customer
    storage_cost_trials[k] <- storage_cost
    overstock_trials[k] <- scooter
    scooters_sold_trials[k] <- scooter_sold
    
  }
  
  df <- data.frame(
    profit_trials = profit_trials,
    customers_trials = customers_trials,
    lost_customers_trials = lost_customers_trials,
    storage_cost_trials = storage_cost_trials,
    overstock_trials = overstock_trials,
    scooters_sold_trials = scooters_sold_trials
  )
  
  return(df)
}

#Strategy 1 metrics
strategy1_monthly <- my_function(4, 1, 0, 0) #4 weeks
strategy1_querterly <- my_function(13, 1, 0, 0) #12 weeks
strategy1_yearly <- my_function(52, 1, 0, 0) #52 weeks

#Strategy 2 metrics

strategy2_monthly <- my_function(4, 0, 7, 0) #4 weeks
strategy2_querterly <- my_function(13, 0, 7, 0) #12 weeks
strategy2_yearly <- my_function(52, 0, 7, 0)  #52 weeks

#Strategy 3 metrics

strategy3_monthly_10 <- my_function(4, 1, 10, 0) #4 weeks , fixed delivery every 10 days
strategy3_3month_10 <- my_function(13, 1, 10, 0) #12 weeks, fixed delivery every 10 days
strategy3_yearly_10 <- my_function(52, 1, 10, 0)  #52 weeks, fixed delivery every 10 days

#Strategy 4 metrics-Delivery to home

strategy4_monthly <- my_function(4, 1, 0, 1) #4 weeks
strategy4_querterly <- my_function(13, 1, 0, 1) #12 weeks
strategy4_yearly <- my_function(52, 1, 0, 1)  #52 weeks

#Testing other delivery schedules
#strategy3_Week1_12<-my_function(1,1,12) #1 week , fixed delivery every 12 days
#strategy3_monthly_12<-my_function(4,1,12) #4 weeks , fixed delivery every 12 days
#strategy3_3month_12<-my_function(13,1,12) #12 weeks, fixed delivery every 12 days
#strategy3_yearly_12<-my_function(52,1,12)  #52 weeks, fixed delivery every 12 days

#strategy3_Week1_15<-my_function(1,1,15) #1 week , fixed delivery every 15 days
#strategy3_monthly_15<-my_function(4,1,15) #4 weeks , fixed delivery every 15 days
#strategy3_3month_15<-my_function(13,1,15) #12 weeks, fixed delivery every 15 days
#strategy3_yearly_15<-my_function(52,1,15)  #52 weeks, fixed delivery every 15 days



#VISUALIZATIONS

#histograms

#Week 1
# This function to select vectors based on weeks and metric
get_vectors <- function(weeks, metric) {
  if (metric == 1) {
    if (weeks == 4) {
      return(list(
        strategy1_monthly[1],
        strategy2_monthly[1],
        strategy3_monthly_10[1],
        strategy4_monthly[1]
      ))
    } else if (weeks == 13) {
      return(list(
        strategy1_querterly[1],
        strategy2_querterly[1],
        strategy3_3month_10[1],
        strategy4_querterly[1]
      ))
    } else if (weeks == 52) {
      return(list(
        strategy1_yearly[1],
        strategy2_yearly[1],
        strategy3_yearly_10[1],
        strategy4_yearly[1]
      ))
    }
  } else if (metric == 2) {
    if (weeks == 4) {
      return(
        list(
          strategy1_monthly[3] / strategy1_monthly[2] * 100,
          strategy2_monthly[3] / strategy2_monthly[2] * 100,
          strategy3_monthly_10[3] / strategy3_monthly_10[2] * 100,
          strategy4_monthly[3] / strategy4_monthly[2] * 100
        )
      )
    } else if (weeks == 13) {
      return(
        list(
          strategy1_querterly[3] / strategy1_querterly[2] * 100,
          strategy2_querterly[3] / strategy2_querterly[2] * 100,
          strategy3_3month_10[3] / strategy3_3month_10[2] * 100,
          strategy4_querterly[3] / strategy4_querterly[2] * 100
        )
      )
    } else if (weeks == 52) {
      return(
        list(
          strategy1_yearly[3] / strategy1_yearly[2] * 100,
          strategy2_yearly[3] / strategy2_yearly[2] * 100,
          strategy3_yearly_10[3] / strategy3_yearly_10[2] * 100,
          strategy4_yearly[3] / strategy4_yearly[2] * 100
        )
      )
    }
  } else if (metric == 3) {
    if (weeks == 4) {
      return(
        list(
          strategy1_monthly[6] / strategy1_monthly[2] * 100,
          strategy2_monthly[6] / strategy2_monthly[2] * 100,
          strategy3_monthly_10[6] / strategy3_monthly_10[2] * 100,
          strategy4_monthly[6] / strategy4_monthly[2] * 100
        )
      )
    } else if (weeks == 13) {
      return(
        list(
          strategy1_querterly[6] / strategy1_querterly[2] * 100,
          strategy2_querterly[6] / strategy2_querterly[2] * 100,
          strategy3_3month_10[6] / strategy3_3month_10[2] * 100,
          strategy4_querterly[6] / strategy4_querterly[2] * 100
        )
      )
    } else if (weeks == 52) {
      return(
        list(
          strategy1_yearly[6] / strategy1_yearly[2] * 100,
          strategy2_yearly[6] / strategy2_yearly[2] * 100,
          strategy3_yearly_10[6] / strategy3_yearly_10[2] * 100,
          strategy4_yearly[6] / strategy4_yearly[2] * 100
        )
      )
    }
  } else if (metric == 4) {
    if (weeks == 4) {
      return(list(
        strategy1_monthly[5],
        strategy2_monthly[5],
        strategy3_monthly_10[5],
        strategy4_monthly[5]
      ))
    } else if (weeks == 13) {
      return(list(
        strategy1_querterly[5],
        strategy2_querterly[5],
        strategy3_3month_10[5],
        strategy4_querterly[5]
      ))
    } else if (weeks == 52) {
      return(list(
        strategy1_yearly[5],
        strategy2_yearly[5],
        strategy3_yearly_10[5],
        strategy4_yearly[5]
      ))
    }
  }
}

# Helper function to plot histograms
# Load the required library
library(ggplot2)

# Function to plot histograms using ggplot with vectors
plot_histograms_ggplot <- function(s1_vector,
                                   s2_vector,
                                   s3_vector,
                                   s4_vector,
                                   weeks,
                                   metric_name) {
  s1_vector <- as.numeric(s1_vector[!is.na(s1_vector)])
  s2_vector <- as.numeric(s2_vector[!is.na(s2_vector)])
  s3_vector <- as.numeric(s3_vector[!is.na(s3_vector)])
  s4_vector<-as.numeric(s4_vector[!is.na(s4_vector)])
  p <- ggplot() +
    
    geom_histogram(
      aes(x = s1_vector, fill = "Strategy 1"),
      alpha = 0.8,
      bins = 30,
      color = "white",
      position = "identity"
    ) +
    
    geom_histogram(
      aes(x = s2_vector, fill = "Strategy 2"),
      alpha = 0.9,
      bins = 30,
      color = "white",
      position = "identity"
    ) +
    
    geom_histogram(
      aes(x = s3_vector, fill = "Strategy 3"),
      alpha = 0.9,
      bins = 30,
      color = "white",
      position = "identity"
    ) +
    
    geom_histogram(
      aes(x = s4_vector, fill = "Strategy 4"),
      alpha = 0.9,
      bins = 30,
      color = "white",
      position = "identity"
    ) +
    
    labs(
      title = paste(weeks, "week", metric_name, "per Strategy"),
      x = metric_name,
      y = "Frequency",
      fill = "Strategies"
    ) +
    
    scale_fill_manual(values = c(
      "Strategy 1" = "#50336b",
      "Strategy 2" = "#f0baa2",
      "Strategy 3" = "#a69ab5",
      "Strategy 4" = "pink"
    )) +
    
    theme_minimal()
  
  # Print the plot
  return(p)
}

# Main visualize function
visualize <- function(weeks, metric) {
  # Assuming get_vectors() is a function that retrieves the necessary data
  vectors <- get_vectors(weeks, metric)
  
  #Switch gives us a different name for each input
  if (metric == 1) {
    metric_name = "Profits in $"
  } else if (metric == 2) {
    metric_name = "Customers Lost %"
  } else if (metric == 3) {
    metric_name = "Customers Served %"
  } else if (metric == 4) {
    metric_name = "Overstock"
  }
  
  # Call the plot function with the retrieved vectors
  plot_histograms_ggplot(vectors[[1]], vectors[[2]], vectors[[3]],vectors[[4]], weeks, metric_name)
}


#Switch codes
#1--> Profit in $
#2--> fraction of served customers
#3-->fraction of lost customers
#4-->Overstock
for (i in 1:4) {
  visualize(4, i)
  visualize(13, i)
  visualize(52, i)
}

visualize(52, 1)

# Create a bar plots for each value
barplots <- function(metric) {
  # Get the vectors and calculate the mean for each strategy for monthly, quarterly, and yearly time periods
  month_baseline <- c(mean(unlist(get_vectors(4, metric)[[1]]), na.rm = TRUE),
                      #null values removed
                      mean(unlist(get_vectors(4, metric)[[2]]), na.rm = TRUE),
                      mean(unlist(get_vectors(4, metric)[[3]]), na.rm = TRUE),
                      mean(unlist(get_vectors(4, metric)[[4]]), na.rm = TRUE))
  
  quarter_baseline <- c(mean(unlist(get_vectors(13, metric)[[1]]), na.rm = TRUE),
                        mean(unlist(get_vectors(13, metric)[[2]]), na.rm = TRUE),
                        mean(unlist(get_vectors(13, metric)[[3]]), na.rm = TRUE),
                        mean(unlist(get_vectors(13, metric)[[4]]), na.rm = TRUE))
  
  yearly_baseline <- c(mean(unlist(get_vectors(52, metric)[[1]]), na.rm = TRUE),
                       mean(unlist(get_vectors(52, metric)[[2]]), na.rm = TRUE),
                       mean(unlist(get_vectors(52, metric)[[3]]), na.rm = TRUE),
                       mean(unlist(get_vectors(52, metric)[[4]]), na.rm = TRUE))
  
  # Set the metric name based on the provided metric
  if (metric == 1) {
    metric_name <- "Profits in $"
  } else if (metric == 2) {
    metric_name <- "Customers Lost %"
  } else if (metric == 3) {
    metric_name <- "Customers Served %"
  } else if (metric == 4) {
    metric_name <- "Overstock"
  }
  
  # Create separate data for each strategy
  strategy1 <- c(month_baseline[1], quarter_baseline[1], yearly_baseline[1])
  strategy2 <- c(month_baseline[2], quarter_baseline[2], yearly_baseline[2])
  strategy3 <- c(month_baseline[3], quarter_baseline[3], yearly_baseline[3])
  strategy4 <- c(month_baseline[4], quarter_baseline[4], yearly_baseline[4])
  
  
  # Combine the data for strategies into a matrix
  strategies <- rbind(strategy1, strategy2, strategy3,strategy4)
  
  # Define colors for each strategy
  color1 <- rgb(80, 51, 107, maxColorValue = 255)  # #50336b
  color2 <- rgb(240, 186, 162, maxColorValue = 255)  # #7b6690
  color3 <- rgb(166, 154, 181, maxColorValue = 255)  # #a69ab5
  color4<- "pink"
  
  # Define time periods as x-axis labels
  groups <- c("Monthly", "Quarterly", "Yearly")
  
  # Create the grouped bar plot
  barplot(
    strategies,
    beside = TRUE,
    # Creates bars side by side
    names.arg = groups,
    # Group labels (time periods)
    col = c(color1, color2, color3,color4),
    main = paste(metric_name, "per Strategy"),
    xlab = "Timeline",
    ylab = metric_name,
    ylim = c(0, max(strategies) * 1.2)
  )
  
  # Add a legend to the plot
  legend(
    "topleft",
    legend = c("Strategy 1", "Strategy 2", "Strategy 3","Strategy 4"),
    fill = c(color1, color2, color3,color4),
    bty = "n"           # No box around the legend
  )
}

# Creating barplots
barplots(1)
barplots(2)
barplots(3)
barplots(4)
visualize(4,1)

#Box Plots

vec1=unlist(get_vectors(52, 1)[1])
vec2=unlist(get_vectors(52, 1)[2])
vec3=unlist(get_vectors(52, 1)[3])
vec4=unlist(get_vectors(52, 1)[4])

boxplot(list(vec1,vec2,vec3,vec4),
        names = c("Strategy 1", "Strategy 2", "Strategy 3", "Strategy 4"),
        main = "Comparison of Strategies",
        xlab = "Strategies",
        ylab = "Profit in USD")

summary(vec1)
summary(vec2)
summary(vec3)
summary(vec4)

library(BSDA)

# Z-test for Strategy 2 vs Strategy 1
Strategy2_test <- z.test(vec2, vec1, alternative = "greater", sigma.x = sd(vec2), sigma.y = sd(vec1))

# Z-test for Strategy 3 vs Strategy 1
Strategy3_test <- z.test(vec3, vec1, alternative = "greater", sigma.x = sd(vec3), sigma.y = sd(vec1))

# Z-test for Strategy 4 vs Strategy 1
Strategy4_test <- z.test(vec4, vec1, alternative = "greater", sigma.x = sd(vec4), sigma.y = sd(vec1))

# Print results
print(Strategy2_test)
print(Strategy3_test)
print(Strategy4_test)

