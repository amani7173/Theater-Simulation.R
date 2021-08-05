# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack
# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch

# Cost for adults and children
colors <- c("#03045e", "#023e8a" , "#0077b6", "#0096c7", "#00b4d8")
snacks_prices <- c(1, 2, 3, 4)
ticket_cost <- 10
ticket_cost_child <- 5
screens <- 5 # How many screens does the theater have? (assume 1 per movie)
movies <- c('Mortal Kombat', 
            'Godzilla vs Kong',
            'Toy Story', 
            'Promising Young Woman', 
            'Tom and Jerry: The Movie')  # List 5 of your favorite movies
seats <- 20 # How many seats does each theater hold
week_days <- c("Sat", "Sun", "Mon", "Tue", "Wed", "Thr", "Fri")  # Store totals for each day

revenue_per_day <- rep(NA, length(week_days))
revenue_per_movie <- rep(0,screens)
revenue_from_snacks_per_movie <- rep(0,screens)
revenue_from_snacks_per_day<- rep(NA, length(week_days))

# iterate through the week
for (day in 1:length(week_days)) {
  cat(paste("\n#### Revenue for", week_days[day],"###\n"))
  # Keep track of total revenue for the day
  screens_revenue <- rep(NA, screens)
  current_screen_revenue <- 0
  
  # iterate through the amount of screens on a particular day
  for (screen in 1:screens) {
    cat(paste("--- Revenue for the movie", movies[screen],"---\n"))
    # Calculate  how many adults and children are watching the movie
    visitors_adults <- sample(0:seats, 1)
    visitors_children <- sample(0:(seats-visitors_adults),1)
    
    
    cat(paste("Number of adult visitors:", visitors_adults, "\n"))
    cat(paste("Number of child visitors:", visitors_children, "\n"))
    
    
    # Calculate the revenue for adults and children
    adults_revenue <- visitors_adults * ticket_cost
    children_revenue <- visitors_children * ticket_cost_child
    cat(paste("revenue from adult visitors: $", adults_revenue, "\n", sep = ''))
    cat(paste("revenue from child visitors: $", children_revenue, "\n", sep = ''))
    
    # Calculate revenue form snacks
    snaks_buyers <- sample(0:sum(visitors_adults,visitors_children), length(snacks_prices), replace = TRUE)
    snaks_revenue <- snaks_buyers*snacks_prices
    revenue_from_snacks_per_movie[screen] <- sum(snaks_revenue)
    
    # Calculate revenue, and add to running total for the day
    current_screen_revenue <- sum(adults_revenue, children_revenue)
    screens_revenue[screen] <- current_screen_revenue
    revenue_per_movie[screen] <- sum(revenue_per_movie[screen], current_screen_revenue)
    cat(paste("total revenue: $",current_screen_revenue, "\n\n", sep = ''))
  }
  
  # Save total to the corresponding day
  revenue_per_day[day] <- sum(screens_revenue)
  
  revenue_from_snacks_per_day[day] <- sum(revenue_from_snacks_per_movie)
  cat(paste("total revenue per day: $",sum(screens_revenue), "\n", sep = ''))
}

revenue_df <- data.frame(
  name=week_days,
  value=revenue_per_day
)

snacks_df <- data.frame(
  name=week_days,
  value=revenue_from_snacks_per_day
)

sorted_revenue_per_day_from_tickets_df <- revenue_df[order(revenue_df$value,decreasing = TRUE),]
sorted_revenue_per_day_from_snacks_df <- snacks_df[order(snacks_df$value, decreasing = TRUE),]

par(mfrow=c(3,1), mar=c(2,2,2,2))

# Plotting the revenue form Tickets
r <- barplot(
  height=revenue_df$value,
  names=revenue_df$name,
  col="#03045e",
  width = 2,
  main="Revenue per Day from Tickets",
  xlab = "Days",
  ylab = "Revenue"
)
text(r,(revenue_df$value - 25),labels=revenue_df$value, col = "white")

# Plotting the revenue from snacks
s <- barplot(
  height=snacks_df$value,
  names=snacks_df$name,
  col="#219ebc",
  main="Revenue per Day from Snacks",
  xlab = "Days",
  ylab = "Revenue"
)
text(s,(snacks_df$value - 15),labels=snacks_df$value, col = "white")

# Plotting the weekly revenue of each movie
x <- revenue_per_movie
labels <- movies
piepercent<- round(100*x/sum(x), 2)
pie(
  revenue_per_movie,
  labels = piepercent,
  clockwise = TRUE,
  radius = 0.85,
  col = colors,
  border = "black",
  main="Weekly Revenue per Movie %",)

legend("bottomright",c(movies),cex= 1, fill = colors)
