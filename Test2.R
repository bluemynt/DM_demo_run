# Part 4

#Customer Analysis

#Analyse Customers based on demographic information

#Firstly, join Customers and Orders tables for furthur analysis

connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecomm.db")

#Join Customers and Orders_items
joint_cust_query <- "SELECT * FROM Customers CROSS JOIN Order_items"
cust_order_items <- DBI::dbGetQuery(connection, joint_cust_query)

cust_order_items$cust_dob <- as.Date(cust_order_items$cust_dob) #To maintain the data structure of cust_dob table is DATE after joined table 


cust_analysis <- cust_order_items %>%
  mutate(cust_yob = year(cust_dob),
         cust_age = 2024 - cust_yob)

cust_group <- cust_analysis %>% 
  group_by(age_range = cut(cust_age, breaks = c(0, 20, 30, 40, 50, Inf), labels = c("Under 20", "21-30", "31-40", "41-50", "51+"))) %>%
  summarize(total_customers = n(),
            average_age = mean(cust_age))

#Summarise Orders quantity based on Orders
# Define custom age ranges
age_ranges <- c("Under 20", "21-30", "31-40", "41-50", "51+")

# Convert cust_age to factor with custom age ranges
cust_analysis$cust_age_range <- cut(cust_analysis$cust_age, breaks = c(0, 20, 30, 40, 50, Inf), labels = age_ranges, right = FALSE)

# Plot the data as a bar plot
cust_order_plot <- plot_ly(data = cust_analysis, x = ~cust_age_range, y = ~order_item_quantity, type = 'bar', marker = list(color = '#1f77b4')) %>%
  layout(xaxis = list(title = "Age Range"),
         yaxis = list(title = "Total Order Items Quantity"),
         title = "Total Order Items Quantity by Age Range")


# View the result
cust_order_plot

this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("figures/cust_order_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))
