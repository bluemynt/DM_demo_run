# Selecting Product ID, Category, and Order Date

connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecomm.db")

query <- "SELECT a.product_id AS Product_ID, a.products_category AS Category, b.order_date AS Order_Date
FROM Products a
JOIN order_items c ON a.product_id = c.product_id
JOIN Orders b ON b.order_id = c.order_id;"

categories_df <- DBI::dbGetQuery(connection, query)

# Manipulating date, creating month column and counting number of times category was ordered in a month
categories_df <- mutate(categories_df, Order_Date = as.POSIXct(categories_df$Order_Date, origin = "1970-01-01", tz = "UTC"),
                        Month = format(Order_Date, "%b")) %>% select(Category, Month) %>% count(Category, Month, name = "Count")

# Ordering months
categories_df$Month <- factor(categories_df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plot showing number of orders placed each month across all categories
fig <- ggplot(categories_df, aes(x = Month, y = Count, group=Category, color=Category)) + geom_point() +geom_line()
ggplotly(fig)

this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("figures/category_plot",
              this_filename_date,"_",
              this_filename_time,".png"))
