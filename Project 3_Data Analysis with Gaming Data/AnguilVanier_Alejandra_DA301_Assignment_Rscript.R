
################################################################################
################################################################################
#####                                                                      #####
#####                        TURTLE GAMES ANALYSIS:                        #####
#####                      PREDICTING FUTURE OUTCOMES                      #####
#####                           SALES DEPARTMENT                           #####
#####                                                                      #####
################################################################################
################################################################################



## BACKGROUND AND CONTEXT 

## Turtle Games is a game manufacturer and retailer. They manufacture and sell 
## their own products, along with sourcing and selling products manufactured by 
## other companies. Their product range includes books, board games, video games 
## and toys. They have a global customer base and have a business objective of 
## improving overall sales performance by utilizing customer trends. 

## The project has been split into two departments:
## - Marketing, which will be performed in Python (Questions 1, 2 and 3)
## - Sales, which is the task we will carry on in R (Questions 4, 5 and 6)

## For the Sales department, the main questions Turtle Games wants to explore are:
## - Question 4: what is the impact on sales per product.
## - Question 5: the reliability of the data (e.g. normal distribution, 
##     Skewness, Kurtosis).
## - Question 6: if there is any possible relationship(s) in sales between North 
##     America, Europe, and global sales.

## dAtaAnalysisAV has been tasked to look into this project for Turtle Games using 
## the information provided about customers, customer reviews and overall sales.
 
## This R script is the collection of the codes applied to uncover trends and 
## patterns, and predict future outcomes, on the data set provided by the Sales
## Department on Turtle Games: turtle_sales.csv

## Please note we have inserted comments along the way to capture insights as we 
## develop the code. For better readability, we have inserted these between 
## lines as per the below:
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Please follow the link below to the GitHub repository where this R Script, the 
## Jypiter Notebook, the Executive Summary and the Presentation are stored, along 
## with all the supporting files:
# https://github.com/AlejandraAnguilVanier/AnguilVanier_Alejandra_DA301_Assignment


################################################################################

# Question 4: what is the impact on sales per product.

# Steps we will follow:
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note our observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include our insights and observations.

################################################################################

# 1. Load and explore the data

# Install Tidyverse.
install.packages('tidyverse')

# Import Tidyverse library.
library(tidyverse)

# Set the working directory and import the data set.
sales <- read.csv('turtle_sales.csv', header = TRUE)
# Otherwise we can use:
sales <- read.csv(file.choose(), header = TRUE)

# Print the data frame.
head(sales)

# Verify the data structure.
class(sales)

# Use the glimpse() function to determine the structure of the data set (column 
# names, data types, dimension, and values).
glimpse(sales)

# Check if there are any NA values.
sum(is.na(sales))
sum(is.na(sales$Year))


#------------------------------------------------------------------------------#
# The class function has confirmed we have a data frame loaded correctly. The 
# glimpse function tells us that this data frame consists in 352 rows and 9 
# columns, with data types that varies between integers, doubles and characters.
# We have also discovered there are two missing values on the Year column. Since 
# we have been asked to remove this column, we are not going to worry about it
# for now. But we keep this in mind if later on we decide to work with this data.
# However, the Product column is treated as an integer, but in fact it is related 
# to the ID of each product. Therefore we are going to change the type to factor:

sales <- mutate(sales, Product = as.factor(Product))

# Check the data types.
glimpse(sales)
#------------------------------------------------------------------------------#


# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns (Ranking, Year, Genre, Publisher). 
sales_clean <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(sales_clean)

# Use the glimpse() function to get an idea of the dimension of the data frame
# as well as the data types, etc.
glimpse(sales_clean)


#------------------------------------------------------------------------------#
# After removing those columns the glimpse function tells us that we have now a 
# data frame consisting in 352 rows and 5 columns, and we still have the same 
# mix of data types (integers, doubles and characters). 
#------------------------------------------------------------------------------#


# View the descriptive statistics.
summary(sales_clean)

# Let's also check if there are any NA values.
sum(is.na(sales_clean))
table(sales_clean$Product)
table(sales_clean$Platform)
table(sales_clean$NA_Sales)
table(sales_clean$EU_Sales)
table(sales_clean$Global_Sales)


#------------------------------------------------------------------------------#
# We can see we have a tidy data set to work with, where each variable has a 
# column, each observation has a row and each value has a cell (therefore all 
# cells have a value). 
# We can also see that there are 22 platforms where these products are being sold, 
# although not all products have been sold in all platforms. By this count we can 
# see there are platforms where lots of different products (more than 30) have 
# been sold (X360, PS3, PC, Wii) while some platforms only sold less than 5 
# (2600, GEN, PSV). This starts to give us an idea of the most popular platforms.    
#------------------------------------------------------------------------------#


################################################################################

# 2. Create plots to review and determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

qplot(Product,
      NA_Sales,
      colour = Platform,
      data = sales_clean)


#------------------------------------------------------------------------------#
# North America Sales: there are three products above £20M.We can also see there
# is a trend on the sales with the lower product IDs having a higher amount of
# sales and this going slightly down as the product ID goes up. Is this because the 
# product IDs correspond to year of release? Or maybe some companies have been 
# in the industry for longer so their products have better performance? Or simply 
# those games are popular because are good?
#------------------------------------------------------------------------------#


qplot(Product,
      EU_Sales,
      colour = Platform,
      data = sales_clean)


#------------------------------------------------------------------------------#
# Europe Sales: there is one product above £20M. We also get a similar trend as 
# above.
#------------------------------------------------------------------------------#


qplot(Product,
      Global_Sales,
      colour = Platform,
      data = sales_clean)


#------------------------------------------------------------------------------#
# Global Sales: there are a number of products above £20M. We can see the trend
# mentioned for North America Sales and Europe Sales more clearly, although 
# as before it doesn't happen for all products.
#------------------------------------------------------------------------------#


qplot(Platform,
      NA_Sales,
      data = sales_clean)


#------------------------------------------------------------------------------#
# North America Sales: the three products above £20M identified earlier belong 
# to Wii and NES platforms. Both belong to Nintendo, so we may need to get this
# column back at look for further trends here.
#------------------------------------------------------------------------------#


qplot(Platform,
      EU_Sales,
      data = sales_clean)


#------------------------------------------------------------------------------#
# Europe Sales: the product above £20M identified earlier belongs to Wii 
# platform. And again, Nintendo is the best seller.
#------------------------------------------------------------------------------#


qplot(Platform,
      Global_Sales,
      data = sales_clean)


#------------------------------------------------------------------------------#
# Global Sales: the products above £20M identified earlier belong to DS, GB, NES,
# and Wii platforms. And again, all of them belong to Nintendo!
#------------------------------------------------------------------------------#


qplot(NA_Sales,
      Global_Sales,
      colour = Product,
      data = sales_clean)


#------------------------------------------------------------------------------#
# As expected, there is a pretty strong positive correlation between the North 
# America Sales and the Global Sales. However, even with the Product color coded 
# is difficult to see which colors correspond to which products in the plot (as 
# there are so many products). Therefore, maybe we can explore further by grouping 
# these into smaller sections (i.e. 10 groups of hundreds codes, 1000 codes, 
# 2000 codes, etc.).
#------------------------------------------------------------------------------#


qplot(EU_Sales,
      Global_Sales,
      colour = Product,
      data = sales_clean)


#------------------------------------------------------------------------------#
# There is also a pretty strong positive correlation between the Europe Sales 
# and the Global Sales.However, we can see a few products that although having
# high sales globally, the sales in Europe is low (less than £5M). This means
# that the other regions are selling these products much better. 
#------------------------------------------------------------------------------#


qplot(NA_Sales,
      EU_Sales,
      colour = Product,
      data = sales_clean)


#------------------------------------------------------------------------------#
# There is also a pretty strong positive correlation between the Europe Sales 
# and the North America Sales. However, we can spot those products identified 
# earlier as not having a consistent sales across regions. There are particularly 
# around the £20M sales in North America which are below £5M in Europe.
#------------------------------------------------------------------------------#


## 2b) Histograms
# Create histograms.

qplot(Product, 
      data = sales_clean)


#------------------------------------------------------------------------------#
# Too many products for this plot to be meaningful. A count will be more useful.
#------------------------------------------------------------------------------#

sales_clean %>%
  count(Product) %>% 
  arrange(desc(n))


#------------------------------------------------------------------------------#
# Now we can see the ratio of products per platform, where the products sold in 
# more than five platforms are 3645, 2518, 3967, 3887, 9080, 1945, 2285, 2521, 
# 3657. We will check later if these are the products with the higher sell numbers.
#------------------------------------------------------------------------------#


qplot(Platform, 
      data = sales_clean)


#------------------------------------------------------------------------------#
# The platforms with the most products in their base (if we draw a threshold of 
# equal or more than 30) are PC, PS3, Wii and X360.
# From the platforms identified earlier with products above £20M (DS, GB, NES and 
# Wii), only one of them have more than 30 products (Wii). The others have:
# - DS just above 25 products.
# - GB just below 15 products.
# - NES around 8 products.
# Can we conclude from this that these products are selling really well? We will
# investigate further.
# Actually, let's do also a count to have the numbers handy for our report:

sales_clean %>%
  count(Platform) %>% 
  arrange(desc(n))

# Now we can confirm the exact numbers are:
# - Wii: 30 products.
# - DS: 26 products.
# - GB: 14 products.
# - NES: 8 products.
#------------------------------------------------------------------------------#


qplot(NA_Sales, 
      bins = 40,
      fill = Platform, 
      data = sales_clean)


#------------------------------------------------------------------------------#
# We can identify again those three outliers or products that have been sold 
# more than £20M in North America. But as stated earlier, we are not going to 
# remove them as these are some of the best selling products. However, we will
# add a note in our report regarding further investigation of possible causes
# why these are not being sold consistently across all regions. 
#------------------------------------------------------------------------------#


qplot(EU_Sales, 
      bins = 40,
      fill = Platform, 
      data = sales_clean)


#------------------------------------------------------------------------------#
# We see again the outlier identified earlier, belonging to the Wii platform as
# per the color code.
#------------------------------------------------------------------------------#


qplot(Global_Sales, 
      bins = 40,
      fill = Platform, 
      data = sales_clean)


## 2c) Boxplots
# Create boxplots.

qplot(NA_Sales, 
      data = sales_clean, 
      colour = I('red'), 
      geom = 'boxplot')


#------------------------------------------------------------------------------#
# And again we can see those three outliers or better, products that have sold
# more than £20M in North America. There is actually another just below 20, which 
# could be included into this group.
#------------------------------------------------------------------------------#


qplot(EU_Sales, 
      data = sales_clean, 
      colour = I('blue'), 
      geom = 'boxplot')


#------------------------------------------------------------------------------#
# The same logic applies here, where we can see that outlier or product that has
# sold more than £20M in Europe.
#------------------------------------------------------------------------------#


qplot(Global_Sales, 
      data = sales_clean, 
      colour = I('dark green'), 
      geom = 'boxplot')


#------------------------------------------------------------------------------#
# And again with the Global Sales, where we can see a few values over £20M.
#------------------------------------------------------------------------------#


qplot(NA_Sales, 
      Platform, 
      data = sales_clean, 
      colour = I('red'), 
      geom = 'boxplot')


#------------------------------------------------------------------------------#
# On this scatterplot we can see the values above the £20M. However, within the 
# platform Wii the value is treated as an outlier, but within the NES platform
# these top figures belong to the upper whisker of the plot, which means they are 
# within the upper quartile (the end of the box) and the maximum value.
#------------------------------------------------------------------------------#


qplot(EU_Sales, 
      Platform, 
      data = sales_clean, 
      colour = I('blue'), 
      geom = 'boxplot')


#------------------------------------------------------------------------------#
# On this one we can clearly see the outlier on the Wii platform.
# If we look at the other platforms, we could consider some other outliers within 
# each platform, but we are again going to consider that not all the games 
# perform equally and we are going to keep this data.
#------------------------------------------------------------------------------#


qplot(Global_Sales, 
      Platform, 
      data = sales_clean, 
      colour = I('dark green'), 
      geom = 'boxplot')


#------------------------------------------------------------------------------#
# Same conclusions as earlier, but a very good way to see which platforms have 
# better overall sales.
#------------------------------------------------------------------------------#


################################################################################

# 3. Observations and insights

## All the plots are showing outliers in the sales columns. However, we can only
# understand that these correspond to the best selling products so we have 
# decided to keep them in our analysis. 

## North America Sales: there are three products above £20M (the metadata 
# explains that these columns use pounds and are displayed in millions). 
# We have identified these as belonging to the Wii and NES platforms.

## Europe Sales: there is one product above £20M. We have identified that this 
# one belongs to the Wii platform.

## Looking at the color coded plots, the best selling product is number 107, 
# which belongs to the Wii platform, by Nintendo.


################################################################################
################################################################################


# Question 5: the reliability of the data.

## Steps we will follow:
# 1. Load and explore the data.
##  - Continue to use the data frame that we prepared for question 4. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
##  - Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 3. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 4. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 5. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(sales_clean)

# Check output: Determine the min, max, and mean values of all the sales data.

# [NA Sales]
min(sales_clean$NA_Sales)
max(sales_clean$NA_Sales)
mean(sales_clean$NA_Sales)
# Or to get a better summary:
summary(sales_clean$NA_Sales)

# [EU Sales]
min(sales_clean$EU_Sales)
max(sales_clean$EU_Sales)
mean(sales_clean$EU_Sales)
# Or to get a better summary:
summary(sales_clean$EU_Sales)

# [Global Sales]
min(sales_clean$Global_Sales)
max(sales_clean$Global_Sales)
mean(sales_clean$Global_Sales)
# Or to get a better summary:
summary(sales_clean$Global_Sales)

# [Or]
sales_desc_summary <- sales_clean %>% 
  summarise(NA_Sales_min = min(NA_Sales),
            NA_Sales_max = max(NA_Sales),
            NA_Sales_mean = mean(NA_Sales),
            EU_Sales_min = min(EU_Sales),
            EU_Sales_max = max(EU_Sales),
            EU_Sales_mean = mean(EU_Sales),
            Global_Sales_min = min(Global_Sales),
            Global_Sales_max = max(Global_Sales),
            Global_Sales_mean = mean(Global_Sales))

sales_desc_summary

# View the descriptive statistics (all the above in one go).
summary(sales_clean)


#------------------------------------------------------------------------------#
# Before going any further, let's add a column for Other_Sales, which as per the
# metadata will be Global_Sales minus the sum of NA_Sales and EU_Sales.

sales_clean <- sales_clean %>%
  mutate(Other_Sales = Global_Sales - (NA_Sales + EU_Sales)) 

# Round the new column to two decimals.
sales_clean$Other_Sales <- round(sales_clean$Other_Sales, digits = 2)

# Reorder the columns so Global_Sales (the total sales) appears at the end again.
sales_clean <- sales_clean[, c("Product", "Platform", "NA_Sales",
                               "EU_Sales", "Other_Sales", "Global_Sales")] 

# View the updated data frame.
head(sales_clean)
str(sales_clean)
#------------------------------------------------------------------------------#


################################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_product <- sales_clean %>% 
  group_by(Product) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Other_Sales_sum = sum(Other_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups = 'drop')

# View the data frame.
head(sales_product)

# Explore the data frame.
dim(sales_product)
str(sales_product)
summary(sales_product)
arrange(sales_product, desc(Global_Sales_sum))
print(arrange(sales_product, desc(Global_Sales_sum)), n = 15)


#------------------------------------------------------------------------------#
# There are 175 products in total. The top five products by Global Sales are 107, 
# 515, 123, 254 and 195.
# However, worth noting at this point that two of these five products (123 and 
# 254) have performed very well in one region (North America) but not in the 
# others (Europe and other unknown regions). Also worth noting that they don't 
# correspond to the products listed earlier as sold in more than five platforms.
#------------------------------------------------------------------------------#


# Let's do the same exercise with the platform column to check how these are
# performing against the overall sales.
sales_platform <- sales_clean %>% 
  group_by(Platform) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Other_Sales_sum = sum(Other_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups = 'drop')
  
# View the data frame.
head(sales_platform)

# Explore the data frame.
dim(sales_platform)
str(sales_platform)
arrange(sales_platform, desc(Global_Sales_sum))
print(arrange(sales_platform, desc(Global_Sales_sum)), n = 22)


#------------------------------------------------------------------------------#
# There are 22 platforms. The top six platforms by Global Sales (more than £100M)
# are Wii, X360, PS3, Ds, GB and PS2.
# However, again worth noting at this point that none of them have a consistent 
# sales across all the regions.  
#------------------------------------------------------------------------------#


## 2b) Determine which plot is the best to compare game sales.

# Create scatterplots.

ggplot(sales_product, aes(x = NA_Sales_sum, y = Global_Sales_sum)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Global Sales",
       subtitle = "Sales in million of £")


ggplot(sales_product, aes(x = EU_Sales_sum, y = Global_Sales_sum)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0, 25, 2.5), "Europe Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Europe and Global Sales",
       subtitle = "Sales in million of £") 


ggplot(sales_product, aes(x = Other_Sales_sum, y = Global_Sales_sum)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0, 10, 1), "Other Regions Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Other and Global Sales",
       subtitle = "Sales in million of £") 


ggplot(sales_product, aes(x = NA_Sales_sum, y = EU_Sales_sum)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 25, 5), "Europe Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Europe Sales",
       subtitle = "Sales in million of £") 


ggplot(sales_product, aes(x = NA_Sales_sum, y = Global_Sales_sum, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Global Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") 


ggplot(sales_clean, aes(x = NA_Sales, y = Global_Sales, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Global Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") +
  facet_wrap(~Platform)


ggplot(sales_product, aes(x = EU_Sales_sum, y = Global_Sales_sum, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 25, 2.5), "Europe Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Europe and Global Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") 


ggplot(sales_clean, aes(x = EU_Sales, y = Global_Sales, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 25, 5), "Europe Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Europe and Global Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") +
  facet_wrap(~Platform)


ggplot(sales_product, aes(x = Other_Sales_sum, y = Global_Sales_sum, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10, 1), "Other Regions Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Other and Global Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") 


ggplot(sales_clean, aes(x = Other_Sales, y = Global_Sales, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10, 1), "Other Regions Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Other and Global Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") +
  facet_wrap(~Platform)


ggplot(sales_product, aes(x = NA_Sales_sum, y = EU_Sales_sum, color = Product)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 25, 5), "Europe Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Europe Sales",
       subtitle = "Sales in million of £",
       color = "Product ID") 


ggplot(sales_clean, aes(x = NA_Sales, y = Global_Sales, color = Platform)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Global Sales",
       subtitle = "Sales in million of £")


ggplot(sales_clean, aes(x = EU_Sales, y = Global_Sales, color = Platform)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5), "Europe Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Europe and Global Sales",
       subtitle = "Sales in million of £")


ggplot(sales_clean, aes(x = Other_Sales, y = Global_Sales, color = Platform)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10, 1), "Other Regions Sales") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Global Sales") +
  theme_minimal() +
  labs(title = "Relationship between Other Regions and Global Sales",
       subtitle = "Sales in million of £") 


ggplot(sales_clean, aes(x = NA_Sales, y = EU_Sales, color = Platform)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5), "North America Sales") +
  scale_y_continuous(breaks = seq(0, 25, 5), "Europe Sales") +
  theme_minimal() +
  labs(title = "Relationship between North America and Europe Sales",
       subtitle = "Sales in million of £") 


#------------------------------------------------------------------------------#
# We can also use the GGally library to get all these scatterplots in one go and 
# be able to compare them in the same plot.

# Install GGally.
install.packages('GGally')

# Import GGally library.
library(GGally)

GGally::ggpairs(sales_clean, columns = 3:6)

# With either the individual plots or the correlation plots, we can draw the same
# conclusions as we did last week. We have however added a component: correlation.
# Which is telling us the strong correlation between the sales variables.
# We have also been able to add more information to the plots so they are better 
# prepared for future use in presentations to the various stakeholders.
# For the plots using a fourth variable (Platform), we have used the cleann data
# and not the grouped one (which doesn't contained this variable).
#------------------------------------------------------------------------------#


# Create histograms.
# We have to use the sales_clean data set for Products and Platforms, but we will
# try the grouped one on the sales columns.

ggplot(sales_clean, aes(x = fct_infreq(Platform))) + 
  geom_histogram(stat = 'count') +
  theme_minimal() +
  labs(title = "Number of Products by Platform",
       x = "Platform",
       y = "Count of Products")


ggplot(sales_clean, aes(x = fct_infreq(Platform))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5) +
  theme_minimal() +
  labs(title = "Number of Products by Platform",
       x = "Platform",
       y = "Count of Products")


#------------------------------------------------------------------------------#
# Using geom_bar instead geom_histogram we can get the number of products for 
# each bar, making this plot more useful. This count refers to the original data 
# (352 products) and not the total count of unique products (175 products) because
# we know some products are sold in more than one platform.
#------------------------------------------------------------------------------#


ggplot(sales_clean, aes(x = fct_infreq(Product))) + 
  geom_histogram(stat = 'count') +
  theme_minimal() +
  labs(title = "Number of Products",
       x = "Product ID",
       y = "Count of Products")


#------------------------------------------------------------------------------#
# There are too many products making this plot unreadable.  
#------------------------------------------------------------------------------#


ggplot(sales_clean, aes(x = NA_Sales)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of North America Sales",
       subtitle="Sales in million of £",
       x = "North America Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the row data in North America sales we get a histogram strongly positively 
# skewed (or right-skewed distribution) with a long right tail. It is a sort of 
# distribution where the measures are dispersing (unlike symmetrically distributed 
# data where all measures of the central tendency -mean, median, and mode- are 
# equal to each other).
#------------------------------------------------------------------------------#


ggplot(sales_product, aes(x = NA_Sales_sum)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of North America Sales",
       subtitle="Sales in million of £",
       x = "North America Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the grouped data we get a better histogram for the North America sales, 
# although still positively skewed with a long right tail. If we were going to 
# remove the outliers (as already mentioned before, the best selling products), 
# we could improve this distribution and make the data closer to a normal 
# distribution. This is something that needs to be discussed with Turtle Games.
#------------------------------------------------------------------------------#


ggplot(sales_clean, aes(x = EU_Sales)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of Europe Sales",
       subtitle="Sales in million of £",
       x = "Europe Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the row data in Europe sales we also get a histogram strongly positively 
# skewed with a long right tail. 
#------------------------------------------------------------------------------#


ggplot(sales_product, aes(x = EU_Sales_sum)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of Europe Sales",
       subtitle="Sales in million of £",
       x = "Europe Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the grouped data we get a better histogram for the Europe sales too, 
# although still positively skewed with a long right tail. If we were going to 
# remove the outliers (as already mentioned before, the best selling products), 
# we could improve this distribution and make the data closer to a normal 
# distribution. This is something that needs to be discussed with Turtle Games.
#------------------------------------------------------------------------------#


ggplot(sales_clean, aes(x = Other_Sales)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of Other Regions Sales",
       subtitle="Sales in million of £",
       x = "Other Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the row data in Other Regions sales we also get a histogram strongly 
# positively skewed with a long right tail. 
#------------------------------------------------------------------------------#


ggplot(sales_product, aes(x = Other_Sales_sum)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of Other Regions Sales",
       subtitle="Sales in million of £",
       x = "Other Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the grouped data we get a better histogram for Other Regions sales too, 
# although still positively skewed with a long right tail. If we were going to 
# remove the outliers (as already mentioned before, the best selling products), 
# we could improve this distribution and make the data closer to a normal 
# distribution. This is something that needs to be discussed with Turtle Games.
#------------------------------------------------------------------------------#


ggplot(sales_clean, aes(x = Global_Sales)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of Global Sales",
       subtitle="Sales in million of £",
       x = "Global Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the row data in Global Regions sales we also get a histogram strongly 
# positively skewed with a long right tail. 
#------------------------------------------------------------------------------#


ggplot(sales_product, aes(x = Global_Sales_sum)) + 
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "Overview of Global Sales",
       subtitle="Sales in million of £",
       x = "Global Sales",
       y = "Count")


#------------------------------------------------------------------------------#
# With the grouped data in Global Regions sales we still get a histogram strongly 
# positively skewed with a long right tail. 
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# These histograms don't tell us anymore than what we got from question 4 plots.
# However, we have been able to add more information so they are better prepared
# for future use in presentations to the various stakeholders. We have also been
# able to slightly improve the distribution of sales data using the grouped 
# data frame (except for the Global_Sales).
#------------------------------------------------------------------------------#


# Create boxplots.

ggplot(sales_product, aes(x = NA_Sales_sum)) + 
  geom_boxplot(color = 'red', outlier.size = 3) +
  theme_minimal() +
  labs(title = "Overview of North America Sales",
       subtitle="Sales in million of £",
       x = "North America Sales")


ggplot(sales_product, aes(x = EU_Sales_sum)) + 
  geom_boxplot(color = 'blue', outlier.size = 3) +
  theme_minimal() +
  labs(title = "Overview of Europe Sales",
       subtitle="Sales in million of £",
       x = "Europe Sales")


ggplot(sales_product, aes(x = Other_Sales_sum)) + 
  geom_boxplot(color = 'purple', outlier.size = 3) +
  theme_minimal() +
  labs(title = "Overview of Other Regions Sales",
       subtitle="Sales in million of £",
       x = "Other Sales")


ggplot(sales_product, aes(x = Global_Sales_sum)) + 
  geom_boxplot(color = 'dark green', outlier.size = 3) +
  theme_minimal() +
  labs(title = "Overview of Global Sales",
       subtitle="Sales in million of £",
       x = "Global Sales")


#------------------------------------------------------------------------------#
# These boxplots don't tell us anymore than what we got from question 4 plots.
# However, we have been able to add more information so they are better prepared
# for future use in presentations to the various stakeholders.
# In the Other Regions Sales boxplot we can see the number of outliers (values 
# beyond the maximum value), and since this data has been calculated using North 
# America and Europe values (as described in Turtle Games metadata file), these 
# points make clear the differences in sales between the other two regions.
#------------------------------------------------------------------------------#


################################################################################


# 3. Determine the normality of the data set (sales data).

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

# [NA Sales]
qqnorm(sales_clean$NA_Sales)
qqline(sales_clean$NA_Sales, 
       col = 'red',
       lwd = 2) 

qqnorm(sales_product$NA_Sales_sum)
qqline(sales_product$NA_Sales_sum, 
       col = 'red',
       lwd = 2) 

# [EU Sales]
qqnorm(sales_clean$EU_Sales)
qqline(sales_clean$EU_Sales, 
       col = 'blue',
       lwd = 2) 

qqnorm(sales_product$EU_Sales_sum)
qqline(sales_product$EU_Sales_sum, 
       col = 'blue',
       lwd = 2) 

# [Other Sales]
qqnorm(sales_clean$Other_Sales)
qqline(sales_clean$Other_Sales, 
       col = 'purple',
       lwd = 2) 

qqnorm(sales_product$Other_Sales_sum)
qqline(sales_product$Other_Sales_sum, 
       col = 'purple',
       lwd = 2) 

# [Global Sales]
qqnorm(sales_clean$Global_Sales)
qqline(sales_clean$Global_Sales, 
       col = 'dark green',
       lwd = 2) 

qqnorm(sales_product$Global_Sales_sum)
qqline(sales_product$Global_Sales_sum, 
       col = 'dark green',
       lwd = 2) 

#------------------------------------------------------------------------------#
# Although this is a visual aid only to determine the normality of the sales data,
# it is quite obvious from the Q-Q Plots that none of these columns have a normal 
# distribution. However, the grouped data performs better as discovered earlier 
# when doing the histograms. In any case, let's investigate further to confirm 
# these assumptions.
#------------------------------------------------------------------------------#


## 3b) Perform Shapiro-Wilk test

# Perform Shapiro-Wilk test.

shapiro.test(sales_clean$NA_Sales)

shapiro.test(sales_clean$EU_Sales)

shapiro.test(sales_clean$Other_Sales)

shapiro.test(sales_clean$Global_Sales)

shapiro.test(sales_product$NA_Sales_sum)

shapiro.test(sales_product$EU_Sales_sum)

shapiro.test(sales_product$Other_Sales_sum)

shapiro.test(sales_product$Global_Sales_sum)


#------------------------------------------------------------------------------#
# All of the sales columns have a p-value smaller than 0.05 and since we have set 
# α=0.05, we have to reject the null hypothesis and state that the data is not 
# normally distributed.
#------------------------------------------------------------------------------#


## 3c) Determine Skewness and Kurtosis

# Install and import Moments.
install.packages('moments') 
library(moments)

# Skewness and Kurtosis.

skewness(sales_clean$NA_Sales) 
kurtosis(sales_clean$NA_Sales)

skewness(sales_clean$EU_Sales) 
kurtosis(sales_clean$Eu_Sales)
# (For some reason this function runs too deep and I get a C stack usage too close
# to the limit error. Since we know this is not a normal distribution data, we are
# not going to worry about this one.)

skewness(sales_clean$Other_Sales) 
kurtosis(sales_clean$Other_Sales)

skewness(sales_clean$Global_Sales) 
kurtosis(sales_clean$Global_Sales)

skewness(sales_product$NA_Sales_sum) 
kurtosis(sales_product$NA_Sales_sum)

skewness(sales_product$EU_Sales_sum) 
kurtosis(sales_product$EU_Sales_sum)

skewness(sales_product$Other_Sales_sum) 
kurtosis(sales_product$Other_Sales_sum)

skewness(sales_product$Global_Sales_sum) 
kurtosis(sales_product$Global_Sales_sum)


#------------------------------------------------------------------------------#
# A skewness of zero means a perfectly symmetric distribution or data set. 
# Here we have all values above 1 (positive skewed), which means the data is
# extremely skewed. Again, the grouped data improves these values but not enough
# to consider here a normal distribution. As mentioned before, we could 
# potentially improve this by removing those extreme values we have found earlier 
# (values above £20M), but this is something we will discuss with Turtle Games 
# in our report because, as mentioned before, these are effectively the best 
# selling products.

# A normal distributions have a kurtosis of 3. Here we have in all cases high 
# positive numbers, which means these data sets have heavy tails and more 
# outliers, as we know. As before, the grouped data improves these values but not 
# enough to consider here a normal distribution.
#------------------------------------------------------------------------------#


## 3d) Determine correlation
# Isolate the sales data in a separate data frame..
sales_only <- select(sales_clean, -Product, -Platform)

# Determine the correlation for the whole data frame.
round (cor(sales_only),
       digits=2)


#------------------------------------------------------------------------------#
#              NA_Sales EU_Sales Other_Sales Global_Sales
# NA_Sales         1.00     0.71        0.64         0.93
# EU_Sales         0.71     1.00        0.67         0.88
# Other_Sales      0.64     0.67        1.00         0.82
# Global_Sales     0.93     0.88        0.82         1.00

# A positive correlation coefficient suggests that the two variables vary in 
# the same direction. That means as the one increases, so does the other; and if 
# one decreases, the other does too. 
# In the correlation results we can see all variables have a strong positive 
# correlation, with all values above 0.64.
# Actually this result makes completely sense as the sum of each one of the sales
# by region makes up the Global Sales. And since originally we didn't have the
# data for Other Sales, we had to calculated ourselves.
# However, when we fit the regression models to help respond the next question, 
# we are looking for strong correlation between the dependent variable (Global 
# Sales) and the independent variables (North America Sales, Europe Sales and 
# Other Regions Sales), but not between the independent variables themselves 
# (called multicollinearity), as this could impact the statistical significance 
# of an independent variable.
#------------------------------------------------------------------------------#


# Let's do the same exercise with the grouped data.
# Isolate the sales data in a separate data frame..
sales_only_grouped <- select(sales_product, -Product)

# Determine the correlation for the whole data frame.
round (cor(sales_only_grouped),
       digits=2)


#------------------------------------------------------------------------------#
#                NA_Sales_sum EU_Sales_sum Other_Sales_sum Global_Sales_sum
# NA_Sales_sum             1.00         0.62            0.53             0.92
# EU_Sales_sum             0.62         1.00            0.54             0.85
# Other_Sales_sum          0.53         0.54            1.00             0.73
# Global_Sales_sum         0.92         0.85            0.73             1.00

# We have slightly improved the correlation between the independent variables.
#------------------------------------------------------------------------------#


################################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Install appropriate packages to be able to use the melt() function.
install.packages('MASS')
install.packages('reshape2')

# Import appropriate libraries to be able to use the melt() function.
library(MASS)
library(reshape2)

# Use the melt() function to create a data frame where all the regions are one
# variable only.
sales_clean_melted <- melt(sales_clean, na.rm = FALSE, value.name = "Sales",
                           variable.name = "Region")

# Sense check the new data frame.
head(sales_clean_melted)
glimpse(sales_clean_melted)

# Group data based on the region and determine the sum per region.
sales_clean_melted_region <- sales_clean_melted %>% 
  group_by(Region) %>%
  summarise(Sales_sum = sum(Sales))

# View the data frame.
head(sales_clean_melted_region)

# Create a color blind palette to be used on all plots.
cbPalette <- c("#999999", "#E69F00", "#0072B2", "#009E73", 
               "#F0E442", "#56B4E9", "#D55E00", "#CC79A7")

# Create a plot to visualise the total sales across regions.
ggplot(sales_clean_melted_region, aes(x = Region, y = Sales_sum)) +
  geom_bar(fill = c("#999999", "#E69F00", "#0072B2", "#009E73"), stat='identity') +
  geom_text(aes(label = Sales_sum), stat = 'identity', vjust = -0.5) +
  scale_x_discrete(labels = c("North America", "Europe", "Other Regions", 
                              "Global")) +
  theme_minimal() +
  labs(title = "Total Sales across Regions and Globally",
       subtitle = "Sales in million of £",
       x = "Regions",
       y = "Total Sales")


#------------------------------------------------------------------------------#
# With this plot we can show Turtle Games the total sales by region, to 
# understand which region is making more sales. We can clearly see North America 
# is first, followed by Europe and in last position Other Regions.
#------------------------------------------------------------------------------#


# Now let's focus on products.
# Since there are too many products, we are going to focus on the products that 
# have a total Global_Sales bigger than £20M.
# Filter the data
products_20 <- sales_product %>% 
  filter(Global_Sales_sum > 20)

# View the data frame.
products_20

# Use the melt() function to create a data frame where all the regions are one
# variable only.
products_20_melted <- melt(products_20, na.rm = FALSE, value.name = "Sales",
                            variable.name = "Region")

# View the data frame.
head(products_20_melted)

# Create a plot to visualise the total sales across regions by Product.
ggplot(products_20_melted, aes(x = Product, y = Sales, fill = Region)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  labs(title = "Impact on Sales by Region of Products with Glabal Sales bigger than £20M",
       subtitle = "Sales in million of £",
       x = "Product ID")


#------------------------------------------------------------------------------#
# With this plot we focus on the products that have a Global Sales bigger than
# £20M, showing the difference in sales between regions. As discovered with 
# previous exercises, the 5 top-selling products are 107, 515, 123, 254 and 195.
# Let's focus on these next.
#------------------------------------------------------------------------------#


# Let's investigate the impact of the top five products have on sales by region.
# Filter the data.
top_products <- sales_product %>% 
  filter(Global_Sales_sum > 28)

# View the data frame.
top_products

# Use the melt() function to create a data frame where all the regions are one
# variable only.
top_products_melted <- melt(top_products, na.rm = FALSE, value.name = "Sales",
                            variable.name = "Region")

# View the data frame.
head(top_products_melted)

# Create a plot to explain the impact of these five products on sales.
# Create first a vector with the position we want to be displayed in the plot.
position_products <- c("107", "515", "123", "254", "195")

ggplot(top_products_melted, aes(x = Product, y = Sales, fill = Region)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(aes(label = Sales), stat = 'identity',
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3.5) +
  scale_x_discrete(limits = position_products) +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  labs(title = "Impact of 5 Top-Selling Products on Sales by Region",
       subtitle = "Sales in million of £",
       x = "Product ID",
       fill = "Region")


#------------------------------------------------------------------------------#
# We can now clearly see the difference in sales by region on these products.
# It will be worth asking Turtle Games more information about how these products 
# are sold in each region, i.e. number of stores, online platforms, etc.
# However, we do have information on genre and publishers. Let's check these next.
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# First, let's add a column for Other_Sales to the original data frame.
sales <- sales %>%
  mutate(Other_Sales = Global_Sales - (NA_Sales + EU_Sales)) 

# Round the new column to two decimals.
sales$Other_Sales <- round(sales$Other_Sales, digits = 2)

# Reorder the columns so Global_Sales (the total sales) appears at the end again.
sales <- sales[, c("Ranking", "Product", "Platform", "Year", "Genre",
                         "Publisher", "NA_Sales", "EU_Sales", "Other_Sales", 
                         "Global_Sales")] 

# View the updated data frame.
head(sales)
str(sales)
#------------------------------------------------------------------------------#


# It will be interesting to see to what genres and publishers belong these top 
# products. 
# We are going to bring back the columns we removed at the beginning of this 
# exercise and see how they can help gathering insights from the data.
# Filter the original data by product and genre.
sales_product_genre <- sales %>% 
  group_by(Product, Genre) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Other_Sales_sum = sum(Other_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups = 'drop')

# View the data frame.
head(sales_product_genre)

# Explore the data frame.
dim(sales_product_genre)
str(sales_product_genre)
summary(sales_product_genre)
arrange(sales_product_genre, desc(Global_Sales_sum))

# Filter the original data by product and publisher.
sales_product_publisher <- sales %>% 
  group_by(Product, Publisher) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Other_Sales_sum = sum(Other_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups = 'drop')

# View the data frame.
head(sales_product_publisher)

# Explore the data frame.
dim(sales_product_publisher)
str(sales_product_publisher)
summary(sales_product_publisher)
arrange(sales_product_publisher, desc(Global_Sales_sum))

# Filter the original data by product, genre and publisher to have both together.
sales_product_original <- sales %>% 
  group_by(Product, Genre, Publisher) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Other_Sales_sum = sum(Other_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups = 'drop')

# View the data frame.
head(sales_product_original)

# Explore the data frame.
dim(sales_product_original)
str(sales_product_original)
summary(sales_product_original)
arrange(sales_product_original, desc(Global_Sales_sum))


#------------------------------------------------------------------------------#
# We can see this data frame has 182 rows instead 175, and therefore we can 
# conclude that some products have more than one publisher (2285, 2829, 4415, 
# 5493, 5758, 8933). However, when arranging the results we can see that those 
# products are not any of the top selling products, hence we are not going to 
# investigate further.
# The top five products by Global Sales identified earlier have a genre and a 
# publisher as below:
# - 107: Sports, Nintendo
# - 515: Action, Take-Two Interactive
# - 123: Platform, Nintendo
# - 254: Puzzle, Nintendo
# - 195: Racing, Nintendo
# We can conclude from this that Nintendo is the best best selling publisher,
# distributing four of the five best selling products. In terms of genre, there 
# no pattern with the best selling products. However, we can look for a pattern 
# overall to help Turtle Games.
#------------------------------------------------------------------------------#


# Filter the data.
top_products_original <- sales_product_original %>% 
  filter(Global_Sales_sum > 28)

# View the data frame.
top_products_original

# Create a plot to show the genre and publisher of the top 5 best selling products.
ggplot(top_products_original, aes(x = Product, y = Publisher, fill = Genre)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = cbPalette) +
  theme_minimal() +
  labs(title = "Genre and Publisher of 5 Top-Selling Products",
       x = "Product ID")

# Use the melt() function to create a data frame where all the regions are one
# variable only and we include the genre and publisher columns.
sales_product_original_melted <- melt(sales_product_original, 
                                      na.rm = FALSE, value.name = "Sales",
                                      variable.name = "Region")

# View the data frame.
head(sales_product_original_melted)

# Create a plot to visualise the total sales across regions by genres.
# Create first a vector with the position we want to be displayed in the plot.
position_genre <- c("Shooter", "Platform", "Action", "Role-Playing", "Sports", 
                    "Racing", "Misc", "Simulation", "Puzzle", "Fighting", 
                    "Adventure", "Strategy")

ggplot(sales_product_original_melted, aes(x = Genre,
                                          y = Sales, fill = Region)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(limits = position_genre) +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  labs(title = "Sales by Regions across Genres",
       subtitle = "Sales in million of £")


# Create a plot to visualise the total sales across regions by publisher.
ggplot(sales_product_original_melted, aes(x = Publisher,
                                          y = Sales, fill = Region)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Sales by Regions across Publishers",
       subtitle = "Sales in million of £")

# Create first a vector with the position we want to be displayed in the plot.
position_publisher <- c("Nintendo", "Activision", "Electronic Arts", 
                        "Take-Two Interactive", "Sony Computer Entertainment",
                        "Microsoft Game Studios", "Ubisoft", "Bethesda Softworks")

# Create a plot to visualise the total sales across regions by the top publishers.
ggplot(sales_product_original_melted, aes(x = Publisher,
                                          y = Sales, fill = Region)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(limits = position_publisher) +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Impact of 8 Top-Selling Publishers on Sales by Region",
       subtitle = "Sales in million of £")



# Now let's focus on platforms.
# Create a plot to visualise the total sales across regions by platform.
# Create first a vector with the position we want to be displayed in the plot.
position_platforms_all <- c("Wii", "X360", "PS3", "DS", "GB", "PS2", "NES",
                            "PS", "3DS", "PS4", "SNES", "GBA", "N64", "PC",
                            "XOne", "GC", "XB", "PSP", "WiiU", "2600", "GEN",
                            "PSV")


ggplot(sales_clean_melted, aes(x = Platform, y = Sales, fill = Region)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(limits = position_platforms_all) +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  labs(title = "Sales by Regions across Platforms",
       subtitle = "Sales in million of £")


# Let's investigate the impact of the top six platforms have on sales by region.
# Filter the data
top_platforms <- sales_platform %>% 
  filter(Global_Sales_sum > 100)

# View the data frame.
top_platforms

# Use the melt() function to create a data frame where all the regions are one
# variable only.
top_platforms_melted <- melt(top_platforms, na.rm = FALSE, value.name = "Sales",
                             variable.name = "Region")

# View the data frame.
head(top_platforms_melted)

# Create a plot to explain the impact of these six platforms on sales.
# Create first a vector with the position we want to be displayed in the plot.
position_platforms <- c("Wii", "X360", "PS3", "DS", "GB", "PS2")

ggplot(top_platforms_melted, aes(x = Platform, y = Sales, fill = Region)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(aes(label = Sales), stat = 'identity',
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_x_discrete(limits = position_platforms) +
  scale_fill_manual(values = cbPalette, 
                    labels = c("North America", "Europe", "Other Regions", 
                               "Global")) +
  theme_minimal() +
  labs(title = "Impact of 6 Top-Selling Platforms on Sales by Region",
       subtitle = "Sales in million of £",
       x = "Platform",
       fill = "Region")

# And finally to be able to compare the most profitable genres and publishers
# against the ones that have a bigger offer on products, let's check the number
# of products by each of these variables.

# By Genre:
ggplot(sales_product_genre, aes(x = fct_infreq(Genre))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5) +
  theme_minimal() +
  labs(title = "Number of Products by Genre",
       x = "Genre",
       y = "Count of Products")

# By Publisher:
ggplot(sales_product_publisher, aes(x = fct_infreq(Publisher))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Number of Products by Publisher",
       x = "Publisher",
       y = "Count of Products")


################################################################################

# 5. Observations and insights

## We have checked how reliable the data is by plotting and testing for normal 
# distribution, skewness, and kurtosis. Unfortunately none of the sales columns 
# are normally distributed, although working with the grouped data by product
# slightly improves the situation. 

## We have also interrogated the data to check how many products are sell by 
# Genre, Publisher, and Platform. And how all these impact on the total sales by
# region and globally. We can clearly see the best selling products are 107, 515, 
# 123, 254 and 195, where most of them belong to Nintendo except 515 which 
# belongs to Take-Two Interactive.

## The top 5 best selling genres are Shooter, Platform, Action, Role-Playing and 
# Sports. 

## The top 6 best selling platforms are Wii, X360, PS3, DS, GB and PS2.


################################################################################
################################################################################


# Question 6: if there is any possible relationship(s) in sales between North 
##            America, Europe, and global sales.

# Steps we will follow:
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared for question 5. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

################################################################################

# 1. Load and explore the data
# View data frame created for question 5.
head(sales_clean)

# Determine a summary of the data frame.
summary(sales_clean)

################################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame and set character size (cex=2).
corPlot(sales_only, cex=2)

# However, we saw earlier that the grouped data performs a bit better in terms 
# normality. Therefore, let's check the correlation on these columns.
corPlot(sales_only_grouped, cex=2)


#------------------------------------------------------------------------------#
# The relation between the dependent variable (Global_Sales) with the independent
# variable (NA_Sales, EU_Sales and Other_Sales) is still high, but with the 
# grouped data we improve the multicollinearity between the independent variables
# (correlation between these). Therefore we will use the grouped data to fit the
# regression model.
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Before doing the linear regression analysis, there are six assumptions that 
# need to be checked to determine the accuracy of a predictive model:

## - Exogeneity
# We need to confirm that the independent variables (in this case either NA_Sales,
# EU_Sales and/ or Other_Sales) are not dependent on the dependent variable y 
# (in this case Global Sales). We know that actually is the other way around, 
# therefore, the independent variables (X) are exogenous and not dependent on 
# the independent variable (y).

## - Residuals are independent
# One way to determine if this assumption is met is to perform a Durbin-Watson 
# test, which is used to detect the presence of autocorrelation in the residuals 
# of a regression. The Durbin-Watson statistic returns a value between 0 and 4:
# - A test statistic of 2 indicates no serial correlation.
# - The closer the test statistics is to 0, the more evidence of positive serial 
# correlation.
# - The closer the test statistics is to 4, the more evidence of negative serial 
# correlation.
# As a rule of thumb, test statistic values between the range of 1.5 and 2.5 are 
# considered normal. However, values outside of this range could indicate that 
# autocorrelation is a problem.
# Once we fit the model we can run the Durbin-Watson test and check this
# assumption.

## - Significant outliers or missing values
# We have already checked there are no missing values on the data set. Regarding
# outliers, we have identified them in the sales columns. However, since these
# mean the best selling products we have made the decision to keep them. This is 
# something we will discuss with Turtle Games.


## - Normality (i.e. normal distribution) of y (dependent variable)
# We have already done a histogram of this variable and confirmed it is not 
# normally distributed. Even with the grouped data this distribution doesn't 
# improves as checked earlier. Let's try to improve it transforming this variable
# with the log function:
sales_product <- mutate(sales_product,
                        log_Global_Sales = log(Global_Sales_sum))
# Let's check with a quick histogram if the distribution has improved:
qplot(log_Global_Sales, 
      data = sales_product)
# The transformed data follows a better distribution but it is still not normally
# distributed. Therefore we will still use the original data as it keeps a 
# consistency with the scale of the other sales columns.

## - Linearity
# The linearity assumption states that the relationship between each independent 
# variable and the dependent variable is linear. We checked this relationship
# earlier plotting scatterplots and concluded there is a positive relationship. 

## - Homoscedasticity
# Homoscedasticity indicates the variances of the residuals are similar or 
# constant variance(s). Without homoscedasticity, the standard errors of the 
# regression coefficient estimates are invalid. This means that you might declare 
# a term as statistically significant when it actually isn’t.  
# When testing for homoscedasticity, you can assume the following hypotheses:
# - Ho = Homoscedasticity at p>=0.05 (the residuals are distributed with equal 
# variance)
# - Ha = Heteroscedasticity at p<0.05 (the residuals are not distributed with 
# equal variance)
# Once we fit the model we can run the Breusch-Pagan test to confirm the 
# presence of homoscedasticity or heteroscedasticity.
#------------------------------------------------------------------------------#


# Create a linear regression model on the original data.
model1 <- lm(Global_Sales_sum ~ NA_Sales_sum,
             data = sales_product)


# View more outputs for the model - the full regression table.
summary(model1)


#------------------------------------------------------------------------------#
# The p value for NA_Sales is very small, telling us that it is a highly 
# significant value, explaining over 83.95% of the variability in the dependent
# variable, Global_Sales.
#------------------------------------------------------------------------------#


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# View residuals on a plot.
plot(model1$residuals)


#------------------------------------------------------------------------------#
# We would like to see no pattern on these residuals, but that is not the case
# here. We can see a kind of cone-shaped plot, which is a sign of 
# heteroscedasticity. Let's run a Breusch–Pagan test to check it.
#------------------------------------------------------------------------------#


# Import the lmtest library.
library(lmtest)

# Perform Breusch-Pagan Test.
bptest(model1)


#------------------------------------------------------------------------------#
# Running the Breusch–Pagan test we get a p value much smaller than 0.05 and we 
# can confirm there is heteroscedasticity. If the assumption of homoscedasticity
# is violated, we may get misleading or unreliable results from the linear 
# regression model, and therefore we discard this model.
#------------------------------------------------------------------------------#


# Plot the relationship with base R graphics.
plot(sales_product$NA_Sales_sum, sales_product$Global_Sales_sum)
# Add line-of-best-fit.
abline(coefficients(model1), col = 'red')

# Perform the Durbin-Watson test.
dwtest(model1)


#------------------------------------------------------------------------------#
# The Durbin-Watson test shows a score of 2.1269 (between the range of 1.5 and 
# 2.5) and therefore we don't consider that autocorrelation is a problem.
#------------------------------------------------------------------------------#


# Let's do the same exercise with EU_Sales and see what we get.
model2 <- lm(Global_Sales_sum ~ EU_Sales_sum,
             data = sales_product)

# View more outputs for the model - the full regression table.
summary(model2)


#------------------------------------------------------------------------------#
# The p value for EU_Sales is also very small, telling us that it is a highly 
# significant value, explaining over 72.01% of the variability in the dependent
# variable, Global_Sales. However, this percentage is lower than NA_Sales.
#------------------------------------------------------------------------------#


# View residuals on a plot.
plot(model2$residuals)


#------------------------------------------------------------------------------#
# Yet again the residuals follow a kind of cone-shaped plot, which is a sign of 
# heteroscedasticity. Let's run a Breusch–Pagan test to check it.
#------------------------------------------------------------------------------#


# Perform Breusch-Pagan Test.
bptest(model2)


#------------------------------------------------------------------------------#
# Running the Breusch–Pagan test we get a p value just above 0.05, so in 
# principle we can assume homoscedasticity, but the result is too tight. 
#------------------------------------------------------------------------------#


# Plot the relationship with base R graphics.
plot(sales_product$EU_Sales_sum, sales_product$Global_Sales_sum)
# Add line-of-best-fit.
abline(coefficients(model2), col = 'red')

# Perform the Durbin-Watson test.
dwtest(model2)


#------------------------------------------------------------------------------#
# The Durbin-Watson test shows a score of 1.1902 and therefore we consider that 
# a positive autocorrelation is a problem in this case.
#------------------------------------------------------------------------------#


# We could also see if we can predict NA_Sales from EU_Sales.
model3 <- lm(NA_Sales_sum ~ EU_Sales_sum,
             data = sales_product)

# View more outputs for the model - the full regression table.
summary(model3)


#------------------------------------------------------------------------------#
# The p value for EU_Sales is also very small, telling us that it is a highly 
# significant value, but it only explains 38.56% of the variability in the 
# dependent variable, which in this case is NA_Sales. Therefore we discard this 
# one. However, let's plot the residuals and the relationship for visual aid.
#------------------------------------------------------------------------------#


# View residuals on a plot.
plot(model3$residuals)

# Plot the relationship with base R graphics.
plot(sales_product$NA_Sales_sum, sales_product$EU_Sales_sum)
# Add line-of-best-fit.
abline(coefficients(model3), col = 'red')

# Perform the Durbin-Watson test.
dwtest(model3)


#------------------------------------------------------------------------------#
# The Durbin-Watson test shows a score of 1.521 (between the range of 1.5 and 
# 2.5) and therefore we don't consider that autocorrelation is a problem.
#------------------------------------------------------------------------------#


################################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Multiple linear regression model.
model4 <- lm(Global_Sales_sum ~ NA_Sales_sum + EU_Sales_sum + Other_Sales_sum,
             data = sales_product)

# View the model.
model4

# Print the summary statistics.
summary(model4)


#------------------------------------------------------------------------------#
# This model is a perfect fit because Global_Sales is the sum of the other three 
# variables. It doesn't make sense to fit a model like this because if we have 
# these three values we just need to add them up to find the Global_Sales, we 
# don't need a prediction. Therefore we discard this model.
#------------------------------------------------------------------------------#


# Let's try to fit a model without the Other_Sales variable that we have added in.
model5 <- lm(Global_Sales_sum ~ NA_Sales_sum + EU_Sales_sum,
             data = sales_product)

# View the model.
model5

# Print the summary statistics.
summary(model5)


#------------------------------------------------------------------------------#
# In this model both independent variables are significant and explain 96.68%
# of the variability on the independent variable. It is a good fit.
# From the coefficients we can conclude that the increase of £1M in North
# America Sales will contribute to the increase of £1.13M in the Global Sales 
# and the increase of £1M in Europe Sales will contribute to the increase of 
# £1.2M in the Global Sales. Probably this is due to the correlation between 
# North America Sales and Europe Sales, with Other Sales (reflecting that every 
# time North America/ Europe Sales increase Other sales increase too). Therefore 
# the extra £0.13M and £0.2M come from the approximate increase that we would see 
# in Other Sales. 
#------------------------------------------------------------------------------#


# View residuals on a plot.
plot(model5$residuals)


#------------------------------------------------------------------------------#
# The residuals are more spread out than before, which is a good sign. However, 
# there is certain resemblance to a cone-shaped plot, which is a sign of 
# heteroscedasticity. Let's run a Breusch–Pagan test to check it.
#------------------------------------------------------------------------------#


# Perform Breusch-Pagan Test.
bptest(model5)


#------------------------------------------------------------------------------#
# Running the Breusch–Pagan test we get a p value bigger than 0.05 and we 
# can confirm there is homoscedasticity. This is a very good sign as we are 
# building the necessary steps to prove the reliability of this model.
#------------------------------------------------------------------------------#


# Test normality of the residuals.
qqnorm(model5$residuals)
qqline(model5$residuals, col = 'red')

# Perform the Durbin-Watson test.
dwtest(model5)


#------------------------------------------------------------------------------#
# The Durbin-Watson test shows a score of 1.6605 (between the range of 1.5 and 
# 2.5) and therefore we don't consider that autocorrelation is a problem.
#------------------------------------------------------------------------------#

# What happens if we add the Year we had from the original data set?
# However we know there were two missing values on that column, so first we will 
# remove those two rows.

# Find the row where ageNum is NA.
filter(sales, is.na(Year))

# Delete the rows with NA values.
sales_noNA <- filter(sales,
                     !is.na(Year))

# Check the wages5 data set dimensions.
dim(sales_noNA)

# Now we can fit a model including the variable Year.
model6 <- lm(Global_Sales ~ NA_Sales + EU_Sales + Year,
             data = sales_noNA)

# Print the summary statistics.
summary(model6)


#------------------------------------------------------------------------------#
# In this model all independent variables are significant and explain 97%
# of the variability on the dependent variable, Global_Sales. This model is 
# sightly better than model 5 looking at the adjusted R squared.
#------------------------------------------------------------------------------#


# View residuals on a plot.
plot(model6$residuals)


#------------------------------------------------------------------------------#
# Yet again the residuals follow a kind of cone-shaped plot, which is a sign of 
# heteroscedasticity. Let's run a Breusch–Pagan test to check it.
#------------------------------------------------------------------------------#


# Perform Breusch-Pagan Test.
bptest(model6)


#------------------------------------------------------------------------------#
# Running the Breusch–Pagan test we get a p value much smaller than 0.05 and we 
# can confirm there is heteroscedasticity. Therefore we can confirm model 5
# is more reliable than this one.
#------------------------------------------------------------------------------#

# Test normality of the residuals.
qqnorm(model6$residuals)
qqline(model6$residuals, col = 'red')

# Perform the Durbin-Watson test.
dwtest(model6)


#------------------------------------------------------------------------------#
# The Durbin-Watson test shows a score of 1.8651 (between the range of 1.5 and 
# 2.5) and therefore we don't consider that autocorrelation is a problem.
#------------------------------------------------------------------------------#


################################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# We will do this exercise with model 5 as it is the most reliable one.
# We know these values belong to the original data set, and we normally test a 
# model with unseen data. However, since we are using the grouped data, we can 
# assimilate this data as unseen for the purpose of this exercise. 

# Create a data frame with the given values.
# Create the vectors to store the information of the data frame.
NA_Sales_sum <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_sum <- c(23.8, 1.56, 0.65, 0.97, 0.52)

# Create the data frame.
test_values <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Check the new data frame.
test_values
str(test_values)

# Create a new object and specify the predict function.
predict_test <- predict(model5, newdata = test_values,
                        interval='confidence')


# Print the object.
predict_test 

# fit       lwr       upr
# 1 68.056548 66.429787 69.683310
# 2  7.356754  7.099418  7.614090
# 3  4.908353  4.614521  5.202185
# 4  4.761039  4.478855  5.043223
# 5 26.625558 25.367353 27.883763

# Let's record here the observed values:

#    observed
# 1 67.85
# 2  6.04
# 3  4.32
# 4  3.53
# 5 23.21

# Let's calculate the accuracy of the prediction.
# Record the observed values in a data frame.
Global_Sales_test <- c(67.85, 6.04, 4.32, 3.53, 23.21)

# View the data frame.
Global_Sales_test

# Calculate the MAPE value.
MAPE_test <- mean(abs(Global_Sales_test - predict_test) / Global_Sales_test) * 100

# View the MAPE value.
cat("MAPE:", round(MAPE_test, 2), "%\n")

#------------------------------------------------------------------------------#
# The MAPE is a statistical measure used mostly for time series predictions, but
# it can also be used for regression. Due to the simplicity of the calculation,
# we will use it in this exercise.

# The MAPE value is 17.34. This means that the average difference between the 
# predicted value and the actual value is 17.34%. According to the rule of thumb, 
# a MAPE value of less than 10% is considered excellent and if it is less than 
# 20% is good. Therefore, we have a good MAPE value which suggests that the 
# model is relatively accurate.  
#------------------------------------------------------------------------------#


# What happens if we do the prediction on the entire data set?
Global_Sales_predicted <- predict(model5, newdata = sales_product,
                                  interval='confidence')

# Print the object.
Global_Sales_predicted

# Check the accuracy of the prediction.
MAPE_Global_Sales <- mean(abs(sales_product$Global_Sales_sum - Global_Sales_predicted)/ 
                            sales_product$Global_Sales_sum) * 100

# View the MAPE value.
cat("MAPE:", round(MAPE_Global_Sales, 2), "%\n")


#------------------------------------------------------------------------------#
# The MAPE value for the whole data set is 12.99. This means that the average 
# difference between the predicted value and the actual value is 12.99%. 
# According to the rule of thumb, a MAPE value of less than 10% is considered 
# excellent and if it is less than 20% is good. Therefore, we have a good MAPE 
# value which suggests that the model is in fact quite accurate.
#------------------------------------------------------------------------------#

# Let's plot the predicted vs observed values.
# First we need to add the predicted values to the data frame.
Global_Sales_predicted_df <- data.frame(Global_Sales_predicted)

sales_product <- mutate(sales_product, 
                        Global_Sales_predicted = Global_Sales_predicted_df$fit)

ggplot(sales_product, aes(x = Global_Sales_predicted, y = Global_Sales_sum)) + 
  geom_point(size = 2, col = '#009E73') +
  geom_abline(intercept = 0, slope = 1, size = 1, col = 'black') +
  scale_x_continuous(breaks = seq(0, 70, 10), "Predicted Values") +
  scale_y_continuous(breaks = seq(0, 70, 10), "Observed Values") +
  theme_minimal() +
  labs(title = "Global Sales Predicted vs Observed",
       subtitle = "Sales in million of £")



################################################################################

# 5. Observations and insights

## The only linear regression model that is reliable is model 5, which predicts
# the Global Sales from the explanatory variables North America Sales and 
# Europe Sales. In this model both independent variables are significant and 
# explain 96.68% of the variability on the independent variable. 

## None of the other models (except model 2 with a very tight score) holds true 
# for homoscedasticity. 

## However, the non normality of the data remains an issue and we would recommend
# to gather more data, try to include other variables in the model and check with 
# Turtle Games the reliability of those products that have sold much more than the 
# others (outliers).


################################################################################
################################################################################


## References/ resources used on this exercise:
# - https://www.rdocumentation.org/
# - https://style.tidyverse.org/index.html
# - https://cengel.github.io/R-intro/
# - https://stackoverflow.com/


################################################################################
################################################################################

