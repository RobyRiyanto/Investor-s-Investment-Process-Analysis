# Load the library
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(lubridate)

# Read the dataset with csv format stored in the df_event variable
df_event <- read.csv('https://dqlab-dataset.s3-ap-southeast-1.amazonaws.com/event.csv', stringsAsFactors = F)

# Displays data summary with the glimpse function in the dplyr package
glimpse(df_event)

# Change Year-Month-Date Hour-Minute-Second formatted characters to timestamp 
# type in column created_at with ymd_hms function
df_event$created_at <- ymd_hms(df_event$created_at)

# Check summary again if it has changed
glimpse(df_event)


# Event Summary

# From data.frame df_event grouped by group by name_event
df_event %>% group_by(nama_event) %>%
  # to find out the number of events, or how many rows
  summarise(jumlah_event = n(),
            # to find out the unique number of loan_id using the n_distinct function
            loan = n_distinct(loan_id),
            # to find out the unique number of investor_id
            investor = n_distinct(investor_id))


# Change the loan investment process data format so that the event name becomes the column name

# The event loan is uploaded to the marketplace

# Create a new data.frame
df_marketplace <-df_event %>%
  #Filter column nama_event 'loan_to_marketplace'
  filter(nama_event == 'loan_to_marketplace') %>% 
  #Select any columns to be retrieved with select, namely loan_id and marketplace (change name from created_at)
  select(loan_id, marketplace = created_at) 

# Shows data on data.frame df_marketplace
df_marketplace


# See loan details

# Create a new data.frame
df_view_loan <- df_event %>%
  # Filter column nama_event 'investor_view_loan'
  filter(nama_event == 'investor_view_loan') %>%
  # Grouping by loan_id and investor_id
  group_by(loan_id, investor_id) %>%
  # Summarise to know an investor, how many times the loan is seen with function n ()
  summarise(jumlah_view = n(),
            # To know when the investor first saw the details of the loan using the min () function
            pertama_view = min(created_at),
            # to know when the investor last saw the details of the loan using the max () function
            terakhir_view = max(created_at))

# Shows data on data.frame df_view_loan
df_view_loan


# Event investors order and pay loan

# Create a new data.frame
df_order_pay <- df_event %>%
  # Filter column nama_event with values 'investor_order_loan' and 'investor_pay_loan'
  filter(nama_event %in% c('investor_order_loan', 'investor_pay_loan')) %>%
  # Using the spread function on column nama_event and created_at so that nama_event becomes column name
  spread(nama_event, created_at) %>%
  # Select the column investor_id, loan_id and nama_event that have been spread
  select(loan_id, 
         investor_id, 
         order = investor_order_loan, 
         pay = investor_pay_loan)

# Shows data on data.frame df_order_pay
df_order_pay


# Combined Investment Loan Data

# Create a new data.frame
df_loan_invest <- df_marketplace %>%
  # Using left join, df_marketplace with df_view_loan, based on loan_id
  left_join(df_view_loan, by = 'loan_id') %>%
  # Using left join with df_order_pay, based on loan_id and investor_id
  left_join(df_order_pay, by = c('loan_id','investor_id'))

# Shows data on data.frame df_loan_invest
df_loan_invest




# Investment Process Analysis

# See the relationship between the number of views and the order

# Will see if there is a relationship between the number of times investors see a loan and the order decision or not
# Using data.frame df_loan_invest
df_loan_invest %>%
  # Create status_order with contents of 'not_order' if no order (empty order column) and 'order' for others
  mutate(status_order = ifelse( is.na(order), 'not_order','order')) %>%
  #Count the combination of the number of views and the newly created status_order with the count function
  count(jumlah_view, status_order) %>%
  #spread status_order as a key with value n which is the result of count, set fill = 0 so that when something is empty it is replaced with 0
  spread(status_order, n, fill = 0) %>%
  #Count percent_order which is the order value divided by the total order and not_order, format with percent
  mutate(persen_order = scales::percent(order/(order + not_order)))

# How long will it take the investor to order from the first look at the loan details
# Using data.frame df_loan_invest
df_loan_invest %>%
  # Filter only orders (column is not empty)
  filter(!is.na(order)) %>%
  # Count lama_order_view, difftime is used to calculate the difference between 2 times (Column order with first_view)
  # Using units "mins" which means the output is displayed in minutes
  mutate(lama_order_view = as.numeric(difftime(order, pertama_view, units = "mins"))) %>%
  # Grouping by jumlah_view
  group_by(jumlah_view) %>%
  # Calculating the summary (number of transactions, min, median, mean and max) from the column lama_order_view
  summarise_at(vars(lama_order_view), funs(total = n(), min, median, mean, max)) %>%
  # The result format is rounded 2 digits after the comma
  mutate_if(is.numeric, funs(round(.,2)))

# Average time of ordering since the loan is uploaded every week
# Create a new data.frame using data.frame df_loan_invest
df_lama_order_per_minggu <- df_loan_invest %>%
  # Filter only orders (column is not empty)
  filter(!is.na(order)) %>%
  # Create a date column which is rounded in week, using the floor_date function from the marketplace column
  mutate(tanggal = floor_date(marketplace, 'week'),
         # Calculate the length of the order since upload to marketplace (in hours)
         lama_order = as.numeric(difftime(order, marketplace, units = "hour"))) %>%
  # Grouping by date column 
  group_by(tanggal) %>%
  # Calculate the median of the column lama_order
  summarise(lama_order = median(lama_order))

# Make a plot containing the trend line of the order length per week
ggplot(df_lama_order_per_minggu) +
  geom_line(aes(x = tanggal, y = lama_order)) +
  theme_bw() + 
  labs(title = "The average length of orders in 2020 is longer than 2019", 
       x = "Date", 
       y = "time in the marketplace until ordered (hours)")

# Does the Investor pay for the orders he made
# Create a new data.frame using data.frame df_loan_invest
df_bayar_per_minggu <- df_loan_invest %>%
  # Filter only orders (column is not empty)
  filter(!is.na(order)) %>%
  # Create a date column that is rounded down from the upload time to the marketplace in week
  mutate(tanggal = floor_date(marketplace, 'week')) %>%
  # Grouping by date column
  group_by(tanggal) %>% 
  # Calculate percent_payment by calculating the average of true conditions
  summarise(persen_bayar = mean(!is.na(pay)))

# Make a plot containing the trend line of percent_payment by date
ggplot(df_bayar_per_minggu) +
  geom_line(aes(x = tanggal, y = persen_bayar)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  labs(title="About 95% paid for the order. At the end of May there were outliers due to Eid",
       x="Date", 
       y="Orders that are paid for")

# The time it takes for the investor to pay for the order
# Create a new data.frame using data.frame df_loan_invest
df_lama_bayar_per_minggu <- df_loan_invest %>% 
  # Filter only orders (column is not empty)
  filter(!is.na(pay)) %>%
  # Create a date column which is rounded down from the upload time to the order in week with the floor _date function
  mutate(tanggal = floor_date(order, 'week'),
         # Calculate the lama_bayar since upload to the order
         lama_bayar = as.numeric(difftime(pay, order, units = "hour"))) %>% 
  # Grouping by date column
  group_by(tanggal) %>%
  # Calculate the median of the column lama_bayar
  summarise(lama_bayar = median(lama_bayar))

# Make a plot that contains the trend line from the length of pay per week
ggplot(df_lama_bayar_per_minggu) +
  geom_line(aes(x =tanggal, y = lama_bayar)) +
  theme_bw() + 
  labs(title="Payment times are trending to worsen, 2x longer than before", 
       x="Date", 
       y="time in the marketplace until ordered (hours)")


# Investor Summary Analysis

# Trend Investor Register
# Create a new data.frame using data.frame df_event
df_investor_register <- df_event %>%
  # Filter column nama_event with values 'investor_register'
  filter(nama_event == 'investor_register') %>%
  # Create a date column which is rounded down from created_at in week with the floor _date function
  mutate(tanggal = floor_date(created_at, 'week')) %>% 
  # Grouping by date column
  group_by(tanggal) %>%
  # Count the number of investors each week
  summarise(investor = n_distinct(investor_id))

# Make a plot that containing the trend line of percent_pay by date
ggplot(df_investor_register) +
  geom_line(aes(x = tanggal, y = investor)) +
  theme_bw() + 
  labs(title="Registered investors had risen in early 2020 but have fallen again",
       x="Date", 
       y="Investor Register")

# Investor's First Investment Trend
# Create a new data.frame using data.frame df_event
df_investor_pertama_invest <- df_event %>% 
  # Filter column nama_event with values 'investor_pay_loan'
  filter(nama_event == 'investor_pay_loan') %>%
  # Grouping by column investor_id
  group_by(investor_id) %>%
  # Find the first date for each investor
  summarise(pertama_invest = min(created_at)) %>%
  #Create a date column which is the first investment in weeks using the floor _date function
  mutate(tanggal = floor_date(pertama_invest, 'week')) %>%
  # Grouping by date column
  group_by(tanggal) %>%
  # Count the number of investors each week
  summarise(investor = n_distinct(investor_id))

# Make a plot that containing the trend line of persen_bayar by date
ggplot(df_investor_pertama_invest) +
  geom_line(aes(x = tanggal, y = investor)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 9)) +
  labs(title = "There is an increasing trend in the number of invest investors, but it has dropped dramatically starting March 2020", 
       x = "Date", 
       y = "First Investor's Investment")

# First Cohort Invest by Month Register
# Create a new data.frame using data.frame df_event
df_register_per_investor <- df_event %>%
  # Filter column nama_event with values 'investor_register'
  filter(nama_event == 'investor_register') %>%
  # Create a registered date column which is the rename of created_at
  rename(tanggal_register = created_at) %>%
  # Create a month register column which is the floor_date of the register date (tanggal_register)
  mutate(bulan_register = floor_date(tanggal_register, 'month')) %>%
  # Selecting the investor_id, tanggal_register dan bulan_register columns
  select(investor_id, tanggal_register, bulan_register)

# Make the date of first investment per investor as df_pertama_invest_per_investor
df_pertama_invest_per_investor <- df_event %>%
  # Filter column nama_event with values 'investor_pay_loan'
  filter(nama_event == 'investor_pay_loan') %>%
  # Groupng by investor_id column
  group_by(investor_id) %>%
  # Calculate the first date of investment
  summarise(pertama_invest = min(created_at))

# Create a new data.frame by joining data.frame df_register_per_investor with df_pertama_invest_per_investor
df_register_per_investor %>%
  # Left join df_register_per_investor with df_pertama_invest_per_investor at investor_id
  left_join(df_pertama_invest_per_investor, by = 'investor_id') %>%
  # Calculate lama_invest in months, the largest difftime unit is week (week), so calculate the difference between the last days divided by 30
  mutate(lama_invest = as.numeric(difftime(pertama_invest, tanggal_register, units = "day")) %/% 30) %>%
  # Grouping by bulan_register dan lama_invest column
  group_by(bulan_register, lama_invest) %>%
  # Calculating investor_per_bulan, namely distinct investors per category
  summarise(investor_per_bulan = n_distinct(investor_id)) %>%
  # Grouping by bulan_register column
  group_by(bulan_register) %>%
  # Calculate total investors in registered month
  mutate(register = sum(investor_per_bulan)) %>%
  # Filter lama_invest is not empty
  filter(!is.na(lama_invest)) %>%
  # Calculate the total investors
  mutate(invest = sum(investor_per_bulan)) %>%
  # Calculate the persen_invest in percent format of investment return divided by register
  mutate(persen_invest = scales::percent(invest/register)) %>%
  # Calculate breakdown_persen_invest in percent format from the result of investor_per_bulan divided by invest
  mutate(breakdown_persen_invest = scales::percent(investor_per_bulan/invest)) %>%
  # Remove the investor_per_bulan column because it is not used
  select(-investor_per_bulan) %>%
  # spread data based on lama_invest as key and breakdown_persen_invest as the value of each key
  spread(lama_invest, breakdown_persen_invest)

# Cohort Retention Invest
# Create investment dates per investor in the df_invest_per_investor data.frame from df_event
df_investasi_per_investor <- df_event %>%
  #Filter column nama_event with values 'investor_pay_loan'
  filter(nama_event == 'investor_pay_loan') %>%
  # Create a new column date invest which is the rename of created_at
  rename(tanggal_invest = created_at) %>% 
  # Select investor_id and tanggal_invest column
  select(investor_id, tanggal_invest)

df_pertama_invest_per_investor %>% 
  # Create a first month invest column which is the floor_date of pertama_invest
  mutate(bulan_pertama_invest = floor_date(pertama_invest, 'month')) %>% 
  # Inner join with data.frame df_invest_per_investor on investor_id
  inner_join(df_investasi_per_investor, by = 'investor_id') %>%
  # Calculate the jarak_invest in months
  mutate(jarak_invest = as.numeric(difftime(tanggal_invest, pertama_invest, units = "day")) %/% 30) %>% 
  # Grouping by bulan_pertama_invest and jarak_invest
  group_by(bulan_pertama_invest, jarak_invest) %>%
  # Calculate investor_per_bulan which is distinct investor
  summarise(investor_per_bulan = n_distinct(investor_id)) %>%
  # Grouping by bulan_pertama_invest
  group_by(bulan_pertama_invest) %>%
  #Calculate how many total investors are with the max function to find the highest number on the first invest month cohort
  mutate(investor = max(investor_per_bulan )) %>%
  # Calculate breakdown_persen_invest as the spread of the invest distance from the first investment
  mutate(breakdown_persen_invest = scales::percent(investor_per_bulan/investor)) %>%
  # Remove investor_per_bulan column because it is not used
  select(-investor_per_bulan) %>%
  # spread data based on the distance invest as key and breakdown_persen_invest as the value of each key
  spread(jarak_invest, breakdown_persen_invest) %>% 
  # Omitting column 0 because the result must be 100% all
  select(-`0`)