install.packages('completejourney')

#Exercise 1

library(tidyverse)
library(completejourney)

transactions <- transactions_sample
products

transactions <- transactions %>%
    mutate(
      regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
      loyalty_price = (sales_value + coupon_match_disc) / quantity,
      coupon_price = (sales_value - coupon_disc) / quantity
    ) %>%
  select(regular_price, loyalty_price, coupon_price, product_id, everything())

top5_loyalty_price <- transactions %>%
   slice_max(order_by = loyalty_price, n = 5)
    print(top5_loyalty_price)

top5_withq <- transactions %>%
  filter(quantity > 0) %>%
  slice_max(order_by = loyalty_price, n = 5)
print(top5_withq)

top_prod <- top5_withq$product_id[1]
top_prod_ans <- products %>%
  filter(product_id == top_prod)
print(top_prod_ans)

#Exercise 2

# <=1 dollar regular price

transactions %>% 
  filter(regular_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

# loyalty price <=1 

transactions %>%
  filter(loyalty_price <= 1) %>%
  select(product_id) %>%
  n_distinct()


#coupon price less than 1

transactions %>%
  filter(coupon_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

#Exercise 3


proportions <- transactions %>%
  group_by(basket_id) %>%
  summarize(total_sales = sum(sales_value)) %>%
  mutate(over10 = total_sales > 10,
         over20 = total_sales > 20) %>%
  summarize(
    prop_over10 = mean(over10),
    prop_over20 = mean(over20)
  )
print(proportions)

#Exercise 4
transactions %>%
  group_by(store_id) %>%
  summarise(total_sales_value = sum(sales_value)) %>%
  arrange(desc(total_sales_value))
  
transactions %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarise(avg_pct_loyalty_disc = mean(pct_loyalty_disc, na.rm = TRUE)) %>%
  arrange(desc(avg_pct_loyalty_disc))

#Part 2


library(readxl)

excel_sheets(path = "data/mbta.xlsx")
mbta <- read_excel(path = "data/mbta.xlsx", skip = 1, na = 'NA')

head(mbta, 6)

summary(mbta)

mbta_missing <- is.na(mbta)

sum(mbta_missing)

view(mbta)

#Exercise 3

mbta_clean <- mbta %>%
  slice(-c(1,7,11)) %>%
  select(-c(1))

dim(mbta_clean)
view(mbta_clean)

#Exercise 4

mbta_obs <- mbta_clean %>%
  pivot_longer(cols = -mode,
               names_to = 'date',
               values_to = 'thou_riders')
dim(mbta_obs)
view(mbta_obs)

#Exercise 5

mbta_sep <- mbta_obs %>%
  separate(col = date, into = c("Year", "Month"), sep = "-")

head(mbta_sep)


#Exercise 7
mbta_sep %>%
  group_by(mode) %>%
  summarize(avg_ridership = mean(thou_riders))

#7.2
mbta_sep %>%
  filter(Month == "01") %>%
  group_by(mode) %>%
  summarize(avg_ridership = mean(thou_riders))





