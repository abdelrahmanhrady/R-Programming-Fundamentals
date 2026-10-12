
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)


# Problem 1

sales_raw <- tibble(
 sale_id = 1:8,
 customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
 item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
              "FOLDER", "Folder ", " ERASER ", "pen"),
 amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                "$30.00", "$25.00", NA, "$20.00"),
 discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
 code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
              "bad", "ST-106", "ON-107X", "on-108"),
 tags = c("gift;urgent", "online", "gift", "member",
          "online;member", "gift", "member", "gift;member")
)

# Part A

sales_clean <- sales_raw |>
 mutate(
  item_clean = str_to_lower(str_squish(item_raw)),
  amount = parse_number(amount_raw),
  discount_rate = parse_number(discount_raw)/100,
  net_amount = amount*(1-discount_rate),
  size = case_when(
    is.na(net_amount) ~ "unrecorded",
    net_amount >= 20 ~ "large",
    TRUE ~ "small"
  )
 )

print(sales_clean |>
 select(sale_id,item_clean,amount,discount_rate,net_amount,size))

# Removed extra spaces and changed items to lowercase.
# Converted amounts and discounts to numbers. Missing amounts stay NA.


# Part B

sales_codes <- sales_clean |>
 mutate(
   code_clean = str_to_upper(str_trim(code_raw)),
   valid_code = str_detect(code_clean,"^(ST|ON)-[0-9]{3}$")
 )

invalid_codes <- sales_codes |>
 filter(!valid_code) |>
 select(sale_id,code_raw,code_clean)

print(invalid_codes)

valid_code_parts <- sales_codes |>
 filter(valid_code) |>
 separate_wider_delim(code_clean,delim = "-",
                      names = c("channel","number"),
                      cols_remove = FALSE)

print(valid_code_parts |>
 select(sale_id,code_clean,channel,number))

sale_tags <- sales_clean |>
 select(sale_id,tags) |>
 separate_longer_delim(tags,delim = ";")

print(sale_tags)

# separate_wider_delim makes new columns.
# separate_longer_delim makes new rows.


# Part C

customer_summary <- sales_clean |>
 summarise(
  sales_n = n(),
  known_n = sum(!is.na(net_amount)),
  large_n = sum(!is.na(net_amount) & net_amount>=20),
  large_share = if_else(known_n>0,large_n/known_n,NA_real_),
  .by = customer_id
 ) |>
 arrange(customer_id)

print(customer_summary)

# For C1 the numerator is 0 large sales and the denominator
# is 1 known sale. So large_share = 0/1 = 0.

sales_factors <- sales_clean |>
 mutate(
   item_factor = fct_infreq(factor(item_clean)),
   item_group = fct_lump_n(factor(item_clean),n=2,other_level="Other")
 )

print(levels(sales_factors$item_factor))
print(levels(sales_factors$item_group))
print(count(sales_factors,item_factor))
print(count(sales_factors,item_group))

# fct_infreq changes the order based on frequency.
# fct_lump_n combines less common categories into Other.


# Problem 2

reports_raw <- tibble(
 form = c("A", "A", "A", "B", "B", "B"),
 line = c(1, 2, 3, 1, 2, 3),
 customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
 round = c(1, 3, 1, 1, 2, 1),
 primary_amount = c(10, -999, 20, NA, 0, 30),
 backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
 customer_id = c("C1", "C2", "C3", "C5"),
 region = c("North", "South", NA, "West")
)


# Part A

reports_labeled <- reports_raw |>
 arrange(form,line) |>
 group_by(form) |>
 fill(customer_id,.direction="down") |>
 ungroup()

print(reports_labeled)

reports_clean <- reports_labeled |>
 mutate(
   primary_clean = na_if(primary_amount,-999),
   amount = coalesce(primary_clean,backup_amount)
 )

print(reports_clean |>
 select(form,line,customer_id,round,primary_amount,backup_amount,amount))

# coalesce keeps the primary amount unless it is missing.
# Zero is still a valid amount so it does not use the backup.

reports_before <- reports_clean |>
 select(customer_id,round,amount) |>
 mutate(recorded_row=TRUE)

reports_complete <- reports_before |>
 complete(customer_id,round=1:3,
          fill=list(recorded_row=FALSE)) |>
 arrange(customer_id,round)

print(reports_complete)

original_rows <- nrow(reports_before)
added_rows <- nrow(reports_complete)-original_rows
missing_before <- sum(is.na(reports_before$amount))
missing_after <- sum(is.na(reports_complete$amount))

print(tibble(original_rows,added_rows,missing_before,missing_after))

# 6 original rows and 6 added rows.
# Missing amounts went from 1 to 7.
# C3 round 1 was explicitly missing.
# C1 round 2 was an implicit missing row.


# Part B

print(sum(is.na(customers$customer_id)))
print(sum(duplicated(customers$customer_id)))

# Both are 0, so no missing or duplicate customer IDs.

duplicate_reports <- reports_clean |>
 count(customer_id,round) |>
 filter(n>1)

print(duplicate_reports)

# No repeated customer-round combinations.

customers_marked <- customers |>
 mutate(customer_found=TRUE)

reports_joined <- reports_clean |>
 left_join(customers_marked,
           by=join_by(customer_id),
           relationship="many-to-one",
           na_matches="never") |>
 mutate(customer_found=coalesce(customer_found,FALSE))

print(reports_joined |>
 select(customer_id,round,amount,region,customer_found))

only_reports <- reports_clean |>
 distinct(customer_id) |>
 anti_join(customers,by=join_by(customer_id),na_matches="never")

only_customers <- customers |>
 distinct(customer_id) |>
 anti_join(reports_clean,by=join_by(customer_id),na_matches="never")

print(only_reports)
print(only_customers)

# C4 is only in reports and C5 is only in customers.
# C3 has customer_found TRUE even though region is NA.
# C4 has FALSE because it does not have a matching customer record.


# Part C

# customers_bad was not defined in the PDF.
# This example gives C2 another region to show the duplicate join.
# Replace this if the instructor provides customers_bad.

customers_bad <- bind_rows(
 customers,
 tibble(customer_id="C2",region="East")
)

reports_duplicated <- reports_clean |>
 left_join(customers_bad,
           by=join_by(customer_id),
           relationship="many-to-many")

print(nrow(reports_clean))
print(nrow(reports_duplicated))
print(sum(reports_clean$amount,na.rm=TRUE))
print(sum(reports_duplicated$amount,na.rm=TRUE))

repeated_reports <- reports_duplicated |>
 count(customer_id,round) |>
 filter(n>1)

print(repeated_reports)

# For this example, rows increased from 6 to 7.
# Total amount increased from 75 to 95.
# C2 round 1 was repeated so its amount of 20 got counted twice.
# many-to-one would give an error since C2 has multiple matches.
# many-to-many allows it but does not fix the conflicting regions.

reports_wide <- reports_complete |>
 select(customer_id,round,amount) |>
 pivot_wider(names_from=round,values_from=amount,
             names_prefix="round_")

print(reports_wide)

reports_long <- reports_wide |>
 pivot_longer(cols=starts_with("round_"),
              names_to="round",values_to="amount",
              values_drop_na=TRUE)

print(reports_long)
print(nrow(reports_long))

# 5 rows remain after dropping NA values.
# Dropping missing values doesn't tell us what the amounts were.


# Problem 3

# Part A

two_sample_t <- function(x,y,equal_var=FALSE,alpha=0.05){

 nx <- length(x)
 ny <- length(y)

 mean_x <- mean(x)
 mean_y <- mean(y)
 var_x <- var(x)
 var_y <- var(y)

 if(equal_var){

  method <- "Pooled"
  pooled_var <- ((nx-1)*var_x+(ny-1)*var_y)/(nx+ny-2)
  se <- sqrt(pooled_var*(1/nx+1/ny))
  df <- nx+ny-2

 } else {

  method <- "Welch"
  se <- sqrt(var_x/nx+var_y/ny)

  df <- (var_x/nx+var_y/ny)^2 /
    ((var_x/nx)^2/(nx-1)+(var_y/ny)^2/(ny-1))
 }

 t_stat <- (mean_x-mean_y)/se
 p_value <- 2*pt(abs(t_stat),df=df,lower.tail=FALSE)
 reject <- p_value<alpha

 return(list(method=method,t_stat=t_stat,df=df,
             p_value=p_value,reject=reject))
}

x_check <- c(12,15,14,10,13)
y_check <- c(9,11,8,10,7,12,9,8)

welch_result <- two_sample_t(x_check,y_check)
pooled_result <- two_sample_t(x_check,y_check,equal_var=TRUE)

print(welch_result)
print(pooled_result)

welch_test <- t.test(x_check,y_check,var.equal=FALSE)
pooled_test <- t.test(x_check,y_check,var.equal=TRUE)

print(welch_test)
print(pooled_test)

print(abs(welch_result$t_stat-welch_test$statistic)<1e-8)
print(abs(welch_result$df-welch_test$parameter)<1e-8)
print(abs(welch_result$p_value-welch_test$p.value)<1e-8)

print(abs(pooled_result$t_stat-pooled_test$statistic)<1e-8)
print(abs(pooled_result$df-pooled_test$parameter)<1e-8)
print(abs(pooled_result$p_value-pooled_test$p.value)<1e-8)

# All 6 checks returned TRUE.
# The results match t.test within 1e-8.


# Part B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
 scenario = c("equal_null","unequal_null",
              "equal_shift","unequal_shift"),
 mean_x = c(0,0,0,0),
 mean_y = c(0,0,1,1),
 sd_x = c(1,3,1,3),
 sd_y = c(1,1,1,1)
)

simulation_results <- data.frame(
 scenario=character(8000),
 replicate=integer(8000),
 method=character(8000),
 t_stat=numeric(8000),
 df=numeric(8000),
 p_value=numeric(8000),
 reject=logical(8000)
)

set.seed(2026)

row_num <- 1

for(i in 1:4){

 for(j in 1:B){

  x <- rnorm(n_x,mean=scenarios$mean_x[i],sd=scenarios$sd_x[i])
  y <- rnorm(n_y,mean=scenarios$mean_y[i],sd=scenarios$sd_y[i])

  welch <- two_sample_t(x,y,equal_var=FALSE)
  pooled <- two_sample_t(x,y,equal_var=TRUE)

  for(result in list(welch,pooled)){

   simulation_results$scenario[row_num] <- scenarios$scenario[i]
   simulation_results$replicate[row_num] <- j
   simulation_results$method[row_num] <- result$method
   simulation_results$t_stat[row_num] <- result$t_stat
   simulation_results$df[row_num] <- result$df
   simulation_results$p_value[row_num] <- result$p_value
   simulation_results$reject[row_num] <- result$reject

   row_num <- row_num+1
  }
 }
}

print(head(simulation_results,6))
print(nrow(simulation_results))

# 4 scenarios * 1000 repetitions * 2 methods = 8000 rows.

simulation_summary <- simulation_results |>
 summarise(
   mean_t=mean(t_stat),
   sd_t=sd(t_stat),
   rejection_rate=mean(reject),
   .by=c(scenario,method)
 ) |>
 arrange(scenario,method)

print(simulation_summary |>
 mutate(across(c(mean_t,sd_t,rejection_rate),~round(.x,3))))

barplot(
 simulation_summary$rejection_rate,
 names.arg=paste(simulation_summary$scenario,
                 simulation_summary$method,sep="\n"),
 las=2,cex.names=0.65,ylim=c(0,1),
 ylab="Rejection Rate",
 main="Simulation Rejection Rates"
)

abline(h=0.05,lty=2)

# Short Report:
# With equal variances, both tests had Type I error rates
# close to 0.05 (3.8% pooled and 4.4% Welch).
# With unequal variances, pooled was 13.9% while Welch
# was 6.1%, which is closer to the expected 5%.
# When the means differed, both tests rejected more often,
# which means they had higher power.
# Higher power alone isn't enough because the pooled test
# also had more false rejections with unequal variances.
