DatasetSolution\_Ques2
================

Based on the data 1. For each Customer calculate the following: a. Total number of order. b. Total quantity of product purchased. c. Total unique number of product purchased. d. Total amount spent. e. Average amount spent per order. f. Average amount spent per product quantity.

Reading data
============

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
order<-read.csv("Order.csv")
customer<-read.csv("Sales.csv")
order_details<-read.csv("OrderDetails.csv")
```

coloumn names in each dataset

``` r
#since the data we have is in three different tables we merge based on unique key as per the requirements
colnames(order)
```

    ## [1] "Order_ID"    "Customer_ID" "Date"

``` r
colnames(customer)
```

    ## [1] "Customer_ID" "first_name"  "last_name"   "gender"      "Location"

``` r
colnames(order_details)
```

    ## [1] "ID"       "Order_ID" "Product"  "Quantity" "Amount"

Section 1(a):- total number of order per customer

There are 85 unique customer and 300 order from table order,which means there are customer who placed multiple orders.

``` r
#total order for each customer can be calculated using table order
order_per_customer<-order %>% group_by(Customer_ID) %>% summarize(n())
names(order_per_customer)<-c("customer_ID","number_of_orders")
head(order_per_customer)
```

    ## # A tibble: 6 x 2
    ##   customer_ID number_of_orders
    ##         <int>            <int>
    ## 1           1                4
    ## 2           2                3
    ## 3           3                3
    ## 4           4                4
    ## 5           5                5
    ## 6           6                5

Section b. Total quantity of product purchased.

``` r
#since we need to calculate the order quantity per customer ,we first merge order and order_details table
order_orderdetails<-merge(order,order_details)

Total_quantity_purchased<-as.data.frame(aggregate(order_orderdetails$Quantity,list(order_orderdetails$Customer_ID),sum))
#providing names for the dataset
names(Total_quantity_purchased)<-c("customer_ID","Total_Quantity_purchassed")
head(Total_quantity_purchased)
```

    ##   customer_ID Total_Quantity_purchassed
    ## 1           1                        59
    ## 2           2                        54
    ## 3           3                        76
    ## 4           4                       109
    ## 5           5                        84
    ## 6           6                        88

section C . Total unique number of product purchased.?

``` r
head(order_orderdetails,6)
```

    ##   Order_ID Customer_ID       Date  ID                       Product
    ## 1        1          27 26-10-2016  15                   Clams - Bay
    ## 2        1          27 26-10-2016 609        Wine - Delicato Merlot
    ## 3        1          27 26-10-2016 814             Nantucket - 518ml
    ## 4        1          27 26-10-2016 360            Cheese - St. Andre
    ## 5        2          52 31-03-2016 416                  Broom - Corn
    ## 6        2          52 31-03-2016 545 Cookie Dough - Chocolate Chip
    ##   Quantity Amount
    ## 1        5     16
    ## 2        9     86
    ## 3        1     30
    ## 4        2     80
    ## 5        1     65
    ## 6        8     18

From the above data table ,we observe that each order is repeated for each customer mulitple time as it contains diferent product,once we merge the two table of orders and order details,taking count of customer would provide us the total unique product from the merged table.

``` r
Unique_product_purchased<-order_orderdetails %>% group_by(Customer_ID) %>% summarize(n())
names(Unique_product_purchased)<-c("customer_ID","#unique_product_per_customer")
head(Unique_product_purchased)
```

    ## # A tibble: 6 x 2
    ##   customer_ID `#unique_product_per_customer`
    ##         <int>                          <int>
    ## 1           1                             10
    ## 2           2                              7
    ## 3           3                             13
    ## 4           4                             18
    ## 5           5                             15
    ## 6           6                             18

Section d. Total amount spent.

``` r
#From the same table we will try to find amount 
Total_Amount_spent<-as.data.frame(aggregate(order_orderdetails$Amount,list(order_orderdetails$Customer_ID),sum))
names(Total_Amount_spent)<-c("customer_ID","Total_amount_spent")
head(Total_Amount_spent)
```

    ##   customer_ID Total_amount_spent
    ## 1           1                679
    ## 2           2                458
    ## 3           3                647
    ## 4           4                923
    ## 5           5                787
    ## 6           6                833

Section e. Average amount spent per order.?

till now we were trying to find amount for each customer ,now we would like find amount spent per order, we will use aggregate function and sum of amount spent grouped by each order

``` r
Total_Amount_Spent_perorder<-as.data.frame(aggregate(order_orderdetails$Amount,list(order_orderdetails$Customer_ID),mean))
names(Total_Amount_Spent_perorder)<-c("customer_ID","average_Amount_spent_perorder")
head(Total_Amount_Spent_perorder)
```

    ##   customer_ID average_Amount_spent_perorder
    ## 1           1                      67.90000
    ## 2           2                      65.42857
    ## 3           3                      49.76923
    ## 4           4                      51.27778
    ## 5           5                      52.46667
    ## 6           6                      46.27778

``` r
#so the average amount per order is :-
```

Section f. Average amount spent per product quantity.

``` r
head(order_orderdetails)
```

    ##   Order_ID Customer_ID       Date  ID                       Product
    ## 1        1          27 26-10-2016  15                   Clams - Bay
    ## 2        1          27 26-10-2016 609        Wine - Delicato Merlot
    ## 3        1          27 26-10-2016 814             Nantucket - 518ml
    ## 4        1          27 26-10-2016 360            Cheese - St. Andre
    ## 5        2          52 31-03-2016 416                  Broom - Corn
    ## 6        2          52 31-03-2016 545 Cookie Dough - Chocolate Chip
    ##   Quantity Amount
    ## 1        5     16
    ## 2        9     86
    ## 3        1     30
    ## 4        2     80
    ## 5        1     65
    ## 6        8     18

``` r
#per product quantity spend ,creating new variable amount/quantity
order_orderdetails$avgamountspent<-order_orderdetails$Amount/order_orderdetails$Quantity
Average_Amount_Spent_perproduct<-as.data.frame(aggregate(order_orderdetails$avgamountspent,list(order_orderdetails$Customer_ID),mean))
names(Average_Amount_Spent_perproduct)<-c("Customer_ID","#average_amountSpent_productquantity")
head(Average_Amount_Spent_perproduct)
```

    ##   Customer_ID #average_amountSpent_productquantity
    ## 1           1                             17.22714
    ## 2           2                              9.34881
    ## 3           3                             13.07842
    ## 4           4                             14.95022
    ## 5           5                             17.03537
    ## 6           6                             14.59932

Section 2. a. Top 5 Customer where amount spent per order increases over time and delta change in amount spend.

``` r
order_orderdetails$Date<-as.Date(order_orderdetails$Date, "%d-%m-%Y")

#Total amount spent by each customer
customer_top5<-as.data.frame(aggregate(order_orderdetails$Amount,list(order_orderdetails$Customer_ID,order_orderdetails$Date),sum))

names(customer_top5)<-c("customer_ID","date","amount_spent")
customer_top5$customer_ID<-as.factor(customer_top5$customer_ID)
head(customer_top5)
```

    ##   customer_ID       date amount_spent
    ## 1           4 2015-01-03          386
    ## 2          17 2015-01-05          136
    ## 3           8 2015-01-14          206
    ## 4          21 2015-01-16           19
    ## 5          45 2015-01-20          111
    ## 6          24 2015-01-27          123

We will take the difference,for each customer,from there last purchase to first purchase and find the percentage change.

``` r
diff_f<-customer_top5 %>% group_by(customer_top5$customer_ID) %>% 
summarize(diff=((last(amount_spent)-first(amount_spent))/first(amount_spent))*100)
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.2

``` r
names(diff_f)<-c("Customer_ID","Delta")

#Getting names of customer from customer table and combining with customer spend overtime  
problem2_finaltable<-merge(customer,diff_f)
head(problem2_finaltable)
```

    ##   Customer_ID first_name last_name gender  Location     Delta
    ## 1           1 Fernandina     Abdon Female   Sangali -80.40541
    ## 2           2      Regen   Channon   Male  Lubuagan  15.69767
    ## 3           3     Aubrey  Costello   Male Vincennes 329.34783
    ## 4           4     Virgie      Sirr Female  Pacaraos -63.73057
    ## 5           5      Byrom Lillecrap   Male Outokumpu -58.21727
    ## 6           6    Glendon   Parlour   Male    Bogale  40.00000

``` r
#combining first and lastname
problem2_finaltable$Customer_name<-paste(problem2_finaltable$first_name,problem2_finaltable$last_name,sep = " ")

#formating output in desired format
prob2_final_output<-problem2_finaltable[order(problem2_finaltable$Delta,decreasing=TRUE),c(1,6,7)]
head(prob2_final_output)
```

    ##    Customer_ID     Delta     Customer_name
    ## 43          43 2171.4286    Odell Burkwood
    ## 21          21 1363.1579  Rikki Peeke-Vout
    ## 3            3  329.3478   Aubrey Costello
    ## 15          15  320.0000 Cherilynn Swalowe
    ## 55          56  272.3077     Duncan Lergan
    ## 26          26  226.8817      Perl Duferie

``` r
#Final output and the top customers whose purchase has increased over time
```
