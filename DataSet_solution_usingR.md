Solving\_Dataproblem\_using\_R
================

in This We will Try to solve some Data Problems Using R.

``` r
#importing Data and library
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Diamond<-read.csv("Diamonds.csv")
head(Diamond)
```

    ##   carat       cut color clarity depth table price    x    y    z
    ## 1  0.23     Ideal     E     SI2  61.5    55   326 3.95 3.98 2.43
    ## 2  0.21   Premium     E     SI1  59.8    61   326 3.89 3.84 2.31
    ## 3  0.23      Good     E     VS1  56.9    65   327 4.05 4.07 2.31
    ## 4  0.29   Premium     I     VS2  62.4    58   334 4.20 4.23 2.63
    ## 5  0.31      Good     J     SI2  63.3    58   335 4.34 4.35 2.75
    ## 6  0.24 Very Good     J    VVS2  62.8    57   336 3.94 3.96 2.48

Question :-

Bucket data into 10 equal bucket using column "carat" such that . Each bucket has equal no of observation. . First bucket has all least values of carat and last bucket has all max values of carat. For each bucket calculate Minimum, Maximum, Average value of "carat", Average value of "depth", number of observation in each bucket.

``` r
#sorting data based on carat and making a new dataset

new_data<-Diamond[order(Diamond$carat),]

#creating a new variable to group into 10 buckets based on observations
new_data$Seq<-seq(1:53940)

#using cut function
```

Using cut function,creating a new variable with equal rows, this function is very helpful in cases we want to class a continous variable in categorical variable with range.

``` r
new_data$busketgroup<-cut(new_data$Seq, seq(from =0, to=53940 , by=5394),labels=c(1:10))
table(new_data$busketgroup)
```

    ## 
    ##    1    2    3    4    5    6    7    8    9   10 
    ## 5394 5394 5394 5394 5394 5394 5394 5394 5394 5394

calculating min,max,average values and ,making final output format

``` r
Bucket<-unique(new_data$busketgroup)
Min_carat<-as.data.frame(aggregate(new_data$carat,list(new_data$busketgroup),min))
Max_carat<-as.data.frame(aggregate(new_data$carat,list(new_data$busketgroup),max))
avg_carat<-as.data.frame(aggregate(new_data$carat,list(new_data$busketgroup),mean))
avg_depth<-as.data.frame(aggregate(new_data$depth,list(new_data$busketgroup),mean))
observation<-new_data %>% group_by(new_data$busketgroup) %>% summarize(n())
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.2

``` r
Final_table1<-cbind(Bucket,Min_carat[,2],Max_carat[,2],avg_carat[,2],avg_depth[,2],observation[,2])
names(Final_table1)<-c("Bucket","min_carat","max_carat","avg_carat","avg_depth","Observations")
Final_table1
```

    ##    Bucket min_carat max_carat avg_carat avg_depth Observations
    ## 1       1      0.20      0.31 0.2890211  61.77034         5394
    ## 2       2      0.31      0.35 0.3258250  61.67189         5394
    ## 3       3      0.35      0.42 0.3919207  61.67959         5394
    ## 4       4      0.42      0.53 0.4927086  61.73524         5394
    ## 5       5      0.53      0.70 0.6183908  61.60934         5394
    ## 6       6      0.70      0.90 0.7557249  61.77722         5394
    ## 7       7      0.90      1.01 0.9622822  62.05124         5394
    ## 8       8      1.01      1.13 1.0522989  61.67798         5394
    ## 9       9      1.13      1.51 1.2805729  61.75386         5394
    ## 10     10      1.51      5.01 1.8106526  61.76735         5394
