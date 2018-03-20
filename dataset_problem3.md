problem3\_datasetsolution
================

``` r
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

Loading Data Employees
----------------------

``` r
Employee<-read.csv("Employee.csv")
str(Employee)
```

    ## 'data.frame':    1000 obs. of  7 variables:
    ##  $ id        : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ first_name: Factor w/ 946 levels "Abbi","Abie",..: 488 739 305 788 829 543 668 26 176 534 ...
    ##  $ last_name : Factor w/ 985 levels "Aberkirdo","Able",..: 879 750 784 149 147 555 755 390 650 253 ...
    ##  $ email     : Factor w/ 1000 levels "aabramowitzl5@constantcontact.com",..: 540 828 311 787 846 588 677 42 201 565 ...
    ##  $ gender    : Factor w/ 2 levels "Female","Male": 1 2 2 1 2 2 2 2 1 1 ...
    ##  $ DOJ       : Factor w/ 852 levels "01-01-2011","01-01-2014",..: 215 129 828 575 285 514 705 331 668 465 ...
    ##  $ DOL       : Factor w/ 465 levels "","01-01-2012",..: 464 1 1 1 1 218 1 1 407 1 ...

DOJ and DOL ,which are date of joining and leaving date for an employee needs to be converted into date format first

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.4.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
Employee$DOJ<-as.Date(Employee$DOJ,"%d-%m-%Y")
Employee$DOL<-as.Date(Employee$DOL,"%d-%m-%Y")
```

creating a new variable which holds year for each employee DOJ

``` r
Employee$Joining_Year<-year(Employee$DOJ)
Employee$Left_year<-year(Employee$DOL)
head(Employee)
```

    ##   id first_name last_name                    email gender        DOJ
    ## 1  1     Karlen    Tacker        ktacker0@ning.com Female 2010-05-08
    ## 2  2  Rafaellle    Recher   rrecher1@google.com.hk   Male 2015-05-05
    ## 3  3       Evin   Rousell           erousell2@g.co   Male 2013-11-30
    ## 4  4     Roslyn  Cassella   rcassella3@walmart.com Female 2016-05-21
    ## 5  5        Sky   Cashman  scashman4@homestead.com   Male 2015-11-10
    ## 6  6     Leslie M'Chirrie lmchirrie5@pinterest.com   Male 2010-06-19
    ##          DOL Joining_Year Left_year
    ## 1 2014-12-31         2010      2014
    ## 2       <NA>         2015        NA
    ## 3       <NA>         2013        NA
    ## 4       <NA>         2016        NA
    ## 5       <NA>         2015        NA
    ## 6 2013-08-15         2010      2013

Now we want to know number of employees after each year, so the logic goes like this 1.we calculate total number of employees, grouping by years,which is but first step towards solving. 2.once we get number of employees in each year we take cumulative sum and this gives number of employees at each year,we still didn't account for those who left 3.In this part we group varaible date of leaving the firm by year and subtract from earlier cumulative sum and we are left with employees after each year.

``` r
Number_of_employees<-Employee %>% group_by(Employee$Joining_Year) %>% summarise(n())
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.2

``` r
number_of_thoseleft<-Employee %>% group_by(Employee$Left_year) %>% summarise(n())
nn<-na.omit(number_of_thoseleft)
names(nn)<-c("year","numberofleft")
```

``` r
#taking cumulative sum at each year

Number_of_employees$cumsum<-cumsum(Number_of_employees$`n()`)

#combining with number of employees left
forma<-cbind(Number_of_employees,nn[,2])

#employees left after each year
forma$Employeeleft_after_each_year<-forma$cumsum-forma$numberofleft

#employees attrition rate each year
forma$Attrition_Rate<-forma$numberofleft/forma$Employeeleft_after_each_year
forma[,c(1,5,6)]
```

    ##   Employee$Joining_Year Employeeleft_after_each_year Attrition_Rate
    ## 1                  2010                          118     0.03389831
    ## 2                  2011                          243     0.11522634
    ## 3                  2012                          351     0.10826211
    ## 4                  2013                          477     0.11320755
    ## 5                  2014                          566     0.13957597
    ## 6                  2015                          655     0.14809160
    ## 7                  2016                          777     0.12483912
    ## 8                  2017                          889     0.11248594
    ## 9                  2018                          979     0.02145046

In this part we will try find average Employee tenure after end of each year.

``` r
head(Employee)
```

    ##   id first_name last_name                    email gender        DOJ
    ## 1  1     Karlen    Tacker        ktacker0@ning.com Female 2010-05-08
    ## 2  2  Rafaellle    Recher   rrecher1@google.com.hk   Male 2015-05-05
    ## 3  3       Evin   Rousell           erousell2@g.co   Male 2013-11-30
    ## 4  4     Roslyn  Cassella   rcassella3@walmart.com Female 2016-05-21
    ## 5  5        Sky   Cashman  scashman4@homestead.com   Male 2015-11-10
    ## 6  6     Leslie M'Chirrie lmchirrie5@pinterest.com   Male 2010-06-19
    ##          DOL Joining_Year Left_year
    ## 1 2014-12-31         2010      2014
    ## 2       <NA>         2015        NA
    ## 3       <NA>         2013        NA
    ## 4       <NA>         2016        NA
    ## 5       <NA>         2015        NA
    ## 6 2013-08-15         2010      2013

``` r
employee1<-Employee
employee1$diff_time<-employee1$DOL-employee1$DOJ
```

we will now ,group the variable difference in time based on year,which give us total number of days at each year and then we will divide who left each year,provide us the average tenure.

``` r
average_tenure<-employee1 %>% group_by(employee1$Left_year) %>% summarize(averge=mean(diff_time,na.rm=T))
names(average_tenure)<-c("Year","Average_Employee_Tenure")

# This gives us average tenure after each year
```

Creating final table

``` r
Employee_after_each_year<-forma$Employeeleft_after_each_year
Attrition_rate<-forma$Attrition_Rate
output<-cbind(average_tenure[1:9,],Employee_after_each_year,Attrition_rate)
output
```

    ##   Year Average_Employee_Tenure Employee_after_each_year Attrition_rate
    ## 1 2010           152.5000 days                      118     0.03389831
    ## 2 2011           266.8929 days                      243     0.11522634
    ## 3 2012           437.1842 days                      351     0.10826211
    ## 4 2013           702.4444 days                      477     0.11320755
    ## 5 2014           845.5570 days                      566     0.13957597
    ## 6 2015           988.1340 days                      655     0.14809160
    ## 7 2016          1392.5876 days                      777     0.12483912
    ## 8 2017          1400.1500 days                      889     0.11248594
    ## 9 2018           880.4762 days                      979     0.02145046

The table provide us imformation about number of Employees at the end of each year,average tenure of employee and the attrition rate.
