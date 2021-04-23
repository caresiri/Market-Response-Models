Homework 7: Univariate Time Series Analysis
================
Carlos Siri
4/4/2021

## Lydia Pinkham

*For both Advertising and Sales Determine the best possible ARIMA
model.*

We are presented with sales and advertising time series data spread
annually from 1907 to 1960.

``` r
summary(df1)
```

    ##      Trend            Year          Sales       Advertising    
    ##  Min.   : 1.00   Min.   :1907   Min.   : 921   Min.   : 339.0  
    ##  1st Qu.:14.25   1st Qu.:1920   1st Qu.:1344   1st Qu.: 619.5  
    ##  Median :27.50   Median :1934   Median :1778   Median : 862.0  
    ##  Mean   :27.50   Mean   :1934   Mean   :1829   Mean   : 934.5  
    ##  3rd Qu.:40.75   3rd Qu.:1947   3rd Qu.:2201   3rd Qu.:1090.0  
    ##  Max.   :54.00   Max.   :1960   Max.   :3438   Max.   :1941.0

``` r
sales <- ts(df1$Sales, start = 1907) #convert sales to time series data
advertising <- ts(df1$Advertising, start = 1907) #convert advertising to time series data
cbind(sales,
      advertising) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Sales and Advertising per year for Lydia Pinkham")
```

![](Univariate-Time-Series-Analysis_files/figure-gfm/summary-1.png)<!-- -->

The plots seem to be relatively correlated, with an initial increase in
sales and advertising for the first 30 years, followed by a a drop of
about 10 years. Both of the plot increase have a steady increase for 10
years, and are followed by a steady decrease thereafter.

The auto.arima function in the forecast package in R is used to estimate
the models for *sales* and *advertising* below.

``` r
auto_sales <- auto.arima(sales, seasonal=FALSE)
auto_sales
```

    ## Series: sales 
    ## ARIMA(2,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ar2       mean
    ##       1.3718  -0.4737  1738.7843
    ## s.e.  0.1171   0.1195   254.8003
    ## 
    ## sigma^2 estimated as 42688:  log likelihood=-364.21
    ## AIC=736.41   AICc=737.23   BIC=744.37

``` r
auto_advertising<- auto.arima(advertising, seasonal=FALSE)
auto_advertising
```

    ## Series: advertising 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ma1      mean
    ##       0.7158  0.3320  903.5071
    ## s.e.  0.1103  0.1519  124.1358
    ## 
    ## sigma^2 estimated as 43689:  log likelihood=-364.2
    ## AIC=736.4   AICc=737.22   BIC=744.36

The function generated the following estimates \* *sales*: ARIMA(2,0,0)
\* *advertising*: ARIMA(1,0,1)

Anything automatic is dangerous. Lets try it manually.

The data seems to be non-stationary. This can be confirmed with a Unit
Root Test.

### Sales

``` r
d1sales <- diff(sales)
d2sales <- diff(d1sales)
adf.test(sales)  #stationary could also use ndiffs(sales) to find the proper number of differences
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  sales
    ## Dickey-Fuller = -2.3883, Lag order = 3, p-value = 0.4181
    ## alternative hypothesis: stationary

``` r
adf.test(d1sales)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  d1sales
    ## Dickey-Fuller = -2.8116, Lag order = 3, p-value = 0.2478
    ## alternative hypothesis: stationary

``` r
adf.test(d2sales)  
```

    ## Warning in adf.test(d2sales): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  d2sales
    ## Dickey-Fuller = -4.8476, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
d2sales %>% ggtsdisplay(main="")
```

![](Univariate-Time-Series-Analysis_files/figure-gfm/unit%20root%20sales-1.png)<!-- -->

The unit root tests show that the sales data, and its first order
difference, is in not stationary. We use second order difference for
sales data.

For the sales model, we see that PACF spikes at 2 lags. We will try
ARIMA(2,2,0)

``` r
(fit2 <- Arima(sales, order=c(2,2,0)))
```

    ## Series: sales 
    ## ARIMA(2,2,0) 
    ## 
    ## Coefficients:
    ##           ar1      ar2
    ##       -0.2714  -0.3236
    ## s.e.   0.1304   0.1287
    ## 
    ## sigma^2 estimated as 55779:  log likelihood=-357.06
    ## AIC=720.11   AICc=720.61   BIC=725.96

``` r
checkresiduals(fit2)
```

![](Univariate-Time-Series-Analysis_files/figure-gfm/check%20residuals%20SALES-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(2,2,0)
    ## Q* = 8.7376, df = 8, p-value = 0.3649
    ## 
    ## Model df: 2.   Total lags used: 10

### Advertising

``` r
d1advertising <- diff(advertising)
d2advertising <- diff(d1advertising)
adf.test(advertising) 
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  advertising
    ## Dickey-Fuller = -1.5545, Lag order = 3, p-value = 0.7539
    ## alternative hypothesis: stationary

``` r
adf.test(d1advertising) 
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  d1advertising
    ## Dickey-Fuller = -2.994, Lag order = 3, p-value = 0.1744
    ## alternative hypothesis: stationary

``` r
adf.test(d2advertising) 
```

    ## Warning in adf.test(d2advertising): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  d2advertising
    ## Dickey-Fuller = -5.9856, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
d2advertising %>% ggtsdisplay(main="")
```

![](Univariate-Time-Series-Analysis_files/figure-gfm/unit%20root%20advertising-1.png)<!-- -->
The unit root tests show that the advertising data, and its first order
difference, is in not stationary. We use second order difference for
sales data.

There is are spike at *q=2* lags in the ACF plot. We can try an
ARIMA(0,2,2) model

``` r
(fit4 <- Arima(advertising, order=c(0,2,2)))
```

    ## Series: advertising 
    ## ARIMA(0,2,2) 
    ## 
    ## Coefficients:
    ##           ma1      ma2
    ##       -0.7953  -0.2047
    ## s.e.   0.1966   0.1875
    ## 
    ## sigma^2 estimated as 50408:  log likelihood=-356.11
    ## AIC=718.23   AICc=718.73   BIC=724.08

``` r
checkresiduals(fit4)
```

![](Univariate-Time-Series-Analysis_files/figure-gfm/check%20residuals%20ADV%20102-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,2,2)
    ## Q* = 32.282, df = 8, p-value = 8.291e-05
    ## 
    ## Model df: 2.   Total lags used: 10

## PL shares

For 8 product categories you find time-varying information for private
label shares in different countries. For each separate category do a
panel unit root test and explore different panel root options.

The following sections test the unit root hypothesis for the eight
product categories provided.

### Air Care

The Air Care variable is stationary

``` r
library(plm)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plm':
    ## 
    ##     between, lag, lead

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Air_Care <- df2 %>% filter(categories == "Air Care")
Air_Care <- data.frame(split(Air_Care$plshare, Air_Care$Geographies))
purtest(Air_Care , pmax = 3, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  Air_Care
    ## chisq = 145.91, df = 92, p-value = 0.0002958
    ## alternative hypothesis: stationarity

``` r
purtest(Air_Care , pmax = 3, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  Air_Care
    ## chisq = 282.03, df = 92, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

``` r
purtest(Air_Care , pmax = 3, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  Air_Care
    ## chisq = 646.18, df = 92, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

### Bath and Shower

NA’s not allowed

### Breakfast Cereals

When no exogenous variables are introduced, the Breakfast and Cereals
variable is non-stationary.

``` r
B_C <- df2 %>% filter(categories == "Breakfast Cereals")
B_C <- data.frame(split(B_C$plshare, B_C$Geographies))
purtest(B_C , pmax = 3, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  B_C
    ## chisq = 62.931, df = 104, p-value = 0.9995
    ## alternative hypothesis: stationarity

``` r
purtest(B_C , pmax = 3, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  B_C
    ## chisq = 449.51, df = 104, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

``` r
purtest(B_C , pmax = 3, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  B_C
    ## chisq = 1417.6, df = 104, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

### Deodorants

The Deodorants variable is stationary

``` r
D <- df2 %>% filter(categories == "Deodorants")
D <- data.frame(split(D$plshare, D$Geographies))
purtest(D , pmax = 3, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  D
    ## chisq = 141.87, df = 74, p-value = 3.545e-06
    ## alternative hypothesis: stationarity

``` r
purtest(D , pmax = 3, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  D
    ## chisq = 540.78, df = 74, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

``` r
purtest(D , pmax = 3, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  D
    ## chisq = 873.36, df = 74, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

### Ice Cream

When no exogenous variables are introduced, The Ice Cream variable is
non-stationary. pmax was changed to 2 for intercept and trend to avoid
the following error: Error in .urcval(arg = q\[i\], nobs = n.sample, niv
= 1, itt = itt, itv = itv, : NA/NaN/Inf in foreign function call (arg 6)

``` r
I <- df2 %>% filter(categories == "Ice Cream")
I <- data.frame(split(I$plshare, I$Geographies))
purtest(I , pmax = 3, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  I
    ## chisq = 113.84, df = 96, p-value = 0.1034
    ## alternative hypothesis: stationarity

``` r
purtest(I , pmax = 2, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  I
    ## chisq = 380.87, df = 96, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

``` r
purtest(I , pmax = 2, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  I
    ## chisq = 512.11, df = 96, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

### Pasta

When no exogenous variables are introduced, The Pasta variable is
non-stationary.

``` r
P <- df2 %>% filter(categories == "Pasta")
P <- data.frame(split(P$plshare, P$Geographies))
purtest(P , pmax = 3, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  P
    ## chisq = 109.46, df = 122, p-value = 0.7849
    ## alternative hypothesis: stationarity

``` r
purtest(P , pmax = 3, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  P
    ## chisq = 829.01, df = 122, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

``` r
purtest(P , pmax = 3, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  P
    ## chisq = 1110.3, df = 122, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

### Skin Care

I was not able to run more than one lag. With one lag, skin care is not
stationary when individual intercepts are included as exogenous
variables.

``` r
S <- df2 %>% filter(categories == "Skin Care")
S  <- data.frame(split(S$plshare, S$Geographies))
purtest(S , pmax = 1, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  S
    ## chisq = 61.754, df = 96, p-value = 0.9974
    ## alternative hypothesis: stationarity

``` r
purtest(S , pmax = 1, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  S
    ## chisq = 107.48, df = 96, p-value = 0.199
    ## alternative hypothesis: stationarity

``` r
purtest(S , pmax = 1, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  S
    ## chisq = 134.34, df = 96, p-value = 0.005984
    ## alternative hypothesis: stationarity

### Snack Bars

When no exogenous variables are introduced, The “Snack Bars” variable is
non-stationary.

``` r
SB <- df2 %>% filter(categories == "Snack Bars")
SB <- data.frame(split(SB$plshare, SB$Geographies))
purtest(SB , pmax = 3, exo = "none", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: None)
    ## 
    ## data:  SB
    ## chisq = 27.489, df = 66, p-value = 1
    ## alternative hypothesis: stationarity

``` r
purtest(SB , pmax = 3, exo = "intercept", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts)
    ## 
    ## data:  SB
    ## chisq = 230.92, df = 66, p-value < 2.2e-16
    ## alternative hypothesis: stationarity

``` r
purtest(SB , pmax = 3, exo = "trend", test = "madwu")
```

    ## 
    ##  Maddala-Wu Unit-Root Test (ex. var.: Individual Intercepts and Trend)
    ## 
    ## data:  SB
    ## chisq = 282.16, df = 66, p-value < 2.2e-16
    ## alternative hypothesis: stationarity
