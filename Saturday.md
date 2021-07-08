ST 558 Project 2
================
David Arthur
6/28/2021

-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarizations](#summarizations)
-   [Modeling](#modeling)
    -   [First linear regression model](#first-linear-regression-model)
    -   [Second linear regression
        model](#second-linear-regression-model)
    -   [Random Forest Model](#random-forest-model)
-   [Comparison of models](#comparison-of-models)

# Introduction

…

# Data

We begin by reading in the data, changing the names of some factor
levels, and filtering by day of week

``` r
day <- readr::read_csv("day.csv", col_types = cols(
  season = col_factor(),
  yr = col_factor(),
  mnth = col_factor(),
  holiday = col_factor(),
  weekday = col_factor(),
  workingday = col_factor(),
  weathersit = col_factor()))

day <- day %>% mutate(season = fct_recode(season, winter = "1", spring = "2", summer = "3", fall = "4")) %>%
  mutate(yr = fct_recode(yr, "2011" = "0", "2012" = "1")) %>%
  mutate(weekday = fct_recode(weekday, Sunday = "0", Monday = "1", Tuesday = "2", Wednesday = "3", Thursday = "4", Friday = "5", Saturday = "6")) %>%
  mutate(weathersit = fct_recode(weathersit, clear = "1", mist = "2", lightRainOrSnow = "3", heavyRainOrSnow = "4")) %>%
  filter(weekday == params$dayOfWeek)

# read in version without factors for correlation plot
dayNF <- readr::read_csv("day.csv", col_types = cols(
  weekday = col_factor()))
```

Next, we partition the data into training and test sets

``` r
set.seed(21)
trainIndex <- createDataPartition(day$cnt, p = 0.7, list = FALSE)
dayTrain <- day[trainIndex, ]
dayTest <- day[-trainIndex, ]
```

# Summarizations

We begin our exploratory analysis of the data with a graphical overview
of the relationships between variables. Obvious patterns in the plots,
as well as high correlation values, indicate associations between
variables.

``` r
GGally::ggpairs(dayTrain %>% select(2:6, 8:9, atemp, windspeed, registered, casual))
```

![](Saturday_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# dayNFCor <- cor(as.matrix(dayNF %>% select(3:9, atemp, windspeed, casual, registered,cnt)))
# corrplot(dayNFCor, type = "upper", tl.pos = "lt")
# corrplot(dayNFCor, type = "lower", method = "number", add = TRUE, diag = FALSE, tl.pos = "n")
```

We will now look in more detail at relationships between time-related
variables and the `registered` response variable. When we do our linear
regression modeling we will need to decide which (if any) of these
predictors to use. For example, the date variable (`dteday`) and
`season` may not be useful in the presence of `weekday`, `mnth`, and
`yr` (or vice versa), as they provide largely redundant information.

``` r
g <- ggplot(data = dayTrain)
g + geom_point(aes(x = dteday, y = registered))
```

![](Saturday_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
meanByMonthYr <- dayTrain %>% group_by(mnth, yr) %>%
  summarize(meanReg = mean(registered))
```

    ## `summarise()` has grouped output by 'mnth'. You can override using the `.groups` argument.

``` r
g2 <- ggplot(meanByMonthYr, aes(x = mnth))
g2 + geom_bar(aes(y = meanReg, fill = yr), position = "dodge", stat = "identity")
```

![](Saturday_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

We will look next in more detail at the relationship between
quantitative weather variables and the `registered` response variable.
The appearance of nonlinear relationships in the plots may indicate the
need for quadratic terms in our linear regression models. The adjusted
temperature variable, `atemp`, seems particularly likely to require a
quadratic term, as both low and high temperatures can discourage people
from bicycling. Similarly, with humidity and windspeed, low to moderate
values may have no effect, but particularly high values could have an
effect, so those variables may also require quadratic terms.

``` r
g + geom_point(aes(x = atemp, y = registered)) + facet_wrap(~ yr)
```

![](Saturday_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
g + geom_point(aes(x = hum, y = registered)) + facet_wrap(~ yr)
```

![](Saturday_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
g + geom_point(aes(x = windspeed, y = registered)) + facet_wrap(~ yr)
```

![](Saturday_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

We now view at a table displaying the mean number of `registered`,
`casual`, and total riders at each level of the categorical `weathersit`
variable. It seems plausible that in rain or snow, the number of casual
riders might decrease by a larger factor than would the number of
registered riders.

``` r
meanByWeather <- dayTrain %>% group_by(weathersit) %>%
  summarize(meanCas = mean(casual), meanReg = mean(registered), meanTotal = mean(cnt))
kable(meanByWeather, digits = 1, col.names = c("Weather", "Mean Casual Riders", "Mean Registered Riders", "Mean Total Riders"), caption = "Average # of riders by weather category")
```

| Weather         | Mean Casual Riders | Mean Registered Riders | Mean Total Riders |
|:----------------|-------------------:|-----------------------:|------------------:|
| mist            |             1187.1 |                 2782.8 |            3969.9 |
| clear           |             1671.3 |                 3370.3 |            5041.7 |
| lightRainOrSnow |              692.5 |                 1934.5 |            2627.0 |

Average \# of riders by weather category

Exploratory data analysis and summary (James)

``` r
ggpairs(dayTrain %>% select(-instant,-dteday, -season, -yr, -cnt, -weekday), 
        ggplot2::aes(colour=workingday))
```

![](Saturday_files/figure-gfm/carr_explore-1.png)<!-- --> Notes from
looking at ggpairs plots: Working days are the highest usage for
registered riders, but non-working days are the highest usage for casual
riders. Registered riders are the primary volume, so we definitely care
most about them but worth keeping in mind. There are two types of
non-working days: weekends and holidays, and there is a difference in
volume for each of those rider types depending on whether it is a
holiday or a weekend.

Air temperature and temperature are nearly 100% correlated. We should
probably figure out which one of them we want to use. Speaking of
correlated, can we drop the date and only use months? Unfortunately, it
looks like we need to keep the year field as well, since year 2 had
better performance than year 1. Do we want to keep season and month? I
lean towards keeping year and month, but dropping season and date. Let
me know what you think.

Looking at the scatter of casual vs registered, broken out by working
day, it’s crazy how separate the linear relationships look:

``` r
g <- ggplot(data=dayTrain, aes(x=registered, y=casual))
g + geom_point(aes(color=workingday))
```

![](Saturday_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> On working
days, registered bikes are the main rider group. On non-working days, it
switches to casual. Looking at day of the week, we may be able to
exclude it since it will be covered by the working day flag and holiday
flag, but I guess we can check the models to see if it provides anything
extra.

``` r
g <- ggplot(data=dayTrain %>% 
                 select(weekday, casual, registered) %>%
                 pivot_longer(cols=c(casual, registered),
                              names_to = 'metrics',
                              values_to = 'riders') %>%
                 group_by(weekday, metrics) %>%
                 summarise(avg_riders = mean(riders)), 
            aes(x=weekday, y=avg_riders, fill=metrics))
```

    ## `summarise()` has grouped output by 'weekday'. You can override using the `.groups` argument.

``` r
g + geom_bar(stat='identity', position='dodge')
```

![](Saturday_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> Looking at
this graph, weekday definitely seems relatively stable across the days
(working days for registered and non-working days for casual are the
jumps), but there may be enough variation to include it.

\#\#I like this graph. I thought about doing one like it, but wasn’t
sure how to code it. pivot\_longer hadn’t occurred to me.

\#\#About which variables to include, I agree with your comments. My
understanding is that each of us comes up with our own models (I do a
linear regression and a random forest, you do a linear regression and a
boosted tree), so you and I don’t need to include the same predictors.
We do need to agree ahead of time on which response we’re going to model
(casual, registered, or cnt), so that the results of the 4 models can be
compared to each other. I’m fine with any of the 3. Do you have a
preference?

Yeah, no preference here either. I guess we could just say registered
since it’s the highest volume customer, and if we were doing this
analysis for that company then registered users would be the most
important group.

\#\#Sounds good, we’ll go with registered.

# Modeling

We will now fit two linear regression models, using differing
approaches, with the goal of creating a model that does a good job of
predicting the number of registered riders on any given day, based on
the values of the predictor variables in the data set. We will fit the
models using the training data set that we partitioned above, and then
test the accuracy of the models’ predictions using the test data set.

Linear regression estimates the effect of each predictor variable on the
mean value of the response variable, with the other predictor variables
held constant. A linear regression model can be expressed as  
*Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*X*<sub>*i*1</sub> + *β*<sub>2</sub>*X*<sub>*i*2</sub> + ... + *β*<sub>*p*</sub>*X*<sub>*i**p*</sub> + *E*<sub>*i*</sub>

where *Y*<sub>*i*</sub> is the response, *i* represents the observation
number, *X*<sub>*i**j*</sub> are the predictor variables, and
*E*<sub>*i*</sub> is the normally distributed random error. The
*β*<sub>*j*</sub> coefficents must be linear, but the predictor
variables can be higher order terms (e.g. *x*<sup>2</sup>) or
interaction terms (e.g. *x*<sub>1</sub>*x*<sub>2</sub>). Creating a
model to estimate the response using observed data, we have  
$$\\hat{y\_i} = \\hat\\beta\_0 + \\hat\\beta\_1x\_{i1} + \\hat\\beta\_2x\_{i2} + ... + \\hat\\beta\_px\_{ip}$$

The *β̂*<sub>*j*</sub> coefficients (estimates for *β*<sub>*j*</sub>) are
calculated for each predictor variable to minimize the residual sum of
squares, using the observed values of *x*<sub>*i**j*</sub> and
*y*<sub>*i*</sub>  
$$min\_{\\beta\_0, \\beta\_1, ..., \\beta\_p}\\sum\_{i=1}^{n}(y\_i - \\beta\_0 - \\beta\_1x\_{i1} - \\beta\_2x\_{i2} - ... - \\beta\_px\_{ip})^2$$

The linear regression model can be used for inference, to understand the
relationships between the predictor variables and the response, as well
as for prediction of a mean response given new values of the predictor
variables. There are varying approaches to choosing which predictor
variables to include in a linear regression model. For our first linear
regression model, we ….  
For our second linear regression model, we ….

### First linear regression model

``` r
library(leaps)

data <- dayTrain %>% 
               filter(weekday == params$dayOfWeek) %>% drop_na() %>%
               select(-instant,-dteday, -season, 
                    -weekday, -atemp, -casual, -cnt)

#this function converts new data to a model matrix
#so that a prediction can be run via matrix multiplication
#on a best subsets model
predict.regsubsets = function(object,newdata,id,...){
      form = as.formula(object$call[[2]]) 
      mat = model.matrix(form,newdata)    
      coefi = coef(object,id=id)          
      xvars = names(coefi)                
      mat[,xvars]%*%coefi               
}


#let's do cross validation with folds
k <- 4
set.seed(21)
folds <- sample(1:k, nrow(data), replace=T)

cv_errors = matrix(NA, k, 16, dimnames = list(NULL, paste(1:16)))

for (j in 1:k) {
  best <- regsubsets(registered ~ ., 
                     data=data[folds!=j,], nvmax=20)
  
  for (i in 1:16) {
    pred <- predict(best, data[folds==j,], id=i)
    
    
    cv_errors[j, i] <- mean((temp_data$registered[folds==j]-pred)^2)
  }
}
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in, : 2 linear
    ## dependencies found

    ## Reordering variables and trying again:

    ## Error in eval(x): object 'newX' not found

``` r
# Take the mean of over all folds for each model size
mean_cv_errors = apply(cv_errors, 2, mean)

# Find the model size with the smallest cross-validation error
min = which.min(mean_cv_errors)

#the model w/ 14 variables was best when using 4 fold cv.
#i did 4 fold because there are only about 80 rows of data per weekday

best_full <- regsubsets(registered ~ ., 
                     data=temp_data[folds!=j,], nvmax=20)
```

    ## Error in is.data.frame(data): object 'temp_data' not found

``` r
fit <- lm(registered ~ temp*hum,
        data=dayTrain %>% 
             filter(weekday == params$dayOfWeek) %>% drop_na() %>%
             select(-instant,-dteday, -season, 
                    -weekday, -atemp, -casual, -cnt))
```

### Second linear regression model

In this approach, we start with a full linear regression model that
includes all of the predictor variables. We will then reduce
collinearity (correlation among predictor variables) by removing
redundant predictors until we reach an optimal (lowest) AIC. We will
calculate the condition number (*κ*) for each of the candidate models,
which is a measure of collinearity. Roughly, *κ* &lt; 30 is considered
desirable. Finally, we will choose among several variations of the
optimal model (including various higher order terms) using cross
validation (described below).

We begin with the full model, which includes all of the predictors.
`holiday` and `workingday` are excluded for days of the week that
include only one level of `holiday` and `workingday`, respectively.

``` r
mlrFull <- lm(registered ~ dteday + season +  yr + mnth + weathersit + temp + 
                    atemp + hum + windspeed, dayTrain)
if(length(unique(dayTrain$workingday)) != 1){
  mlrFull <- update(mlrFull, . ~ . + workingday)
}
if(length(unique(dayTrain$holiday)) != 1){
  mlrFull <- update(mlrFull, . ~ . + holiday)
}

summary(mlrFull)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ dteday + season + yr + mnth + weathersit + 
    ##     temp + atemp + hum + windspeed, data = dayTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1186.15  -213.57   -14.13   218.83  1002.73 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               150236.781 112993.387   1.330   0.1893    
    ## dteday                        -9.981      7.542  -1.323   0.1914    
    ## seasonspring                 922.775    448.552   2.057   0.0446 *  
    ## seasonsummer                 930.220    442.412   2.103   0.0403 *  
    ## seasonfall                  1766.784    328.780   5.374 1.76e-06 ***
    ## yr2012                      5296.895   2784.147   1.903   0.0625 .  
    ## mnth2                        546.558    439.877   1.243   0.2195    
    ## mnth3                       1115.675    577.739   1.931   0.0588 .  
    ## mnth4                        858.936    983.044   0.874   0.3862    
    ## mnth5                       1449.638   1178.162   1.230   0.2240    
    ## mnth6                       1860.343   1393.776   1.335   0.1877    
    ## mnth7                       1985.744   1609.330   1.234   0.2227    
    ## mnth8                       2151.102   1779.539   1.209   0.2321    
    ## mnth9                       3158.701   1960.297   1.611   0.1130    
    ## mnth10                      2711.448   2174.549   1.247   0.2179    
    ## mnth11                      2271.923   2356.105   0.964   0.3393    
    ## mnth12                      3207.932   2579.366   1.244   0.2191    
    ## weathersitclear              154.292    190.396   0.810   0.4213    
    ## weathersitlightRainOrSnow   -741.065    464.233  -1.596   0.1164    
    ## temp                       -8626.436   5957.619  -1.448   0.1535    
    ## atemp                      12584.634   6194.986   2.031   0.0472 *  
    ## hum                        -1053.077    644.218  -1.635   0.1080    
    ## windspeed                  -1916.409    980.161  -1.955   0.0558 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 519.8 on 53 degrees of freedom
    ## Multiple R-squared:  0.902,  Adjusted R-squared:  0.8613 
    ## F-statistic: 22.17 on 22 and 53 DF,  p-value: < 2.2e-16

``` r
AIC(mlrFull)
```

    ## [1] 1186.811

``` r
x <- model.matrix(mlrFull)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition number = sqrt(e$val[1]/min(e$val))
```

We see that *κ* = 2.2038974^{6}, which is a sign of high collinearity,
so we begin removing insignificant predictors one at a time, each time
checking to confirm that AIC declines, or at least that it increases
only marginally.

To help in consideration of which variables to remove, we view the
correlations. For days of the week that don’t include any holidays, `?`
will appear in the `holiday` and `workingday` rows and columns.

``` r
dayNFCor <- cor(as.matrix(dayNF %>%
                            mutate(weekday = fct_recode(weekday, Sunday = "0", Monday = "1", Tuesday = "2", Wednesday = "3", Thursday = "4", Friday = "5", Saturday = "6")) %>%
                            mutate(dteday = as.numeric(dteday)) %>%
                            filter(weekday == params$dayOfWeek) %>%
                            select(2:6, 8:13, registered)))
corrplot(dayNFCor, type = "upper", tl.pos = "lt")
corrplot(dayNFCor, type = "lower", method = "number", add = TRUE, diag = FALSE, tl.pos = "n")
```

![](Saturday_files/figure-gfm/arthur_bestMLR-1.png)<!-- -->

First, we remove `workingday`, as it is fully determined by the day of
the week and the `holiday` variable, so adds nothing to the model. We
also remove `temp`, as it is almost perfectly correlated with `atemp`,
and `dteday`, which adds little if any predictive value beyond `yr` plus
`mnth` plus `season`.

``` r
mlr2 <- update(mlrFull, . ~ . - workingday - temp - dteday)
summary(mlr2)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ season + yr + mnth + weathersit + atemp + 
    ##     hum + windspeed, data = dayTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1339.31  -226.56    32.84   231.38  1049.40 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  977.4      573.2   1.705  0.09378 .  
    ## seasonspring                1128.9      445.0   2.537  0.01405 *  
    ## seasonsummer                 926.4      448.7   2.065  0.04370 *  
    ## seasonfall                  1848.7      332.7   5.557 8.27e-07 ***
    ## yr2012                      1566.8      126.1  12.422  < 2e-16 ***
    ## mnth2                        209.0      383.3   0.545  0.58786    
    ## mnth3                        447.7      375.9   1.191  0.23873    
    ## mnth4                       -421.1      606.8  -0.694  0.49056    
    ## mnth5                       -219.6      665.3  -0.330  0.74260    
    ## mnth6                       -237.5      689.4  -0.344  0.73180    
    ## mnth7                       -416.0      727.8  -0.572  0.56991    
    ## mnth8                       -374.8      680.8  -0.550  0.58423    
    ## mnth9                        396.8      601.3   0.660  0.51209    
    ## mnth10                      -215.9      561.6  -0.384  0.70220    
    ## mnth11                      -927.4      481.0  -1.928  0.05901 .  
    ## mnth12                      -283.3      358.3  -0.791  0.43260    
    ## weathersitclear              127.0      193.1   0.658  0.51344    
    ## weathersitlightRainOrSnow   -577.7      461.9  -1.251  0.21633    
    ## atemp                       3768.8     1165.2   3.235  0.00206 ** 
    ## hum                        -1092.4      656.2  -1.665  0.10168    
    ## windspeed                  -2663.1      913.0  -2.917  0.00511 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 529.9 on 55 degrees of freedom
    ## Multiple R-squared:  0.8943, Adjusted R-squared:  0.8559 
    ## F-statistic: 23.27 on 20 and 55 DF,  p-value: < 2.2e-16

``` r
AIC(mlr2)
```

    ## [1] 1188.561

``` r
x <- model.matrix(mlr2)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition number = sqrt(e$val[1]/min(e$val))
```

We see that AIC has changed little, and that *κ* = 39.3, which indicates
a large reduction in collinearity.

`mnth`, `weathersit` and `windspeed` appear to be marginally
significant, so we look at the effect of removing each of them from the
model:  
Remove `mnth`

``` r
mlr3 <- update(mlr2, . ~ . - mnth)
summary(mlr3)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ season + yr + weathersit + atemp + 
    ##     hum + windspeed, data = dayTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1717.32  -226.17    25.77   280.85  1623.51 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 514.11     591.00   0.870   0.3875    
    ## seasonspring                554.68     263.52   2.105   0.0391 *  
    ## seasonsummer                289.83     316.93   0.914   0.3638    
    ## seasonfall                 1246.86     207.56   6.007 9.00e-08 ***
    ## yr2012                     1572.12     135.05  11.641  < 2e-16 ***
    ## weathersitclear              95.18     196.94   0.483   0.6305    
    ## weathersitlightRainOrSnow  -839.43     451.75  -1.858   0.0676 .  
    ## atemp                      4753.40     779.23   6.100 6.21e-08 ***
    ## hum                        -729.23     645.62  -1.130   0.2628    
    ## windspeed                 -2264.17     875.38  -2.587   0.0119 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 577.9 on 66 degrees of freedom
    ## Multiple R-squared:  0.8492, Adjusted R-squared:  0.8286 
    ## F-statistic: 41.28 on 9 and 66 DF,  p-value: < 2.2e-16

``` r
AIC(mlr3)
```

    ## [1] 1193.586

``` r
x <- model.matrix(mlr3)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition number = sqrt(e$val[1]/min(e$val))
```

Remove `weathersit`

``` r
mlr4 <- update(mlr2, . ~ . - weathersit)
# summary(mlr4)
AIC(mlr4)
```

    ## [1] 1187.218

``` r
x <- model.matrix(mlr4)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition # =
# sqrt(e$val[1]/min(e$val))
```

Remove `windspeed`

``` r
mlr5 <- update(mlr2, . ~ . - windspeed)
# summary(mlr5)
AIC(mlr5)
```

    ## [1] 1197.492

``` r
x <- model.matrix(mlr5)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition # =
# sqrt(e$val[1]/min(e$val))
```

For `mnth`, `weathersit`, and `windspeed`, removal from the model
results in only marginal change to AIC. If our main goal were inference
and understanding the relationships between the variables, we might want
to remove them from the model for the sake of simplicity,
interpretability, and more narrow confidence intervals. Because our
primary goal here is prediction, we will leave them in the model, and
choose mlr2 as our base linear regression model.

We will now do some diagnostic plots on our base model, and then
consider adding higher order terms to the model.

``` r
# compare to model chosen by leaps::step() function
mlrStep <- step(mlrFull)
```

    ## Start:  AIC=969.13
    ## registered ~ dteday + season + yr + mnth + weathersit + temp + 
    ##     atemp + hum + windspeed
    ## 
    ##              Df Sum of Sq      RSS    AIC
    ## <none>                    14320637 969.13
    ## - weathersit  2    847907 15168544 969.50
    ## - dteday      1    473154 14793792 969.60
    ## - temp        1    566505 14887142 970.08
    ## - hum         1    722008 15042645 970.87
    ## - yr          1    978014 15298651 972.15
    ## - windspeed   1   1032925 15353562 972.43
    ## - atemp       1   1115031 15435669 972.83
    ## - mnth       11   6032786 20353424 973.85
    ## - season      3   8031468 22352106 996.97

``` r
names(mlrStep)
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"       
    ##  [7] "qr"            "df.residual"   "contrasts"     "xlevels"       "call"          "terms"        
    ## [13] "model"         "formula"       "anova"

``` r
mlrStep$call
```

    ## lm(formula = registered ~ dteday + season + yr + mnth + weathersit + 
    ##     temp + atemp + hum + windspeed, data = dayTrain)

``` r
mlr2$call
```

    ## lm(formula = registered ~ season + yr + mnth + weathersit + atemp + 
    ##     hum + windspeed, data = dayTrain)

``` r
AIC(mlr2, mlrStep)
```

    ##         df      AIC
    ## mlr2    22 1188.561
    ## mlrStep 24 1186.811

``` r
# does mlr3  agrees with step() choice?
```

We can check for constant variance of our error term, an assumption of
our model, by looking at a plot of the model’s fitted values vs the
residuals (difference between fitted response and observed response). A
“megaphone” shape can indicate non-constant variance.

``` r
plot(mlr2$fitted, mlr3$residuals)
```

![](Saturday_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Another way to assess constant variance is with the Box-Cox method,
which can suggest transformations of the response to address problems
with non-constant variance. If the maximum log-likelihood of *λ* close
to 1, as in this case, indicates that non-constant variance is not a
problem with the existing model.

``` r
MASS::boxcox(mlr2)
```

![](Saturday_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

We will also look at for signs of nonlinearity, which can indicate the
need for quadratic terms for some of the predictors. The partial
residual plots below plot the relationship between each predictor and
the response, with the effect of the other predictors removed.

``` r
termplot( mlr2, partial.resid = TRUE, terms = c("atemp", "windspeed", "hum"))
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](Saturday_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](Saturday_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

For at least some days of the week there is a nonlinear pattern to the
plots, particularly for `atemp`, so we will try adding quadratic terms
for each of them to our base model.

Try adding *a**t**e**m**p*<sup>2</sup>

``` r
mlr8 <- update(mlr2, . ~ . + I(atemp^2))
summary(mlr8)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ season + yr + mnth + weathersit + atemp + 
    ##     hum + windspeed + I(atemp^2), data = dayTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1301.37  -238.33    22.15   208.32  1052.66 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 309.978    789.177   0.393  0.69602    
    ## seasonspring               1093.657    443.913   2.464  0.01697 *  
    ## seasonsummer               1016.148    452.698   2.245  0.02891 *  
    ## seasonfall                 1847.706    331.185   5.579 7.99e-07 ***
    ## yr2012                     1574.241    125.704  12.523  < 2e-16 ***
    ## mnth2                        -2.848    419.018  -0.007  0.99460    
    ## mnth3                       245.970    408.843   0.602  0.54994    
    ## mnth4                      -544.437    612.374  -0.889  0.37791    
    ## mnth5                      -252.318    662.865  -0.381  0.70496    
    ## mnth6                      -158.206    689.375  -0.229  0.81935    
    ## mnth7                      -287.448    732.106  -0.393  0.69614    
    ## mnth8                      -335.113    678.531  -0.494  0.62339    
    ## mnth9                       308.763    602.888   0.512  0.61064    
    ## mnth10                     -392.841    577.490  -0.680  0.49925    
    ## mnth11                    -1146.486    511.185  -2.243  0.02904 *  
    ## mnth12                     -469.428    387.773  -1.211  0.23133    
    ## weathersitclear             171.639    195.686   0.877  0.38431    
    ## weathersitlightRainOrSnow  -766.691    485.037  -1.581  0.11979    
    ## atemp                      7992.958   3640.404   2.196  0.03244 *  
    ## hum                       -1042.652    654.550  -1.593  0.11701    
    ## windspeed                 -2485.053    920.435  -2.700  0.00924 ** 
    ## I(atemp^2)                -5503.314   4495.577  -1.224  0.22621    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 527.6 on 54 degrees of freedom
    ## Multiple R-squared:  0.8971, Adjusted R-squared:  0.8571 
    ## F-statistic: 22.43 on 21 and 54 DF,  p-value: < 2.2e-16

``` r
AIC(mlr8)
```

    ## [1] 1188.48

Reduced or similar AIC, so keep mlr8 as new base model.

Try adding *h**u**m*<sup>2</sup>

``` r
mlr9 <- update(mlr8, . ~ . + I(hum^2))
summary(mlr9)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ season + yr + mnth + weathersit + atemp + 
    ##     hum + windspeed + I(atemp^2) + I(hum^2), data = dayTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1237.56  -195.42     5.67   226.02  1030.36 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -988.20    1256.21  -0.787   0.4350    
    ## seasonspring               1045.27     442.38   2.363   0.0218 *  
    ## seasonsummer                942.99     452.98   2.082   0.0422 *  
    ## seasonfall                 1878.82     329.75   5.698 5.46e-07 ***
    ## yr2012                     1539.03     127.65  12.057  < 2e-16 ***
    ## mnth2                       190.64     441.12   0.432   0.6674    
    ## mnth3                       263.58     406.26   0.649   0.5193    
    ## mnth4                      -450.66     612.29  -0.736   0.4650    
    ## mnth5                      -140.95     663.68  -0.212   0.8326    
    ## mnth6                       -74.17     687.59  -0.108   0.9145    
    ## mnth7                      -203.77     729.83  -0.279   0.7812    
    ## mnth8                      -298.14     674.46  -0.442   0.6603    
    ## mnth9                       352.47     599.66   0.588   0.5592    
    ## mnth10                     -450.05     575.16  -0.782   0.4374    
    ## mnth11                    -1180.07     508.31  -2.322   0.0241 *  
    ## mnth12                     -496.11     385.64  -1.286   0.2039    
    ## weathersitclear             115.75     198.89   0.582   0.5630    
    ## weathersitlightRainOrSnow  -686.65     485.50  -1.414   0.1631    
    ## atemp                      7852.02    3617.00   2.171   0.0344 *  
    ## hum                        3423.78    3439.65   0.995   0.3241    
    ## windspeed                 -2117.76     955.39  -2.217   0.0310 *  
    ## I(atemp^2)                -5397.70    4465.45  -1.209   0.2321    
    ## I(hum^2)                  -3598.33    2721.17  -1.322   0.1917    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 523.9 on 53 degrees of freedom
    ## Multiple R-squared:  0.9004, Adjusted R-squared:  0.8591 
    ## F-statistic: 21.79 on 22 and 53 DF,  p-value: < 2.2e-16

``` r
AIC(mlr9)
```

    ## [1] 1188.013

Similar AIC for most days of week, so keep mlr9 as a candidate model to
compare using cross validation.

Try adding *w**i**n**d**s**p**e**e**d*<sup>2</sup>

``` r
mlr10 <- update(mlr8, . ~ . + I(windspeed^2))
summary(mlr10)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ season + yr + mnth + weathersit + atemp + 
    ##     hum + windspeed + I(atemp^2) + I(windspeed^2), data = dayTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1172.10  -239.04    26.84   243.77  1141.57 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 -579.0      803.8  -0.720  0.47447    
    ## seasonspring                1029.3      417.6   2.465  0.01699 *  
    ## seasonsummer                 849.8      429.2   1.980  0.05293 .  
    ## seasonfall                  1850.0      311.1   5.946 2.21e-07 ***
    ## yr2012                      1541.8      118.6  12.997  < 2e-16 ***
    ## mnth2                        331.2      410.6   0.807  0.42349    
    ## mnth3                        147.3      385.6   0.382  0.70405    
    ## mnth4                       -551.7      575.3  -0.959  0.34190    
    ## mnth5                       -301.9      623.0  -0.485  0.62995    
    ## mnth6                       -297.4      649.4  -0.458  0.64892    
    ## mnth7                       -396.1      688.8  -0.575  0.56769    
    ## mnth8                       -361.9      637.5  -0.568  0.57268    
    ## mnth9                        260.1      566.6   0.459  0.64808    
    ## mnth10                      -527.7      544.6  -0.969  0.33692    
    ## mnth11                     -1072.7      480.9  -2.231  0.02997 *  
    ## mnth12                      -410.4      364.9  -1.125  0.26569    
    ## weathersitclear              158.7      183.9   0.863  0.39203    
    ## weathersitlightRainOrSnow   -656.9      457.3  -1.437  0.15669    
    ## atemp                       9260.5     3448.5   2.685  0.00965 ** 
    ## hum                        -1433.1      629.9  -2.275  0.02696 *  
    ## windspeed                   5749.3     3004.7   1.913  0.06110 .  
    ## I(atemp^2)                 -6219.0     4230.7  -1.470  0.14748    
    ## I(windspeed^2)            -17511.9     6119.7  -2.862  0.00602 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 495.6 on 53 degrees of freedom
    ## Multiple R-squared:  0.9109, Adjusted R-squared:  0.8739 
    ## F-statistic: 24.63 on 22 and 53 DF,  p-value: < 2.2e-16

``` r
AIC(mlr10)
```

    ## [1] 1179.561

Similar AIC for most days of week, so keep mlr10 as a candidate model to
compare using cross validation.

Try including all 3 quadratic terms

``` r
mlr11 <- update(mlr8, . ~ . + I(hum^2) + I(windspeed^2))
summary(mlr11)
```

    ## 
    ## Call:
    ## lm(formula = registered ~ season + yr + mnth + weathersit + atemp + 
    ##     hum + windspeed + I(atemp^2) + I(hum^2) + I(windspeed^2), 
    ##     data = dayTrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1178.0  -231.7    28.2   228.3  1150.7 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 -361.9     1225.2  -0.295   0.7689    
    ## seasonspring                1035.6      422.3   2.453   0.0176 *  
    ## seasonsummer                 855.5      433.8   1.972   0.0539 .  
    ## seasonfall                  1843.7      315.1   5.852 3.29e-07 ***
    ## yr2012                      1547.3      121.9  12.695  < 2e-16 ***
    ## mnth2                        310.1      423.8   0.732   0.4676    
    ## mnth3                        138.1      391.0   0.353   0.7253    
    ## mnth4                       -571.4      586.4  -0.974   0.3344    
    ## mnth5                       -327.6      637.9  -0.514   0.6098    
    ## mnth6                       -322.4      663.8  -0.486   0.6292    
    ## mnth7                       -419.4      702.0  -0.597   0.5528    
    ## mnth8                       -371.0      644.4  -0.576   0.5673    
    ## mnth9                        248.4      573.9   0.433   0.6669    
    ## mnth10                      -523.5      549.8  -0.952   0.3454    
    ## mnth11                     -1061.7      487.5  -2.178   0.0340 *  
    ## mnth12                      -401.7      370.0  -1.085   0.2827    
    ## weathersitclear              169.5      191.1   0.887   0.3792    
    ## weathersitlightRainOrSnow   -667.2      463.5  -1.440   0.1559    
    ## atemp                       9360.5     3505.2   2.670   0.0101 *  
    ## hum                        -2373.3     4027.0  -0.589   0.5582    
    ## windspeed                   6135.1     3443.0   1.782   0.0806 .  
    ## I(atemp^2)                 -6280.8     4276.8  -1.469   0.1480    
    ## I(hum^2)                     739.9     3129.1   0.236   0.8140    
    ## I(windspeed^2)            -18493.0     7439.5  -2.486   0.0162 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 500.1 on 52 degrees of freedom
    ## Multiple R-squared:  0.911,  Adjusted R-squared:  0.8716 
    ## F-statistic: 23.14 on 23 and 52 DF,  p-value: < 2.2e-16

``` r
AIC(mlr11)
```

    ## [1] 1181.48

Similar AIC for most days of week, so keep mlr11 as a candidate model to
compare using cross validation.

We will now compare the 4 candidate models using cross validation. Cross
validation subdivides the training set into *k* folds, then fits a model
using *k* − 1 of those folds, and tests its accuracy predicting on the
*k*<sup>*t**h*</sup> fold. This is repeated *k* − 1 more times, so that
each fold gets a turn as the test set. Several measures of the
performance of the model are returned. We will choose the best model in
terms of lowest Root Mean Squared Error.

``` r
if(length(unique(dayTrain$holiday)) != 1){
  mlrFit8 <- train(registered ~ season + yr + mnth + holiday + weathersit + atemp + hum + windspeed + I(atemp^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
  
  mlrFit9 <- train(registered ~ season + yr + mnth + holiday + weathersit + atemp + hum + windspeed + I(atemp^2) + I(hum^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
  
  mlrFit10 <- train(registered ~ season + yr + mnth + holiday + weathersit + atemp + hum + windspeed + I(atemp^2) + I(windspeed^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
  
  mlrFit11 <- train(registered ~ season + yr + mnth + holiday + weathersit + atemp + hum + windspeed + I(atemp^2) + I(hum^2)+ I(windspeed^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
}else{
  mlrFit8 <- train(registered ~ season + yr + mnth + weathersit + atemp + hum + windspeed + I(atemp^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
  
  mlrFit9 <- train(registered ~ season + yr + mnth + weathersit + atemp + hum + windspeed + I(atemp^2) + I(hum^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
  
  mlrFit10 <- train(registered ~ season + yr + mnth + weathersit + atemp + hum + windspeed + I(atemp^2) + I(windspeed^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
  
  mlrFit11 <- train(registered ~ season + yr + mnth + weathersit + atemp + hum + windspeed + I(atemp^2) + I(hum^2)+ I(windspeed^2), data = dayTrain,
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3))
}
comparison <- data.frame(t(mlrFit8$results), t(mlrFit9$results), t(mlrFit10$results), t(mlrFit11$results))
colnames(comparison) <- c("mlrFit8", "mlrFit9", "mlrFit10", "mlrFit11")
kable(comparison)
```

|            |     mlrFit8 |     mlrFit9 |    mlrFit10 |    mlrFit11 |
|:-----------|------------:|------------:|------------:|------------:|
| intercept  |   1.0000000 |   1.0000000 |   1.0000000 |   1.0000000 |
| RMSE       | 658.8992539 | 685.1717587 | 627.7581359 | 684.3004401 |
| Rsquared   |   0.7975275 |   0.7846582 |   0.8173184 |   0.7925075 |
| MAE        | 516.9324838 | 532.3416180 | 487.5367634 | 540.9811451 |
| RMSESD     |  98.8737481 |  82.5313917 | 124.2029983 | 116.4721403 |
| RsquaredSD |   0.0764101 |   0.0536655 |   0.0620118 |   0.0930346 |
| MAESD      |  90.8054710 |  90.9884942 |  97.6073726 | 110.5795647 |

Save the model with the lowest RMSE as our second linear regression
model.

``` r
candidates <- list(mlrFit8 = mlrFit8, mlrFit9 = mlrFit9, mlrFit10 = mlrFit10, mlrFit11 = mlrFit11)
indexLowestRMSE <- which.min(c(candidates[[1]][["results"]]["RMSE"], candidates[[2]][["results"]]["RMSE"], candidates[[3]][["results"]]["RMSE"], candidates[[4]][["results"]]["RMSE"]))
mlrFinal2 <- candidates[[1]]
mlrFinal2$call
```

    ## train.formula(form = registered ~ season + yr + mnth + weathersit + 
    ##     atemp + hum + windspeed + I(atemp^2), data = dayTrain, method = "lm", 
    ##     preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv", 
    ##         number = 4, repeats = 3))

The model with the lowest RMSE for Saturday is mlrFit10

### Random Forest Model

Intro to Random Forest …

``` r
rfFit <- train(registered ~ . - instant - casual - cnt, data = dayTrain,
               method = "rf",
               trControl = trainControl(method = "repeatedcv", number = 4, repeats = 3),
               preProcess = c("center", "scale"),
               tuneGrid = expand.grid(mtry = c(2, 7, 10:16, 20, 24)))
rfFit
```

    ## Random Forest 
    ## 
    ## 76 samples
    ## 15 predictors
    ## 
    ## Pre-processing: centered (30), scaled (30) 
    ## Resampling: Cross-Validated (4 fold, repeated 3 times) 
    ## Summary of sample sizes: 57, 58, 56, 57, 57, 57, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared   MAE     
    ##    2    959.8635  0.7675730  783.2876
    ##    7    681.6384  0.7935114  543.6402
    ##   10    676.1309  0.7875707  538.1746
    ##   11    676.1543  0.7856866  537.7880
    ##   12    679.8853  0.7832378  536.9567
    ##   13    679.7218  0.7803604  537.3444
    ##   14    681.2633  0.7789662  538.5076
    ##   15    680.2354  0.7794359  537.0779
    ##   16    681.7500  0.7766418  536.4635
    ##   20    687.4200  0.7713105  537.6429
    ##   24    695.9541  0.7640608  539.5025
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 10.

# Comparison of models

Discussion …

``` r
# declaration of "winner" needs to be automated
mlrFinal2Pred <- predict(mlrFinal2, newdata = dayTest)
rfFitPred <- predict(rfFit, newdata = dayTest)
postResample(mlrFinal2Pred, dayTest$registered)
```

    ##        RMSE    Rsquared         MAE 
    ## 718.9604946   0.7316676 536.7887559

``` r
postResample(rfFitPred, dayTest$registered)
```

    ##        RMSE    Rsquared         MAE 
    ## 619.1994182   0.7600043 465.6051142

I added an initial version of my MLR and random forest models, and wrote
a separate R script (ST558RenderProject2.r) to automate the reports for
each day of the week. There’s still plenty of clean-up to do with the
output, but the automation generally seems to be working. Since the
dataset now includes only one weekday at a time, some of our graphs,
tables, etc. that included weekday don’t make as much sense. I still
have some questions about whether we’re supposed to do any initial data
exploration with the full training set, or if we only work with one day
at a time. I might post a question on the discussion board or go to
Wednesday office hours unless it’s clear to you.
