ST 558 Project 2
================
By David Arthur and James Carr
6/28/2021

-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarizations](#summarizations)
-   [Modeling](#modeling)
    -   [First linear regression model](#first-linear-regression-model)
    -   [Second linear regression
        model](#second-linear-regression-model)
    -   [Random Forest Model](#random-forest-model)
    -   [Boosted Regression Tree](#boosted-regression-tree)
-   [Comparison of models](#comparison-of-models)

# Sunday

# Introduction

The data set this program analyzes can be found
[here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset).
The data describes the count of rental bikes between years 2011 and 2012
in the Capital bikeshare system by a few dimensions:

-   season
-   day of the week
-   year
-   month
-   holiday (y/n flag)
-   working day (y/n flag)
-   weather (good, fair, poor, bad)
-   temperature
-   humidity
-   wind

It is further broken down into three response variables:

-   Casual: non-registered riders who use the service casually
-   Registered: registered riders who use the service more regularly
-   Total: casual and registered combined

The split between casual and registered is important, because they
behave completely differently, use the service on different days, times,
holidays, etc. Often, their behavior is inverse of each other, though
the registered rider group is largest portion of riders and would be the
primary client of the business. Keeping in mind that the registered
client represents the largest portion of the clientele, this program
focuses on the registered metric and splits the behavior by each day of
the week.

This analysis constructs 2 linear regression models, a random forest
model, and a boosted tree model, each designed to predict the number of
registered riders on any given day based on the values of the predictor
variables in the data set.

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

![](Sunday_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

We will now look in more detail at relationships between time-related
variables and the `registered` response variable. When we do our linear
regression modeling we will need to decide which (if any) of these
predictors to use. For example, the date variable (`dteday`) and
`season` may not be useful in the presence of `weekday`, `mnth`, and
`yr` (or vice versa), as they provide largely redundant information.

``` r
g <- ggplot(data = dayTrain)
g + geom_point(aes(x = dteday, y = registered)) +
  labs(title = "Registered riders by date", x = "date", y = "# of registered riders")
```

![](Sunday_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
meanByMonthYr <- dayTrain %>% group_by(mnth, yr) %>%
  summarize(meanReg = mean(registered))
g2 <- ggplot(meanByMonthYr, aes(x = mnth))
g2 + geom_bar(aes(y = meanReg, fill = yr), position = "dodge", stat = "identity") +
  labs(title = "Mean daily registered riders by month, grouped by year", x = "month", y = "Mean daily registered riders", fill = "year")
```

![](Sunday_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

We will look next in more detail at the relationship between
quantitative weather variables and the `registered` response variable.
The appearance of nonlinear relationships in the scatter plots below may
indicate the need for quadratic terms in our linear regression models.
The adjusted temperature variable, `atemp`, seems particularly likely to
require a quadratic term, as both low and high temperatures can
discourage people from bicycling. Similarly, with humidity and
windspeed, low to moderate values may have no effect, but particularly
high values could have an effect, so those variables may also require
quadratic terms.

``` r
g + geom_point(aes(x = atemp, y = registered)) + facet_wrap(~ yr) + 
  labs(x = "adjusted temperature", y = "registered riders")
```

![](Sunday_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
g + geom_point(aes(x = hum, y = registered)) + facet_wrap(~ yr) + 
  labs(x = "humidity", y = "registered riders")
```

![](Sunday_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
g + geom_point(aes(x = windspeed, y = registered)) + facet_wrap(~ yr) + 
  labs(x = "wind speed", y = "registered riders")
```

![](Sunday_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

We now view a table displaying the mean number of `registered`,
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
| mist            |             1111.6 |                 2765.2 |            3876.8 |
| clear           |             1441.1 |                 3044.8 |            4485.8 |
| lightRainOrSnow |              120.0 |                  907.0 |            1027.0 |

Average \# of riders by weather category

``` r
scatter_james <- ggplot(data=dayTrain, aes(x=temp, y=registered)) +
                 geom_point(aes(color=weathersit))
hist_james <- ggplot(data=dayTrain, aes(x=weathersit)) +
              geom_histogram(stat='count', aes(fill=workingday)) +
              ggtitle('Frequency of Weather') +
              xlab('Weather Type') + ylab('Count')

bar_james <- ggplot(data=dayTrain %>% 
                 select(season, casual, registered) %>%
                 pivot_longer(cols=c(casual, registered),
                              names_to = 'metrics',
                              values_to = 'riders') %>%
                 group_by(season, metrics) %>%
                 summarise(avg_riders = mean(riders)), 
            aes(x=season, y=avg_riders, fill=metrics)) +     
            geom_bar(stat='identity', position='dodge') +
            ggtitle('Average Number of Riders') +
              xlab('Season') + ylab('Average # of Riders')
  
box_james <- ggplot(data=dayTrain, aes(x=mnth, y=temp)) +
             geom_boxplot(aes(color=season))
```

Looking at the bar graph below, in all seasons the registered user base
far out-performs the casual base. This further confirms our plan of
analyzing the registered group as the priority.

![](Sunday_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Since we don’t have a domain expert, we need to try to figure out what
variables are important and which we could exclude. We already know that
the two temperature variables have near perfect correlation, and clearly
date is redundant with mnth and yr. I would think season is as well
covered by mnth.

That leaves temperature, weather, and the working day flag as the most
likely to be relevant parameters. Looking at the plots below, I think we
can make a few obvious inferences:

-   looking at the scatter plot on the left, we can see that as the
    temperature goes up, the number of riders also goes up - at least up
    to a point. And even in the highest temperatures, ridership is way
    up over lowest temperatures.
-   the middle figure displays that temperature is highest in spring,
    summer, and early fall
-   looking at the figure on the right, there are very few days of
    extremely poor weather. Most days are clear, which are the best days
    for ridership.

![](Sunday_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
summ_james <- dayTrain %>% rename(total = cnt) %>%
              pivot_longer(cols=c(casual, registered, total),
                           names_to = 'metrics',
                           values_to = 'riders') %>%
              group_by(metrics) %>%
              summarise(min = min(riders),
                        lower25 = quantile(riders, 0.25),
                        median = median(riders),
                        mean = mean(riders),
                        upper75 = quantile(riders, 0.75),
                        max = max(riders))  %>%
              pivot_longer(cols=c(min, lower25, median,
                                  mean, upper75, max),
                           names_to = 'Summary',
                           values_to = 'stats') %>%
              pivot_wider(names_from = metrics, values_from = stats)

kable(summ_james, digits=0)
```

| Summary | casual | registered | total |
|:--------|-------:|-----------:|------:|
| min     |     54 |        491 |   605 |
| lower25 |    633 |       2242 |  2940 |
| median  |   1408 |       2926 |  4396 |
| mean    |   1333 |       2939 |  4272 |
| upper75 |   2093 |       3750 |  5356 |
| max     |   2704 |       5657 |  8227 |

``` r
pct_diff <- round((summ_james$registered[3] / summ_james$casual[3] - 1) 
                  * 100, 0)
pct_str <- paste0(pct_diff, '%')

inc_dec <- ''
if (pct_diff >= 0) {
  inc_dec <- 'greater'
} else {
  inc_dec <- 'less'
}
```

On the day of the week, Sunday, ridership by registered users is greater
than casual users by 108%.

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

![Y\_i = \\beta\_0 + \\beta\_1X\_{i1} + \\beta\_2X\_{i2} + ... + \\beta\_pX\_{ip} + E\_i](https://latex.codecogs.com/png.latex?Y_i%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1X_%7Bi1%7D%20%2B%20%5Cbeta_2X_%7Bi2%7D%20%2B%20...%20%2B%20%5Cbeta_pX_%7Bip%7D%20%2B%20E_i "Y_i = \beta_0 + \beta_1X_{i1} + \beta_2X_{i2} + ... + \beta_pX_{ip} + E_i")

where ![Y\_i](https://latex.codecogs.com/png.latex?Y_i "Y_i") is the
response, ![i](https://latex.codecogs.com/png.latex?i "i") represents
the observation number,
![X\_{ij}](https://latex.codecogs.com/png.latex?X_%7Bij%7D "X_{ij}") are
the predictor variables, and
![E\_i](https://latex.codecogs.com/png.latex?E_i "E_i") is the normally
distributed random error. The
![\\beta\_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j")
coefficients must be linear, but the predictor variables can be higher
order terms
(e.g. ![x^2](https://latex.codecogs.com/png.latex?x%5E2 "x^2")) or
interaction terms
(e.g. ![x\_1x\_2](https://latex.codecogs.com/png.latex?x_1x_2 "x_1x_2")).
Creating a model to estimate the response using observed data, we have

![\\hat{y\_i} = \\hat\\beta\_0 + \\hat\\beta\_1x\_{i1} + \\hat\\beta\_2x\_{i2} + ... + \\hat\\beta\_px\_{ip}](https://latex.codecogs.com/png.latex?%5Chat%7By_i%7D%20%3D%20%5Chat%5Cbeta_0%20%2B%20%5Chat%5Cbeta_1x_%7Bi1%7D%20%2B%20%5Chat%5Cbeta_2x_%7Bi2%7D%20%2B%20...%20%2B%20%5Chat%5Cbeta_px_%7Bip%7D "\hat{y_i} = \hat\beta_0 + \hat\beta_1x_{i1} + \hat\beta_2x_{i2} + ... + \hat\beta_px_{ip}")

The
![\\hat\\beta\_j](https://latex.codecogs.com/png.latex?%5Chat%5Cbeta_j "\hat\beta_j")
coefficients (estimates for
![\\beta\_j](https://latex.codecogs.com/png.latex?%5Cbeta_j "\beta_j"))
are calculated for each predictor variable to minimize the residual sum
of squares, using the observed values of
![x\_{ij}](https://latex.codecogs.com/png.latex?x_%7Bij%7D "x_{ij}") and
![y\_i](https://latex.codecogs.com/png.latex?y_i "y_i")

![min\_{\\beta\_0, \\beta\_1, ..., \\beta\_p}\\sum\_{i=1}^{n}(y\_i - \\beta\_0 - \\beta\_1x\_{i1} - \\beta\_2x\_{i2} - ... - \\beta\_px\_{ip})^2](https://latex.codecogs.com/png.latex?min_%7B%5Cbeta_0%2C%20%5Cbeta_1%2C%20...%2C%20%5Cbeta_p%7D%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%28y_i%20-%20%5Cbeta_0%20-%20%5Cbeta_1x_%7Bi1%7D%20-%20%5Cbeta_2x_%7Bi2%7D%20-%20...%20-%20%5Cbeta_px_%7Bip%7D%29%5E2 "min_{\beta_0, \beta_1, ..., \beta_p}\sum_{i=1}^{n}(y_i - \beta_0 - \beta_1x_{i1} - \beta_2x_{i2} - ... - \beta_px_{ip})^2")

The linear regression model can be used for inference, to understand the
relationships between the predictor variables and the response, as well
as for prediction of a mean response given new values of the predictor
variables. There are varying approaches to choosing which predictor
variables to include in a linear regression model. In both of our
models, our goal is accurate prediction when applied to new data. To
accomplish this we take two different approaches to choosing a subset of
predictor variables, in both cases using a combination of
criterion-based comparison and cross validation.

Criterion-based selection generally involves balancing bias and variance
by adding to the residual sum of squares some penalty that increases
with the number of predictors. This compensates for the fact that
including more predictors will always reduce the RSS for the training
set, but beyond a certain point overfitting becomes a risk, responding
too much to the noise in the training set, and reducing the accuracy of
the model when applied to new data.

Cross validation subdivides the training set into
![k](https://latex.codecogs.com/png.latex?k "k") folds, then fits a
model using
![k - 1](https://latex.codecogs.com/png.latex?k%20-%201 "k - 1") of
those folds, and tests its accuracy predicting on the
![k^{th}](https://latex.codecogs.com/png.latex?k%5E%7Bth%7D "k^{th}")
fold. This is repeated
![k - 1](https://latex.codecogs.com/png.latex?k%20-%201 "k - 1") more
times, so that each fold gets a turn as the test set. The
![k](https://latex.codecogs.com/png.latex?k "k") results (residual sum
of squares, etc.) are then averaged. Cross validation can be performed
on a number of candidate models, and the model with the lowest resulting
mean squared error can be chosen as likely to perform best predicting on
new data.

### First linear regression model

I am starting with a best subsets approach, meaning we will look at all
of the predictors and use cross-validation to choose the one that has
the best prediction capability. Since the training set is only around 80
rows, I opted for four-fold cross validation to leave some data in each
fold.

``` r
library(leaps)

data <- dayTrain %>% 
               drop_na() %>%
               select(-instant,-dteday, -season, -holiday,
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
    
    
    cv_errors[j, i] <- mean((data$registered[folds==j]-pred)^2)
  }
}
```

    ## Reordering variables and trying again:

    ## Error in eval(x): object 'newX' not found

``` r
# Take the mean of over all folds for each model size
mean_cv_errors = apply(cv_errors, 2, mean)

# Find the model size with the smallest cross-validation error
min = which.min(mean_cv_errors)

#the model w/ 14 variables was best when using 4 fold cv.
#i did 4 fold because there are only about 80 rows of data per weekday

if(length(unique(dayTrain$workingday)) == 1){
  lm.fit1 <- lm(registered ~ yr + mnth + weathersit + temp + hum +
               windspeed, data=dayTrain)
}else{
  lm.fit1 <- lm(registered ~ yr + mnth + weathersit + temp + hum +
               windspeed +workingday, data=dayTrain)
}
```

Using best subsets, the following model was obtained: registered \~ yr +
mnth + weathersit + temp + hum + windspeed

### Second linear regression model

In this approach, we start with a full linear regression model that
includes all of the predictor variables. We will then reduce
collinearity (correlation among predictor variables) by removing
redundant predictors until we reach an optimal (lowest) AIC, which is
one of the criteria for predictor subset selection described above. We
will calculate the condition number
(![\\kappa](https://latex.codecogs.com/png.latex?%5Ckappa "\kappa")) for
each of the candidate models, which is a measure of collinearity.
Roughly,
![\\kappa &lt; 30](https://latex.codecogs.com/png.latex?%5Ckappa%20%3C%2030 "\kappa < 30")
is considered desirable. Finally, we will choose among several
variations of the optimal model (including various higher order terms)
using cross validation (described above).

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
    ## -1611.61  -161.48    48.47   237.60   948.65 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               -8.251e+03  1.125e+05  -0.073  0.94179    
    ## dteday                     6.343e-01  7.500e+00   0.085  0.93293    
    ## seasonspring               1.159e+03  4.644e+02   2.496  0.01569 *  
    ## seasonsummer               1.064e+03  4.700e+02   2.265  0.02765 *  
    ## seasonfall                 1.719e+03  4.063e+02   4.232 9.25e-05 ***
    ## yr2012                     1.111e+03  2.768e+03   0.401  0.68970    
    ## mnth2                      2.490e+02  3.671e+02   0.678  0.50062    
    ## mnth3                      5.463e+02  5.578e+02   0.979  0.33182    
    ## mnth4                     -1.884e+02  9.172e+02  -0.205  0.83806    
    ## mnth5                      2.984e+02  1.095e+03   0.272  0.78636    
    ## mnth6                      3.131e+02  1.281e+03   0.244  0.80779    
    ## mnth7                     -1.657e+02  1.512e+03  -0.110  0.91315    
    ## mnth8                     -6.291e+01  1.720e+03  -0.037  0.97096    
    ## mnth9                      6.828e+02  1.893e+03   0.361  0.71977    
    ## mnth10                    -7.126e+01  2.121e+03  -0.034  0.97333    
    ## mnth11                    -5.355e+02  2.367e+03  -0.226  0.82192    
    ## mnth12                    -8.072e+02  2.564e+03  -0.315  0.75415    
    ## weathersitclear            5.758e+01  1.995e+02   0.289  0.77400    
    ## weathersitlightRainOrSnow -1.946e+03  5.801e+02  -3.354  0.00148 ** 
    ## temp                       1.339e+03  5.435e+03   0.246  0.80642    
    ## atemp                      5.987e+02  5.700e+03   0.105  0.91674    
    ## hum                       -1.189e+03  7.040e+02  -1.688  0.09723 .  
    ## windspeed                 -1.598e+03  9.472e+02  -1.688  0.09738 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 484.7 on 53 degrees of freedom
    ## Multiple R-squared:  0.8863, Adjusted R-squared:  0.8391 
    ## F-statistic: 18.78 on 22 and 53 DF,  p-value: < 2.2e-16

``` r
AIC(mlrFull)
```

    ## [1] 1176.186

``` r
x <- model.matrix(mlrFull)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition number
kappa <- sqrt(e$val[1]/min(e$val))
```

We see that
![\\kappa](https://latex.codecogs.com/png.latex?%5Ckappa "\kappa") =
2.0828104^{6}, which is a sign of high collinearity, so we try removing
some of the insignificant predictors, checking to confirm that AIC
declines, or at least that it increases only marginally.

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

![](Sunday_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

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
    ## -1613.94  -169.43    50.24   237.51   932.62 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1234.829    608.372   2.030 0.047233 *  
    ## seasonspring               1128.351    443.304   2.545 0.013752 *  
    ## seasonsummer               1052.891    459.979   2.289 0.025943 *  
    ## seasonfall                 1704.944    395.102   4.315 6.69e-05 ***
    ## yr2012                     1343.888    117.391  11.448 3.53e-16 ***
    ## mnth2                       274.789    283.436   0.969 0.336543    
    ## mnth3                       585.913    320.396   1.829 0.072867 .  
    ## mnth4                       -94.644    550.928  -0.172 0.864234    
    ## mnth5                       426.048    595.768   0.715 0.477557    
    ## mnth6                       480.738    604.401   0.795 0.429802    
    ## mnth7                         9.742    650.491   0.015 0.988105    
    ## mnth8                       128.435    600.973   0.214 0.831562    
    ## mnth9                       886.057    542.729   1.633 0.108267    
    ## mnth10                      118.230    511.926   0.231 0.818210    
    ## mnth11                     -331.082    477.311  -0.694 0.490827    
    ## mnth12                     -595.928    369.250  -1.614 0.112275    
    ## weathersitclear              52.247    195.060   0.268 0.789814    
    ## weathersitlightRainOrSnow -1960.389    563.790  -3.477 0.000998 ***
    ## atemp                      1979.763   1048.527   1.888 0.064287 .  
    ## hum                       -1194.828    686.516  -1.740 0.087378 .  
    ## windspeed                 -1503.374    862.654  -1.743 0.086970 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 476.2 on 55 degrees of freedom
    ## Multiple R-squared:  0.8861, Adjusted R-squared:  0.8447 
    ## F-statistic:  21.4 on 20 and 55 DF,  p-value: < 2.2e-16

``` r
AIC(mlr2)
```

    ## [1] 1172.31

``` r
x <- model.matrix(mlr2)[, -1]
e <- eigen(t(x) %*% x)
# e$val
# condition number
kappa <- sqrt(e$val[1]/min(e$val))
```

We see that AIC has changed little, and that
![\\kappa](https://latex.codecogs.com/png.latex?%5Ckappa "\kappa") =
40.24, which indicates a large reduction in collinearity.

`mnth`, `weathersit` and `windspeed` appear to be marginally
significant, so we look at the effect of removing each of them from the
model:  
Remove `mnth`

``` r
mlr3 <- update(mlr2, . ~ . - mnth)
# summary(mlr3)
AIC(mlr3)
```

    ## [1] 1187.78

Remove `weathersit`

``` r
mlr4 <- update(mlr2, . ~ . - weathersit)
# summary(mlr4)
AIC(mlr4)
```

    ## [1] 1183.458

Remove `windspeed`

``` r
mlr5 <- update(mlr2, . ~ . - windspeed)
# summary(mlr5)
AIC(mlr5)
```

    ## [1] 1174.395

For `mnth`, `weathersit`, and `windspeed`, removal from the model
results in an increase or marginal decrease in AIC. If our main goal
were inference and understanding the relationships between the
variables, we might want to remove them from the model for the sake of
simplicity, interpretability, and more narrow confidence intervals.
Because our primary goal here is prediction, we will leave them in the
model, and choose mlr2 as our base linear regression model.

We will now do some diagnostic plots on our base model, and then
consider adding higher order terms to the model.

We can check for constant variance of our error term, an assumption of
our model, by looking at a plot of the model’s fitted values vs the
residuals (difference between fitted response and observed response). A
“megaphone” shape can indicate non-constant variance.

``` r
g <- ggplot(mlr2)
g + geom_point(aes(x = .fitted, y = .resid)) + labs (title = "Fitted values vs residuals (difference between predicted and actual value)", x = "fitted (predicted) values", y = "residuals")
```

![](Sunday_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Another way to assess constant variance is with the Box-Cox method,
which can suggest transformations of the response to address problems
with non-constant variance. If the maximum log-likelihood of
![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda "\lambda")
close to 1, as in this case, indicates that non-constant variance is not
a problem with the existing model.

``` r
MASS::boxcox(mlr2)
```

![](Sunday_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

We will also look at for signs of nonlinearity, which can indicate the
need for quadratic terms for some of the predictors. The partial
residual plots below plot the relationship between each predictor and
the response, with the effect of the other predictors removed.

``` r
termplot(mlr2, partial.resid = TRUE, terms = c("atemp", "windspeed", "hum"))
```

![](Sunday_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](Sunday_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](Sunday_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

For at least some days of the week there is a nonlinear pattern to the
plots, particularly for `atemp`, so we will try adding quadratic terms
for each of them to our base model.

Try adding
![atemp^2](https://latex.codecogs.com/png.latex?atemp%5E2 "atemp^2") to
base model

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
    ## -1512.30  -195.46    12.61   211.16   870.16 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 311.10     696.60   0.447 0.656954    
    ## seasonspring               1148.84     424.96   2.703 0.009158 ** 
    ## seasonsummer               1189.21     444.43   2.676 0.009849 ** 
    ## seasonfall                 1706.25     378.68   4.506 3.58e-05 ***
    ## yr2012                     1321.55     112.89  11.707  < 2e-16 ***
    ## mnth2                       180.57     274.42   0.658 0.513329    
    ## mnth3                       337.14     323.78   1.041 0.302388    
    ## mnth4                      -323.91     536.43  -0.604 0.548492    
    ## mnth5                       365.24     571.55   0.639 0.525511    
    ## mnth6                       587.28     580.94   1.011 0.316573    
    ## mnth7                       374.68     641.38   0.584 0.561535    
    ## mnth8                       209.39     576.96   0.363 0.718081    
    ## mnth9                       825.00     520.78   1.584 0.118998    
    ## mnth10                      -85.93     497.83  -0.173 0.863599    
    ## mnth11                     -574.86     468.40  -1.227 0.225036    
    ## mnth12                     -759.42     360.27  -2.108 0.039696 *  
    ## weathersitclear             137.46     190.23   0.723 0.473041    
    ## weathersitlightRainOrSnow -2005.21     540.67  -3.709 0.000493 ***
    ## atemp                      7378.05    2443.56   3.019 0.003862 ** 
    ## hum                       -1135.20     658.44  -1.724 0.090415 .  
    ## windspeed                 -1183.03     837.30  -1.413 0.163418    
    ## I(atemp^2)                -7007.77    2891.42  -2.424 0.018746 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 456.4 on 54 degrees of freedom
    ## Multiple R-squared:  0.8973, Adjusted R-squared:  0.8574 
    ## F-statistic: 22.47 on 21 and 54 DF,  p-value: < 2.2e-16

``` r
AIC(mlr8)
```

    ## [1] 1166.462

Reduced or similar AIC, so keep mlr8 as a candidate model to compare
using cross validation.

Try adding
![atemp^2](https://latex.codecogs.com/png.latex?atemp%5E2 "atemp^2") and
![hum^2](https://latex.codecogs.com/png.latex?hum%5E2 "hum^2") to base
model

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
    ## -1446.18  -221.02    23.59   221.06   831.36 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -1535.7     1650.3  -0.931  0.35632    
    ## seasonspring                1181.1      423.7   2.787  0.00736 ** 
    ## seasonsummer                1268.5      447.0   2.838  0.00642 ** 
    ## seasonfall                  1888.2      404.7   4.665 2.13e-05 ***
    ## yr2012                      1309.8      112.7  11.617 3.44e-16 ***
    ## mnth2                        217.4      274.7   0.791  0.43231    
    ## mnth3                        496.4      347.1   1.430  0.15862    
    ## mnth4                       -260.1      536.4  -0.485  0.62969    
    ## mnth5                        369.5      568.8   0.650  0.51872    
    ## mnth6                        616.1      578.6   1.065  0.29181    
    ## mnth7                        322.7      639.7   0.504  0.61604    
    ## mnth8                        204.8      574.2   0.357  0.72274    
    ## mnth9                        799.5      518.7   1.541  0.12917    
    ## mnth10                      -244.1      511.8  -0.477  0.63537    
    ## mnth11                      -741.8      485.4  -1.528  0.13241    
    ## mnth12                      -776.6      358.8  -2.164  0.03496 *  
    ## weathersitclear              117.0      190.0   0.615  0.54094    
    ## weathersitlightRainOrSnow  -2009.9      538.1  -3.735  0.00046 ***
    ## atemp                       6586.8     2515.1   2.619  0.01148 *  
    ## hum                         5314.6     5271.4   1.008  0.31794    
    ## windspeed                   -931.2      858.0  -1.085  0.28269    
    ## I(atemp^2)                 -6389.2     2921.0  -2.187  0.03315 *  
    ## I(hum^2)                   -5005.9     4059.5  -1.233  0.22297    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 454.2 on 53 degrees of freedom
    ## Multiple R-squared:  0.9002, Adjusted R-squared:  0.8587 
    ## F-statistic: 21.72 on 22 and 53 DF,  p-value: < 2.2e-16

``` r
AIC(mlr9)
```

    ## [1] 1166.313

Similar AIC for most days of week, so keep mlr9 as a candidate model to
compare using cross validation.

Try adding
![atemp^2](https://latex.codecogs.com/png.latex?atemp%5E2 "atemp^2") and
![windspeed^2](https://latex.codecogs.com/png.latex?windspeed%5E2 "windspeed^2")
to base model

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
    ## -1543.19  -220.36    22.04   223.39   877.11 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 171.273    726.090   0.236 0.814432    
    ## seasonspring               1102.634    431.646   2.554 0.013544 *  
    ## seasonsummer               1171.303    447.112   2.620 0.011454 *  
    ## seasonfall                 1722.436    381.037   4.520 3.50e-05 ***
    ## yr2012                     1317.702    113.519  11.608 3.55e-16 ***
    ## mnth2                       225.416    282.579   0.798 0.428599    
    ## mnth3                       418.083    344.055   1.215 0.229689    
    ## mnth4                      -205.920    563.134  -0.366 0.716069    
    ## mnth5                       503.032    605.084   0.831 0.409510    
    ## mnth6                       716.606    610.486   1.174 0.245712    
    ## mnth7                       454.851    653.772   0.696 0.489635    
    ## mnth8                       307.923    595.435   0.517 0.607210    
    ## mnth9                       937.511    545.888   1.717 0.091744 .  
    ## mnth10                        9.532    517.284   0.018 0.985368    
    ## mnth11                     -530.375    474.522  -1.118 0.268734    
    ## mnth12                     -690.360    374.345  -1.844 0.070750 .  
    ## weathersitclear             136.010    191.091   0.712 0.479737    
    ## weathersitlightRainOrSnow -1950.936    548.283  -3.558 0.000797 ***
    ## atemp                      6722.709   2617.359   2.569 0.013067 *  
    ## hum                       -1100.361    663.149  -1.659 0.102963    
    ## windspeed                  1180.848   3384.536   0.349 0.728550    
    ## I(atemp^2)                -6447.483   3006.505  -2.145 0.036596 *  
    ## I(windspeed^2)            -5523.472   7660.302  -0.721 0.474047    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 458.5 on 53 degrees of freedom
    ## Multiple R-squared:  0.8983, Adjusted R-squared:  0.8561 
    ## F-statistic: 21.28 on 22 and 53 DF,  p-value: < 2.2e-16

``` r
AIC(mlr10)
```

    ## [1] 1167.72

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
    ##      Min       1Q   Median       3Q      Max 
    ## -1478.38  -179.34    50.08   191.01   835.02 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               -2002.71    1719.34  -1.165 0.249411    
    ## seasonspring               1123.00     428.12   2.623 0.011408 *  
    ## seasonsummer               1255.97     447.36   2.808 0.007013 ** 
    ## seasonfall                 1937.52     408.08   4.748 1.65e-05 ***
    ## yr2012                     1302.84     113.03  11.526 6.15e-16 ***
    ## mnth2                       284.02     283.25   1.003 0.320654    
    ## mnth3                       630.51     373.62   1.688 0.097487 .  
    ## mnth4                       -89.79     564.41  -0.159 0.874213    
    ## mnth5                       557.96     601.09   0.928 0.357570    
    ## mnth6                       796.67     607.88   1.311 0.195762    
    ## mnth7                       424.16     648.43   0.654 0.515907    
    ## mnth8                       338.40     590.64   0.573 0.569150    
    ## mnth9                       949.04     541.18   1.754 0.085383 .  
    ## mnth10                     -137.67     523.55  -0.263 0.793619    
    ## mnth11                     -706.20     487.03  -1.450 0.153061    
    ## mnth12                     -685.10     371.09  -1.846 0.070563 .  
    ## weathersitclear             111.91     190.21   0.588 0.558862    
    ## weathersitlightRainOrSnow -1936.61     543.59  -3.563 0.000796 ***
    ## atemp                      5575.34    2722.22   2.048 0.045617 *  
    ## hum                        6327.87    5375.60   1.177 0.244497    
    ## windspeed                  2327.81    3454.62   0.674 0.503407    
    ## I(atemp^2)                -5533.05    3051.75  -1.813 0.075594 .  
    ## I(hum^2)                  -5755.41    4133.77  -1.392 0.169760    
    ## I(windspeed^2)            -7526.83    7728.50  -0.974 0.334612    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 454.5 on 52 degrees of freedom
    ## Multiple R-squared:  0.902,  Adjusted R-squared:  0.8586 
    ## F-statistic:  20.8 on 23 and 52 DF,  p-value: < 2.2e-16

``` r
AIC(mlr11)
```

    ## [1] 1166.939

Similar AIC for most days of week, so keep mlr11 as a candidate model to
compare using cross validation.

We will now compare the 4 candidate models using cross validation.
Several measures of the performance of the model are returned. We will
choose the best model in terms of lowest Root Mean Squared Error.

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
| RMSE       | 613.8765681 | 630.8308743 | 633.6088713 | 621.2770931 |
| Rsquared   |   0.7600009 |   0.7313525 |   0.7376393 |   0.7513900 |
| MAE        | 451.2631118 | 465.9069769 | 467.3882468 | 477.7484900 |
| RMSESD     | 122.2709192 | 107.9614900 | 142.1090332 | 169.1847648 |
| RsquaredSD |   0.0967079 |   0.1210146 |   0.1087482 |   0.1147708 |
| MAESD      |  84.2157565 |  71.1511847 | 111.0904818 | 124.5141579 |

Save the model with the lowest RMSE as our second linear regression
model.

``` r
candidates <- list(mlrFit8 = mlrFit8, mlrFit9 = mlrFit9, mlrFit10 = mlrFit10, mlrFit11 = mlrFit11)
indexLowestRMSE <- which.min(c(candidates[[1]][["results"]]["RMSE"], candidates[[2]][["results"]]["RMSE"], candidates[[3]][["results"]]["RMSE"], candidates[[4]][["results"]]["RMSE"]))
mlrFinal2 <- candidates[[indexLowestRMSE]]
mlrFinal2$call[[2]]
```

    ## registered ~ season + yr + mnth + weathersit + atemp + hum + 
    ##     windspeed + I(atemp^2)

The model with the lowest RMSE for Sunday is mlrFit8, with a formula of
registered \~ season + yr + mnth + weathersit + atemp + hum + windspeed
+ I(atemp^2)

### Random Forest Model

The random forest model is an improvement on the bagged tree model,
which is an improvement on the basic decision tree model. Decision trees
make predictions by dividing the predictor space into a number of
regions, and determining which region the predictor values of the new
observation fall into by applying a series of splits, each based on the
value of a single predictor. The response for the new observation is
then predicted to be the mean of the responses of the observations in
that region (for regression models; for classification models, the
prediction is the predominant class observed in the region). First a
large tree is grown, with the goal of minimizing the residual sum of
squares, resulting in a tree with many regions, each containing a small
number of observations. But this complex tree will generally be overfit,
with low bias and high variance, so it gets pruned back to an optimal
size, determined by cross validation, that will have higher bias but
lower variance, and ideally perform better when predicting on new data.

Bagged tree models improve on basic decision trees by using the
bootstrap to take many samples from the training data set and producing
an unpruned tree from each sample, then averaging the predictions of
those trees to get the bagged tree model. The averaging of hundreds of
high-variance trees results in a much lower variance model.

The random forest model is a further improvement on the bagged tree,
which works by decorrelating the trees that are generated and averaged
together. In a bagged tree model, many of the trees can end up being
similar, with the main splits dominated by the strongest predictor(s).
The correlation between these trees means that averaging them results in
a smaller reduction in variance than desired. To remedy this, random
forest models consider only a random subset of predictors for each
split, resulting in less correlation between trees, and lower variance
in the final model. The number of predictors considered for each split
is a tuning parameter, whose value can be chosen using cross validation.
…

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
    ##    2    844.3006  0.7126450  685.7087
    ##    7    591.8746  0.7942838  470.8939
    ##   10    559.3407  0.8082543  440.8554
    ##   11    561.7518  0.8013550  438.2368
    ##   12    557.4349  0.8039915  432.9133
    ##   13    550.1264  0.8082367  424.7765
    ##   14    552.2727  0.8050016  426.6464
    ##   15    553.1003  0.8048563  426.6679
    ##   16    556.0060  0.7995008  425.8662
    ##   20    554.5109  0.7997206  421.4100
    ##   24    565.3686  0.7874888  424.1280
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 13.

### Boosted Regression Tree

A boosted regression tree is similar to other tree-based regression
models, in that the regression is based on decision trees. Like other
decision-tree models, random samples are taken to build iterations of
the tree, but what makes boosted trees unique is that each iteration
attempts to build and improve on the last version and predictions are
updated as the trees are grown.

The predictions are initialized to 0 and residuals are created. A tree
is then fit to those residuals as a response. The predictions are then
updated again, the response becomes the new residuals, and the process
continues until a pre-determined number of **B** times has been reached.

There are four parameters that can be chosen using cross-validation to
create the best fit:

-   the shrinkage parameter: also known as the learning rate, this
    parameter determines how quickly the model learns. This parameter is
    between 0 and 1, and when tuning, 0.1 is usually a good place to
    start.
-   number of trees: this can lead to over-fitting if the number of
    trees is too large.
-   interaction depth: an interaction depth of 1 would be an additive
    model, while 2 would have two-way interactions. It is generally
    recommended to look at a interaction depth of something near 2 to 8.
-   min observations per node: default is 10, but this number can be
    dropped especially if data is sparse.

``` r
n.trees <- seq(5, 100, 5)
int.depth <- 1:10
shrinkage <- seq(0.05, 0.2, 0.05)
minobs <- seq(2, 12, 2)
grid <- expand.grid(n.trees = n.trees, 
                    interaction.depth = int.depth, 
                    shrinkage = shrinkage, 
                    n.minobsinnode = minobs)

trControl <- trainControl(method='repeatedcv', number=4, repeats=10)
set.seed(1)
fit_boost <- train(registered ~ ., 
                   data=dayTrain %>% 
                        drop_na() %>%
                        select(-instant, -casual, -cnt),
                   method='gbm',
                   tuneGrid = grid,
                   trControl=trControl, 
                   verbose=FALSE)
```

# Comparison of models

We will now compare the performance of the two linear regression models,
the random forest model, and the boosted tree model, by using each to
predict the `registered` response based on the values of the predictor
variables in the test data set that we partitioned at the beginning. We
will choose the best model on the basis of lowest Root Mean Squared
Error.

``` r
final4 <- list(first_linear_regression = lm.fit1, second_linear_regression = mlrFinal2, random_forest = rfFit, boosted_tree = fit_boost)
rmse <- numeric()
results <- list()
predFinal4 <- predict(final4, newdata = dayTest)
for(i in 1:length(final4)){
  results[[i]] <- postResample(predFinal4[[i]], dayTest$registered)
  rmse[i] <- postResample(predFinal4[[i]], dayTest$registered)["RMSE"]
}
resultsComparison <- data.frame(results)
colnames(resultsComparison) <- names(final4)
kable(t(resultsComparison), digits = 3)
```

|                            |    RMSE | Rsquared |     MAE |
|:---------------------------|--------:|---------:|--------:|
| first\_linear\_regression  | 705.928 |    0.670 | 525.142 |
| second\_linear\_regression | 585.750 |    0.763 | 445.043 |
| random\_forest             | 568.756 |    0.741 | 393.496 |
| boosted\_tree              | 581.685 |    0.752 | 391.842 |

``` r
winnerIndex <- which.min(rmse)
```

The best-performing model for Sunday is random\_forest
