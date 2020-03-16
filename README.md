Spencer Retcher
[sretcher.github.io](https://github.com/sretcher/sretcher.github.io)
# One Sample Means Z-Test

In Aquatic Biology (Vol. 9. 2010), a study was conducted on the green sea turtles inhabiting the Sound Lagoon of Grand Cayman. The carapace length (cm) was recorded from a sample of 76 green sea turtles. We will use a z-test to make an inference about our population mean (true mean carapace length of all green sea turtles in Sound Lagoon) using sample data. 

The following code loads the data, removes irrelevant columns, and gives us a brief summary of the data.


```
library(tidyverse)

turtle <- read_csv("TURTLES.csv") %>%
          select(Length)

head(turtle)

## # A tibble: 6 x 1
##   Length
##    <dbl>
## 1   34.0
## 2   30.4
## 3   32.6
## 4   31.5
## 5   36.5
## 6   35.5

summary(turtle)

##      Length     
##  Min.   :30.37  
##  1st Qu.:49.50  
##  Median :56.78  
##  Mean   :55.47  
##  3rd Qu.:64.60  
##  Max.   :81.63

str(turtle)

## Classes ‘spec_tbl_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	76 obs. of  1 variable:
## $ Length: num  34 30.4 32.6 31.5 36.5 ...
## - attr(*, "spec")=
##  .. cols(
##  ..   Range = col_character(),
##  ..   Captures = col_double(),
##  ..   Length = col_double()
##  .. )
```

As with most parametric tests, its important to check that we've fulfilled the assumptions so that our inference about our population mean is valid.

## Conditions Required For Z-Test
1. A random sample is selected from the target population.
2. The population is normally distributed.
3. The population's standard deviation is known. 


Looking back at the study conducted, a random sample of 76 was taken from the population. To test if the population is normally distributed, we will use a histogram and a normal probability plot. 
```
ggplot(mapping=aes(x=Length),data=turtle) +
  geom_histogram()

qqnorm(turtle$Length)
qqline(turtle$Length,col="blue",lwd=1)
```

![histogram](histogram)
![histogram](qqplot)

Examining the histogram and the normal probability plot, we see a slight departure from normality. The data appears to be left skewed and has more data located at the tails instead of the center of distribution. 

Lucky for us, the z-test is robust against violations of normality. Since our sample size is greater than 30, by the Central Limit Theorem, our sampling distribution of x-bar will be approximately normal regardless of the population distribution. Since x-bar is a point estimater for a population mean and we know the sampling distribution of x-bar is normal, we can still make inferences about our population mean using the standard normal distribution. 

The last assumption entails that we must know the population standard deviation. In most cases, we don't know what it is. Since our sample size is large, we will use our sample standard deviation which will provide a good approximation for this value.


## Setting Up Hypothesis Tests
We are interested in making a statement about the numerical value of our population mean. Suppose we wanted to test if the true mean carapace length of the turtle was greater than 60.0 cm. Our null/alternative hypothesis would be

Ho: u = 60.0

Ha: u > 60.0

where u is our population parameter.

## Programming the Z-Test

Sadly, there is no implementation of z-tests in base R so we will have to create the test ourselves. Each function does something specific--one computes our z_scores, one computes the confidence inteval, and the others compute the z-test depending on the direction of the alternative hypothesis.   







## Z-Score Function
The function takes in a sample mean (x_bar), uo (the value specified in the null hypothesis), sigma(either population or sample standard deviation) and n (sample size). 

The function computes the z_score, which we will use as our test statistic. This test statistic will allow us to decide between the null and alternative hypothesis. The z statistic is the distance between the sample mean and u0 in standard deviation units. 
```
z_score <- function(x_bar,uo,sigma,n){
  se <- sigma/sqrt(n)
  (x_bar-uo)/se
}
```
  
  
## Confidence Interval for u, Based on a Normal (z) Statistic

The function below computes the confidence interval using sample mean (x_bar), the direction of the alternative hypothesis (alternative = less, greater, or two.sided), sigma (either population or sample standard deviation), confidence level (as decimal) and n (sample size). 

Doing a one sided test will result in a one sided confidence interval, since we are only interested that the population mean is less than or greater than a value. For a two sided test, we must take in account that the area of the critical region is divided among both tails. For example, a two sided 95% confidence inteval requires a quartile corresponding to z.975--qnorm(.975)--1.96

```
z_ci <- function(x_bar, alternative, conf.level, sigma, n) {
  
  if (alternative == "two.sided") conf.level= conf.level + ((1-conf.level)/2)
  
  se <- sigma / sqrt(n)
  conf = c(x_bar - qnorm(conf.level) * se, x_bar + qnorm(conf.level) * se)
  
  switch(alternative,
         "less" = (conf[2]=Inf),
         "greater" = (conf[1]=-Inf))
  
  return(conf)
}
```

## Main Z-Test Function

The Z_test function takes in z (our test statistic we can compute from `z_score`), the direction specified in the alternative hypothesis (alternative = less, greater, or two.sided), and the confidence level (conf.level as decimal).

Depending on the direction specified in the alternative hypothesis, the calculation of the z-test is performed differently.  This is why I created a seperate z-test function for each value of alternative. For example, the function `z_test_less` performs a lower tail z-test where we want to see if the population mean is less than a value.

Depending on the value of alternative, the main z_test simply redirects to the corresponding function.

```
z_test <- function(z,alternative,conf.level){
  
  switch(alternative,
         "less" = z_test_less(z, conf.level),
         "greater" = z_test_greater(z, conf.level),
         "two.sided" = z_test_two.sided(z, conf.level))
}
```


## Lower Tail Z-Test

This function performs a lower tail z-test. If the z statistic < -criticial value, we can reject the null hypothesis. We can find the p-value by finding probability of getting a test statistic less than our z statistic.
```
z_test_less <- function(z,conf.level){
  
    if (z < (qnorm(conf.level) * -1)) {
      cat("Reject Null Hypothesis", z, "<", qnorm(conf.level)*-1, "\n p value:", pnorm(z,lower.tail = TRUE))
    }
    else {
      cat("Do Not Reject Null Hypothesis", z, ">", qnorm(conf.level)*-1, "\n p value:", pnorm(z,lower.tail = TRUE))
    }
}
```


## Upper Tail Z-Test

This function performs a upper tail z-test. If the z statistic > criticial value, we can reject the null hypothesis. We can find the p-value by finding probability of getting a test statistic greater than our z statistic.


z_test_greater <- function(z,conf.level){
  
    if (z > qnorm(conf.level)) {
      cat("Reject Null Hypothesis", z, ">", qnorm(conf.level), "\n p value:", pnorm(z,lower.tail = FALSE))
    }
    else {
      cat("Do Not Reject Null Hypothesis", z, "<", qnorm(conf.level), "\n p value:", pnorm(z,lower.tail = FALSE))
    }
  
}


## Two-Tail Z-Test

This function performs a two tailed tail z-test. If the z statistic > absolute value of the criticial value, we can reject the null hypothesis. The p-value of a two-tailed test is either 2*P(z>zc) or 2*P(z<zc) depending on is our z is positive or negative. Since this is a two-tailed test, we must take in account that the area of the critical region is divided among both tails.
```
z_test_two.sided <- function(z,conf.level) {
  
  conf.level= conf.level + ((1-conf.level)/2)
  
  if ((abs(z) > (qnorm(conf.level)) && z <= 0)) {
    cat("Reject Null Hypothesis", abs(z), ">", qnorm(conf.level), "\n p value:", 2 * pnorm(z,lower.tail = TRUE))
  }
  else if ((abs(z) > (qnorm(conf.level)) && z >= 0)) {
    cat("Reject Null Hypothesis", abs(z), ">", qnorm(conf.level), "\n p value:", 2 * pnorm(z,lower.tail = FALSE))
  }
  else {
    if (z <= 0) {
      cat("Do Not Reject Null Hypothesis", abs(z), "<", qnorm(conf.level), "\n p value:", 2 * pnorm(z,lower.tail = TRUE))
    }
    else {
      cat("Do Not Reject Null Hypothesis", abs(z), "<", qnorm(conf.level), "\n p value:", 2 * pnorm(z,lower.tail = FALSE))
    }
  }
  
}
```

## Analysis

In order to calculate our z statistic, z-test, and confidence interval, we need to calculate some sample statistics. Here we calculate the sample mean, sample standard deviation and sample size. Since we want to test if u = 60 u > 60, uo = 60, which represents the null hypothesis value. 

```
x_bar = mean(turtle$Length, na.rm = TRUE)
## 55.47224
s = sd(turtle$Length, na.rm = TRUE)
## 11.33905
n = length(turtle$Length)
## 76
uo = 60
```

Next we calculate our z-statistic
```
z <- z_score(x_bar,60,s,n)
## -3.481077
```

Using a level of significance of 0.05, we can test if our population mean is above 60 with the following function

```
z_test(z,"greater",.95)

## Do Not Reject Null Hypothesis -3.481077 < 1.644854 
## p value: 0.9997503

```
We interpret the p-value as the probability of observing a test statistic greater than -3.481007 is 99.97503% when the true value of u is 60. Since 0.9997503 is more than 0.05 and the z value is less than the critcial value 1.644854, we do not have enough evidence to prove that the true mean carapce length of green sea turtles in Grand Cayman's Sound Lagoon is above 60 cm. 

If we wanted to estimate the true value of u, we can form a 95% confidence interval
```
z_ci(x_bar,"greater",.95,s,n)
## -Inf 57.61166

# two tailed ci

z_ci(x_bar,"two.sided",.95,s,n)
## 52.92295 58.02152
```
We interpret this as we are 95% confident that highest value u could be is 57.61166 cms. Using a two tailed confidence interval, we are 95% confident that the true value of u is in the interval (52.92295,58.02152). 

What if we wanted to test if u is less than 54 cm?
```
 z <- z_score(x_bar,54,s,n)
 ## 1.131899
 z_test(z,"less",.95)
 ## Do Not Reject Null Hypothesis 1.131899 > -1.644854 
 ## p value: 0.8711615
 z_ci(x_bar,"less",.95,s,n)
 ## 53.33281      Inf
 
 ```
 Looking above, we do not have enough evidence to prove that the true mean value u is below 54 cm. Another way we can think about this is that there is not enoguh evidence to prove that u is not 54. Checking the one/two tailed confidence level, we do see that 54 is within the confidence interval which makes sense. 
 
 
 That's it! I've included some more tests below if you're a curious one
 
 ```
 # Ho: u = 49 Ha: u > 49
 
 z <- z_score(x_bar,49,s,n)
 z_test(z,"greater",.95)
 ## Reject Null Hypothesis 4.976046 > 1.644854 
 ## p value: 3.244813e-07
 
 
 # Ho: u = 58 Ha: u < 58

 z <- z_score(x_bar,58,s,n)
 z_test(z,"less",.95)
 ## Reject Null Hypothesis -1.943419 < -1.644854 
 ## p value: 0.0259828
 
 
 # Ho: u = 56 Ha: u != 56
 
 z <- z_score(x_bar,56,s,n)
 ## -0.4057598
 
 z_test(z,"two.sided",.95)
 ## Do Not Reject Null Hypothesis 0.4057598 < 1.959964 
 ## p value: 0.6849191
 
 
 # CI
 z_ci(x_bar,"two.sided",.95,s,n)
 ## 52.92295 58.02152
 
 z_ci(x_bar,"two.sided",.99,s,n)
 ## 52.12191 58.82256
   
   
 # Ho: u = 52.2 Ha: u != 52.2
 
 z <- z_score(x_bar,52.2,s,n)
 z_test(z,"two.sided",.99)
 ## Do Not Reject Null Hypothesis 2.515792 < 2.575829 
 ## p value: 0.01187653
 z_test(z,"two.sided",.95)
 ## Reject Null Hypothesis 2.515792 > 1.959964 
 ## p value: 0.01187653
 ```

