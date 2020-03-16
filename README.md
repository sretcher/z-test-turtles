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

## Preparing the data

x_bar = mean(turtle$Length, na.rm = TRUE)
s = sd(turtle$Length, na.rm = TRUE)
n = length(turtle$Length)
uo = 60




z_score <- function(x_bar,uo,sigma,n){
  se <- sigma/sqrt(n)
  (x_bar-uo)/se
}

  
z <- z_score(x_bar,60,s,n)
  


z_ci <- function(x_bar, alternative, conf.level, sigma, n) {
  
  if (alternative == "two.sided") conf.level= conf.level + ((1-conf.level)/2)
  
  se <- sigma / sqrt(n)
  conf = c(x_bar - qnorm(conf.level) * se, x_bar + qnorm(conf.level) * se)
  
  switch(alternative,
         "less" = (conf[2]=Inf),
         "greater" = (conf[1]=-Inf))
  
  return(conf)
}


  
  
z_test <- function(z,alternative,conf.level){
  
  switch(alternative,
         "less" = z_test_less(z, conf.level),
         "greater" = z_test_greater(z, conf.level),
         "two.sided" = z_test_two.sided(z, conf.level))
}





z_test_less <- function(z,conf.level){
  
    if (z < (qnorm(conf.level) * -1)) {
      cat("Reject Null Hypothesis", z, "<", qnorm(conf.level)*-1, "\n p value:", pnorm(z,lower.tail = TRUE))
    }
    else {
      cat("Do Not Reject Null Hypothesis", z, ">", qnorm(conf.level)*-1, "\n p value:", pnorm(z,lower.tail = TRUE))
    }
}


z_test_greater <- function(z,conf.level){
  
    if (z > qnorm(conf.level)) {
      cat("Reject Null Hypothesis", z, ">", qnorm(conf.level), "\n p value:", pnorm(z,lower.tail = FALSE))
    }
    else {
      cat("Do Not Reject Null Hypothesis", z, "<", qnorm(conf.level), "\n p value:", pnorm(z,lower.tail = FALSE))
    }
  
}


z_test_two.sided <- function(z,conf.level) {
  
  conf.level= conf.level + ((1-conf.level)/2)
  
  if ((abs(z) > (qnorm(conf.level)) && z <= 0)) {
    cat("Reject Null Hypothesis", abs(z), ">", qnorm(conf.level), "\n pvalue:", 2 * pnorm(z,lower.tail = TRUE))
  }
  else if ((abs(z) > (qnorm(conf.level)) && z >= 0)) {
    cat("Reject Null Hypothesis", abs(z), ">", qnorm(conf.level), "\n pvalue:", 2 * pnorm(z,lower.tail = FALSE))
  }
  else {
    if (z <= 0) {
      cat("Do Not Reject Null Hypothesis", abs(z), "<", qnorm(conf.level), "\n pvalue:", 2 * pnorm(z,lower.tail = TRUE))
    }
    else {
      cat("Do Not Reject Null Hypothesis", abs(z), "<", qnorm(conf.level), "\n pvalue:", 2 * pnorm(z,lower.tail = FALSE))
    }
  }
  
}





