Spencer Retcher
[sretcher.github.io](https://github.com/sretcher/sretcher.github.io)
# One Sample Means Z-Test


library(tidyverse)

turtle <- read_csv("TURTLES.csv") %>%
          select(Length)

head(turtle)

summary(turtle)



ggplot(mapping=aes(x=Length),data=turtle) +
  geom_histogram()

qqnorm(turtle$Length)
qqline(turtle$Length,col="blue",lwd=1)


x_bar = mean(turtle$Length, na.rm = TRUE)
s = sd(turtle$Length, na.rm = TRUE)
n = length(turtle$Length)
uo = 78




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





