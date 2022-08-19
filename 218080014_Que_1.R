# Use ANOVA to test for the equality of output of two factories, i.e., are they equally productive, on average, or not

# Creating the dataset
factory1 <- c(415, 430, 395, 399, 408, 418, 399)
factory2 <- c(385, 410, 409, 403, 405, 400)

# We write the null and alternative hypothesis
  ## H0 : mu1 = mu2
  ## H1 : mu1 is not equal to mu2 (the means are different from each other)

# In order perform ANOVA we calculate the, 
  ## Total sum of squares (TSS)
  ## Between sum of squares (BSS)
  ## Within sum of squares (WSS)
# Note : TSS = BSS + WSS
# Total number of observations
  n <- length(c(factory1,factory2))
  n
# The number of factors
  k <- 2

# TSS
  ## Calculate the grand average   
  grand.avg <- mean(c(factory1, factory2))
  grand.avg
  ## Calculate the grand average square
  gavg.sqr <- grand.avg^2
  gavg.sqr
  ## Calculate the sum of square of all observations
  sum.sqr <- sum(c(factory1^2,factory2^2))
  sum.sqr  
  ## TSS
  tss <- sum.sqr-(n * gavg.sqr)
  tss  

# BSS
  ## Calculate mean for each factor
  ## Mean factory1
  f1.avg <- mean(factory1)
  f1.avg
    ## Square of mean of factory1
    f1avg.sqr <- f1.avg^2
    f1avg.sqr
  ## Mean factory2
  f2.avg  <- mean(factory2)
  f2.avg
    ## Square of mean of factory2
    f2avg.sqr <- f2.avg^2
    f2avg.sqr
  ## Number of observation for each factory
  n1 <- length(factory1)
  n1 
  n2 <- length(factory2)
  n2
  ## BSS
  bss <- ((n1*f1avg.sqr)+(n2*f2avg.sqr)) - (n*gavg.sqr) 
  bss

# WSS
  ## TSS = BSS + WSS, i.e., WSS = TSS - WSS
  wss <- tss - bss
  wss  
  ## Alternatively
  w1 <- sum((factory1-f1.avg)^2)
  w1  
  w2 <- sum((factory2-f2.avg)^2)
  w2
  wss1 <- w1 + w2
  wss1

# Now we calculate the test statistic
  ftest.s <- (bss/(k-1)) / (wss/(n-k))
  ftest.s  
# The critical value of the test is F*(siglevel,k-1,n-k)
  f.critical <- 4.844
# Compare the test statistic with critical value
  f.compare <- ftest.s < f.critical
  f.compare
# Thus, the test statistic is less than the critical value
  print("We do not reject the null hypothesis H0. Thus, the claim that they are equally productive on average holds.")
  