# To perform a proportion hypothesis test (not ND proportion test)
# We write down the null and alternative hypothesis
## H0 : prop = 0.5
## H1 : prop < 0.5 (discrimination against women)

# Now we derive proportion from the sample data
# Number of total judges
n <- 564
# Number of women judges
nw <- 72
# Sample Proportion
p <- (nw / n)
p


# We now calculate the test statistic

# Population proportion 
pie <- 0.5
# Calculate the denominator for the z statistic formulae
dnom <- sqrt(pie*(1 - pie)/n)
dnom
# Calculate the test statistic
ztest.stat <- (p - pie)/dnom
ztest.stat
# Z critical value at 5% significance level is
z.critical <- qnorm(0.05)
z.critical
# Compare the test statistic with the critical value
z.compare <- ztest.stat < (-(z.critical))
z.compare 

# Since in statistical sense z score of 17.7 is clearly significant, moreover 12.8% is way lower than 50%
print("We can reject the null hypothesis H0. Thus,the claim that there is no form discrimination against women judges can be rejected")
