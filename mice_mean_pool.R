  library("mice")

  library("lattice")


## https://www.jstatsoft.org/article/view/v045i03
## Journal of Statistical Software 17
## Method   Description                     Scale type              Default
## pmm      Predictive mean matching        numeric                 Y
## norm     Bayesian linear regression      numeric
## norm.nob Linear regression, non-Bayesian numeric
## mean     Unconditional mean imputation   numeric
## 2l.norm  Two-level linear model          numeric
## logreg   Logistic regression             factor, 2 levels        Y
## polyreg  Polytomous (unordered) regression                       Y
## lda      Linear discriminant analysis    factor
## sample   Random sample from the observed data    any
## Table 1:  Built-in elementary imputation techniques.  
## The techniques are coded as functions named mice.impute.pmm(), and so on.

#gegevens binnenhalen uit de csv file met missing data
data <- read.csv (file="c:/stat/mi/fitness.csv", head=T, sep=",")

md.pattern(data)


imp <- mice(data, maxit = 2, m = 5)
fit <- with(data=imp,exp=lm(Oxygen~RunTime+RunPulse))
summary(pool(fit))

