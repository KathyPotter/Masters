library(psych)
library(readxl)
library(BlandAltmanLeh)

Observations <- read_excel("data/chickExp1/ICC_ObserversE1.xlsx")

### Intraclass correlations are recommended 
ICC(Observations,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

bland.altman.plot( Observations$Observer1, Observations$Observer2, main="", xlab="Means", ylab="Differences")

Observations <- read_excel("data/chickExp2/ICC_ObserversE2.xlsx")
attach(Observations)

### Intraclass correlations are recommended 

ICC(Observations,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)


bland.altman.plot( Observations$Observer1, Observations$Observer2, main="", xlab="Means", ylab="Differences")
