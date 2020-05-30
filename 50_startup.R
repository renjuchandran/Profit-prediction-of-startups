start <- 50_Startups

summary(start)
library(plyr)

View(start)
cols.num <- c("State")
start[cols.num] <- sapply(start[cols.num],as.numeric)
sapply(start, class)
pairs(start)
cor(start)
sapply(start, class)
sapply(car, class)

library(corpcor)
cor2pcor(cor(start))


# The Linear Model 

model.start1 <- lm(Profit~.,data=start)
summary(model.start1)

# Multicollinearity check
model.start2 <- lm(Profit~.-Marketing.Spend,data=start)
summary(model.start2)

model3 <- lm(Profit~ (R.D.Spend +Marketing.Spend  ))
summary(model3)

model4 <- lm(Profit~(Administration+Marketing.Spend  ))
summary(model4)
#####vif#############
library(car)
vif(model.start)# Original model

#vif value is <10 for all variables

library("MASS")
stepAIC(model.start)

## plotting Influential measures 
windows()
influenceIndexPlot(model.start,id.n=8)
influenceIndexPlot(model.start1,id.n=8)
influencePlot(model.start1,id.n=3) 



final_model<-lm(Profit~.,data=start[-c(46,49,50),])
summary(final_model)
plot(finalmodel)

hist(residuals(final_model)) 
