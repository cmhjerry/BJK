setwd("~/Desktop/Brown_Data_Science/data2020/Midterm") # set working dir
census_df_full = read.csv("Midterm Project/nyc_census.csv") # read in file

set.seed(1) # reproducibility
dim(census_df_full) # check dimensions (2167 36)

names(census_df_full)
summary(census_df_full)

#census_df[,c(-1,-2,-14,-15,-16)] # one method of column removal

# removing some redundant or useless stuff for fast analysis of Media Income

census_df <- census_df_full[,-which(names(census_df_full) %in% c("Borough", 
                                                                 "IncomeErr",
                                                                 "IncomePerCap",
                                                                 "IncomePerCapErr"))]
# investigating null values - not too terrible
percent_na <- colMeans(is.na(census_df))
sort(percent_na*100, decreasing=TRUE)

census_df_naomit <- na.omit(census_df)

dim(census_df_naomit) # check dims after na.omit 2095 31

# Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)    #Grid of lambda values
x=model.matrix(Income~.,census_df_naomit)[,-1]
y=census_df_naomit$Income
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
paste("Best Lambda", bestlam)  # Print out best lambda
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
paste("MSE w/ Best Lambda", mean((ridge.pred-y.test)^2)) #Test MSE associated with smallest lambda
out=glmnet(x,y,alpha=0)
sort(abs(predict(out,type="coefficients",s=bestlam)[1:32,]), decreasing=TRUE)

# Quickly: County information is highly predictive, but maybe not what we want.
# County information may be good for *prediction* but not for any explanation.
# Removing that information for next analysis.

census_df_rd2 <- census_df_naomit[,-which(names(census_df_naomit) %in% c("County",
                                                                         "White",
                                                                         "Men"))]

# Lasso

library(glmnet)
grid=10^seq(10,-2,length=100)    #Grid of lambda values
x2=model.matrix(Income~.,census_df_rd2)[,-1]
y2=census_df_rd2$Income

train=sample(1:nrow(x2), nrow(x2)/2)
test=(-train)
y2.test=y2[test]

lasso.mod=glmnet(x2[train,],y2[train],alpha=1,lambda=grid)
plot(lasso.mod)

cv.out=cv.glmnet(x2[train,],y2[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min # print out best lambda
lasso.pred=predict(lasso.mod,s=bestlam,newx=x2[test,])
mean((lasso.pred-y2.test)^2)

lasso.coef=predict(out,type="coefficients",s=bestlam)[1:29,]
sort(abs(lasso.coef), decreasing=TRUE)


library(ggplot2)
require(gridExtra)
p1 <- ggplot(census_df_rd2, aes(OtherTransp,Income)) + geom_point() + geom_smooth(method ="lm")
p2 <- ggplot(census_df_rd2, aes(log10(Poverty),Income)) + geom_point() + geom_smooth(method ="lm")
p3 <- ggplot(census_df_rd2, aes(WorkAtHome,Income)) + geom_point() + geom_smooth(method ="lm")
p4 <- ggplot(census_df_rd2, aes(Office,Income)) + geom_point() + geom_smooth(method ="lm")
p5 <- ggplot(census_df_rd2, aes(log10(Service),Income)) + geom_point() + geom_smooth(method ="lm")
p6 <- ggplot(census_df_rd2, aes(log10(Production),Income)) + geom_point() + geom_smooth(method ="lm")
p7 <- ggplot(census_df_rd2, aes(Professional,Income)) + geom_point() + geom_smooth(method ="lm")
p8 <- ggplot(census_df_rd2, aes(Construction,Income)) + geom_point() + geom_smooth(method ="lm")
p9 <- ggplot(census_df_rd2, aes(Transit,Income)) + geom_point() + geom_smooth(method ="lm")
p10 <- ggplot(census_df_rd2, aes(Carpool,Income)) + geom_point() + geom_smooth(method ="lm")
p11 <- ggplot(census_df_rd2, aes(ChildPoverty,Income)) + geom_point() + geom_smooth(method ="lm")
p12 <- ggplot(census_df_rd2, aes(Native,Income)) + geom_point() + geom_smooth(method ="lm")
p13 <- ggplot(census_df_rd2, aes(Drive,Income)) + geom_point() + geom_smooth(method ="lm")
p14 <- ggplot(census_df_rd2, aes(Asian,Income)) + geom_point() + geom_smooth(method ="lm")
p15 <- ggplot(census_df_rd2, aes(Hispanic,Income)) + geom_point() + geom_smooth(method ="lm")
p16 <- ggplot(census_df_rd2, aes(MeanCommute,Income)) + geom_point() + geom_smooth(method ="lm")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16, ncol=4, nrow=4)
plot(census_df_rd2$Income)


model.fit = lm(Income
               ~Poverty
               +WorkAtHome
               +Office
               +Service
               +Professional
               +Construction
               +Transit
               +Carpool
               +ChildPoverty
               +Native
               +Drive
               +Asian
               +Hispanic
               +MeanCommute
               , data=census_df_rd2)
summary(model.fit)