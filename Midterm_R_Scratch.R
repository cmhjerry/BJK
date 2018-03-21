## setwd("~/Desktop/Brown_Data_Science/data2020/Midterm") # set working dir
library(leaps)
census_df_full = read.csv("nyc_census.csv") # read in file

set.seed(1) # reproducibility
dim(census_df_full) # check dimensions (2167 36)

names(census_df_full)
summary(census_df_full)

# census_df[,c(-1,-2,-14,-15,-16)] # one method of column removal

# removing some redundant or useless stuff for fast analysis of Media Income

census_df <- census_df_full[,-which(names(census_df_full) %in% c("Borough", 
                                                                 "IncomeErr",
                                                                 "IncomePerCap",
                                                                 "IncomePerCapErr"))]
# investigating null values - not too terrible
percent_na <- colMeans(is.na(census_df))
sort(percent_na*100, decreasing=TRUE)

census_df_naomit <- na.omit(census_df)

dim(census_df_naomit) # check dims after na.omit 2095 31 --- getting 32

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
sort(abs(predict(out,type="coefficients",s=bestlam)[1:33,]), decreasing=TRUE)

# Quickly: County information is highly predictive, but maybe not what we want.
# County information may be good for *prediction* but not for any explanation.
# Removing that information for next analysis.

census_df_rd2 <- census_df_naomit[,-which(names(census_df_naomit) %in% c("County",
                                                                         "White",
                                                                         "Men"))]

## Here we try to determine the most predictive features and how many we should incorporate into our model.

dim(census_df_rd2)
regfit.full = regsubsets(Income~., data=census_df_rd2, nvmax = 29)
reg.summary = summary(regfit.full)
reg.summary
reg.summary$rsq
par(mfrow = c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)

sort(abs(coef(regfit.full,9)), decreasing = TRUE)

## As you can see from the graphs, After implementing about 8 variables, our model does not improve very much.
## The 8 most predictive variables (from reg.summary) are: Professsional, Poverty, OtherTransp, Transit, WorkAtHome, Employed, Citizen, and PublicWork


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
out2 = glmnet(x2, y2, alpha = 1)
lasso.coef=predict(out2,type="coefficients",s=bestlam)[1:29,]
sort(abs(lasso.coef), decreasing=TRUE)


library(ggplot2)
library(gridExtra)
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

## Plot features vs. income that look like they need to be log transformed

plot(census_df_rd2$Poverty, census_df_rd2$Income)
logPoverty = log10(census_df_rd2$Poverty+1)
plot(census_df_rd2$Service, census_df_rd2$Income)
logService = log10(census_df_rd2$Service+1)
plot(census_df_rd2$Production, census_df_rd2$Income)
logProduction = log10(census_df_rd2$Production+1)
plot(census_df_rd2$ChildPoverty, census_df_rd2$Income)
logChildPoverty = log10(census_df_rd2$ChildPoverty+1)
plot(census_df_rd2$MeanCommute, census_df_rd2$Income)
logMeanCommute = log10(census_df_rd2$MeanCommute+1)

## NOTE: we add 1 to the values of log transformed features to avoid taking the log of 0

model.fit = lm(Income
               ~Poverty
               +WorkAtHome
               +Office
               +Service
               +Production
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

model.test = lm(Income
                ~logPoverty
                +WorkAtHome
                +Office
                +logService
                +logProduction
                +Professional
                +Construction
                +Transit
                +Carpool
                +logChildPoverty
                +Native
                +Drive
                +Asian
                +Hispanic
                +MeanCommute
                , data=census_df_rd2)

summary(model.test)

location_information = read.csv("census_tract_loc.csv") # read in file
location_information_df = data.frame(location_information)
# there has to be a better way to remove the last 4 zeros........
location_information$CensusTract <- substr(location_information$BlockCode, 1,11)
location_aggregation <- aggregate(cbind(Latitude, Longitude)~CensusTract,location_information,mean)


census_df_rd2_with_location <- merge(x = census_df_rd2, y = location_aggregation, all.x = TRUE)
summary(census_df_rd2_with_location)

library(ggplot2)
library(ggmap)

map <- get_map(location = "newyork", maptype = "roadmap", zoom = 12)
ggmap(map) + geom_tile(data = census_df_rd2_with_location, aes(x = Longitude, y = Latitude, alpha = Income),
                       fill = 'red')

ggmap(map) + + geom_density2d(data = census_df_rd2_with_location, aes(x = Longitude, y = Latitude), size = 0.3) + stat_density2d(data = census_df_rd2_with_location, aes(x = Longitude, y = Latitude, fill = Income, alpha = Income), size = 0.01, bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)

mean.longitude <- mean(census_df_rd2_with_location$Longitude)
mean.latitude <- mean(census_df_rd2_with_location$Latitude)
drone.map <- get_map(location = 'new york', zoom = 11, scale= 2)
## Convert into ggmap object
drone.map <- ggmap(drone.map)
drone.map <- drone.map + stat_density2d(data=census_df_rd2_with_location,
                                        aes(x=Longitude, y=Latitude, fill=..level.., alpha=..level..), geom="polygon")
drone.map + geom_point(data=census_df_rd2_with_location,
                                    aes(x=Longitude, y=Latitude), fill="red", shape=21, alpha=0.8)




