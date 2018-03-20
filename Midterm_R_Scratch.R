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
sort(abs(predict(out,type="coefficients",s=bestlam)[1:31,]), decreasing=TRUE)