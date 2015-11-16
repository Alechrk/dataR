fd = read.csv(file.choose(),head=TRUE)

fd$Features  = factor(fd$Features)
fd$Promotion = factor(fd$Promotion)
fd$Region    = factor(fd$Region)

ddfdf = lm(Users~Tweets_pos  +Tweets_neg  +  Region + Ads_Displayed +Price+ Features +Promotion,data=fd)


summary(ddfdf)
plot(ddfdf

pairs(fd)
anova(ddfdf )




boxplot(fd$Users~fd$Period)

fd$Period[Trend==2]<-"Month11"
fd$Period[Trend==3]<-"Month11"
fd$Period[Trend==4]<-"Month11"
fd$Period[Trend==5]<-"Month11"
fd$Period[Trend==6]<-"Month11"
fd$Period[Trend==7]="Month11"
fd$Period[Trend==8]="Month11"
fd$Period[Trend==9]="Month11"
fd$Period[Trend==10]="Month11"
fd$Period[Trend==11]="Month11"


#############################################
#reload the data, in the previous lesson we replaced some of the values

fd = read.csv(file.choose(),head=TRUE)
fd$Features  = factor(fd$Features)
fd$Promotion = factor(fd$Promotion)
fd$Region    = factor(fd$Region)
ddfdf = lm(Users~Tweets_pos  +Tweets_neg  +  Region + Ads_Displayed +Price+ Features +Promotion + Period,data=fd)
summary(ddfdf)

###################################

library(MASS)
step <- stepAIC(ddfdf , direction="both")
step$anova # display results


library(leaps)
attach(fd)
leaps<-regsubsets(Users~Tweets_pos+ Tweets_neg+ Region+ Ads_Displayed +Price+ Features +Promotion   +Period,data=fd ,nbest=10)
 
summary(leaps)

plot(leaps,scale="r2")
