fd = read.csv(file.choose(),head=TRUE)


fd$Features  = factor(fd$Features)
fd$Promotion = factor(fd$Promotion)
fd$Region    = factor(fd$Region)





ddfdf = lm(Users~Tweets_pos + Tweets_neg + Region + Features + Tweets_neg + Ads_Displayed +fd$Price+ Promotion ,data=fd)
summary(ddfdf)

plot(ddfdf)
library(fBasics)
shapiroTest(ddfdf$residuals)



hist(ddfdf$residuals)



jarqueberaTest(ddfdf$residuals)


 plot(fd$Ads_Displayed, ddfdf$residuals, 
     ylab="Residuals", xlab=" ", 
     main="Residuals vs Ads") 
 abline(0, 0) 


ddfdf = lm(Users~Tweets_pos + Features + Ads_Displayed +Price+ Promotion + Period ,data=fd)
plot(ddfdf)
shapiroTest(ddfdf$residuals)