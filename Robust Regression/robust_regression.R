fd = read.csv(file.choose(),head=TRUE)

fd$Features  = factor(fd$Features)
fd$Promotion = factor(fd$Promotion)
fd$Region= factor(fd$Region)

ddfdf = lm(Users~Tweets_pos + Tweets_neg + Region+ Ads_Displayed +Price+ Features +Promotion ,data=fd)
summary(ddfdf)

sqp = fd[-22,]
sqp = sqp [-3,]

ddfdf = lm(Users~Tweets_pos + Tweets_neg + Region+ Ads_Displayed +Price+ Features +Promotion ,data=sqp )
summary(ddfdf)



a=rlm(Users~Tweets_pos + Tweets_neg + Region+ Ads_Displayed +Price+ Features +Promotion ,data=fd)
summary(a)
a$weight

ax = lmRob(Users~Tweets_pos + Tweets_neg + Region+ Ads_Displayed +Price+ Features +Promotion , data = fd)
summary(ax)

