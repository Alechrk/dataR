fd = read.csv(file.choose(),head=TRUE)

fd$Features  = factor(fd$Features)
fd$Promotion = factor(fd$Promotion)
fd$Features  = factor(fd$Features )

ddfdf = lm(Users~Tweets_pos + Tweets_neg + Region+ Ads_Displayed +Price+ Features +Promotion + Period ,data=fd)

plot(ddfdf)


d1 <- cooks.distance(ddfdf )
r1 <- stdres(ddfdf )
sdp = cbind(fd , d1, r1)
sdp = sdp[d1 > 4/204, ]


LEVERAGE     x     OUTLIERNESS(RESIDUAL) = INFLUENCE