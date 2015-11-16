d = read.csv(file.choose(),head=T)





d$Sales = log(d$Sales)

d$Price = log(d$Price)

d$Competitor.Price  = log(d$Competitor.Price)




result = lm(d$Sales~d$Price + d$Competitor.Price)

summary(result)


y = beta0 + beta1 * price + beta2 * price_competitor
dy/dprice = beta1


ln(y) = beta0 + beta1 * ln(price) + beta2 * ln(price_competitor)
(price/y) * (dy/dprice)      = beta1 = E 
