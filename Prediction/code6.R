d = read.csv(file.choose(),head=T)

result = lm(d$Sales~d$Price + d$Competitor.Price)
summary(result)

Price = c(3.45,12)
Competitor.Price = c(12,2)
d = data.frame(Price,Competitor.Price)

predict(result,d, interval="prediction",level=0.99)
