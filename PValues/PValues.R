
d = read.csv(file.choose(),head=T)
result = lm(d$Sales~d$Price + d$Competitor.Price)
summary(result)


t = 4.65913/0.15486


"Significance of each coefficient"
H0: "the coefficient = 0"
H1: "the coefficient is different from 0"


r = rt(10000,31)

qt(0.975,31)

hist(r)

t = 2 