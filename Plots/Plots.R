d = read.csv(file.choose(),head=T)

result = lm(d$Sales~d$Price + d$Competitor.Price)
plot(result)


cutoff <- 4/(nrow(d)) 
plot(result, which=4, cook.levels=cutoff)