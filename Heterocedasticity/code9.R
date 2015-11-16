
l = read.csv(file.choose(),head=T)


model = lm(l$Sales.Value ~ l$Bounce.rate + l$Views...Campaign.A )
