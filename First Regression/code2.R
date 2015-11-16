d = read.csv(file.choose(),head=T)

result = lm(d$Sales~d$Price + d$Competitor.Price)
summary(result)

attributes(result)
anova(result)

df = 34 - 2 - 1

tvalue =  4.65913/ 0.15486
pvalue = 2*(1-pt(30.09,31))



SSE = sum((result$fitted.values-d$Sales)^2)
SST = sum((mean(d$Sales)-d$Sales)^2 )
SSM = sum((mean(d$Sales)-result$fitted.values)^2)
SST = SSE + SSM


DFE = 34 -1 - 2
DFT = 34 - 1 = 33
DFM = 3  - 1
DFT = DFE + DFM


MSSE = SSE/DFE
MSST = SST/DFT
MSSM = SSM/DFM

summary(lm(d$Sales~d$Price ))



F = MSSM/MSSE
F
1-pf(F,2,31)

#Ho: "all of our coefficients = 0"
#H1: "at least one of our coefficients are different from 0"
