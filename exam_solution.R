# 1 exercise 
# X~Pois(10)
X <- rpois(100, 10)
#estimatior for EX, lambda = EX with mle
log.l <- function( lambda= 7){
  r=dpois(X, lambda);
  -sum(log(r));
}
library(stats4);
mle(log.l)
#9.88

# 2 exercise 
Y <- c(1.13,1.83, 13.14, 41.22, 27.19,  7.13,  6.94, 24.22, 31.77,  1.03,
       7.80, 26.90,  2.27,  3.21, 19.01,  5.98,  6.57, 9.47,  1.29,  6.99, 28.12,
       6.13, 13.91, 21.37, 12.23,  7.29, 13.93, 40.32,  0.17,  7.23,  4.44,  3.59,
       0.16, 22.27, 7.32,  4.82,  6.04,  3.80,  3.23,  6.04)
n= length(Y)
n
#a) statistiki
min(Y);max(Y);mean(Y);median(Y);sd(Y);var(Y);range(Y);IQR(Y);quantile(Y);sum(Y);
hist(Y)

#b) Y~Exp(lambda)
log.l <- function(lambda = 6){
  r = dexp(Y, lambda);
  -sum(log(r));
}
library(stats4);
mle(log.l)
#lambda = 0.087

lambda = 0.087
#Kolmogorov smirnov test
#h0: Y has exp distribution
#ha: Y does not have exp distribution
ks.test(Y, "pexp", lambda)
#PVAL=0.4854>0.05;h0 accepted; Y has exp distribution

#c)
#h0: mu=20
#ha: mu!=20
t.test(Y, alt="two.sided", mu=20, conf.level=0.95)
#pval =0.0106< 0.05; se prifakja ha, vremeto na cekanje ne e 20 min

# 3 exercise
pod <- matrix(c(65,55,85,80,135,145,115,120), 4,2, byrow=TRUE)
dimnames(pod) <- list(c("mat", "bio", "fiz", "hem"), c("m", "z"))
pod
#alpha = 0.05
#koristime hi kvadrat test za nezavisnost na obelezja
#X-polot na studentot; Y-nasoka na studentot
#h0:X i Y se nezavisni
#ha:X i Y se zavisni
chisq.test(pod, correct=FALSE)
#p-value=0.6942>0.05; se prifakja h0, X i Y se nezavisni
#izborot na nasokata ne zavisi od polot na studentot

# 4 exercise
before <- c(121, 125, 130, 145, 160, 180, 145, 184, 178, 169, 178 ,179 ,154,
            155, 165) #n=15
after <- c(125, 132, 135, 169, 132, 188, 198, 200, 202,
           145, 154, 178, 141, 122, 131, 128, 118, 145, 185, 158) #n=20
#alpha =0.05
#imame mal primerok, i nepoznati disperzii, koristime t test
#h0: prosecniot broj na novozarazeni e ist i kaj before i kaj after
#ha: procecniot broj na novozarazeni na before e pomal od prosecniot broj
# novozarazeni na after  [ pros. br. na zarazeni e pomal pred otvaranje na granicite ]
t.test(before,after, alt="less", conf.level=0.95, var.equal=TRUE)
#p-value=0.6575>0.05; se prifakja h0;
#prosecniot broj na novozarazeni e ist pred i po otvaranje na granicite



