
library(bayesAB)
a <- c(1048186.2, 1111170, 1081608.6, 1035531.6, 1069407, 1073191.5, 1149624.9, 1029717, 1037918.1, 1072230.6, 998883.3,1045217.7, 979078.2, 1114703.7,1086142.5, 1061615.7, 1000016.4,1047059.7, 1009198.5, 967327.2, 997769.7, 1061553)
b <- c(1746977, 1851950, 1802681, 1725886, 1782345, 1788652.5, 1916041.5, 1716195, 1729863.5, 1787051, 1664805.5, 1742029.5, 1631797, 1857839.5, 1810237.5, 1769359.5, 1666694, 1745099.5, 1681997.5, 1612212, 1662949.5, 1769255)
n <- c(3493954, 3703900, 3605362, 3451772, 3564690, 3577305, 3832083, 3432390, 3459727, 3574102, 3329611, 3484059, 3263594, 3715679, 3620475, 3538719, 3333388, 3490199, 3363995, 3224424, 3325899, 3538510)
boxplot(a)
boxplot(b)
shapiro.test(a)
shapiro.test(b)
barplot(a)
barplot(b)

qqnorm(a)
qqnorm(b)

6) какой метод победил
sum(a)
sum(b)
data <- data.frame(a, b, n)
step1 <- c(sum(data$n), sum(data$n))
step2 <- c(sum(data$a), sum(data$b))
std_funnel <- data.frame(step1, step2)

std_funnel$CR <- std_funnel$step2/std_funnel$step1
power.prop.test(p1=std_funnel$CR[1], p2=std_funnel$CR[2], power=0.9, alternative='one.sided', sig.level=0.05)
data <- prop.test(x = c(std_funnel$step2[1], std_funnel$step2[2]), n = c(std_funnel$step1[1], std_funnel$step1[2]), alternative = c("less"), conf.level = 0.95, correct=TRUE)
std_funnel$interval <- data$estimate

binom_a <- binom.test(23077151, 76923837, p=0.3)
binom_b <- binom.test(38461919, 76923837, p=0.5) 

A_a <- rbinom(23077151, 0, .3)
B_b <- rbinom(38461919, 0, .5)
top <- bayesTest(A_a, B_b, priors = c('alpha' = 50, 'beta' = 200), n_samples = 1e5, distribution = 'bernoulli')
print(top)
summary(top)
plot(top)
qqnorm(CRcarproce)