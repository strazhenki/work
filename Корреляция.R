traffick <- c(5815, 8624, 12768, 12278, 14421, 11293, 10901, 16842, 15311)
conversion <- c(581, 862, 1276, 1227, 1444, 1129, 1090, 1684, 1531)
sdtraffick <- sd(traffick)
sdconversion <- sd(conversion)
Y <- mean(conversion)
X <- mean(traffick)
covariacion <- var(traffick,conversion)
Rxy <- cor(traffick, conversion)
intercept <- (sdconversion/sdtraffick)*Rxy
slope <-  Y - intercept*X

plot(traffick, conversion)

prop.test(traffick)

qqplot(traffick, conversion)

??FFt
log2((sin)(atan(1))^2)
150+190+70+60+195+110+45+60+55+120+45+25+205+115

param <- (100:300)
mean(param)
var(param)
sd(param)

?log
log2(sin^2(atan(1)))

x <- c(5,8)
y <- c(x, 1, c(3,4), x, NA)
