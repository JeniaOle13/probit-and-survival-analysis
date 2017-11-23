library(drc)
library(ggplot2)

probit.analysis <- function(x){
  fitl.glm <- glm(cbind(x[,3], x[,2]-x[,3]) ~ log(x[,1]),
                  family=binomial('probit'), data=x[x$dose != 0, ])
  e <- summary(fitl.glm)
  fit <- anova(fitl.glm, test = 'LRT')
  dose.p
  xp <- dose.p(fitl.glm, p=c(0.50, 0.95, 0.99))
  xp.ci <- xp + attr(xp, "SE") %*% matrix(qnorm(1 - 0.05/2)*c(-1,1), nrow=1)
  zp.est <- exp(cbind(xp.ci[,1],xp,xp.ci[,2]))
  dimnames(zp.est)[[2]] <- c("zp.lcl","zp","zp.ucl")
  a <- fitl.glm$fitted.values
  b <- x[,3]/x[,2]
  d <- x[,1]
  c <- data.frame(a,b,d)
  
  plot <- ggplot(c, aes(x = log(d), y = log(b)))+
    geom_point(size = 3)+
    geom_smooth(method = "lm", aes(x = log(d), y = log(a)))+
    theme_linedraw()
  chisq <- fit$`Resid. Dev`[2]
  DF <- fit$`Resid. Df`[2]
  chisq.df <- data.frame(chisq, DF)
  slope <- e$coefficients[2,1]
  SE <- e$coefficients[2,2]
  slope.SE <- data.frame(slope, SE)
  print(zp.est)
  print(slope.SE)
  print(chisq.df)
  plot
}
