weibull.surv.time <- function(df){
  # Проведение анализа выживаемости 
  library(survival)
  fit <- survreg(Surv(время) ~ группа, dist = 'weibull', df)
  # Созание пустого data frame 
  a <- seq(.99,.01,by=-.01)
  a <- as.data.frame(a)
  # Генерация предсказанных значений, создание data frame
  for (i in 1:length(levels(df$группа))){
    a[i] <- predict(fit, newdata=list(группа = levels(df$группа)[i]),
                    type="quantile",p=seq(.01,.99,by=.01))
  }
  colnames(a) <- levels(df$группа)
  a$percent <- seq(.99,.01,by=-.01)
  # Формат под ggplot2
  library(reshape2)
  df1 <- melt(a, id.vars = c("percent"))
  colnames(df1) <- c('percent', 'group', 'Predict time')
  # Генерация LT50 и LT95
  for (i in 1:length(levels(df$группа))){
    halfLife <- predict(fit, newdata=list(группа = levels(df$группа)[i]), 
                        type='uquantile', p=c(0.5, 0.99), se=T)
    a <- as.table(exp(cbind(lower=halfLife$fit - 1.96*halfLife$se, 
                            LT=halfLife$fit, upper=halfLife$fit + 1.96*halfLife$se)))
    rownames(a) <- c('LT50', 'LT99')
    print(levels(df$группа)[i])
    print(a)
  }
  # Рисование графика
  library(ggplot2)
  plot <- ggplot(df1, aes(df1$`Predict time`, df1$percent, group = df1$group))+
    geom_smooth(col = 'black', aes(linetype = df1$group))+
    theme_linedraw()+
    xlab('ВРЕМЯ НОКДАУНА, СЕК')+
    ylab('СМЕРТНОСТЬ')+
    scale_linetype_discrete(name = "%", 
                            labels = levels(df$группа))
  print(plot)
}

