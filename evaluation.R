library(data.table)
lr.result <- function(asset,lag,dt){
  signal.original <- dt[,paste0(asset,'.pred')] - dt[,asset]
  tick.size = ge.info$minpxincr
  diff <- diff(dt[,asset], lag = lag) / tick.size
  signal <- head(signal.original, -lag) / tick.size
  lr <- lm(diff ~ signal)
  corr = cor(signal,diff,use="complete.obs")
  ret = c(corr,
          summary(lr)$coefficients["signal","t value"],
          summary(lr)$coefficients["signal","Pr(>|t|)"])
  return(ret)
}


lr.results <- function(assets,lag,dt){
  ret = data.frame()
  
  for(i in 1:length(assets)){
    ret = rbind(ret,lr.result(assets[i],lag,dt))
  }
  colnames(ret) = c('corr','tval','pval')
  rownames(ret) = assets
  return(ret)
}

lr.plot <- function(assets,lag,dt){
  par(mfrow=c(2,2))
  for (i in 1:length(assets)){
    asset = assets[i]
    signal.original <- dt[,paste0(asset,'.pred')] - dt[,asset]
    tick.size = ge.info$minpxincr
    diff <- diff(dt[,asset], lag = lag) / tick.size
    signal <- head(signal.original, -lag) / tick.size
    lr <- lm(diff ~ signal)
    plot(signal, diff , main =  paste0(asset,', lag=',lag),ylab='forward price change (in ticksize)',xlab='signal (in ticksize)')
    abline(lr, col='red')
  }
  
}

corr.plot <- function(assets,maxlag,dt){
  par(mfrow=c(2,2))
  for (i in 1:length(assets)){
    asset = assets[i]
    tick.size = ge.info$minpxincr
    signal.original <- dt[,paste0(asset,'.pred')] - dt[,asset]
    corr = c(0)
    for (j in 1:maxlag){
      lag = j
      diff <- diff(dt[,asset], lag = lag) / tick.size
      signal <- head(signal.original, -lag) / tick.size
      corr = c(corr, cor(signal, diff))
    }
    plot(corr, type = 'l', main = paste0(asset,' correlation as function of lag'), xlab = 'lag')
    points(corr)
  }
  
}