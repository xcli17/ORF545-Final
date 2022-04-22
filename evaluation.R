library(data.table)
lr.result <- function(asset,lag,dt){
  signal.original = dt[,paste0(asset,'.sig')]
  tick.size = ge.info$minpxincr
  diff = diff(dt[,paste0(asset,'.pmid')], lag = lag) / tick.size
  signal = head(signal.original, -lag) / tick.size
  lr = lm(diff ~ signal)
  corr = cor(signal,diff,use="complete.obs")
  ret = c(corr,
          summary(lr)$coefficients["signal","t value"],
          summary(lr)$coefficients["signal","Pr(>|t|)"])
  return(ret)
}


lr.results <- function(lag,dt){
  assets = syms
  ret = data.frame()
  
  for(i in 1:length(assets)){
    ret = rbind(ret,lr.result(assets[i],lag,dt))
  }
  colnames(ret) = c('corr','tval','pval')
  rownames(ret) = assets
  return(ret)
}

lr.plot <- function(lag,dt){
  assets = syms
  layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10),ncol=3,byrow=TRUE),heights=c(0.5,3,3,3))
  par(mai=c(0,0,0,0))
  plot.new()
  text(0.5,0.5,paste0("Linear Regression Plot - lag=",lag),cex=2,font=2)
  par(mai=rep(0.6,4))
  for (i in 1:length(assets)){
    asset = assets[i]
    signal.original = dt[,paste0(asset,'.sig')]
    tick.size = ge.info$minpxincr
    diff = diff(dt[,paste0(asset,'.pmid')], lag = lag) / tick.size
    signal = head(signal.original, -lag) / tick.size
    lr = lm(diff ~ signal)
    plot(signal, diff , main =  paste0(asset,', lag=',lag),ylab='forward price change (in ticksize)',xlab='signal (in ticksize)')
    abline(lr, col='red')
  }
}

corr.plot <- function(maxlag,dt){
  assets = syms
  layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10),ncol=3,byrow=TRUE),heights=c(0.5,3,3,3))
  par(mai=c(0,0,0,0))
  plot.new()
  text(0.5,0.5,paste0("Correlation Plot"),cex=2,font=2)
  par(mai=rep(0.6,4))
  for (i in 1:length(assets)){
    asset = assets[i]
    tick.size = ge.info$minpxincr
    signal.original <- dt[,paste0(asset,'.sig')]
    corr = c(0)
    for (j in 1:maxlag){
      lag = j
      diff <- diff(dt[,paste0(asset,'.pmid')], lag = lag) / tick.size
      signal <- head(signal.original, -lag) / tick.size
      corr = c(corr, cor(signal, diff))
    }
    plot(corr, type = 'l', main = paste0(asset,' correlation as function of lag'), xlab = 'lag')
    points(corr)
  }
}