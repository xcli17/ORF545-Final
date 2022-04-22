get.simple.return <- function(pmid,balance.step,filter.size=0.005){
    pmid.names = paste0(syms,".pmid")
    sig.names = paste0(syms,".sig")
    dp.names = paste0(syms,".dp")
    filter.size = 0.01 * tick.size #
    pmid[,sig.names] = apply(pmid[,sig.names],2,function(x){ifelse(abs(x)>filter.size,x,0)})
    pmid[,sig.names] = apply(pmid[,sig.names],2,function(x){ifelse(x>0,1,ifelse(x<0,-1,0))})
    pmid[1:(nrow(pmid)-balance.step),dp.names] = (pmid[(balance.step+1):nrow(pmid),pmid.names] 
                                                  - pmid[1:(nrow(pmid)-balance.step),pmid.names])
    pmid[,"ret"] = apply(pmid[,sig.names] * (pmid[,dp.names]),1,mean)
    pmid[is.na(pmid)] = 0
    rets = pmid[,"ret"]
    ret.path = rep(0,length(seq(from=1,to=nrow(pmid),by=balance.step)))
    for(i in 1:balance.step){
        balance.points = seq(from=i,to=nrow(pmid),by=balance.step)
        ret.path = ret.path + rets[balance.points]
    }
    ret.path = ret.path / balance.step
    return(ret.path)
}