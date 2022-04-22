get.simple.return <- function(pmid,balance.step,filter.size=0.005){
    pmid = data.frame(pmid)
    pmid.names = paste0(syms,".pmid")
    sig.names = paste0(syms,".sig")
    dp.names = paste0(syms,".dp")
    N = nrow(pmid)
    l = balance.step+1
    r = N - balance.step
    pmid[,sig.names] = apply(pmid[,sig.names],2,function(x){ifelse(abs(x)>filter.size,x,0)})
    pmid[,sig.names] = apply(pmid[,sig.names],2,function(x){ifelse(x>0,1,ifelse(x<0,-1,0))})
    pmid[1:r,dp.names] = (pmid[l:N,pmid.names] - pmid[1:r,pmid.names])
    pmid[,"ret"] = apply(pmid[,sig.names] * pmid[,dp.names] / pmid[,pmid.names],1,mean)
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

get.simple.returns <- function(pmids,balance.step,filter.size=0.005){
    ret.paths = c()
    for(i in 1:length(pmids)){
        ret.paths = rbind(ret.paths,get.simple.return(pmids[[i]],balance.step,filter.size))
    }
    return(ret.paths)
}

get.real.return <- function(pmid,balance.step,filter.size=0.005,pmid.w=0){
    pmid = data.frame(pmid)
    pmid.names = paste0(syms,".pmid")
    sig.names = paste0(syms,".sig")
    bid.names = paste0(syms,".bid")
    ask.names = paste0(syms,".ask")
    p.names = paste0(syms,".p")
    pnext.names = paste0(syms,".pnext")
    N = nrow(pmid)
    l = balance.step+1
    r = N - balance.step
    pmid[,sig.names] = apply(pmid[,sig.names],2,function(x){ifelse(abs(x)>filter.size,x,0)})
    pmid[,sig.names] = apply(pmid[,sig.names],2,function(x){ifelse(x>0,1,ifelse(x<0,-1,0))})
    pmid[1:r,p.names] = (pmid[1:r,bid.names] * 0.5 * (1 - pmid[1:r,sig.names]) +
                  pmid[1:r,ask.names] * 0.5 * (1 + pmid[1:r,sig.names])) * (1 - pmid.w) + 
                  pmid[1:r,pmid.names] * pmid.w
    pmid[1:r,pnext.names] = (pmid[l:N,bid.names] * 0.5 * (1 + pmid[1:r,sig.names]) + 
                     pmid[l:N,ask.names] * 0.5 * (1 - pmid[1:r,sig.names])) * (1 - pmid.w) + 
                     pmid[l:N,pmid.names] * pmid.w
    pmid[,"ret"] = apply(((pmid[,pnext.names]/pmid[,p.names])-1)*pmid[,sig.names],1,mean)
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

get.real.returns <- function(pmids,balance.step,filter.size=0.005,pmid.w=0){
    ret.paths = c()
    for(i in 1:length(pmids)){
        ret.paths = rbind(ret.paths,get.real.return(pmids[[i]],balance.step,filter.size,pmid.w))
    }
    return(ret.paths)
}
