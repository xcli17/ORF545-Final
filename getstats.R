library(matrixcalc)
stats <- function(x,mode,T=NULL){
    x = as.matrix(x)
    y1 = x[1,]
    y2 = list()
    y2[[1]] = matrix(rep(0,ncol(x)**2),nrow=ncol(x),ncol=ncol(x))
    A1n = x[1,]
    A2n = x[1,] %*% t(x[1,])
    Bn = 1
    Cn = 0
    if(mode == "uniform"){
        for(n in 2:nrow(x)){
            A1n = A1n + x[n,]
            A2n = A2n + x[n,] %*% t(x[n,])
            Bn = Bn + 1
            y1 = rbind(y1, A1n/Bn)
            Cn = (A1n %*% t(A1n)) / Bn
            y2[[n]] = (A2n - Cn) / Bn
        }
    } else if(mode == "window"){
        for(n in 2:T){
            A1n = A1n + x[n,]
            A2n = A2n + x[n,] %*% t(x[n,])
            Bn = Bn + 1
            y1 = rbind(y1, A1n/Bn)
            Cn = (A1n %*% t(A1n)) / Bn
            y2[[n]] = (A2n - Cn) / Bn
        }
        for(n in (T+1):nrow(x)){
            A1n = A1n + x[n,] - x[(n-T),]
            A2n = A2n + x[n,] %*% t(x[n,]) - x[(n-T),] %*% t(x[(n-T),])
            y1 = rbind(y1, A1n/Bn)
            Cn = (A1n %*% t(A1n)) / Bn
            y2[[n]] = (A2n - Cn) / Bn
        }
    } else if(mode == "exponential"){
        factor = exp(-1/T)
        for(n in 2:nrow(x)){
            A1n = factor * A1n + x[n,]
            A2n = factor * A2n + x[n,] %*% t(x[n,])
            Bn = factor * Bn + 1
            y1 = rbind(y1, A1n/Bn)
            Cn = (A1n %*% t(A1n)) / Bn
            y2[[n]] = (A2n - Cn) / Bn
        }
    }
    y = list()
    y[[1]] = y1
    y[[2]] = y2
    return(y)
}

cutoff.negative <- function(x){
    return(max(x,0))
}

# Warning: Only for this project.
pmid.stats <- function(pmid,mode,T=NULL){
    results = stats(pmid[,syms],mode,T)
    y1 = data.frame(results[[1]])
    rownames(y1) = NULL
    colnames(y1) = paste0(syms,".mean")
    pmid = cbind(pmid,y1)
    covmats = results[[2]]
    syms.sds = matrix(rep(0,nrow(pmid)*8),ncol=8)
    corrs.upper.flat = matrix(rep(0,nrow(pmid)*28),ncol=28)
    for(i in 2:nrow(pmid)){
        covmat = covmats[[i]]
        sds = sqrt(sapply(diag(covmat),cutoff.negative))
        syms.sds[i,] = sds
#         if(is.positive.definite(as.matrix(covmat),tol=1e-8)){
#             corrmat = cov2cor(covmat)
#             corrs.upper.flat[i,] = corrmat[upper.tri(corrmat)]
#         }
        # if there are some symbols performed badly, we can still continue with others 
        # if the effective symbols are not too few
#         if(sum(sds>0) >= 4){
        sd.replace = sds
        sd.replace[sd.replace==0] = Inf
        sdmat = sd.replace %*% t(sd.replace)
        corrmat = covmat / sdmat
        corrs.upper.flat[i,] = corrmat[upper.tri(corrmat)]
#         }
    }
    raw.names = colnames(pmid)
    pmid = cbind(pmid,syms.sds,corrs.upper.flat)
    colnames(pmid) = c(raw.names,paste0(syms,".sd"),paste0("corr.",seq(1,28)))
    return(pmid)
}

pmids.stats <- function(pmids,mode,T){
    df.pmids = list()
    for(i in 1:length(pmids)){
        df.pmids[[i]] = pmid.stats(pmids[[i]],mode,T)
    }
    return(df.pmids)
}