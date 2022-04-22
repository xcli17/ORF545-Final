# Warning: Correlated with other code files' definition of dataframe. Only for this project.

recover.corrmat <- function(upper.vec){
    corrmat = diag(8) / 2
    corrmat[upper.tri(corrmat)] = upper.vec
    corrmat = corrmat + t(corrmat)
    return(corrmat)
}

pca.pred.row <- function(row,nvec=1){
    row = as.numeric(row)
    val = row[2:9]
    means = row[10:17]
    sds = row[18:25]
    sds.replace = sds
    sds.replace[sds.replace<=0] = Inf
    syms.standard = (val - means) / sds.replace
    corrmat = recover.corrmat(row[26:53])
    eigen.decomp = eigen(corrmat)
    proj = t(eigen.decomp$vectors) %*% syms.standard
    proj[(nvec+1):length(proj)] = 0
    pred = t((eigen.decomp$vectors %*% proj) * sds) + means
    return(pred)
}

pmid.preds <- function(pmid,nvec=1){
    preds = rep()
    for(i in 1:nrow(pmid)){
        preds = rbind(preds,pca.pred.row(pmid[i,],nvec))
    }
    preds = data.frame(preds)
    colnames(preds) = paste0(syms,".pred")
    pmid = cbind(pmid,preds)
    return(pmid)
}

pmids.preds <- function(pmids,nvec=1){
    df.pmids = list()
    pred.name = paste0(syms,".pred")
    sig.name = paste0(syms,".sig")
    for(i in 1:length(pmids)){
        df.pmids[[i]] = pmid.preds(pmids[[i]],nvec)
        df.pmids[[i]][,sig.name] = df.pmids[[i]][,pred.name] - df.pmids[[i]][,syms]
    }
    return(df.pmids)
}