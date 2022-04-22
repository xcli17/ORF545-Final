source("./database.R")

get.pmid <- function(date,sym,tmin=0,tmax=24,dsec=10){
    dt = sprintf("%.0f",dsec*1e9)
    nobs = 3600*(tmax-tmin)/dsec
    if(tmin<10){
        t0 = paste0("0D0",tmin,":00:00")
    } else {
        t0 = paste0("0D",tmin,":00:00")
    }
    q1 = paste0("([] time:",t0,"+",dt,"*til ",nobs,")")
    q2 = sprintf("select pmid:0.5*(last bid+last ask)
                  by time from quote where date=%s,sym=`%s",date,sym)
    q3 = sprintf("aj[`time;(%s);%s]",q1,q2)
    return(h(q3))
}

get.pmids <- function(dates,syms,tmin=0,tmax=24,dsec=10){
    pmids = list()
    for(i in 1:length(dates)){
        date = dates[i]
        pmid.sym1 = get.pmid(date,syms[1],dsec=dsec)
        class(pmid.sym1$time) = "double"
        colnames(pmid.sym1) = c("time",syms[1])
        pmid = pmid.sym1
        for(j in 2:length(syms)){
            pmid.symj = get.pmid(date,syms[j],dsec=dsec)
            class(pmid.symj$time) = "double"
            colnames(pmid.symj) = c("time",syms[j])
            pmid = dplyr::inner_join(pmid,pmid.symj,by="time")
        }
#         pmid$date = date
        pmids[[i]] = pmid
    }
    return(pmids)
}

# Warning: Only for this project's symbols
get.pmids.rearrange <- function(dates,syms,dsec=10){
    pmids1 = get.pmids(dates[1:(length(dates)-1)],syms,tmin=17,tmax=24)
    pmids2 = get.pmids(dates[2:length(dates)],syms,tmin=0,tmax=16)
    pmids = list()
    for(i in 1:length(pmids1)){
        pmids[[i]] = rbind(pmids1[[i]],pmids2[[i]])
    }
    return(pmids)
}