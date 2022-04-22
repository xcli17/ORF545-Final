library(rkdb)
library(ggplot2)
library(dplyr)
library(latex2exp)


my_theme = theme_minimal() + 
           theme(plot.title=element_text(hjust=0.5,size=11),
                 axis.title=element_text(size=8),
                 axis.text=element_text(size=8),
                 legend.title=element_text(size=9),
                 legend.text=element_text(size=8),
                 strip.text=element_text(size=8))
set.seed(2022)

db = open_connection("hfm.princeton.edu",6005)
CACHE = "./cache"
dir.create(CACHE, showWarnings=FALSE)

h <- function(x,force=FALSE){
    tryLeft = 5
    cacheName = sprintf("%s/%s.RDS", CACHE, 
                        digest::digest(x, algo="md5", serialize=FALSE))
    if((!force) && file.exists(cacheName)){
        return(readRDS(cacheName))
    }
    tmp = try(execute(db,x), silent=TRUE)
    while(class(tmp)=="try-error" && tryLeft>0){
        tryLeft = tryLeft - 1
        port = 6000 + sample.int(9,1)
        print(paste("[INFO] Sleeping 1 seconds and reconnecting to port:",port))
        Sys.sleep(1)
        db = try(open_connection("hfm.princeton.edu",port))
        if(class(db)!="try-error"){
            tmp = try(execute(db,x), silent=TRUE)
        }
    }
    if(class(tmp)=="try-error"){
        print("Failed maximum number of tries. Quitting")
        quit(status=1)
        return(tmp)
    }
    saveRDS(tmp, file=cacheName)
    return(tmp)
}
