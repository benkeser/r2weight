#! /usr/bin/env Rscript

# get environment variables
MYSCRATCH <- Sys.getenv('MYSCRATCH')
RESULTDIR <- Sys.getenv('RESULTDIR')
STEPSIZE <- as.numeric(Sys.getenv('STEPSIZE'))
TASKID <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

# set defaults if nothing comes from environment variables
MYSCRATCH[is.na(MYSCRATCH)] <- '.'
RESULTDIR[is.na(RESULTDIR)] <- '.'
STEPSIZE[is.na(STEPSIZE)] <- 1
TASKID[is.na(TASKID)] <- 0

# get command lines arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 1){
    stop("Not enough arguments. Please use args 'listsize', 'prepare', 'run <itemsize>' or 'merge'")
}

excludeX <- 1:9
bigB <- 1000
# 
# # 
# # simulation parameters
parm <- expand.grid(seed=1:bigB,excludeX=excludeX,n=1000)

# load r2weight package
library(r2weight)

# get the list size #########
if (args[1] == 'listsize') {
    cat(nrow(parm))
}

# execute prepare job ##################
if (args[1] == 'prepare') {
    # for(i in 1:nrow(parm)){
    #     set.seed(parm$seed[i])
    #     dat <- makeData(n=parm$n[i])
    #     save(dat, file=paste0("~/cvr2/scratch/dataList_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
    # }
    # print(paste0('initial datasets saved to: ~/cvr2/scratch/dataList ... .RData'))
}

# execute parallel job #################################################
if (args[1] == 'run') {
    if (length(args) < 2) {
        stop("Not enough arguments. 'run' needs a second argument 'id'")
    }
    id <- as.numeric(args[2])
    print(paste(Sys.time(), "arrid:" , id, "TASKID:",
                TASKID, "STEPSIZE:", STEPSIZE))
    for (i in (id+TASKID):(id+TASKID+STEPSIZE-1)) {
        print(paste(Sys.time(), "i:" , i))
        print(parm[i,])
        
        # load data
        load(paste0("~/cvr2/scratch/dataList_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
        # load previous fits on all variables
        load(paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
        load(paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
        # set seed
        set.seed(parm$seed[i])
        
        # load libraries
        library(SuperLearner)
        library(Rsolnp)
        
        # super learner library
        SL.lib <- c("SL.glm","SL.step.forward","SL.mean")
        
        # exclude variable from X
        X <- dat$X[,-parm$excludeX[i]]
        
        #-----------------------------
        # estimate optimal weights
        #-----------------------------
        fit.exclude <- optWeight(
            Y = dat$Y, X = X, SL.library = SL.lib, family = gaussian(),
            CV.SuperLearner.V = 10, return.CV.SuperLearner = FALSE
        )
        save(fit.exclude, file = paste0(
            "~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"
        ))
        fit.exclude
        file.rename(
            paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"),
            paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
        )
        print("Saved fit")
        
        #-----------------------------
        # estimate performance
        #-----------------------------
        perf.fit.exclude <- r2.optWeight(
            object = fit, Y = dat$Y, X = X, evalV = 10, verbose = TRUE
        )
        perf.fit.exclude
        save(perf.fit, file = paste0(
            "~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"
        ))
        file.rename(
            paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"),
            paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
        )
        
        #-----------------------------
        # compare univariate results
        #-----------------------------
        comp.fit <- r2.compare(object1 = fit, object2 = fit.exclude)
        save(comp.fit, file = paste0(
            "~/cvr2/out/comp_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"
        ))
        file.rename(
            paste0("~/cvr2/out/comp_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"),
            paste0("~/cvr2/out/comp_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
        )
        
        #-----------------------------
        # compare multivariate results
        #-----------------------------
        comp.perf.fit <- r2.compare(object1 = perf.fit, object2 = perf.fit.exclude)
        save(comp.perf.fit, file = paste0(
            "~/cvr2/out/comp_perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"
        ))
        file.rename(
            paste0("~/cvr2/out/comp_perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"),
            paste0("~/cvr2/out/comp_perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
        )
        
    }
}

# merge job ###########################
if (args[1] == 'merge') {
    set.seed(125857)
    trueOptR2 <- getTrueOptR2(n=1e6)
    trueUniR2 <- getTrueUnivariateR2(n=1e6)
    # vector of true optimal R2 when excluding
    trueOptR2Exclude <- apply(matrix(1:9),1,function(x){
        getTrueOptR2Exclude(n = 1e6, excludeX = x)
    })
    # vector of true individual outcome R2 when excluding
    trueUniR2ExcludeY1 <- apply(matrix(1:9),1,function(x){
        getTrueUnivariateR2Exclude(n = 1e6, excludeX = x, whichY = 1)
    })
    trueUniR2ExcludeY2 <- apply(matrix(1:9),1,function(x){
        getTrueUnivariateR2Exclude(n = 1e6, excludeX = x, whichY = 2)
    })
    trueUniR2ExcludeY3 <- apply(matrix(1:9),1,function(x){
        getTrueUnivariateR2Exclude(n = 1e6, excludeX = x, whichY = 3)
    })
    
    out <- NULL
    for(i in 1:nrow(parm)){
        tmp <- tryCatch({
            load(
                paste0("~/cvr2/out/comp_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
            )
            load(
                paste0("~/cvr2/out/comp_perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
            )
            c(
                # parameters
                parm$n[i], parm$seed[i],
                # true values for difference in R2
                trueOptR2 - trueOptR2Exclude[parm$excludeX[i]], 
                trueUniR2 - trueUniR2ExcludeY1[parm$excludeX[i]],
                trueUniR2 - trueUniR2ExcludeY2[parm$excludeX[i]],
                trueUniR2 - trueUniR2ExcludeY3[parm$excludeX[i]],
                # true values for ratio 1-R2
                (1-trueOptR2)/(1-trueOptR2Exclude[parm$excludeX[i]]), 
                (1-trueUniR2)/(1-trueUniR2ExcludeY1[parm$excludeX[i]]),
                (1-trueUniR2)/(1-trueUniR2ExcludeY2[parm$excludeX[i]]),
                (1-trueUniR2)/(1-trueUniR2ExcludeY3[parm$excludeX[i]]),
                # estimated values for difference in R2
                
                # univariate results
                perf.fit$univariateR2$y1,
                perf.fit$univariateR2$y2,
                perf.fit$univariateR2$y3
            )
        }, error=function(e){
            c(parm$n[i], parm$seed[i], rep(NA, 14))
        })
        out <- rbind(out, tmp)
    }
    # format
    out <- data.frame(out)
    colnames(out) <- c(
        "n","seed","trueOptR2","trueUniR2",
        "optR2","optR2CI.l","optR2CI.h",
        "y1R2","y1R2CI.l","y1R2CI.h",
        "y2R2","y2R2CI.l","y2R2CI.h",
        "y3R2","y3R2CI.l","y3R2CI.h"
    )
    # coverage (take mean by sample size to get coverage)
    out$covOptR2 <- as.numeric(out$optR2CI.l <= out$trueOptR2 & out$optR2CI.h >= out$trueOptR2)
    out$covY1R2 <- as.numeric(out$y1R2CI.l <= out$trueUniR2 & out$y1R2CI.h >= out$trueUniR2)
    out$covY2R2 <- as.numeric(out$y2R2CI.l <= out$trueUniR2 & out$y2R2CI.h >= out$trueUniR2)
    out$covY3R2 <- as.numeric(out$y3R2CI.l <= out$trueUniR2 & out$y3R2CI.h >= out$trueUniR2)
    # error (take mean by sample size to get bias)
    out$errOptR2 <- out$optR2 - out$trueOptR2
    out$errY1R2 <- out$y1R2 - out$trueUniR2
    out$errY2R2 <- out$y2R2 - out$trueUniR2
    out$errY3R2 <- out$y3R2 - out$trueUniR2
    
    row.names(out) <- NULL
    
    save(out, file=paste0('~/cvr2/out/allOut.RData'))
    print("results saved")
}