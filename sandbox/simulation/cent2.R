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

ns <- c(100,500,1000,5000)
excludeX <- c(2,7)
bigB <- 1000
# 
# # 
# # simulation parameters
parm <- expand.grid(seed=1:bigB,excludeX=excludeX,n=ns)

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
        # add family argument to output (ran before newer version)
        fit$family <- gaussian()
        perf.fit$family <- gaussian()
        
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
            CV.SuperLearner.V = 10, return.CV.SuperLearner = FALSE, return.SuperLearner = TRUE
        )
        
        #----------------------------
        # get data adaptive r2 measure
        #----------------------------
        set.seed(123321)
        bigData <- makeData(n = 1e5)
        omega_n <- matrix(fit.exclude$SL.weights)
        Y_omegan <- data.matrix(bigData$Y)%*%omega_n
        psi_omegan <- predict(fit.exclude, newdata = bigData$X)
        R2_0omegan.psin <- 1 - mean((Y_omegan - psi_omegan)^2)/mean((Y_omegan - mean(Y_omegan))^2)
        save(R2_0omegan.psin, 
             file = paste0("~/cvr2/out/truth_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData"))

        fit.exclude$SL.fits <- NULL
        save(fit.exclude, file = paste0(
            "~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"
        ))
        fit.exclude
        file.rename(
            paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"),
            paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
        )
        print("Saved fit")
        load(paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData"))
        
        #-----------------------------
        # estimate performance
        #-----------------------------
        perf.fit.exclude <- r2.optWeight(
            object = fit.exclude, Y = dat$Y, X = X, evalV = 10, verbose = TRUE
        )
        perf.fit.exclude
        save(perf.fit.exclude, file = paste0(
            "~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"
        ))
        file.rename(
            paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData.tmp"),
            paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
        )
        # load(paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData"))
        # 
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
    
    ns <- c(100,500,1000,5000)
    excludeX <- c(2,7)
    bigB <- 1000
    # 
    # # 
    # # simulation parameters
    parm <- expand.grid(seed=1:bigB,excludeX=excludeX,n=ns)
    
    # load r2weight package
    library(r2weight)
    
    set.seed(125857)
    #trueOptR2 <- getTrueOptR2(n=1e6)
    # trueUniR2 <- getTrueUnivariateR2(n=1e6)
    # # vector of true optimal R2 when excluding
    # trueOptR2Exclude <- apply(matrix(1:9),1,function(x){
    #     if(x %in% c(2,7)){
    #         getTrueOptR2Exclude(n = 1e6, excludeX = x)
    #     }else{
    #         NA
    #     }
    # })
    # # vector of true individual outcome R2 when excluding
    # trueUniR2ExcludeY1 <- apply(matrix(1:9),1,function(x){
    #     getTrueUnivariateR2Exclude(n = 1e6, excludeX = x, whichY = 1)
    # })
    # trueUniR2ExcludeY2 <- apply(matrix(1:9),1,function(x){
    #     getTrueUnivariateR2Exclude(n = 1e6, excludeX = x, whichY = 2)
    # })
    # trueUniR2ExcludeY3 <- apply(matrix(1:9),1,function(x){
    #     getTrueUnivariateR2Exclude(n = 1e6, excludeX = x, whichY = 3)
    # })
    
    out <- NULL
    for(i in 1:nrow(parm)){
        tmp <- tryCatch({
        r2Exclude <- get(load(paste0("~/cvr2/out/truth_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")))
        trueOptR2 <- get(load(
        paste0("~/cvr2/out/truth_n=",parm$n[i],"_seed=",parm$seed[i],".RData"
          )))
            # load(
            #     paste0("~/cvr2/out/comp_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
            # )
            load(
                paste0("~/cvr2/out/comp_perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],"_exclude=",parm$excludeX[i],".RData")
            )
            c(
                # parameters
                parm$n[i], parm$seed[i], parm$excludeX[i],
                # true values for difference in R2
                trueOptR2 - r2Exclude, 
                # trueUniR2 - trueUniR2ExcludeY1[parm$excludeX[i]],
                # trueUniR2 - trueUniR2ExcludeY2[parm$excludeX[i]],
                # trueUniR2 - trueUniR2ExcludeY3[parm$excludeX[i]],
                # true values for ratio 1-R2
                (1-trueOptR2)/(1-r2Exclude), 
                # (1-trueUniR2)/(1-trueUniR2ExcludeY1[parm$excludeX[i]]),
                # (1-trueUniR2)/(1-trueUniR2ExcludeY2[parm$excludeX[i]]),
                # (1-trueUniR2)/(1-trueUniR2ExcludeY3[parm$excludeX[i]]),
                # estimated values for difference in R2
                as.numeric(comp.perf.fit$diff)[1:3],
                # as.numeric(comp.fit$y1$diff)[1:3],
                # as.numeric(comp.fit$y2$diff)[1:3],
                # as.numeric(comp.fit$y3$diff)[1:3],
                # estimated values for ratio of 1-R2
                as.numeric(comp.perf.fit$ratio)[1:3]
                # as.numeric(comp.fit$y1$ratio)[1:3],
                # as.numeric(comp.fit$y2$ratio)[1:3],
                # as.numeric(comp.fit$y3$ratio)[1:3]
            )
        }, error=function(e){
            c(parm$n[i], parm$seed[i], parm$excludeX[i], rep(NA, 8))
        })
        out <- rbind(out, tmp)
    }
    # format
    out <- data.frame(out)
    colnames(out) <- c(
        "n","seed","excludeX",
        "trueOptR2.diff",
        #"trueUniR2Y1.diff","trueUniR2Y2.diff","trueUniR2Y3.diff",
        "trueOptR2.ratio",
        #"trueUniR2Y1.ratio","trueUniR2Y2.ratio","trueUniR2Y3.ratio",
        paste0(c("optR2","optR2CI.l","optR2CI.h"),
        #"y1R2","y1R2CI.l","y1R2CI.h",
        #"y2R2","y2R2CI.l","y2R2CI.h",
        #"y3R2","y3R2CI.l","y3R2CI.h"),
        ".diff"),
        paste0(c("optR2","optR2CI.l","optR2CI.h"),
                 # "y1R2","y1R2CI.l","y1R2CI.h",
                 # "y2R2","y2R2CI.l","y2R2CI.h",
                 # "y3R2","y3R2CI.l","y3R2CI.h"),
                 ".ratio")
        )
    # coverage (take mean by sample size to get coverage)
    out$covOptR2.diff <- as.numeric(out$optR2CI.l.diff <= out$trueOptR2.diff & out$optR2CI.h.diff >= out$trueOptR2.diff)
    out$errOptR2.diff <- out$optR2.diff - out$trueOptR2.diff
    out$covOptR2.ratio <- as.numeric(out$optR2CI.l.ratio <= out$trueOptR2.ratio & out$optR2CI.h.ratio >= out$trueOptR2.ratio)
    out$errOptR2.ratio <- out$optR2.ratio - out$trueOptR2.ratio
    
    row.names(out) <- NULL
    
    save(out, file=paste0('~/cvr2/out/allOutExclude.RData'))
    print("results saved")
}