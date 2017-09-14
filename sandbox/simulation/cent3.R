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
bigB <- 1000
parm <- expand.grid(seed=1:bigB,
                    n=ns)

# load r2weight package
library(r2weight)

# get the list size #########
if (args[1] == 'listsize') {
  cat(nrow(parm))
}

# execute prepare job ##################
if (args[1] == 'prepare') {
  for(i in 1:nrow(parm)){
     set.seed(parm$seed[i])
     dat <- makeData(n=parm$n[i])
     save(dat, file=paste0("~/cvr2/scratch/dataList_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
   }
   print(paste0('initial datasets saved to: ~/cvr2/scratch/dataList ... .RData'))
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
    
    # set seed
    set.seed(parm$seed[i])
    
    # load libraries
    library(SuperLearner)
    library(Rsolnp)
    
    SL.lib <- c("SL.glm","SL.step.forward","SL.mean")
    
    # estimate optimal weights
    fit <- optWeight(
        Y = dat$Y, X = dat$X, SL.library = SL.lib, family = gaussian(),
        CV.SuperLearner.V = 10, return.CV.SuperLearner = FALSE, return.SuperLearner = TRUE
        )

    # get combined outcome
    Y_omegan <- predict(fit, newdata = dat$X)

    # get tmle
    library(tmle)
    tmle.fit <- tmle(Y = Y_omegan, A = dat$X$x6, W = dat$X[,c(1:5,7:9)],
                     Q.SL.library = SL.lib, gform = "A ~ 1")

    # find true data-adaptive parameter values
    set.seed(123321)
    bigData <- makeData(n = 1e5)
    omega_n <- matrix(fit$SL.weights)
    Y_omegan <- data.matrix(bigData$Y)%*%omega_n
    psi_omegan <- predict(fit, newdata = bigData$X)
    R2_0omegan.psin <- 1 - mean((Y_omegan - psi_omegan)^2)/mean((Y_omegan - mean(Y_omegan))^2)
    save(R2_0omegan.psin, file = paste0("~/cvr2/out/truth_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))

    # get rid of SL.fit before saving
    fit$SL.fits <- NULL

    save(fit, file = paste0(
        "~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData.tmp"
    ))
    fit
    file.rename(
        paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData.tmp"),
        paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData")
    )
    print("Saved fit")
    
    # estimate performance
    perf.fit <- r2.optWeight(
        object = fit, Y = dat$Y, X = dat$X, evalV = 10, verbose = TRUE
    )
    perf.fit
    save(perf.fit, file = paste0(
        "~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData.tmp"
    ))
    file.rename(
        paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData.tmp"),
        paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData")
    )
  }
}

# merge job ###########################
if (args[1] == 'merge') {
    ns <- c(100,500,1000,5000)
    bigB <- 1000
    parm <- expand.grid(seed=1:bigB,
                        n=ns)
    
    # load r2weight package
    library(r2weight)
    
    set.seed(125857)
    
    trueUniR2 <- 0
    out <- NULL
    for(i in 1:nrow(parm)){
        tmp <- tryCatch({
            load(
                paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData")
            )
          load(
              paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData")
          )
          trueOptR2 <- get(load(
        paste0("~/cvr2/out/truth_n=",parm$n[i],"_seed=",parm$seed[i],".RData"
          )))

          c(
              # parameters
              parm$n[i], parm$seed[i],
              # true value
              trueOptR2, trueUniR2,
              # combined results
              perf.fit$r2, perf.fit$r2.ci,
              # univariate results
              perf.fit$univariateR2$y1,
              perf.fit$univariateR2$y2,
              perf.fit$univariateR2$y3,
              # weights
              fit$SL.weights
          )
        }, error=function(e){
          c(parm$n[i], parm$seed[i], rep(NA, 17))
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
        "y3R2","y3R2CI.l","y3R2CI.h",
        paste0("w",1:3)
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
    out$errw1 <- out$w1 - 1/3
    out$errw2 <- out$w2 - 1/3
    out$errw3 <- out$w3 - 1/3
    
    row.names(out) <- NULL
    
    save(out, file=paste0('~/cvr2/out/allOut.RData'))
    print("results saved")
}