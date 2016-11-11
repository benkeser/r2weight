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
# 
# # 
# # simulation parameters
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
    
    SL.lib <- c("SL.glm","SL.bayesglm","SL.step","SL.mean")
    
    # estimate optimal weights
    fit <- optWeight(
        Y = dat$Y, X = dat$X, SL.library = c("SL.glm","SL.mean"), family = gaussian(),
        outerV = 10, return.CV.SuperLearner = FALSE
        )
    save(fit, file = paste0(
        "~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData.tmp"
    ))
    file.rename(
        paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData.tmp"),
        paste0("~/cvr2/out/fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData")
    )
    print("Saved fit")
    
    # estimate performance
    perf.fit <- r2.optWeight(
        object = fit, Y = dat$Y, X = dat$X, evalV = 10
    )
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
    set.seed(125857)
    trueOptR2 <- getTrueOptR2(n=1e6)
    trueUniR2 <- getTrueUnivarateR2(n=1e6)
    out <- NULL
    for(i in nrow(parm)){
        tmp <- tryCatch({
          load(
              paste0("~/cvr2/out/perf_fit_n=",parm$n[i],"_seed=",parm$seed[i],".RData")
          )
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
              perf.fit$univariateR2$y3
          )
        }, error=function(e){
          c(parm$n[i], parm$seed[i], rep(NA, 14))
        })
        out <- rbind(out, tmp)
    }
    save(out, file=paste0('~/cvr2/out/allOut.RData'))
    print("results saved")
}