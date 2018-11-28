
autoTrain <- function(args){
  #
  # Automatically tests and calculates treatment/control effects per Varian (2014)
  # 
  # Args: 
  #   *args* is a list object with specific parameters:
  #       formula:          Formula object
  #       data:             data frame 
  #       treatment (bool): boolean vector indicating treatment partitions
  #       window (int):     1 = Fixed, 2 = Adaptive
  #       w.size (int):     window size
  #       w.factor (int):   max extent to test as a multiple factor of w.size
  #       algo (int):       1 = OLS, 2 = Random Forest (default = 2)
  #       run.var (char):   Running variable name
  #       rdd.thresh (int): RDD threshold 
  #       kval (int):       Number of partitions for cross validation (default = 10)
  #       
  # Returns:
  #   List object containing:
  #       formula:          Original formula object
  #       run.var:          Name of running variable
  #       rdd.thresh:       RDD threshold
  #       models:           List of trained models by window
  #       diagnostics:      Data frame of R2, RMSE, N
  #       preds:            Data frame of y (treatment) and yhat (control) by window
  #       ate:              Average Treatment Effect by window
  #

  ###########################
  #PART 1: Triage components#
  ###########################
  
    message("Part 1 - Screening inputs:")
  
  #Check if formula exists
    if(class(args$formula) == "formula"){
      form.obj <- args$formula
      yvar.name <- as.character(form.obj)[2]
      xvar.name <- unlist(strsplit(as.character(form.obj)[3], split = "\\+"))
      xvar.name <- trimws(xvar.name)
      
      message("..Formula detected")
    } else {
      warning("Missing formula argument.")
      break
    }
    
  #Check if data exists
    if(class(args$data) == "data.frame"){
      data <- args$data
      message("..Data detected")
    } else {
      warning("Missing data frame")
      break
    }
  
  #Check if there are NAs
    for(i in c(yvar.name,xvar.name)){
      if(sum(is.na(data[,i])) > 0){
        warning(paste0("Missing values detected in variable ", i))
        break
      }
    }


  #Check if Treatment boolean exists
    if(class(args$treatment) == "logical"){
      treatment <- args$treatment
      message("..Treatment boolean detected")
    } else {
      warning("Missing treatment boolean")
      break
    }
  
  #Check if running var is specified
    if(sum(args$run.var %in% colnames(args$data)) == 1 & !is.null(args$rdd.thresh)){
      #Rescale relative to threshold
      run.var <- args$data[args$run.var]
      run.var <- run.var - args$rdd.thresh
      message("..Running variable and threshold detected")
    } else {
      warning("Missing running variable and RDD threshold")
      break
    }

  #Check if algorithm specified
      if(args$algo == 1){
        algo <- "lm"
        message("..Use OLS")
      } else if(args$algo == 2 || is.null(args$algo)){
        algo <- "rf"
        require(ranger)
        message("..Loading Random Forest")
      }
    
    #Set window size
      if(args$window.type == 1 && !is.null(args$w.size)){
        #1 = Fixed
        win <- args$w.size
        message(paste0("..Fixed window of ", win))
      } else  if(args$window.type == 2 && !is.null(args$w.size)){
        #2 = Adaptive -- create full list of options
        win.max <- ifelse(max(args$data[args$run.var]) > args$w.size * args$w.factor,
                          args$w.size * args$w.factor,
                          max(args$data[args$run.var]))
                          
        win <- seq(args$w.size, win.max,  args$w.size)
        message(paste0("..Adaptive window of ", paste(win, collapse = ", ")))
      } else {
        warning("window and w.size need to be specified")
        break
      }
  
  #Check if window size is workable
    for(i in win){
      for(j in unique(kfolds)){
        if(nrow(data[treatment == F,])==0){
          warning("Sample size too small: Training window n = 0")
          break
        } else if(nrow(data[treatment == F & kfolds == j,])==0){
          warning("Sample size too small: Training window for k-folds: n = 0")
          break
        } 
      }
    }
  
  #Check if kfolds var is specified
  if(class(args$kval) == "numeric"){
    kval <- args$kval
    
    message("..k-folds parameter detected")
  } else {
    warning("..Missing k-folds parameter, assuming k = 10")
    kval <- 10
  }
  

  
  ####################################
  #PART 2: Search for best parameters#
  ####################################
    message("Part 2 - Estimation")
  
  #Set up folds
    train <- data[treatment == F,]
    test <- data[treatment == T,]
    
  #Predict Treatment
    win.out <- list()
    diagnostics <- data.frame()
    mods <- list()
  
    for(j in win){

      
      message(paste0("Starting Window ", j))
      
        #Extract window with folds
        subtrain <- train[abs(run.var) <= j ,]
        subtrain <- subtrain[complete.cases(subtrain),]
        
        subtest <- test[abs(run.var) <= j ,]
        subtest <- subtest[complete.cases(subtest),]
        
        kvec <- rep(1:kval, nrow(subtrain)/kval * 1.5)
        kvec <- sample(kvec, nrow(subtrain))
        
        #Train/Test
        if(algo == "rf"){
          #RF Option
          
          temp <- data.frame()
          
          for(k in unique(kvec)){
            ktrain <- subtrain[kvec != k,]
            ktrain <- ktrain[complete.cases(ktrain),]
            
            ktest <- subtest[kvec == k,]
            ktest <- ktest[complete.cases(ktest),]
            
            out.k <- ranger(form.obj, data = ktrain, 
                          num.trees = 500)
            temp <- rbind(temp, 
                          data.frame(y = ktest[[yvar.name]],
                                     yhat = predict(out.k, ktest)$predictions))
            message(paste0("..Fold = ",k))
          }
          
          #Calculate diagnostics
            r2 <- (cor(temp, use = "pairwise.complete.obs")[1,2])^2
            rmse <- sqrt(mean((temp$y - temp$yhat)^2))
            n <- nrow(temp)
            diagnostics <- rbind(diagnostics,
                                 data.frame(w.size = j,
                                            n = n,
                                            r2 = r2,
                                            rmse = rmse))
            
          #Predict Control/Treatment
             out <- ranger(form.obj, subtrain, 
                          num.trees = 500)
             mods[[paste0("ranger_",j)]] <- out
            
            win.out[[j]] <- data.frame(win = j,
                                        ytreat = subtest[[yvar.name]],
                                        ycontrol = predict(out, subtest)$predictions)
        } else {
          
          #LM Option
          temp <- data.frame()
          
          for(k in unique(kvec)){
            #split out each fold
            ktrain <- subtrain[kvec != k,]
            ktrain <- ktrain[complete.cases(ktrain),]
            
            ktest <- subtest[kvec == k,]
            ktest <- ktest[complete.cases(ktest),]
            
            #Estimate
            out.k <- lm(form.obj, data = ktrain)
            temp <- rbind(temp, 
                          data.frame(y = ktest[[yvar.name]],
                                     yhat = predict(out.k, ktest)))
            message(paste0("..Fold = ",k))
          }
          
          #Calculate diagnostics
          r2 <- (cor(temp, use = "pairwise.complete.obs")[1,2])^2
          rmse <- sqrt(mean((temp$y - temp$yhat)^2))
          n <- nrow(temp)
          diagnostics <- rbind(diagnostics,
                               data.frame(w.size = j,
                                          n = n,
                                          r2 = r2,
                                          rmse = rmse))
          
          #Predict
          out <- lm(form.obj, data = subtrain)
          #mods[[paste0("lm",j)]] <- out
          
          win.out[[j]] <- data.frame(win = j,
                                      ytreat = subtest[[yvar.name]],
                                      ycontrol = predict(out, subtest))
        }

        message(paste0("..Window ", j, " finished"))
        
      }

    ###########################
    #PART 3: Treatment Effects #
    ###########################
    
    #Calculate effects
      win.out <- do.call(win.out, rbind)
      win.out$pct.effect <- win.out$ytreat/win.out$ycontrol-1
      win.out$lvl.effect <- win.out$ytreat - win.out$ycontrol
      
      ate <- aggregate(cbind(pct.effect, lvl.effect) ~ win,
                       data = win.out,
                       FUN = mean)
      colnames(ate) <- c("window", "pct.effect", "lvl.effect")
      ate$p.ks <- NA
      ate$p.ttest <- NA
      
    #Calculate if statistically different
      for(w in unique(win.out$win)){
        temp <- win.out[win.out$win == w,]
        ate$p.ks[ate$win == w] <- ks.test(temp$ytreat, temp$ycontrol)$p.value
        ate$p.ttest[ate$win == w] <- t.test(temp$ytreat, temp$ycontrol)$p.value
       
      }
      
    
    ###########################
    #PART 4: Save Outputs     #
    ###########################
    
    result <- list(formula = args$formula, 
                   algorithm = class(mods[[1]]),
                   models = mods,
                   diagnostics = diagnostics,
                   preds = win.out,
                   ate = ate,
                   rdd.thresh = args$rdd.thresh,
                   run.var = args$run.var)
    return(result)
}

