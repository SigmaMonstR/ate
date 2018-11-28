
#Prototype code

#Create example set
n <- 10000
df <- data.frame(id = 1:n,
                 time = round((1:n)/100),
                 y = as.numeric(1:n + rnorm(n,30,300)),
                 x1 = rnorm(n, 100,100),
                 x2 = rnorm(n, 200,100),
                 x3 = runif(n)*(1:n),
                 x4 = rnorm(n, 100,100),
                 x5 = rnorm(n, 200,100),
                 x6 = runif(n)*(1:n))
df <- df[complete.cases(df),]
tt <- runif(nrow(df)) >= 0.6


args <- list(formula = as.formula("y~x1+x2+x3"),
             data = df,
             treatment = tt,
             window = 2,
             w.size = 10,
             w.factor = 5,
             algo = 2,
             run.var = "time",
             rdd.thresh = 50,
             kval = 10,
             save = FALSE)

autoTrain <- function(args){
  
#PART 1: Triage components
  message("Part 1 - Reviewing inputs:")
  
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
    
  
  #Check if kfolds var is specified
    if(class(args$kval) == "numeric"){
      kval <- args$kval
      kfolds <- rep(1:kval, round(nrow(data)/kval)+1)
      kfolds <- kfolds[order(runif(length(kfolds)))]
      kfolds <- kfolds[1:nrow(data)]
      kfolds <- kfolds[order(runif(length(kfolds)))]
      kfolds[treatment == TRUE] <- NA
      
      message("..k-folds parameter detected detected")
    } else {
      warning("Missing k-folds parameter")
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
        require(randomForest)
        message("..Assuming and loading RF")
      }
    
    #Set window size
      if(args$window == 1 && !is.null(args$w.size)){
        #1 = Fixed
        win <- args$w.size
        message(paste0("..Fixed window of ", win))
      } else  if(args$window == 2 && !is.null(args$w.size)){
        #2 = Adaptive -- create full list of options
        win.max <- ifelse(max(args$data[args$run.var]) > w.size*w.factor,
                          w.size*w.factor,
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

  

#PART 2: Search for best parameters
  message("Part 1 - Reviewing inputs:")
  
  #Set up folds
  train <- data[treatment == F,]
  test <- data[treatment == T,]
  
  #Set up parking lot
    #yhat results from kcv
    win.kcv <- data.frame()
  
    for(j in win){
      
      for(k in unique(kfolds[!is.na(kfolds)])){
        
        #Extract window with folds
        subtrain <- train[abs(run.var) <= j & kfolds != k,]
        subtrain <- subtrain[complete.cases(subtrain),]
        
        subtest <- test[abs(run.var) <= j & kfolds == k,]
        subtest <- subtest[complete.cases(subtest),]
        
        #Tune OOB
        # rf.opt <- tuneRF(y = subtrain[[yvar.name]],
        #                  x = subtrain[, xvar.name], mtryStart = 1,
        #                  stepFactor = 2, improve = 0.05,
        #                  plot = FALSE, trace = FALSE)
        # try.opt <- rf.opt[rf.opt[,2] %in% min(rf.opt[,2]),1]
        # 
        #Train
        out <- randomForest(form.obj, subtrain, 
                            mtree = 500)
        win.kcv <- rbind(win.kcv,
                         data.frame(k = k,
                                    win = j,
                                    y = subtest[[yvar.name]],
                                    yhat = predict(out, subtest)))
        message(paste0("Window ", j,": kCV iter ", k))
        
      }
     }

}


autoTrain(args)
