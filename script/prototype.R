
#Prototype code

#Create example set
n <- 10000
df <- data.frame(id = 1:n,
                 time = round((1:n)/100),
                 y = 1:n + rnorm(n,30,300),
                 x1 = rnorm(n, 100,100),
                 x2 = rnorm(n, 200,100),
                 x3 = runif(n)*(1:n))
tt <- runif(nrow(df)) > 0.6


args <- list(formula = as.formula("y~x1+x2+x3"),
             data = df,
             treatment = tt,
             window = 2,
             w.size = 30,
             algo = 2,
             run.var = "time",
             kval = 10,
             save = FALSE)
autoTrain <- function(args){
  
#PART 1: Triage components
  message("Part 1 - Reviewing inputs:")
  
  #Check if formula exists
    if(class(args$formula) == "formula"){
      form.obj <- args$formula
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

  #Check if data exists
    if(class(args$data) == "data.frame"){
      data <- args$data
      message("..Data detected")
    } else {
      warning("Missing data frame")
      break
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
      train_folds <- rep(1:kval, round(nrow(data)/kval)+1)
      train_folds <- train_folds[1:nrow(data)]
      train_folds <- train_folds[order(runif(length(train_folds)))]
      train_folds[treatment == TRUE] <- NA
      
      message("..k-folds parameter detected detected")
    } else {
      warning("Missing k-folds parameter")
      break
    }
    
  #Check if running var is specified
    if(sum(args$run.var %in% colnames(args$data)) == 1){
      run.var <- args$data[args$run.var]
      message("..Running var detected")
    } else {
      warning("Missing running variable")
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
        win <- seq(args$w.size, max(args$data[args$run.var]),  args$w.size)
        message(paste0("..Adaptive window of ", paste(win, collapse = ", ")))
      } else {
        warning("window and w.size need to be specified")
        break
      }
  

#PART 2: Search for best parameters
  message("Part 1 - Reviewing inputs:")
  
  for()
  rf.opt <- tuneRF(form.obj, data, mtryStart = 1,
         stepFactor = 2, improve = 0.05,
         plot = FALSE, trace = FALSE)
  try.opt <- rf.opt[rf.opt[,2] %in% min(rf.opt[,2]),1]
  out <- randomForest(form.obj, data, 
                      mtry = try.opt,
                      mtree = 500)
  
  
}


autoTrain(args)