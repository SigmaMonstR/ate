#Create example set

source()

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
             window.type = 2,
             w.size = 10,
             w.factor = 5,
             algo = 2,
             run.var = "time",
             kval = 10,
             rdd.thresh = 50)


out <- autoTrain(args)