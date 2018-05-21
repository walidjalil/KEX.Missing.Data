
for (i in c(1:23)) {
HCC_t[,i] <- impute(HCC_t[,i], mode)
}

for (i in c(50)) {
  HCC_t[,i] <- impute(HCC_t[,i], mode)
}

for (i in c(27:29)) {
  HCC_t[,i] <- impute(HCC_t[,i], mode)
}

for (i in c(24:26)) {
  HCC_t[,i] <- impute(HCC_t[,i], mean)
}

for (i in c(30:49)) {
  HCC_t[,i] <- impute(HCC_t[,i], mean)
}



famd.matrix <- data
for (j in c(1:23)) {
  
  famd.matrix[,j] <- as.numeric(as.character(famd.matrix[,j]))
}

for (j in c(27:29)) {
  
  famd.matrix[,j] <- as.numeric(as.character(famd.matrix[,j]))
}

famd.matrix[,50] <- as.numeric(as.character(famd.matrix[,50]))

numb <- c(1:29)

paste("V",numb[27], "_0",sep = "")

for (j in c(27)) {
  for (i in c(1:165)) {
    if (data[i,j] == paste("V",numb[j], "_0",sep = "") ) {
      famd.matrix[i,j] <- 0
    } else if (data[i,j] == paste("V",numb[j], "_1",sep = "") ) {
      famd.matrix[i,j] <- 1
    } else if (data[i,j] == paste("V",numb[j], "_2",sep = "")) {
      famd.matrix[i,j] <- 2
    } else if (data[i,j] == paste("V",numb[j], "_3",sep = "")) {
      famd.matrix[i,j] <- 3
    } else {
      famd.matrix[i,j] <- 4
    }
  }
}


for (j in c(30:49)) {
  famd.matrix[,j] <- squeeze(x = famd.matrix[,j], bounds = c(min(HCC_t[,j],na.rm = TRUE), max(HCC.test[,j],na.rm = TRUE), r = rep.int(TRUE, length(famd.matrix[,30]))))
}

HCC.test <- famd.matrix

cols <- sprintf("V%s", 1:23)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)   # turn columns into factors. !!

cols <- sprintf("V%s", 27:29)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)   # turn columns into factors. !!

cols <- sprintf("V%s", 50)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)

#rand.ini <- sample(1:165, 25, replace = FALSE)
all.conf.matrix <- list()
conf.matrix <- matrix(data=NA, nrow=40, ncol=6)
rand.ini <- c(67, 64, 14, 108, 41, 119, 159, 98, 116, 120, 63, 89, 151, 158, 129, 69, 130, 23, 73, 88, 79, 160, 124, 141, 26)
options(warn=-1)


for (loop in c(1:1)) {
  # ----- Start Imputation with Random Forests: 
  HCC.imp <- HCC_t
  
  for (r in c(27:40)) {
    #rand.ini <- sample(1:165, 15, replace = FALSE)
    print(rand.ini)
    imputedfinished <- HCC.imp
    
    for (i in c(1:50)) {
      imputedfinished[,i] <- as.numeric(as.character(imputedfinished[,i]))
    }
    
    imputedscaled <- scale(imputedfinished)
    # ----- Scale before you split the dataset into training data & test data. 
    testdata <- imputedscaled[rand.ini,]
    trainingscaled <- imputedscaled[-rand.ini,]
    
    representative <- list()
    p <- 1
    j <- 1;
    while (j < 21) {
      data.imp <- trainingscaled
      # turn factors into numeric.
      for (i in c(1:50)) {
        data.imp[,i] <- as.numeric(as.character(data.imp[,i]))
      }
      
      # Z-score transformation
      #data.imp <- scale(data.imp)
      
      # -------------- Find optimal number of Clusters ------------
      #optimal.clustering <- Optimal_Clusters_KMeans(data = imputedscaled, max_clusters = 30, criterion = "dissimilarity", num_init = 10, max_iters = 200,
      #initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
      #verbose = FALSE, tol_optimal_init = 0.3, seed = 1)
      
      
      # ------------- Start Clustering -------------
      
      
      kmeans <- KMeans_rcpp(data = data.imp, 5, num_init = p, max_iters = 10,
                            initializer = "kmeans++", fuzzy = FALSE, verbose = FALSE,
                            CENTROIDS = NULL, tol = 1e-04, tol_optimal_init = 0.3, seed = r+2)
      
      clustervector <- kmeans$clusters               # ------- vector of the clusters that each point belongs to.
      obs.percluster <- kmeans$obs_per_cluster       # ------- Number of Observations/datapoints per Cluster.
      print(obs.percluster)
      # Remake the factors -------->
      data.imp <- as.data.frame(data.imp)
      data.imp$V51 <- clustervector          # ------- Insert vector with clusters
      
      cols <- sprintf("V%s", 1:23)  # create vector of column names.
      data.imp[cols] <- lapply(data.imp[cols], factor)   # turn columns into factors. !!
      
      cols <- sprintf("V%s", 27:29)  # create vector of column names.
      data.imp[cols] <- lapply(data.imp[cols], factor)   # turn columns into factors. !!
      
      cols <- sprintf("V%s", 50)  # create vector of column names.
      data.imp[cols] <- lapply(data.imp[cols], factor)   # turn columns into factors. !!
      
      data.imp$V50 <- factor(data.imp$V50,levels = c(-1.26855634846672,0.783520097582384),labels = c("Dies", "Survives")) # gör om faktorerna till lever eller dör
      
      cols <- sprintf("V%s", 51)  # create vector of column names.
      data.imp[cols] <- lapply(data.imp[cols], factor)   # turn columns into factors. !!
      
      data.imp$V51 <- factor(data.imp$V51,
                             levels = c(1,2,3,4,5),
                             labels = c("One", "Two", "Three", "Four", "Five")) # gör om faktorerna till "namn"
      
      # ------ Cluster-based SMOTE OVERSAMPLING -----------------------------------------------
      # ---------------------------------------------------------------------------------------
      
      CBSO <- function(data, clusters, k=7) {
        if (k < 20) {
          data.balanced <- SmoteClassif(V51~., data.imp, C.perc = list(One = max(obs.percluster)/obs.percluster[1], Two = max(obs.percluster)/obs.percluster[2], Three = max(obs.percluster)/obs.percluster[3], Four = 1, Five = max(obs.percluster)/obs.percluster[5]), k, repl = FALSE,
                                        dist = "HEOM")
          cat("j = ", j)  
          j <<- j + 1
          return(data.balanced)
        } else {
          return(c("One of the clusters is too small! Less than 3 observations!. Please rerun!"))
        }
      }
      
      data.balanced <- CBSO(data = data.imp, clusters = obs.percluster, k = 1)
      cat("p = ", p)
      p <<- p + 1
      if (class(data.balanced) == "character") {
        print("not enough clusters!")
      } else {
        data.balanced <- list(data.balanced)
        representative <- c(representative, data.balanced)
      }
    }
    
    
    # ------ ANVÄND STRATIFIED SAMPLING ??? --------- VIKTIGT !!!!
    
    
    # # -------------- CHECK SIZE OF EACH CLUSTER -------------
    # length(which(data.balanced$V51 == "One"))
    # length(which(data.balanced$V51 == "Two"))
    # length(which(data.balanced$V51 == "Three"))
    # length(which(data.balanced$V51 == "Four"))
    # length(which(data.balanced$V51 == "Five"))
    # length(which(data.balanced$V51 == "Six"))
    # length(which(data.balanced$V51 == "Seven"))
    # 
    # length(which(data.balanced$V50 == "Dies"))
    # length(which(data.balanced$V50 == "Survives"))
    
    
    # -------------------------------------------------------------------------------
    #------------ BEGIN SAMPLING FOR REPRESENTATIVE SET APPROACH --------------------
    rep.set <- data.frame()
    for (g in c(1:length(representative))) {
      rep.set <- rbind(rep.set, representative[[g]][sample(nrow(representative[[g]]),round(size=nrow(representative[[g]])*0.2),replace=FALSE),])
      
    }
    #BRA !!!  TESTA NU SVM ---------------------------------
    rep.set <- rep.set[,-51]
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 40,
      ## repeated ten times
      repeats = 1)
    
    svmFit <- train(V50 ~ ., data = rep.set, 
                    method = "svmRadial", 
                    trControl = fitControl,
                    tuneLength = 10,
                    metric = "Accuracy")
    
    trainingscaled <- as.data.frame(trainingscaled)
    testdata <- as.data.frame(testdata)
    
    cols <- sprintf("V%s", 1:23)  # create vector of column names.
    testdata[cols] <- lapply(testdata[cols], factor)   # turn columns into factors. !!
    
    cols <- sprintf("V%s", 27:29)  # create vector of column names.
    testdata[cols] <- lapply(testdata[cols], factor)   # turn columns into factors. !!
    
    cols <- sprintf("V%s", 50)  # create vector of column names.
    testdata[cols] <- lapply(testdata[cols], factor)   # turn columns into factors. !!
    
    testdata$V50 <- factor(testdata$V50,levels = c(-1.26855634846672,0.783520097582384),labels = c("Dies", "Survives")) # gör om faktorerna till lever eller dör
    
    
    cols <- sprintf("V%s", 50)  # create vector of column names.
    trainingscaled[cols] <- lapply(trainingscaled[cols], factor)   # turn columns into factors. !!
    
    trainingscaled$V50 <- factor(trainingscaled$V50,levels = c(-1.26855634846672,0.783520097582384),labels = c("Dies", "Survives")) # gör om faktorerna till lever eller dör
    
    compareSVM <- predict(svmFit, newdata = testdata)
    
    compareReal <- testdata[,50]
    
    #print(sum(c(compareSVM == compareReal)) / 35)
    
    TP <- sum(c((compareSVM == "Survives") & (compareReal == "Survives")))
    TN <- sum(c((compareSVM == "Dies") & (compareReal == "Dies")))
    FP <- sum(c((compareSVM == "Survives") & (compareReal == "Dies")))
    FN <- sum(c((compareSVM == "Dies") & (compareReal == "Survives")))
    
    
    F.score1 <- (2* TP)/(2*TP + FN + FP) 
    print(F.score1)
    
    F.score2 <- (2*TN)/(2*TN + FN + FP)
    print(F.score2)
    colnamez <- c("TP","TN","FP","FN","F.score1","F.score2")
    varde <- c(TP, TN, FP, FN, F.score1, F.score2)
    
    conf.matrix[r,] <- varde
  }
}
all.conf.matrix <- rbind(all.conf.matrix, conf.matrix)


