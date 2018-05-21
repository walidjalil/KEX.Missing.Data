rm(list=ls())   
HCC.data <- read.csv("~/HCC_R.txt", header=FALSE)                    # -- Load & Store the CSV file without headers.
HCC.test <- HCC.data

imp.method <- sprintf("logreg", 1:23)                             # Create a vector with 23 elements each containing string "logreg"
imp.method <- append(imp.method,"norm")                            # Append the vector with pmm (predictive mean matching)

for (i in 1:2){
  imp.method <- append(imp.method,"pmm")
}

for (i in 1:3){
  imp.method <- append(imp.method,"polr")
}

for (i in 1:2){
  imp.method <- append(imp.method,"pmm")
}

for (i in 1:2){
  imp.method <- append(imp.method,"norm")
}

for (i in 1:2){
  imp.method <- append(imp.method,"pmm")
}

imp.method <- append(imp.method,"norm")

for (i in 1:7){
  imp.method <- append(imp.method,"pmm")
}

imp.method <- append(imp.method,"polyreg")

for (i in 1:2){
  imp.method <- append(imp.method,"pmm")
}

for (i in 1:2){
  imp.method <- append(imp.method,"norm")
}

imp.method <- append(imp.method,"pmm")

imp.method <- append(imp.method,"")

cols <- sprintf("V%s", 1:23)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)   # turn columns into factors. !!

cols <- sprintf("V%s", 27:29)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)   # turn columns into factors. !!

cols <- sprintf("V%s", 44)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)

cols <- sprintf("V%s", 50)  # create vector of column names.
HCC.test[cols] <- lapply(HCC.test[cols], factor)

imputed.Data <- mice(HCC.test, m=4, maxit = 50, method = imp.method)

completeData <- complete(imputed.Data,2)

stripplot(imputed.Data, pch = 20, cex = 1.2)

