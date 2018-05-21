
imputed_Data <- mice(HCC_t, m=2, maxit = 50, seed = 500)

completeData <- complete(imputed_Data,2)