##### Cross Validation #####

# El siguiente código fue escrito por Manuel Amunategui, y puede encontrarse 
# su página de github: (http://amunategui.github.io/multinomial-neuralnetworks-walkthrough/)

totalAccuracy <- c()
cv <- 359
cvDivider <- floor(nrow(ech_wa) / (cv+1))

for (cv in seq(1:cv)) {
      # assign chunk to data test
      dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
      dataTest <- ech_wa[dataTestIndex,]
      
      # everything else to train
      dataTrain <- ech_wa[-dataTestIndex, ]
      crossval <- multinom(grupos_agnes_mah_redu_wa_6~ ., data=dataTrain, maxit=500, trace=F) 
      pred <- predict(crossval, newdata=dataTest, type="class")
      
      #  classification error
      cv_ac <- postResample(dataTest$grupos_agnes_mah_redu_wa_6, pred)[[1]]
      print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
      totalAccuracy <- c(totalAccuracy, cv_ac)
}

mean(totalAccuracy)

# Con cv=359, mean(totalAccuracy) = 0.7966574
