# rmsle analysis
best_rsmle <- 1
best_k<- 0

for (k in c(30:200)) {
  
  cur_close <- gglpost$close[1]
  gglpost$predicted <- cur_close
  for (i in c(1:(nrow(gglpost)-1))) {
    cur_close <- cur_close * (1 + (gglpost$pred[i]-0.5)/k);
    gglpost$predicted[i+1] <- cur_close;
  }
  
  cur_rmsle <- sqrt(mean((log(gglpost$predicted)-log(gglpost$close))^2,na.rm=TRUE))
  if (cur_rmsle < best_rsmle) {
    best_k <- k
    best_rsmle <- cur_rmsle
    print(best_k)
    print(best_rsmle)
  }
}

print(best_k)
# 112

# best difference prediction
best_rsmle <- 100000
best_k<- 0

for (k in c(30:200)) {
  
  cur_close <- gglpost$close[1]
  gglpost$predicted <- cur_close
  for (i in c(1:(nrow(gglpost)-1))) {
    cur_close <- cur_close * (1 + (gglpost$pred[i]-0.5)/k);
    gglpost$predicted[i+1] <- cur_close;
  }
  
  ###
  sum_diff <- 0
  for (i in c(1:(nrow(gglpost)-1))) {
    pred_diff <- abs(gglpost$predicted[i+1] - gglpost$predicted[i]);
    close_diff <- abs(gglpost$close[i+1] - gglpost$close[i]);
    sum_diff <- sum_diff + (log(pred_diff)-log(close_diff))^2;
  }
  
  cur_rmsle <- sqrt(sum_diff / (nrow(gglpost))-1)
  ###
  
  if (cur_rmsle < best_rsmle) {
    best_k <- k
    best_rsmle <- cur_rmsle
    print(best_k)
    print(best_rsmle)
  }
}

print(best_k)
# 54