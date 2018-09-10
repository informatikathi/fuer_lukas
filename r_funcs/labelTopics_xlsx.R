library(xlsx)


labelTopics_xlsx <- function(model, n) {

  topics <-labelTopics(model, n=n)
  df = data.frame()
  
  for (index in 1:length(topics$topicnums)) {
    prob_str = paste(topics$prob[index,],collapse=", ")
    frex_str = paste(topics$frex[index,],collapse=", ")
    lift_str = paste(topics$lift[index,],collapse=", ")
    score_str = paste(topics$score[index,],collapse=", ")
    
    df = rbind(df,c(prob_str, frex_str, lift_str, score_str), stringsAsFactors = FALSE)
  }
  colnames(df) <- c("Prob", "frex", "lift", "score")
  write.xlsx(df, paste0("Topics_k_",length(topics$topicnums),".xlsx"))


}