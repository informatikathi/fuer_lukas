library("jsonlite")
library("feather")
library("hashmap")
library("stm")
library("quanteda")
library("ggplot2")
library("data.table")
library("devtools")
#install_github("mroberts/stmBrowser",dependencies=TRUE)
library("stmBrowser")
#stmBrowser(data = "model_find_k_70_superset.RData")


rm(list=ls())
gc()
setwd("C:\\Users\\eurhope\\Desktop\\Bachelorarbeit\\Code_R")

#save.image("env.RData")
#load("env.rad)
load_data <- function(path, json_col){
  
  df = read_feather(path)
  
  start = Sys.time()
  
  #extract the metadata-columns (all except the JSON one)
  meta = df[,-which(names(df) == json_col)]
  
  #create named list as docs, where words are the names and 'number of occurence in doc' are values
  print("Parsing JSON...")
  docs = lapply(df[[json_col]], fromJSON)
  
  #create the sorted list of unique words in the corpus
  print("Creating vocab and hashtable ...")
  vocab = sort(unique(names(unlist(docs))))
  
  #create Mapping from words to numbers in vocab
  #hashmap is used for O(1) lookup complexity. Makes things a myriad times faster
  w2i = hashmap(vocab, 1:length(vocab))
  
  #create the progressbar for the upcoming loop
  print("Reformatting and remapping documents...")
  progressbar <- txtProgressBar(min = 1, max = length(docs), style = 3)
  
  #reformat the documents from lists to matrices, where the first row is the mapped word-index
  #and the second row is the 'number of occurence in doc' of each mapped word-index
  
  documents = list()
  for (index in 1:length(docs)) {
    setTxtProgressBar(progressbar, index)
    
    #map the words in a doc to their indices
    word_indexes = c()
    names = names(docs[[index]])
    for (i in 1:length(names)) {
      word_indexes[i] <- w2i[[names[i]]]
    }
    
    word_indexes <- as.integer(word_indexes)
    
    #get the values of the JSON converted list 'number of occurence in doc'
    word_counts = as.integer(unname(docs[[index]]))
    
    #create and assign matrix
    documents[[index]] <- unname(rbind(word_indexes, word_counts))
  }
  close(progressbar)
  end = Sys.time()
  print("Creation of data took:")
  print(end-start)
  
  
  return(list("documents" = documents, "vocab" = vocab, "meta" = meta))
  
}

data = load_data("data_fuer_lukas.feather", "export_to_r")

k_cand_grob <- c(40,50,60,70,80,90,100,110) 

for(k in k_cand_grob){
  model_grob <- stm(documents = out$documents, 
                            vocab = out$vocab,
                            K = k,
                            prevalence = ~s(out$meta$year) + out$meta$source,
                            max.em.its = 100,
                            data = out$meta,
                            init.type = "Spectral") 
  
  # save data
  save(model_grob ,out, file = paste("model_grob_",as.character(k),".RData", sep = ""))
  save.image()
  gc()
}

for(k in k_cand_fein){
  model_fein <- stm(documents = out$documents, 
                    vocab = out$vocab,
                    K = k,
                    prevalence = ~s(out$meta$year) + out$meta$source,
                    max.em.its = 100,
                    data = out$meta,
                    init.type = "Spectral") 
  
  # save data
  save(model_fein ,out, file = paste("model_fein_",as.character(k),".RData", sep = ""))
  save.image()
  gc()
}