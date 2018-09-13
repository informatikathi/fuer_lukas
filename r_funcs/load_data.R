library(feather)
library(jsonlite)
library(hashmap)

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
