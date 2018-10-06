library(stm)

read_model_data <- function(path, data) {
  load(path)
  
  bound <- max(model$convergence$bound) + lfactorial(model$settings$dim$K)
  coherence = semanticCoherence(model, data$documents, M=7)
  excl = exclusivity(model, M=25)
  resid = checkResiduals(model, data$documents, tol=0.01)
  
  
  rm(model)
  gc()
  
  return(list("bound" = bound,"coherence"=coherence, "exclusivity"=excl, "residuals"=resid))
}
