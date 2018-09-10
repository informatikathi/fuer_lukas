library(stm)
library(xlsx)
library(stmBrowser)

setwd("C:/Users/habet/Desktop/fuer_lukas")

rm(list=ls())
source("r_funcs/load_data.R")

data = load_data("data_fuer_lukas.feather", "export_to_r")
out <- prepDocuments(data$documents, data$vocab, data$meta, lower.thresh = 3) # default: lower.thresh=1 (words in only 1 doc will be dropped)

load("model_grob_50.RData")

# Topics in Excel-Tabelle schreiben
source("r_funcs/labelTopics_xlsx.R")
labelTopics_xlsx(model_grob, 5)

####################
#####stmBrowser#####
####################
#notwendig, weil stmBrowser noch nicht für R3.5.1 auf CTAN, also selber compilen.
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)

library(stmBrowser)
#Create stmBrowser Folder
stmBrowser(
  mod = model_grob, 
  data = out$meta, 
  covariates = c("year", "source"), 
  text = "raw", ######### dies muss eine Spalte in Meta sein, die die Dokumente in Rohfrom (als String) enthält.
  labeltype = "frex",
  n = 1000
)

# to remove stmBrowser-Folder
unlink("stm-visualization", recursive=TRUE)



####################
##estimateEffect####
####################

time_effect = estimateEffect(
  stmobj = model_grob,
  formula = 1:model_grob$settings$dim$K ~ s(year),
  metadata = out$meta,
  nsims=25
)

time_source_effect = estimateEffect(
  stmobj = model_grob,
  formula = 1:model_grob$settings$dim$K ~ s(year) + source,
  metadata = out$meta,
  nsims=25
)

####################
#####stminsights####
####################
library(stminsights)

#nach Möglichkeit nichts unnötiges ins Image mit aufnehmen, das wird sehr schnell sehr groß
rm(list=c("data", "labelTopics_xlsx", "load_data"))

# Wenn wordclouds gewünscht sind, vorher install.packages("wordcloud") ausführen
# Auch hier hilfreich, wenn die Originalartikel als Rohdaten in out$meta enthalten hat, 
# dann kann man sie mit anzeigen lassen.

# Das ist ein merkwürdiges Package. Es will ein Image, in dem das out enthalten ist (Variable MUSS out heißen)
# und will in dem selben Image noch ein oder mehrere Modelle (Variablen dürfen NICHT model heißen!)
# kann einen oder mehrere estimateEffect-Variablen haben.
save.image("for_stminsights.RData") 

#startet einen Server, der den Browser öffnet
run_stminsights()


# Mal ein paar Zeiteffekte plotten
# als method stehen zur verfügung:
# contiuous (für metrische Variablen)
# difference (für binäre Variablen oder solche, bei denen nur 2 Ausprägungen untersucht werden sollen)
# poinestimate (für kategoriale Variablen)

plot.new()
par(mfrow = c(4,5), mar=c(2,2,1.3,0.1))

for (i in 1:20) {
  plot(time_effect,
       covariate="year",
       topics = i,
       model = model_grob,
       method="continuous",
       main=paste("Topic", i),
       printlegend=FALSE
  )
}

#Um originale par() Einstellungen wieder herzustellen
dev.off()
