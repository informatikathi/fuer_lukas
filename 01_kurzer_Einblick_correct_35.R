rm(list=ls())
setwd("C:/Users/eurhope/Desktop/Bachelorarbeit/Code_R")
#setwd("C:/Users/eurhope/Documents/GitHub/kathi_ba")
#setwd("C:/Users/eurhope/Documents/GitHub/fuer_lukas")
library(stm)
library(xlsx)
library(stmBrowser)
#setwd("C:/Users/habet/Desktop/fuer_lukas")

source("r_funcs/load_data.R")
source("r_funcs/labelTopics_xlsx.R")

data = load_data("finaldata.feather", "export_to_r")
out <- prepDocuments(data$documents, data$vocab, data$meta, lower.thresh = 3) # default: lower.thresh=1 (words in only 1 doc will be dropped)
out_new <- out$meta[!duplicated(out$meta$raw),]
length(out$meta[!duplicated(out$meta$raw),])
load("models/correct_model_35.RData")
#load("results/eval_models_result.RData")
model_35_correct <- model

# Topics in Excel-Tabelle schreiben

labelTopics_xlsx(model, 5)

####################
#####stmBrowser#####
####################
#notwendig, weil stmBrowser noch nicht für R3.5.1 auf CTAN, also selber compilen.
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)

library(stmBrowser)
#Create stmBrowser Folder
stmBrowser(
  mod = model, 
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
  stmobj = model,
  formula = 1:model$settings$dim$K ~ s(year),
  metadata = out$meta,
  nsims=25
)

time_source_effect = estimateEffect(
  stmobj = model,
  formula = 1:model$settings$dim$K ~ s(year) + source,
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


###PLOT für alle Prävalenzverlaufe
# Mal ein paar Zeiteffekte plotten
# als method stehen zur verfügung:
# contiuous (für metrische Variablen)
# difference (für binäre Variablen oder solche, bei denen nur 2 Ausprägungen untersucht werden sollen)
# poinestimate (für kategoriale Variablen)
pdf("whatever.pdf")
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



#########################
#####VISUALISIERUNG#####
########################

topicNames<-c("Kirche","Antisemitismus","Betreuungsgeld","[noise]","Außenpolitik","*Literatur",
              "*Theater","abgehängtes_Pekariat","US-Wahlen","*Kino","*Kultur","Finanzpolitik",
              "Bildungsungleichheiten","USA_Rassismus","soz_Ungleichheit","[noise]","Ernährung",
              "Theorie","Straftaten","Asien", "Parteipolitik", "Erziehung", "*TV", "[noise]", 
              "GB", "*Rap", "Brasilien-WM", "*Unterhaltung", "Kindesmissbrauch", "Nahost",
              "Sarrazin", "[noise]", "Stadt", "[noise]", "[noise]" )

#Das sind meine Top10Topics, die ich gerne in so einem Plot hätte
top_ten <- c(32,8,13,21,15,34,10,7,6,26)

topic_labels = vector()
for (i in 1:length(top_ten)) {
  topic_labels[i] = topicNames[top_ten[i]]
}

pdf("top_ten.pdf") 
plot.STM(model,type="summary",custom.labels="",topics=top_ten,topic.names=topic_labels, main="Top 10 Topics")
dev.off()

pdf("top_ten_alt.pdf")
plot(model,type="summary",custom.labels="",topic.names=topicNames,topics=top_ten,main="Top 10 Topics")
dev.off()
#beide geben als Output einen Plot mit Topicnummer 1-10 