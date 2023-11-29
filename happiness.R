library(mclust)
library(magrittr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(NbClust)
library(cluster)
library(dplyr)
library(factoextra)
library(lme4) 
library(haven) 
library(clustvarsel)
library(plotly)
library(corrplot)
library(reshape2)

# Analisi descrittiva -----------------------------------------------------

#CARICHIAMO IL DATASET
data <-X2018


#PLOT DELLE CORRELAZIONI DELLE VARIABILI
#ANDAMENTO LINEARE ( ES: LIFE EXPECTANCHY)
#GRUPPI CON DENSITA DIFFERENTI 
#PUNTI ANOMALI
plot(data[,-c(1,2)])

#Controllo i valori mancanti 
missing_values_per_variable <- colSums(is.na(data))
print(missing_values_per_variable)


#Creo un barplot dei valori assoluti di score in ordine decrescente
#GREFICO DEL LIVELLO DI FELICITA DECRESCENTE
#NOTIAMO UNA DISCESA UNIFORME
#2 GRADINI 
Scoreplot <- ggplot(data, aes(x = reorder(`Country.or.region`, -Score), y = Score, fill = Score)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  labs(title = "Country or Region and Happiness Score",
       x = "Country or region",
       y = "Happiness Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient(low = "darkblue", high = "lightcoral") 

print(Scoreplot)

#i 20 più FELICI
top_countries <- head(data[order(-data$Score), ], 20)
print(top_countries)

#i 20 meno FELICI
bottom_countries <- head(data[order(data$Score), ], 20)
print(bottom_countries)

#---------------------------------------------------------------------------

#GRAFICO DELLA COMPOSIZIONE DELLE VARIABILI 
#da questo grafico vediamo ogni occorrenza come è formata

#somma delle variabili di interesse
data$Sum_Variables <- rowSums(data[, c("GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")], na.rm = TRUE)
melted_data <- reshape2::melt(data, id.vars = c("Country.or.region", "Sum_Variables"), measure.vars = c("GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption"))
my_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")

# grafico a barre con colori diversi per ogni variabile
Compplot <- ggplot(melted_data, aes(x = reorder(`Country.or.region`, -Sum_Variables), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_palette) +
  labs(title = "Grafico composizione Score",
       x = "Country",
       y = "Valore") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Etichette a 90 gradi
        legend.position = "top", legend.title = element_blank())

print(Compplot)

# Regressioni -------------------------------------------------------------

# analiziamo alcune regressioni per vedere le correlazioni tra le variabili 
# per tutte le regressioni abbiamo diviso 
# blu : i 20 peasei meno felici
# rosse : i 20 paesei piu felici


top_20 <- head(data[order(-data$Score), ], 20)
bottom_20 <- head(data[order(data$Score), ], 20)

# Rapporto tra GDP e Score
# all'aumentare del gdp aumenta score (scontato)
# un punto anomalo rosso sono GLI EMIRATI ARABI
scatter_plot <- ggplot(data, aes(x = `GDP.per.capita`, y = Score, text = `Country.or.region`)) +
  geom_point(aes(color = ifelse(`Country.or.region` %in% top_20$`Country.or.region`, "Top 20 Score Alto", 
                                ifelse(`Country.or.region` %in% bottom_20$`Country.or.region`, "Top 20 Score Basso", "Altri Paesi"))),
             size = 2, alpha = 0.8) +
  scale_color_manual(values = c("gray", "red", "blue"), 
                     labels = c("Altri Paesi", "Top 20 Score Alto", "Top 20 Score Basso")) +
  labs(title = "Rapporto tra Score e GDP",
       x = "GDP per capita",
       y = "Score") +
  theme_minimal() +
  guides(color = FALSE)  

plotly11 <- ggplotly(scatter_plot, tooltip = "text")
print(plotly11)

#Rapporto tra Score e Percezione della corruzione

Scatter_corr <- ggplot(data, aes(x = `Perceptions.of.corruption`, y = Score, text = `Country.or.region`)) +
  geom_point(aes(color = ifelse(`Country.or.region` %in% top_20$`Country.or.region`, "Top 20 Score Alto", 
                                ifelse(`Country.or.region` %in% bottom_20$`Country.or.region`, "Top 20 Score Basso", "Altri Paesi"))),
             size = 2, alpha = 0.8) +
  scale_color_manual(values = c("gray", "red", "blue"), 
                     labels = c("Altri Paesi", "Top 20 Score Alto", "Top 20 Score Basso")) +
  labs(title = "Rapporto tra Score e Perceptions of corruption",
       x = "Perceptions of corruption",
       y = "Score") +
  theme_minimal() +
  guides(color = guide_legend(title = NULL))  # Rimuovere il titolo della legenda


plotly22 <- ggplotly(Scatter_corr, tooltip = "text")
print(plotly22)


#Rapporto tra Score e Social Support

Scatter_social <- ggplot(data, aes(x = `Social.support`, y = Score, text = `Country.or.region`)) +
  geom_point(aes(color = ifelse(`Country.or.region` %in% top_20$`Country.or.region`, "Top 20 Score Alto", 
                                ifelse(`Country.or.region` %in% bottom_20$`Country.or.region`, "Top 20 Score Basso", "Altri Paesi"))),
             size = 2, alpha = 0.8) +
  scale_color_manual(values = c("gray", "red", "blue"), 
                     labels = c("Altri Paesi", "Top 20 Score Alto", "Top 20 Score Basso")) +
  labs(title = "Rapporto tra Score e Social support ",
       x = "Social Support",
       y = "Score") +
  theme_minimal() +
  guides(color = guide_legend(title = NULL))  # Rimuovere il titolo della legenda


plotly33 <- ggplotly(Scatter_social, tooltip = "text")
print(plotly33)


# Scatter tra Freedom e GDP
Scatter_free_GDP <- ggplot(data, aes(x = `Freedom.to.make.life.choices`, y = `GDP.per.capita`, text = `Country.or.region`)) +
  geom_point(aes(color = ifelse(`Country.or.region` %in% top_20$`Country.or.region`, "Top 20 Score Alto", 
                                ifelse(`Country.or.region` %in% bottom_20$`Country.or.region`, "Top 20 Score Basso", "Altri Paesi"))),
             size = 2, alpha = 0.8) +
  scale_color_manual(values = c("gray", "red", "blue"), 
                     labels = c("Altri Paesi", "Top 20 Score Alto", "Top 20 Score Basso")) +
  labs(title = "Rapporto tra Freedom e GDP ",
       x = "Freedom",
       y = "GDP") +
  theme_minimal() +
  guides(color = guide_legend(title = NULL))  # Rimuovere il titolo della legenda


plotly44 <- ggplotly(Scatter_free_GDP, tooltip = "text")
print(plotly44)


#Freedom e generosità
Scatter_free_Gen <- ggplot(data, aes(x = `Freedom.to.make.life.choices`, y = `Generosity`, text = `Country.or.region`)) +
  geom_point(aes(color = ifelse(`Country.or.region` %in% top_20$`Country.or.region`, "Top 20 Score Alto", 
                                ifelse(`Country.or.region` %in% bottom_20$`Country.or.region`, "Top 20 Score Basso", "Altri Paesi"))),
             size = 2, alpha = 0.8) +
  scale_color_manual(values = c("gray", "red", "blue"), 
                     labels = c("Altri Paesi", "Top 20 Score Alto", "Top 20 Score Basso")) +
  labs(title = "Rapporto tra Freedom e Generosity ",
       x = "Freedom",
       y = "Generosity") +
  theme_minimal() +
  guides(color = guide_legend(title = NULL))  # Rimuovere il titolo della legenda


plotly55 <- ggplotly(Scatter_free_Gen, tooltip = "text")
print(plotly55)

#------------------------------------------------------------------------------------------
#ABBIAMO STANDARDIZZATO 
#PER CENTRARE LE MEDIE E LAVORARE SULLE STESSE SCALE 
#boxplot delle variabili
BOXPLOTORIG <- data[,-c(1,2)]%>%
  gather(Attributes, values) %>%
  ggplot(aes(x = reorder(Attributes, values, FUN = median), y = values, fill = Attributes)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Boxplot di Tutte le Variabili") +
  theme_bw() +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  coord_flip()

print(BOXPLOTORIG)

#Standardizzo le variabili e creo un subset con solo variabili continue 
datastan <- as.data.frame(scale(as.data.frame(data[,-c(1,2)])))

#boxplot delle variabili standardizzato 
BOXSTAND <- datastan%>%
  gather(Attributes, values) %>%
  ggplot(aes(x = reorder(Attributes, values, FUN = median), y = values, fill = Attributes)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Boxplot di Tutte le Variabili") +
  theme_bw() +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  coord_flip()

print(BOXSTAND)
# Clustering --------------------------------------------------------------


# Metodo gerarchico 
DistEuc=dist(datastan,method="euclidean") #uso un dataset senza variabili categoriche 

EucWard=hclust(DistEuc,method="ward.D2",members=NULL)#scelgo metodo WARD



plot(EucWard)#ipotizzo 3 cluster
rect.hclust(EucWard, k=3, border = "red")
fviz_dend(EucWard, k = 3, show_labels = F, rect = T)#dendogramma colorato
TaglioEucWard=cutree(EucWard,k=3)

plot(datastan, col= TaglioEucWard)#il gruppo piu piccolo si distacca sempre, potrebbero essere anche solo 2 gruppi, ma con 3 dovremmo essere più accurati
NbClust(datastan, distance = "euclidean", min.nc = 2, max.nc = 12 , method = "ward.D2", index = "all")#propone 3 cluster


#K-means

fviz_nbclust(datastan,kmeans, method= "silhouette")#3 cluster
fviz_nbclust(datastan,kmeans, method= "wss")#3 cluster

kmeans_result <- kmeans(datastan, 3, nstart = 100)

cluster_visualization <- fviz_cluster(kmeans_result, data = datastan)
print(cluster_visualization)

cluster_occurrences <- table(kmeans_result$cluster)
# Visualizza la tabella delle occorrenze
print(cluster_occurrences)

#Creaiamo una colonna extra contenete il cluster di appartenenza
datastan$cluster <- kmeans_result$cluster
write.csv(datastan, "data_with_cluster.csv", row.names = FALSE)

#distribuzione di Score
x <- data$Score
hist(x,breaks=20,freq=FALSE)


CLUSTERING <- Mclust(x,G=3,modelNames = "V")
SEQ <- seq(min(x),max(x),length=1500)

DENS1 <- dnorm(SEQ,mean=CLUSTERING$parameters$mean[1],
               sd=sqrt(CLUSTERING$parameters$variance$sigmasq[1])) 
DENS2 <- dnorm(SEQ,mean=CLUSTERING$parameters$mean[2], 
               sd=sqrt(CLUSTERING$parameters$variance$sigmasq[2]))
DENS3 <- dnorm(SEQ,mean=CLUSTERING$parameters$mean[3], 
               sd=sqrt(CLUSTERING$parameters$variance$sigmasq[3]))

P1 <- CLUSTERING$parameters$pro[1]
P2 <- CLUSTERING$parameters$pro[2]
P3 <- CLUSTERING$parameters$pro[3]

DENSTOT <- P1*DENS1 + P2*DENS2 + P3*DENS3

lines(SEQ,DENSTOT)
lines(SEQ,P1*DENS1,col=2)
lines(SEQ,P2*DENS2,col=3)
lines(SEQ,P3*DENS3,col=4)

# Selezione delle variabili -----------------------------------------------
#HEATMAP
print(names(data))
#selezione solo le variabili di interesse
variables <- c("Score","GDP.per.capita","Social.support","Healthy.life.expectancy","Freedom.to.make.life.choices","Generosity","Perceptions.of.corruption")
correlation_data <- data[, variables]

# Calcola la matrice delle correlazioni
cor_matrix <- cor(correlation_data, use = "complete.obs")

#trasformo la matrice delle correlazioni in un formato lungo
melted_cor_matrix <- melt(cor_matrix)

#heatmap delle correlazioni con annotazioni dei valori
heatmap_correlation <- ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Aggiungi annotazioni dei valori
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Heatmap delle Correlazioni",
       x = "Variabile",
       y = "Variabile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1))

print(heatmap_correlation)


#SELEZIONE CON BORUTA
install.packages('Boruta')
library(Boruta)
install.packages("ranger")
library(ranger)



boruta_result <- Boruta(Score ~ ., data = datastan[,-c(8,9)], doTrace = 2)
print(boruta_result)

# Visualizza il grafico delle variabili rilevanti
plot(boruta_result, cexAxis = 0.7, las = 2, xlab = "", main = "Boruta Analysis: Importance of Variables")




#CLUSVARSEL
datastanclean <- datastan[, -c(8, 9)]

cluststanDBACK <- clustvarsel(datastanclean,G=1:3, direction = "backward") #Seleziona: Perceptions of corruption, Freedom to make life choices, Generosity, Healthy life expectancy
#seleziona G= 3 e variabili 1,3,4,5,7
plot(cluststanDBACK$model)
cluststanDBACK$model$classification
cluststanDBACK$subset





