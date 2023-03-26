# T4Rstudio
tarea final curso de R Elias Intriago
head(tena)
str(tena)
dim(tena)
str(tena)
colnames(tena)
anuales<-tena[,c(1,2,15)]
str(anuales)
TS<-subset(anuales,anuales$PARAMETER=="TS")[,-c(1,2)]
RH2M<-subset(anuales,anuales$PARAMETER=="RH2M")[,-c(1,2)]
QV2M<-subset(anuales,anuales$PARAMETER=="QV2M")[,-c(1,2)]
PRECTOTCORR_SUM<-subset(anuales,anuales$PARAMETER=="PRECTOTCORR_SUM")[,-c(1,2)]
PRECTOTCORR<-subset(anuales,anuales$PARAMETER=="PRECTOTCORR")[,-c(1,2)]
sapply(anuales,class)


#MATRIZ/ CLASIFICACION POR GRUPOS
nombres<-as.character(anuales[1:41,2])
YEAR<-c(rep("1981-1989",9),rep("1990-1999",10),rep("2000-2010",11),rep("2011-2021",11))
class(YEAR)
matriz<-cbind.data.frame(TS,RH2M,QV2M,PRECTOTCORR_SUM,PRECTOTCORR,YEAR)
colnames(matriz)<-c("TS","RH2M","QV2M","PRECTOTCORR_SUM2","PRECTOTCORR","YEAR")
names(anuales)
length(anuales$YEAR)
str(matriz)

###ANALISIS EXPLORATORIO 

#ANALIS GENERAL MATRIZ

install.packages("carData")
library("carData")
install.packages("car")
library(car)
install.packages("psych")
library("psych")
describeBy(matriz, group = YEAR)   #descripción general medidas centrales
row.names(matriz)<-anuales$YEAR[1:41]
Boxplot(scale(matriz[,-6]),main="Todos los datos",col=rainbow(20))
library("psych")
matriz_sample1<-matriz[sample(1:dim(matriz)[1],size=41,replace=F),]
row.names<-paste(as.character(matriz$YEAR),1:41)                    
stars(matriz_sample1[1:5] , key.loc = c(-0.6, 2.35)) #diagrama de estrella



##DATOS ATÍPICOS & DIAGRAMA DE BIGOTES
#TEMPERATURA
boxplot(TS~YEAR,data=matriz,main= " TEMPERATURA",xlab= "Año",
        ylab="Temperatura por Año en °C"
        ,col=rainbow(10))

#HUMEDAD RELATIVA 
boxplot(RH2M~YEAR,data=matriz,main= " HUMEDAD RELATIVA",xlab= "AÑO",
        ylab="Humedad Relativa por Año en kJ/kg"
        ,col=rainbow(10))

#HUMEDAD ESECIFICA
boxplot(QV2M~YEAR,data=matriz,main= " HUMEDAD ESPECIFICA EN EL AMBIENTE",xlab= "AÑO",
        ylab="Humedad Especifica por Año en kJ/kg"
        ,col=rainbow(10))

#PRECIPITACIÓN  MEDIA MENSUAL 
boxplot(PRECTOTCORR~YEAR,data=matriz,main= " PRECIPITACIÓN",xlab= "AÑO",
        ylab="Precipitación en mm"
        ,col=rainbow(10))




##Contraste de Normalidad

class(matriz$YEAR)
install.packages("MVN")
library(MVN)
res<-mvn(matriz[,-6],mvnTest = "energy")
res$multivariateNormality

##Contraste de homogeniedad


install.packages("biotools")
library(biotools)
boxM(matriz[,-6],matriz$YEAR)


#ANALISIS DE COMPONENTES PRINCIPALES
#Exploración de las relaciones entre las variables
my_data <-matriz[,-c(6)]
head(my_data)
R<-round(cor(my_data),2);R
#Opcionales Gráficas para la correlación
#Correlation matrix
install.packages("GGally")
library("GGally")
ggcorr(my_data, palette = "RdBu", label = TRUE)

#Instrucción básica para hacer ACP
res.pca <- prcomp(my_data, scale = TRUE)
#El resumen del ACP y diagnóstico de la varianza absorbida
install.packages("devtools")
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
get_eig(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 85))
#Resumen para las variables y los individuos en el primer plano factorial
facto_summarize(res.pca, "var",matriz$YEAR,axes = c(1,2))
facto_summarize(res.pca, "ind", matriz$YEAR,axes = c(1,2))
#Gráficos clásicos del ACP para el primer plano factorial
fviz_pca_ind(res.pca, axes = c(1,2))

#Grafica
fviz_pca_var(res.pca,col.ind =matriz$YEAR,axes = c(1,2))

#Biplot de Componentes principales
fviz_pca_biplot(res.pca,axes=c(1,2))
#El Biplot de Componentes principales considerando variables ilustrativas (grupos)
fviz_pca_biplot(res.pca, col.ind =matriz$YEAR,axes = c(1,5), 
                col.var = "black",
                palette = "jco", 
                geom = "point",
                addEllipses = TRUE,
                ellipse.level=0.70,
                label=c("ind", "var"),
                title = "INDICE CLIMATOLOGICO")

#CLUSTER-KMEANS
#2. Compute dissimilarity matrix
install.packages("vegan")
install.packages("readxl")
library(cluster)
library("factoextra")
d <- dist(my_data, method = "euclidean");d
res.dist <- get_dist(my_data, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#NUMERO OPTIMO DE CLUSTER
fviz_nbclust(my_data, kmeans, method = "gap_stat")
#K means 
km.res <- kmeans(my_data, 3, nstart = 25)
fviz_cluster(km.res, data = my_data, frame.type = "convex",
             title = "Resultados clustering K-means") 
              
pam.res <- pam(my_data, 3)#ATIPICOS
# Visualize

fviz_cluster(pam.res, geom = "point", ellipse.type = "norm",
             show.clust.cent = TRUE,star.plot = TRUE)+
  labs(title = "Resultados clustering K-means")+ theme_bw()

# PCA
pca <- prcomp(matriz[,-6], scale=TRUE)
df.pca <- pca$x
# Cluster over the three first PCA dimensions
kc <- kmeans(df.pca[,1:5], 3)
fviz_pca_biplot(pca, label="var", habillage=as.factor(kc$cluster)) +
  labs(color=NULL) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))
