## Esse código pertence a Victor Aguiar de Souza Penha
## Código para o TCC para a especialização em Ciências de dados pela PUC Minas
## Data da entrega: 15/08/2021

## Início do código
## Pacotes necessários
library(dplyr)
library(tidyr)
## Primeiramente, vamos remover resquícios de análises anteriores
rm(list=ls())

## Importando os dados
## Dados sobre saúde dos municípios dos EUA:
saude <- read.csv("/Users/victoraguiardesouzapenha/Desktop/TCC PUC MG/BASES/US/Social Health/us_county_sociohealth_data.csv")

## Dados sobre votos nos EUA nas eleições de 2021
votos <- read.csv("/Users/victoraguiardesouzapenha/Desktop/TCC PUC MG/BASES/US/Votes and COVID19/county_statistics.csv")

## Checar quais municípios estão presentes em todas as bases
saude <- saude[-which(!saude$county %in% votos$county),]
votos <- votos[-which(!votos$county %in% saude$county),]

## Checando se todos em 1 estão em 3 e vice-versa
all(saude$county %in% votos$county)
all(votos$county %in% saude$county)

## Todos os dados das duas planilhas estão presentes nas duas

## Vamos manipular as planilhas:
dim(saude)
dim(votos)
## Planilha d votos é um pouco maior que a planilha de saúde

## Checar se tem algum municipio duplicado 
duplicated(saude$county)
any(duplicated(paste0(saude$state, " ",saude$county)))
any(duplicated(paste0(votos$state, " ",votos$county)))

unique(saude$state)
unique(votos$state)

## Colocar nome dos estados na planilha de votos
nome <- read.csv("/Users/victoraguiardesouzapenha/Desktop/TCC PUC MG/BASES/US/State_Ab.csv")
which(!votos$state %in% nome$Ab)
votos$State_Name <- nome[match(votos$state, nome$Ab),"State"]

## Checar quais colunas estão em ambas
all(votos$State_Name %in% saude$state)
all(saude$state %in% votos$State_Name)
## Quais estados não tem dados iguais em ambos?
a <- table(saude$state) == table(votos$State_Name)
which(a == FALSE)

length(which(votos$State_Name == "Connecticut"))
length(which(saude$state == "Connecticut"))
## 45 e 8
length(which(votos$State_Name == "Louisiana"))
length(which(saude$state == "Louisiana"))
## 65 e 64
length(which(votos$State_Name == "Maine"))
length(which(saude$state == "Maine"))
## 112 e 16
length(which(votos$State_Name == "Massachusetts"))
length(which(saude$state == "Massachusetts"))
## 85 e 14
length(which(votos$State_Name == "New Hampshire"))
length(which(saude$state == "New Hampshire"))
## 76 e 10
length(which(votos$State_Name == "Rhode Island"))
length(which(saude$state == "Rhode Island"))
## 11 e 5
length(which(votos$State_Name == "Vermont"))
length(which(saude$state == "Vermont"))
## 66 e 14
length(which(votos$State_Name == "Virginia"))
length(which(saude$state == "Virginia"))
## 110 e 99

## Vamos retirar os dados de ambas e juntar para poder rodar as análises
votos_filt <- votos[-which(votos$State_Name == "Connecticut" | votos$State_Name == "Louisiana" | 
                                votos$State_Name == "Maine" | 
                                votos$State_Name == "Massachusetts" | votos$State_Name == "New Hampshire" | 
                                votos$State_Name == "Rhode Island" | votos$State_Name == "Vermont" | 
                                votos$State_Name == "Virginia"),]
saude_filt <- saude[-which(saude$state == "Connecticut" | saude$state == "Louisiana" | 
                                   saude$state == "Maine" | 
                                   saude$state == "Massachusetts" | saude$state == "New Hampshire" | 
                                   saude$state == "Rhode Island" | saude$state == "Vermont" | 
                                   saude$state == "Virginia"),]
dim(votos_filt)
dim(saude_filt)

## Ambos tem a mesma dimensão
votos_filt <- votos_filt[with(votos_filt, order(votos_filt$State_Name)),]
saude_filt <- saude_filt[with(saude_filt, order(saude_filt$state)),]

## Checar se tem nome de cidades diferentes
all(votos_filt$county %in% saude_filt$county)
all(saude_filt$county %in% votos_filt$county)

## Unir estado e cidade para checar duplicados em ambas
votos_filt$NomeComp <- paste0(votos_filt$State_Name, " ",votos_filt$county)
saude_filt$NomeComp <- paste0(saude_filt$state, " ",saude_filt$county)

any(duplicated(votos_filt$NomeComp))
any(duplicated(saude_filt$NomeComp))
## Não tem municipios duplicados
## Juntar pelo nome de municipios e estados
dim(merge(votos_filt, saude_filt, by="NomeComp"))
df <- merge(votos_filt, saude_filt, by="NomeComp")
dim(df)

## Ultima checagem
which(colnames(df) == "county.x")
which(colnames(df) == "county.y")

all(df[,3] == df[,56])
all(df[,56] == df[,3])

## Perfeito, ambas planilhas estão unidas com dados ordenados e em ordem correta
#write.csv(df, "/Users/victoraguiardesouzapenha/Desktop/Planilha.csv")
###############################################################

## Para a produção do mapa
library(rgdal)
library(raster)
library(ggplot2)
library(mapproj)
library(viridis)

## Pegando o arquivo em shapefile
mapa <- readOGR("/Users/victoraguiardesouzapenha/Desktop/TCC PUC MG/Shape/USA_States/USA_States.shp")

## Plotando o mapa
jpeg("/Users/victoraguiardesouzapenha/Desktop/Mapa1.jpg", width = 6, height = 5, units = 'in', res = 800) 
ggplot() + geom_polygon(data = mapa, aes(x=long, y = lat,  group = group)) + theme_bw() +
  geom_point(data=df, aes(x=long, y=lat.x), size = 0.05, col = "red") + ylab("Latitude") + xlab("Longitude")
dev.off()

############################# Importação da planilha final:
dados <- read.csv("/Users/victoraguiardesouzapenha/Desktop/Planilha_Final.csv", sep = ";")
str(dados)

## Dimensionamento da planilha
dim(dados)
## Checagem dos dados

## Primeiramente, há alguns dados que estão com as classes diferentes do que podiam estar, vamos corrigir:
dados$IncomePerCap <- as.numeric(dados$IncomePerCap)
dados$TotalPop <- as.numeric(dados$TotalPop)
dados$deaths <- as.numeric(dados$deaths)
dados$cases <- as.numeric(dados$cases)

## Checar dados ausentes:
all(complete.cases(dados$NomeComp)) ## todos os dados estão presentes

## Variável resposta:
all(complete.cases(dados$deaths)) ## todos os dados estão presentes

## Transformar variávei politica para democratas e republicanos
length(which(dados$percentage20_Donald_Trump >= 0.500))
length(which(dados$percentage20_Donald_Trump < 0.500))


length(which(dados$percentage20_Joe_Biden >= 0.500))

length(which(!complete.cases(dados$percentage20_Donald_Trump)))
length(which(!complete.cases(dados$percentage20_Joe_Biden)))
2412 + 387 + 34

## Coluna nova
dados$VisaoPolitica <- NA
dados[which(dados$percentage20_Donald_Trump >= 0.500),"VisaoPolitica"] <- "Republicano"
dados[which(dados$percentage20_Joe_Biden >= 0.500),"VisaoPolitica"] <- "Democrata"

table(dados$VisaoPolitica)
## Vamos retirar os dados que não tem dados para essa questão:
length(which(!complete.cases(dados$VisaoPolitica)))
## Somente 79 dados ausentes aqui
dados <- dados[-which(!complete.cases(dados$VisaoPolitica)),]

## Checar de novo
any(is.na(dados$VisaoPolitica))
table(dados$VisaoPolitica)
## Temos muito mais municípios que são republicanos que democratas
## Vamos analisar:
library(ggplot2)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig1.jpg", width = 4, height = 4, units = 'in', res = 800) 
ggplot(data = dados, aes(x = VisaoPolitica, y = log(TotalPop))) + 
  geom_jitter(size = 0.5, position = position_jitter(0.2)) + geom_boxplot(alpha = 0.5) + theme_bw() + 
  xlab("Visão Política majoritária") + ylab("Total de indivíduos no município")
dev.off()

## Agora vamos checar se todas as outras variaveis tme dados
all(complete.cases(dados$cases)) ## sim
all(complete.cases(dados$deaths)) ## sim
all(complete.cases(dados$TotalPop)) ## sim
all(complete.cases(dados$IncomePerCap)) ## sim
all(complete.cases(dados$Poverty)) ## sim
all(complete.cases(dados$ChildPoverty)) ## sim
all(complete.cases(dados$Unemployment)) ## sim
all(complete.cases(dados$population_density_per_sqmi)) ## sim
all(complete.cases(dados$percent_no_highschool_diploma)) ## sim
all(complete.cases(dados$percent_age_65_and_older)) ## sim
all(complete.cases(dados$percent_rural)) ## NÃO
all(complete.cases(dados$percent_smokers)) ## sim
all(complete.cases(dados$percent_adults_with_obesity)) ## sim
all(complete.cases(dados$percent_physically_inactive)) ## sim
all(complete.cases(dados$percent_excessive_drinking)) ## sim
all(complete.cases(dados$life_expectancy)) ## NÃO
all(complete.cases(dados$percent_adults_with_diabetes)) ## sim
all(complete.cases(dados$percent_food_insecure)) ## sim
all(complete.cases(dados$violent_crime_rate)) ## NÃO
all(complete.cases(dados$homicide_rate)) ## NÃO

## Quais não tem dados: percent_rural; life_expectancy; violent_crime_rate; homicide_rate
## Vamos avaliar a quantidade de dados ausentes
## percent_rural
which(!complete.cases(dados$percent_rural)) ## só 1, dá pra retirar
dados <- dados[-which(!complete.cases(dados$percent_rural)),]

## life_expectancy
length(which(!complete.cases(dados$life_expectancy))) ## 64 sem dados: 
## Vamos ver a distribuição dos dados, onde estão ausentes:
dados[which(!complete.cases(dados$life_expectancy)),"NomeComp"]
## Maior parte está em Nebraska, Montana e Texas
## Vamos utilizar a imputação mediana
dados[which(!complete.cases(dados$life_expectancy)),"life_expectancy"] <- median(dados$life_expectancy, na.rm = TRUE)
## Checar de novo:
which(!complete.cases(dados$life_expectancy)) ## deu certo

## violent_crime_rate
length(which(!complete.cases(dados$violent_crime_rate)))
## Vams checar onde estão:
dados[which(!complete.cases(dados$violent_crime_rate)),"NomeComp"] ## Tem vários: vamos usar mesmo método
## Imputação
dados[which(!complete.cases(dados$violent_crime_rate)),"violent_crime_rate"] <- median(dados$violent_crime_rate, na.rm = TRUE)
length(which(!complete.cases(dados$violent_crime_rate)))

## homicide_rate
length(which(!complete.cases(dados$homicide_rate))) ## Não tem mais da metade dos dados, não vamos utilizar essa variável
colnames(dados)
dados <- dados[,-23]
## coluna retirada

## Agora vamos checar se todos tem dados
all(complete.cases(dados)) ## SIM

colnames(dados)
dados <- dados[,-c(2,3)]
## Vamos analisar a distribuição de dados
hist(scale(dados$population_density_per_sqmi))
hist(log(dados$cases))
hist(log(dados$deaths))
hist(log(dados$TotalPop))
hist(log(dados$IncomePerCap))
hist(sqrt(dados$Poverty))
hist(sqrt(dados$ChildPoverty))
hist(sqrt(dados$Unemployment))
hist(sqrt(dados$violent_crime_rate))
hist(sqrt(dados$percent_no_highschool_diploma))
hist(sqrt(dados$percent_age_65_and_older))
hist(sqrt(dados$percent_adults_with_obesity))
hist(sqrt(dados$percent_physically_inactive))
hist(sqrt(dados$percent_adults_with_diabetes))
hist(log(dados$percent_food_insecure))

## Distribuições nem tão boas
hist(sqrt(dados$percent_smokers))
hist(sqrt(dados$percent_excessive_drinking))
hist(scale(dados$life_expectancy/100000000))
hist(scale(dados$percent_rural))

## Análise de multicolinearidade
library(psych)

## Grupo economia:
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig2.jpg", width = 7, height = 7, units = 'in', res = 800) 
pairs.panels(dados[,c("IncomePerCap", "Poverty", "ChildPoverty", "Unemployment", "population_density_per_sqmi",
                      "percent_no_highschool_diploma", "percent_age_65_and_older", "percent_rural")], method = "spearman")
dev.off()


## Análise de componentes principais para reduzir a dimensionaidade das variáveis com alta auto-correlação
library(stats)
colnames(dados)
pca.economia <- prcomp(dados[,c("IncomePerCap", "Poverty", "ChildPoverty", "Unemployment", "population_density_per_sqmi",
                       "percent_no_highschool_diploma", "percent_age_65_and_older", "percent_rural")], scale = T)
summary(pca.economia)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig3.jpg", width = 7, height = 7, units = 'in', res = 800) 
biplot(pca.economia)
dev.off()
library(factoextra)
fviz_eig(pca.economia)
dados$ECO_PC1 <- pca.economia$x[,1]
dados$ECO_PC2 <- pca.economia$x[,2]

## Distribuição dos dados
a <- ggplot(data = dados, aes(x = ECO_PC1)) + geom_histogram() + theme_bw()
b <- ggplot(data = dados, aes(x = ECO_PC2)) + geom_histogram() + theme_bw()
library(ggpubr)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig4.jpg", width = 7, height = 7, units = 'in', res = 800) 
ggarrange(a,b,ncol=2)
dev.off()


### Grupo de saúde:
## Grupo saúde:
colnames(dados)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig5.jpg", width = 7, height = 7, units = 'in', res = 800) 
pairs.panels(dados[,c("percent_smokers", "percent_adults_with_obesity", "percent_physically_inactive", "percent_excessive_drinking", 
                      "life_expectancy",
                      "percent_adults_with_diabetes", "percent_food_insecure")], method = "spearman")
dev.off()

## Análise de componentes principais para reduzir a dimensionaidade das variáveis com alta auto-correlação para o grupo de saúde
library(stats)
colnames(dados)
pca.saude <- prcomp(dados[,c("percent_smokers", "percent_adults_with_obesity", "percent_physically_inactive", "percent_excessive_drinking", 
                                "life_expectancy",
                                "percent_adults_with_diabetes", "percent_food_insecure")], scale = T)
summary(pca.saude)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig6.jpg", width = 7, height = 7, units = 'in', res = 800) 
biplot(pca.saude)
dev.off()
library(factoextra)
fviz_eig(pca.saude)
dados$SAU_PC1 <- pca.saude$x[,1]
dados$SAU_PC2 <- pca.saude$x[,2]

## Distribuição dos dados
c <- ggplot(data = dados, aes(x = SAU_PC1)) + geom_histogram() + theme_bw()
d <- ggplot(data = dados, aes(x = log10(SAU_PC2))) + geom_histogram() + theme_bw()
library(ggpubr)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig7.jpg", width = 7, height = 7, units = 'in', res = 800) 
ggarrange(c,d,ncol=2)
dev.off()

### VIF
library(regclass)
modelo <- glm(sqrt(deaths) ~ ECO_PC1+ECO_PC2+SAU_PC1+scale(SAU_PC2)+VisaoPolitica+sqrt(violent_crime_rate), data = dados)
VIF(modelo)

## Análises exploratórias: 
## Matriz de correlação entre as variáveis restantes
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig8.jpg", width = 7, height = 7, units = 'in', res = 800) 
pairs.panels(dados[,c("VisaoPolitica", "ECO_PC1", "ECO_PC2", "SAU_PC1", 
                      "SAU_PC2","violent_crime_rate")], method = "spearman")
dev.off()
colnames(dados)
## Análise par-a-par entre variavel resposta e explicativas
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig9.jpg", width = 4, height = 4, units = 'in', res = 800) 
ggplot(data = dados, aes(x = VisaoPolitica, y = log(deaths/TotalPop))) + 
  geom_jitter(position = position_jitter(0.2), size = 0.5) + theme_bw() +geom_boxplot(alpha=0.5) 
dev.off()

## Economia
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig10.jpg", width = 8, height = 4, units = 'in', res = 800) 
e <- ggplot(data = dados, aes(x = ECO_PC1, y = log(deaths/TotalPop))) + theme_bw() + 
  geom_point() + geom_smooth(method = "glm", color = "black", se = F)
f <- ggplot(data = dados, aes(x = ECO_PC2, y = log(deaths/TotalPop))) + theme_bw() + 
  geom_point() + geom_smooth(method = "glm", color = "black", se = F)
ggarrange(e,f, ncol=2)
dev.off()

## Saúde:
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig11.jpg", width = 8, height = 4, units = 'in', res = 800) 
g <- ggplot(data = dados, aes(x = SAU_PC1, y = log(deaths/TotalPop))) + theme_bw() + 
  geom_point() + geom_smooth(method = "glm", color = "black", se = F)
h <- ggplot(data = dados, aes(x = log10(SAU_PC2), y = log(deaths/TotalPop))) + theme_bw() + 
  geom_point() + geom_smooth(method = "glm", color = "black", se = F)
ggarrange(g,h, ncol=2)
dev.off()

## Seguranca:
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig12.jpg", width = 4, height = 4, units = 'in', res = 800) 
ggplot(data = dados, aes(x = sqrt(violent_crime_rate), y = log(deaths/TotalPop))) + theme_bw() + 
  geom_point() + geom_smooth(method = "glm", color = "red", se = F)
dev.off()

####################################################################################################
## Machine learning

## Vamos primeiro pegar os dados do estado somente
df2 <- df[which(df$NomeComp %in% dados$NomeComp),]
dados$Estado <- df2$state.x

## Pesando a variável resposta pelo total da população
dados$Morte_Pesada <- (dados$deaths / dados$TotalPop)*100

## Checando novamente a distribuição
hist(log(dados$Morte_Pesada))

## Removendo dados que ferem a correção
dados2 <- dados[-which(log(dados$Morte_Pesada) == -Inf),]

dados2$Morte_Cr <- log(dados2$Morte_Pesada)
library(ggplot2)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig13.jpg", width = 4, height = 4, units = 'in', res = 800) 
ggplot(data = dados2, aes(x=Morte_Cr)) + geom_histogram() + theme_bw()
dev.off()

## Regressão linear
library(caTools)
amostragem <- sample.split(dados2$deaths, SplitRatio = 0.2)
validacao <- subset(dados2, amostragem == TRUE)
dado_efetivo <- subset(dados2, amostragem == FALSE)

## Modelo propriamente dito
library(lme4)

## Proposição dos modelos
## Em primeiro lugar, com as métricas para os modelos:
library(caret)
library(e1071)
control <- trainControl(method="cv", number=6)
metric <- "RMSE"

## Algoritmo linear
modelo.linear <- train(Morte_Cr ~ scale(ECO_PC1)+scale(ECO_PC2)+scale(SAU_PC1)+scale(SAU_PC2)+
                         VisaoPolitica+scale(violent_crime_rate), data=validacao, method="glm", metric=metric, trControl=control)
summary(modelo.linear)
## Support vector machine
modelo.SVM <- train(Morte_Cr ~ scale(ECO_PC1)+scale(ECO_PC2)+scale(SAU_PC1)+scale(SAU_PC2)+
                         VisaoPolitica+scale(violent_crime_rate), data=validacao, method="svmRadial", metric=metric, trControl=control)
plot(varImp(modelo.RF))
## Random Forest
modelo.RF <- train(Morte_Cr ~ scale(ECO_PC1)+scale(ECO_PC2)+scale(SAU_PC1)+scale(SAU_PC2)+
                      VisaoPolitica+scale(violent_crime_rate), data=validacao, method="rf", metric=metric, trControl=control)

## Comparando resultados:
results <- resamples(list(glm=modelo.linear, svm=modelo.SVM, rf=modelo.RF))
summary(results)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig14.jpg", width = 6, height = 6, units = 'in', res = 800) 
dotplot(results, ncol=3)
dev.off()

## Aparentemente o melhor modelo é o RF
print(modelo.RF)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig15.jpg", width = 6, height = 6, units = 'in', res = 800) 
plot(modelo.RF)
dev.off()

## Residuos do modelo
residuo <- as.data.frame(residuals(modelo.RF))
head(residuo)
## Plotando os resíduos do modelo
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig16.jpg", width = 6, height = 6, units = 'in', res = 800) 
ggplot(data = residuo, aes(residuals(modelo.RF))) + geom_histogram()
dev.off()

## Valor predito
predito <- predict(modelo.RF, dado_efetivo)


## Valor de p para o modelo random farest
library(rfUtilities)
library(randomForest)
modelo.RF2 <- randomForest(Morte_Cr ~ scale(dado_efetivo$ECO_PC1)+dado_efetivo$ECO_PC2+
                             scale(dado_efetivo$SAU_PC1)+scale(dado_efetivo$SAU_PC2)+
                             dado_efetivo$VisaoPolitica+dado_efetivo$violent_crime_rate, data=dado_efetivo, importance = T, mtry = 6)
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig17.jpg", width = 12, height = 4, units = 'in', res = 800) 
varImpPlot(modelo.RF2)
dev.off()

importance(modelo.RF2, type = 2)
modelo.RF2$importance

## Variable significance
varsig <- rf.significance(modelo.RF2,dado_efetivo[,c("ECO_PC1","ECO_PC2","SAU_PC1")])

## Checando significancia das variáveis
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig17.jpg", width = 6, height = 6, units = 'in', res = 800) 
varImpPlot(modelo.RF2, pch = 20, main = "Importance of Variables")
dev.off()

jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig18.jpg", width = 6, height = 6, units = 'in', res = 800) 
plot(modelo.RF2, main = "Error rate of random forest")
dev.off()

library(ggRandomForests)
## Plot das variáveis importântes
jpeg("/Users/victoraguiardesouzapenha/Desktop/Fig20.jpg", width = 7, height = 5, units = 'in', res = 800) 
p1 <- plot(gg_variable(modelo.RF2), xvar="ECO_PC1", method = "glm") + theme_bw() 
p2 <- plot(gg_variable(modelo.RF2), xvar="SAU_PC1", method = "glm") + theme_bw() 
p3 <- plot(gg_variable(modelo.RF2), xvar="ECO_PC2", method = "glm") + theme_bw() 
library(ggpubr)
ggarrange(p1,p2,p3, ncol = 2, nrow=2)
dev.off()

write.csv(dados2, "/Users/victoraguiardesouzapenha/Desktop/Final.csv")
## Fim do código