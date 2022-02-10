library(ggplot2)
library(corrplot)
library(readr)
library(tm)
library(wordcloud)
library(RColorBrewer)

#Pregunta #1

# A)
head(stackloss)

#    Air.Flow  Water.Temp  Acid.Conc.  stack.loss
# 1        80         27         89         42
# 2        80         27         88         37
# 3        75         25         90         37
# 4        62         24         87         28
# 5        62         22         87         18
# 6        62         23         87         18

# B)

Air.Flow <- stackloss[ , 1]
Air.Flow          #[1] 80 80 75 62 62 62 62 62 58 58 58 58 58 58 50 50 50 50 50 56 70
min(Air.Flow)     #[1] 50   
max(Air.Flow)     #[1] 80
mean(Air.Flow)    #[1] 60.42857


Water.Temp <- stackloss[ , 2]
Water.Temp        #[1] 27 27 25 24 22 23 24 24 23 18 18 17 18 19 18 18 19 19 20 20 20 
min(Water.Temp)   #[1] 17
max(Water.Temp)   #[1] 27
mean(Water.Temp)  #[1] 21.09524

Acid.Conc <- stackloss[ , 3]
Acid.Conc         #[1] 89 88 90 87 87 87 93 93 87 80 89 88 82 93 89 86 72 79 80 82 91
min(Acid.Conc)    #[1] 72
max(Acid.Conc)    #[1] 93
mean(Acid.Conc)   #[1] 86.28571

Stack.Loss <- stackloss[ , 4]
Stack.Loss        #[1] 42 37 37 28 18 18 19 20 15 14 14 13 11 12  8  7  8  8  9 15 15
min(Stack.Loss)   #[1] 7
max(Stack.Loss)   #[1] 42
mean(Stack.Loss)  #[1] 17.52381

# C)
typeof(Air.Flow)   #[1] "double"
typeof(Water.Temp) #[1] "double"
typeof(Acid.Conc)  #[1] "double"
typeof(Stack.Loss) #[1] "double"

# D)
datos.correlacion <- round(cor(stackloss),2)  

#           Air.Flow  Water.Temp  Acid.Conc.  Stack.Loss
#Air.Flow       1.00       0.78       0.50       0.92
#Water.Temp     0.78       1.00       0.39       0.88
#Acid.Conc.     0.50       0.39       1.00       0.40
#stack.loss     0.92       0.88       0.40       1.00

#La mayor correlacion es el 0.92 entre Air Flow y Stack Loss

corrplot(datos.correlacion, method = "color")

# E)
linea.diagonal <- lm(stackloss[ , 4]~stackloss[ , 1])
plot(stackloss[ , 1], stackloss[ , 4], xlab="Air Flow", ylab="Stack Loss", main = "Gráfico de correlación entre Air Flow y Stack Loss.")
abline(linea.diagonal, col="blue")
# Según el gráfico, existe una correlación positiva significativa entre Air Flow y Stack Loss, lo que quiere decir que entre más flujo
# de aire, mayor es la cantidad de amoníaco que se escapa antes de ser absorbido

# Pregunta #2

ruta.oscar <- "C:\\Users\\Nayib\\Desktop\\Examen Big Data\\oscar_arias.txt"
ruta.guillermo <-"C:\\Users\\Nayib\\Desktop\\Examen Big Data\\laura_chinchilla.txt"
ruta.laura <-  "C:\\Users\\Nayib\\Desktop\\Examen Big Data\\laura_chinchilla.txt"
ruta.carlos <-"C:\\Users\\Nayib\\Desktop\\Examen Big Data\\carlos_alvarado.txt"

oscar.arias  <- suppressWarnings(paste(readLines(ruta.oscar), collapse = ""))
guillermo.solis <- suppressWarnings(paste(readLines(ruta.guillermo), collapse = ""))
carlos.alvarado <- suppressWarnings(paste(readLines(ruta.carlos), collapse = ""))
laura.chinchilla <- suppressWarnings(paste(readLines(ruta.laura), collapse = ""))


discursos <- data.frame(doc_id = c("OscarArias", "GuillermoSolis", "LauraChinchilla","CarlosAlvarado"),
                        text = c(oscar.arias, guillermo.solis, laura.chinchilla, carlos.alvarado),
                        stringsAsFactors = F)

ds.discursos <- DataframeSource(discursos)

corpus.discursos <- Corpus(ds.discursos)

limpiar_corpus <- function (wordCorpus) {
  wordCorpus <- tm_map(wordCorpus, removeNumbers)
  wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
  wordCorpus <- tm_map(wordCorpus, removePunctuation)
  wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("spanish"))
  return (wordCorpus)
}

corpus.discursos <- limpiar_corpus (corpus.discursos)

tdm.discursos <- TermDocumentMatrix(corpus.discursos)
tdm.discursos <- as.matrix(tdm.discursos)
colnames(tdm.discursos) <- c("Oscar Arias", "L.Guillermo Solis", "Laura Chinchilla", "Carlos Alvarado")
head(tdm.discursos)


comparison.cloud(tdm.discursos,scale=c(1,.8), max.words=50,
                 random.order=FALSE, rot.per=.1,
                 colors=brewer.pal(7 ,"Dark2"),
                 title.size=1.5,
)

commonality.cloud(term.matrix = tdm.discursos, commonality.measure = min, max.words = 50)
