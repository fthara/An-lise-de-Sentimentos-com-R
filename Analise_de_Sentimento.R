#Análise de Sentimentos em rede social - Twitter

setwd("/Users/fernando/Google Drive/DSA/BigDataRAzure/Projetos/Analise_de_Sentimentos")
getwd()

# Conexão ao app do twitter

library(twitteR)
library(httr)

# Chaves de autenticação no Twitter
key <- "HJpuSXz3JRn2pkis1PvnWf1d0"
secret <- "sHwZe2q5ytoZMHP5Bowa2WcDmcwErGOlhowmsJzeKyMdLlxfat"
token <- "155073443-q9YjaNnGkNaLTis6vSs2UOUtLTf0GJvyMvj4zajE"
tokensecret <- "kDCs0Lou5eYCZdIkyb5vCkyd7PopnSsWXJXyKvqY1awT8"

# Autenticação. Responda 1 quando perguntado sobre utilizar direct connection.
setup_twitter_oauth(key, secret, token, tokensecret)

# Captura dos twitters
tema <- "emprego"
qnt_tweets <- 3000
lingua <- 'pt'
tweet <- searchTwitter(tema, n = qnt_tweets, lang = lingua)

# Visualizando as primeiras linhas do objeto tweet
head(tweet)

# Limpeza dos dados coletados (text mining)
library(SnowballC)
library(tm)
options(warn=-1)

# Criando uma cópia dos dados originais
tweets <- tweet

#transformando a lista em um vetor.
tweets <- sapply(tweets, function(x) x$getText())
#tranformando os textos em UTF-8
tweets <- iconv(tweets, to = "utf-8", sub="")
# Removendo caracteres especiais
tweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
# Removendo @
tweets = gsub("@\\w+", "", tweets)
# Removendo pontuação
tweets = gsub("[[:punct:]]", "", tweets)
# Removendo digitos
tweets = gsub("[[:digit:]]", "", tweets)
# Removendo links html
tweets = gsub("http\\w+", "", tweets)
# Removendo \n
tweets = gsub("\n", " ", tweets)
# Removendo espacos desnecessários
tweets = gsub("[ \t]{2,}", " ", tweets)
tweets = gsub("^\\s+|\\s+$", "", tweets)
# Tranformando letras maiusculas em minusculas
tweets = tolower(tweets)

# Extraindo stopwords (palavras comuns) dos tweets
stopwords = stopwords("portuguese")
stopwords = stopwords[-157]
rm_stopwords <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], 
                                    collapse = " "), character(1))
}
tweets_sem_stopwords <- rm_stopwords(tweets, stopwords)

# Excluindo tweets vazios
vet <- c()
j=1
for(i in 1:length(tweets_sem_stopwords)){
  if(tweets_sem_stopwords[i] == ""){
    vet[j] <- i
    j=j+1
  }
}
if(length(vet) > 0)
  tweets_sem_stopwords <- tweets_sem_stopwords[-vet]

#Tirando os tweets duplicados
tweets_unicos <- (unique(tweets_sem_stopwords))

# Transformando os dados em Corpus
tweetc <- Corpus(VectorSource(tweets_sem_stopwords))
# Remove pontuação
tweetc <- tm_map(tweetc, removePunctuation)
# Remove números
tweetc <- tm_map(tweetc, function(x)removeWords(x, stopwords()))


# Convertendo o objeto texto para o formato de matriz
tweet_mat <- TermDocumentMatrix(tweetc)
tweet_mat

# Encontrando as palavras que aparecem com mais frequência
findFreqTerms(tweet_mat, lowfreq = 15)

# Buscando associações
findAssocs(tweet_mat, 'emprego', 0.1)

# Gerando uma ""nuvem de palavras"
library(RColorBrewer)
library(wordcloud)

# Gerando uma nuvem palavras
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetc, 
          min.freq = 2, 
          scale = c(4,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

#função para analisar o sentimento
# Lendo o arquivo SentilexPT
sentilex <- read.csv("sentilex.csv", header = TRUE, row.names = 1)
# Convertendo a coluna Word de factor para character
sentilex$Word <- as.character(sentilex$Word)

# Criando o data frame sentimentos, com os tweets e a pontuação. Inicialmente igual a 0.
sentimentos <- data.frame("tweet"=tweets_unicos,
                          "score"=rep(0, length(tweets_unicos)))
# Convertendo a coluna tweet de factor para character
sentimentos$tweet <- as.character(sentimentos$tweet)

# Calculando o score de sentimento em cada tweet.
score <- lapply(sentimentos$tweet, 
                function(sentence, word, feeling){
                  sentence <- strsplit(sentence, " ", fixed = TRUE)
                  unlist_sentence <- unlist(sentence, use.names=FALSE)
                  list_score <- match(unlist_sentence, word)
                  list_score<-list_score[!is.na(list_score)]
                  score <- sum(feeling[list_score])
                  return(score)
                }, sentilex$Word, sentilex$Feeling)

# Gravando o score em na coluna score do data frame.
score <- unlist(score, use.names=FALSE)
sentimentos$score <- score

#Criando a coluna sentimento com o tipo sentimento em cada publicação.
sentimento <- sapply(sentimentos$score, 
                     function(x){
                       if(x > 0)
                         return("Positivo")
                       else if(x == 0)
                         return("Neutro")
                       else
                         return("Negativo")
                     })
sentimentos$sentimento <- sentimento

#Tabela com a proporção de cada tipo de sentimento
prop.table(table(sentimentos$sentimento))

#boxplot
boxplot(sentimentos$score)

#Histograma
library("lattice")
histogram(data = sentimentos, ~score, main = "Análise de Sentimentos",
          xlab = "", sub = "Score")

#Gráfico de barras
library(ggplot2)
ggplot(sentimentos, aes(x=sentimento)) +
  geom_bar(aes(y=..count.., fill=sentimento)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x = "Categorias de Sentimento", y = "Numero de Tweets")








