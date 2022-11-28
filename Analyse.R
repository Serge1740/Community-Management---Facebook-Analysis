library(dplyr)
library(readxl)
library(tm)
library(wordcloud)
library(e1071)
library(gmodels)
library("tidytext")
library("tidyverse")
library("stopwords")
library(SnowballC)
library(tm)
library(rtweet)
library(xgboost)
library(caret)
library(Hmisc)
library(dplyr)
library(reshape2)
library(wordcloud2)
library(tidytext)
library(DT)
library(stringr)
library("readtext") # Ouverture de plusieurs documents  de tous les types
library(quanteda) # textmining
library(textclean) # Pour le nettoyage
library(stringr)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
quanteda_options(language_stemmer = "french")
library(class)


#Importation de la base
df_mtn <- read_excel("C:/Users/GANHOUNOUTO JOANES/OneDrive/M??moire/df_mtn.xlsx")

df_mtn$sentence <- df_mtn$Commentaires
df_mtn$score <- factor(df_mtn$Score)

df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]

# subset
positive_mtn <- subset(df_mtn, score == 1)
negative_mtn  <- subset(df_mtn, score == -1)
neutre_mtn  <- subset(df_mtn, score == 0)

# Check the counts of positive and negative scores
barplot(table(df_mtn$score), col = rainbow(10))


### ReprC)sentation graphique
df_mtn_corpus <- VCorpus(VectorSource(df_mtn$sentence))
df_mtn_corpus <- tm_map(df_mtn_corpus, removeNumbers) #enlever les nombre
df_mtn_corpus <- tm_map(df_mtn_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
df_mtn_corpus <- tm_map(df_mtn_corpus, content_transformer(tolower)) #mettre en minuscule
df_mtn_corpus <- tm_map(df_mtn_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
df_mtn_corpus <- tm_map(df_mtn_corpus, removeWords, c("mtn","africa","vous","famille","cest","sil", "cC4te", "divoir","divoire","inbox")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
df_mtn_corpus <- tm_map(df_mtn_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
df_mtn_corpus <- tm_map(df_mtn_corpus, stemDocument, language = "french") #on cherche les radicaux des termes


positive_mtn <- VCorpus(VectorSource(positive_mtn))
positive_mtn <- tm_map(positive_mtn, removeNumbers) #enlever les nombre
positive_mtn <- tm_map(positive_mtn, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
positive_mtn <- tm_map(positive_mtn, content_transformer(tolower)) #mettre en minuscule
positive_mtn <- tm_map(positive_mtn, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
positive_mtn <- tm_map(positive_mtn, removeWords, c("mtn","africa","vous","famille","cest","sil", "cC4te", "divoir","divoire","inbox")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
positive_mtn <- tm_map(positive_mtn, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
positive_mtn <- tm_map(positive_mtn, stemDocument, language = "french") #on cherche les radicaux des termes


negative_mtn <- VCorpus(VectorSource(negative_mtn))
negative_mtn <- tm_map(negative_mtn, removeNumbers) #enlever les nombre
negative_mtn <- tm_map(negative_mtn, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
negative_mtn <- tm_map(negative_mtn, content_transformer(tolower)) #mettre en minuscule
negative_mtn <- tm_map(negative_mtn, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
negative_mtn <- tm_map(negative_mtn, removeWords, c("mtn","africa","vous","famille","cest","sil", "cC4te", "divoir","divoire","inbox")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
negative_mtn <- tm_map(negative_mtn, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
negative_mtn <- tm_map(negative_mtn, stemDocument, language = "french") #on cherche les radicaux des termes


neutre_mtn <- VCorpus(VectorSource(neutre_mtn))
neutre_mtn <- tm_map(neutre_mtn, removeNumbers) #enlever les nombre
neutre_mtn <- tm_map(neutre_mtn, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
neutre_mtn <- tm_map(neutre_mtn, content_transformer(tolower)) #mettre en minuscule
neutre_mtn <- tm_map(neutre_mtn, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
neutre_mtn <- tm_map(neutre_mtn, removeWords, c("mtn","africa","vous","famille","cest","sil", "cC4te", "divoir","divoire","inbox")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
neutre_mtn <- tm_map(neutre_mtn, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
neutre_mtn <- tm_map(neutre_mtn, stemDocument, language = "french") #on cherche les radicaux des termes


TDM_mtn <- df_mtn_corpus %>%
  TermDocumentMatrix() %>%
  as.matrix()

TDM_mtn <- sort(rowSums(TDM_mtn),decreasing=TRUE)
TDM_mtn <- data.frame(word = names(TDM_mtn),freq=TDM_mtn)
TDM_mtn <- subset(TDM_mtn)

barplot(height=head(TDM_mtn,10)$freq, names.arg=head(TDM_mtn,10)$word, xlab="Mots",col = brewer.pal(8,"Dark2"), ylab="Fr??quence", main="Les mots les plus fr??quents")


TDM_pos_mtn <- positive_mtn %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM_pos_mtn <- sort(rowSums(TDM_pos_mtn),decreasing=TRUE)
TDM_pos_mtn <- data.frame(word = names(TDM_pos_mtn),freq=TDM_pos_mtn)
TDM_pos_mtn <- subset(TDM_pos_mtn, freq>10)
wordcloud2(TDM_pos_mtn, size = 0.8, rotateRatio = 0, minSize = 10)


TDM_neg_mtn <- negative_mtn %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM_neg_mtn <- sort(rowSums(TDM_neg_mtn),decreasing=TRUE)
TDM_neg_mtn <- data.frame(word = names(TDM_neg_mtn),freq=TDM_neg_mtn)
TDM_neg_mtn <- subset(TDM_neg_mtn, freq>10)
wordcloud2(TDM_neg_mtn, size = 0.8, rotateRatio = 0, minSize = 10)


TDM_neu_mtn <- neutre_mtn %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM_neu_mtn <- sort(rowSums(TDM_neu_mtn),decreasing=TRUE)
TDM_neu_mtn <- data.frame(word = names(TDM_neu_mtn),freq=TDM_neu_mtn)
TDM_neu_mtn <- subset(TDM_neu_mtn, freq>10)
wordcloud2(TDM_neu_mtn, size = 0.8, rotateRatio = 0, minSize = 10)

# Create a corpus from the sentences
df_mtn_corpus_av <- VCorpus(VectorSource(df_mtn$sentence))


df_mtn_corpus <- VCorpus(VectorSource(df_mtn$sentence))
df_mtn_corpus <- tm_map(df_mtn_corpus, removeNumbers) #enlever les nombre
df_mtn_corpus <- tm_map(df_mtn_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
df_mtn_corpus <- tm_map(df_mtn_corpus, content_transformer(tolower)) #mettre en minuscule
df_mtn_corpus <- tm_map(df_mtn_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
df_mtn_corpus <- tm_map(df_mtn_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
df_mtn_corpus <- tm_map(df_mtn_corpus, stemDocument, language = "french") #on cherche les radicaux des termes

# create a document-term sparse matrix directly from the corpus

df_mtn_dtm <- DocumentTermMatrix(df_mtn_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter", language = "french")
))

#### Nombre de stopwords

df_mtn_corpus_stw <- VCorpus(VectorSource(df_mtn$sentence))
df_mtn_corpus_stw <- tm_map(df_mtn_corpus_stw, removeNumbers) #enlever les nombre
df_mtn_corpus_stw <- tm_map(df_mtn_corpus_stw, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
df_mtn_corpus_stw <- tm_map(df_mtn_corpus_stw, content_transformer(tolower)) #mettre en minuscule
df_mtn_corpus_stw <- tm_map(df_mtn_corpus_stw, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
df_mtn_corpus_stw <- tm_map(df_mtn_corpus_stw, stemDocument, language = "french") #on cherche les radicaux des termes

TDM_mtn_stw <- df_mtn_corpus_stw %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM_mtn_stw <- sort(rowSums(TDM_mtn_stw),decreasing=TRUE)
TDM_mtn_stw <- data.frame(word = names(TDM_mtn_stw),freq=TDM_mtn_stw)
TDM_mtn_stw <- subset(TDM_mtn_stw)

(nbre_mot_stw <- sum(TDM_mtn_stw$freq))
(nbre_mot_stw <- nrow(as.data.frame(TDM_mtn_stw$word)))

##### Effet de traitement

TDM_mtn_av <- df_mtn_corpus_av %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM_mtn_av <- sort(rowSums(TDM_mtn_av),decreasing=TRUE)
TDM_mtn_av <- data.frame(word = names(TDM_mtn_av),freq=TDM_mtn_av)
TDM_mtn_av <- subset(TDM_mtn_av)

(nbre_mot_mtn_av <- sum(TDM_mtn_av$freq))
(nbre_mot_mtn_uniqu_av <- nrow(as.data.frame(TDM_mtn_av$word)))


(nbre_mot_mtn_ap <- sum(TDM_mtn$freq))
(nbre_mot_mtn_uniqu_ap <- nrow(as.data.frame(TDM_mtn$word)))

##### Fin de effet de traitement

dim(df_mtn_dtm)

term_frequency<- function(term){term/sum(term)}

inverse_document_frequency<- function(document){
  corpus_size<- length(document)
  document_count<- length(which(document>0))
  log10(corpus_size/document_count)
}

term_freq_inv_doc_freq<- function(term_f,inv_doc_f){ term_f*inv_doc_f}

df_mtn_dtm_tf<- apply(df_mtn_dtm, 1, term_frequency)
df_mtn_dtm_idf<- apply(df_mtn_dtm, 2, inverse_document_frequency)
df_mtn_dtm_tfidf<- apply(df_mtn_dtm_tf,2, term_freq_inv_doc_freq, inv_doc_f = df_mtn_dtm_idf)
df_mtn_dtm_tfidf<- t(df_mtn_dtm_tfidf)
incomplete.cases<- which(!complete.cases(df_mtn_dtm_tfidf))
df_mtn_dtm_tfidf[incomplete.cases,]<- rep(0.0, ncol(df_mtn_dtm_tfidf))


df_mtn_dtm_tfidf.df <- cbind(df_mtn, data.frame(df_mtn_dtm_tfidf))


### Wordcloud
library(qdapTools)

positive <- subset(df_mtn, score == 1)
negative  <- subset(df_mtn, score == -1)
neutre  <- subset(df_mtn, score == 0)

wordcloud(positive$sentence, max.words = 40, scale = c(3, 0.5))

dataWord <- reactive({
  v <- sort(colSums(as.matrix(df_mtn_dtm)), decreasing = TRUE)
  data.frame(Kata=names(v), Jumlah=as.integer(v), row.names=NULL, stringsAsFactors = FALSE) %>%
    filter(Jumlah > 0)
})

v <- sort(colSums(as.matrix(df_mtn_dtm)), decreasing = TRUE)
v <- data.frame(Kata=names(v), Jumlah=as.integer(v), row.names=NULL, stringsAsFactors = FALSE) %>%
  filter(Jumlah > 0)

wordcloud2(top_n(v, 50, Jumlah))

wordcloud2(top_n(positive, 50))

wordcloud(negative$sentence, max.words = 40, scale = c(3, 0.5))

wordcloud(neutre$sentence, max.words = 40, scale = c(3, 0.5))


### Premier modC(le: naC/ve bayes
# creating training and test datasets

dim(df_mtn_dtm)
df_mtn_dtm_train <- df_mtn_dtm[1:2064, ]
df_mtn_dtm_test  <- df_mtn_dtm[2064:2581, ]

# also save the labels
df_mtn_train_labels <- df_mtn[1:2064, ]$score
df_mtn_test_labels  <- df_mtn[2065:2581, ]$score

# Create random samples
set.seed(123)
train_index <- sample(2581, 2064)

df_mtn_train <- df_mtn[train_index, ]
df_mtn_test  <- df_mtn[-train_index, ]

df_mtn_train_labels <- df_mtn[train_index, ]$score
df_mtn_test_labels  <- df_mtn[train_index, ]$score

# check the proportion of class variable
prop.table(table(df_mtn_train$score))
prop.table(table(df_mtn_test$score))

#create a document-term sparse matrix directly for train and test

train_corpus <- VCorpus(VectorSource(df_mtn_train$sentence))
train_corpus <- tm_map(train_corpus, removeNumbers) #enlever les nombre
train_corpus <- tm_map(train_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
train_corpus <- tm_map(train_corpus, content_transformer(tolower)) #mettre en minuscule
train_corpus <- tm_map(train_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
train_corpus <- tm_map(train_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
train_corpus <- tm_map(train_corpus, stemDocument, language = "french") #on cherche les radicaux des termes

train_dtm <- DocumentTermMatrix(train_corpus)

test_corpus <- VCorpus(VectorSource(df_mtn_test$sentence))
test_corpus <- tm_map(test_corpus, removeNumbers) #enlever les nombre
test_corpus <- tm_map(test_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
test_corpus <- tm_map(test_corpus, content_transformer(tolower)) #mettre en minuscule
test_corpus <- tm_map(test_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
test_corpus <- tm_map(test_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
test_corpus <- tm_map(test_corpus, stemDocument, language = "french") #on cherche les radicaux des termes

test_dtm <- DocumentTermMatrix(test_corpus)

train_labels <- df_mtn[1:2641, ]$score
test_labels  <- df_mtn[2642:3308, ]$score

train_dtm
test_dtm

# create function to convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() convert_counts() to columns of train/test data
train_dtm_binary <- apply(train_dtm, MARGIN = 2, convert_counts)
test_dtm_binary  <- apply(test_dtm, MARGIN = 2, convert_counts)

# Trainaing a model on a data
df_mtn_classifier <- naiveBayes(as.matrix(train_dtm_binary), df_mtn_train$score)

#Evaluating model performance
df_mtn_test_pred <- predict(df_mtn_classifier, as.matrix(test_dtm_binary))

CrossTable(df_mtn_test_pred, df_mtn_test$score,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

confusionMatrix(df_mtn_test$score, df_mtn_test_pred)

### DeuxiC(me modC(le: les k voisins plus proches
library(class)
library(caret)

#creating training and test datasets
ran <- sample(1:nrow(df_mtn_dtm_tfidf), 0.8 * nrow(df_mtn_dtm_tfidf))
dim(df_mtn_dtm)

df_mtn_Train<- (df_mtn_dtm_tfidf[ran,])
df_mtn_Test<- (df_mtn_dtm_tfidf[-ran,])

#Let us work with K values test
df_mtn_knn <- knn(df_mtn_Train, df_mtn_Test, df_mtn$score[ran], k=2)
conf.matrix<- table("Predictions"= df_mtn_knn, "Actual"= df_mtn$score[-ran])
conf.matrix

#Calculate the proportion of correct classification
(ACC <- 100 * sum(df_mtn$score[-ran] == df_mtn_knn)/NROW(df_mtn$score[-ran]))
confusionMatrix(table(df_mtn_knn ,df_mtn$score[-ran]))

#Optimisation
i=1
k.optm=1
for (i in 1:10){
  knn.mod <- knn(df_mtn_Train, df_mtn_Test, df_mtn$score[ran], k=i)
  k.optm[i] <- 100 * sum(df_mtn$score[-ran] == knn.mod)/NROW(df_mtn$score[-ran])
  k=i
  cat(k,'=',k.optm[i],'')
}

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")


### TroisiC(me modC(le: Random forest
library(randomForest)
ran <- sample(1:nrow(df_mtn_dtm_tfidf.df), 0.8 * nrow(df_mtn_dtm_tfidf.df))
dim(df_mtn_dtm_tfidf.df)

df_mtn_rf <- randomForest(df_mtn_dtm_tfidf.df[ran, 3:3938], df_mtn_dtm_tfidf.df[ran,2], ntree = 10, keep.forest = T)

df_mtn_Pred= predict(df_mtn_rf, df_mtn_dtm_tfidf.df[-ran, 3:3246])
conf.matrix<- table("Predictions"= df_mtn_Pred, "Actual"= df_mtn_dtm_tfidf.df[-ran,2])
conf.matrix

confusionMatrix(table(df_mtn_Pred ,df_mtn_dtm_tfidf.df[-ran,2]))

df_mtn_rf$confusion

#Importance des variables
var.imp <- df_mtn_rf$importance
varImpPlot(x = df_mtn_rf, sort = TRUE)

#Affichage de l'C)volution des erreurs OBB et de classification: 
View(df_mtn_rf$err.rate)
plot(df_mtn_rf$err.rate[,1],type="l", xlab="nombre d'arbres", ylab="erreur OOB")

# Random Search

library(tune)
tuned.forest <- tune("randomForest", df_mtn_dtm_tfidf.df[ran, 3:3246], df_mtn_dtm_tfidf.df[ran,2], 
                     ranges = list("mtry","ntree","nodesize"), 
                     tunecontrol = tune.control(sampling="bootstrap", nboot=1000))

tuned.forest$best.performance
tuned.forest$best.model			

### QuatriC(me modC(le

dtad <- df_mtn_dtm_tfidf.df[ran, 2:3246]
dtaf <- df_mtn_dtm_tfidf.df[-ran, 2:3246]

model <- e1071::svm(score ~ ., 
                    data = dtad, type = "nu-classification", kernal = "linear")

predictions <- predict(model, dtaf)

test_pred <- predictions

confusionMatrix(table(test_pred, dtaf$score))

agreement <- predictions == dtaf$score

prop.table(table(agreement))


tune.out <- tune(svm,score~.,data=dtad, type ="nu-classification", kernel="linear", ranges=list(cost=c(0.001,0.01,1,10,100,1000)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

model <- e1071::svm(score ~ ., 
                    data = dtad, type = "nu-classification", kernal = "linear",gamma =0.0003082614, nu = 0.5)

predictions <- predict(model, dtaf)

test_pred <- predictions

confusionMatrix(table(test_pred, dtaf$score))


### CinquiC(me modC(le: Gradient boosting

df_mtn <- read_excel("E:/Stage_Omedia/MC)moire/df_mtn.xlsx")

df_mtn$sentence <- df_mtn$Commentaires
df_mtn$score <- as.integer(df_mtn$Score) + 1

df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]


# Check the counts of positive and negative scores
table(df_mtn$score)

# Create a corpus from the sentences

df_mtn_corpus <- VCorpus(VectorSource(df_mtn$sentence))
df_mtn_corpus <- tm_map(df_mtn_corpus, removeNumbers) #enlever les nombre
df_mtn_corpus <- tm_map(df_mtn_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
df_mtn_corpus <- tm_map(df_mtn_corpus, content_transformer(tolower)) #mettre en minuscule
df_mtn_corpus <- tm_map(df_mtn_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
df_mtn_corpus <- tm_map(df_mtn_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
df_mtn_corpus <- tm_map(df_mtn_corpus, stemDocument, language = "french") #on cherche les radicaux des termes

# create a document-term sparse matrix directly from the corpus

df_mtn <- read_excel("E:/Stage_Omedia/MC)moire/df_mtn.xlsx")

species = as.factor(df_mtn$Score)

df_mtn$sentence <- df_mtn$Commentaires
df_mtn$score <- as.integer(df_mtn$Score) + 1

df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]
df_mtn <- df_mtn[-1]


# Check the counts of positive and negative scores
table(df_mtn$score)

# Create a corpus from the sentences

df_mtn_corpus <- VCorpus(VectorSource(df_mtn$sentence))
df_mtn_corpus <- tm_map(df_mtn_corpus, removeNumbers) #enlever les nombre
df_mtn_corpus <- tm_map(df_mtn_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
df_mtn_corpus <- tm_map(df_mtn_corpus, content_transformer(tolower)) #mettre en minuscule
df_mtn_corpus <- tm_map(df_mtn_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
df_mtn_corpus <- tm_map(df_mtn_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
df_mtn_corpus <- tm_map(df_mtn_corpus, stemDocument, language = "french") #on cherche les radicaux des termes

# create a document-term sparse matrix directly from the corpus

df_mtn_dtm <- DocumentTermMatrix(df_mtn_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter", language = "french")
))

dim(df_mtn_dtm)

term_frequency<- function(term){term/sum(term)}

inverse_document_frequency<- function(document){
  corpus_size<- length(document)
  document_count<- length(which(document>0))
  log10(corpus_size/document_count)
}

term_freq_inv_doc_freq<- function(term_f,inv_doc_f){ term_f*inv_doc_f}

df_mtn_dtm_tf<- apply(df_mtn_dtm, 1, term_frequency)
df_mtn_dtm_idf<- apply(df_mtn_dtm, 2, inverse_document_frequency)
df_mtn_dtm_tfidf<- apply(df_mtn_dtm_tf,2, term_freq_inv_doc_freq, inv_doc_f = df_mtn_dtm_idf)
df_mtn_dtm_tfidf<- t(df_mtn_dtm_tfidf)
incomplete.cases<- which(!complete.cases(df_mtn_dtm_tfidf))
df_mtn_dtm_tfidf[incomplete.cases,]<- rep(0.0, ncol(df_mtn_dtm_tfidf))


df_mtn_dtm_tfidf.df <- cbind(df_mtn, data.frame(df_mtn_dtm_tfidf))


tdm_td<-DocumentTermMatrix(df_mtn_corpus)
data_mod=as.matrix(tdm_td) %>%as.data.frame()
dim(data_mod)

tdm_td2=removeSparseTerms(tdm_td, sparse = 0.9997)
data_mod2=as.matrix(tdm_td2) %>%as.data.frame()

dim(data_mod2)


data_mod2$target= factor(df_mtn$score)

set.seed(123)

valid_rows=createDataPartition(y=data_mod2$target,p = 0.1)$Resample1
valid_data=data_mod2[valid_rows,]

train_test_data=data_mod2[-valid_rows,]

sample_train_test=createDataPartition(y=train_test_data$target,p = 0.8)

trainx=as.matrix(train_test_data[sample_train_test$Resample1,-ncol(train_test_data)])
trainy=train_test_data$target[sample_train_test$Resample1] %>%
  as.character() %>% as.numeric()

testx=as.matrix(train_test_data[-sample_train_test$Resample1,-ncol(train_test_data)])
testy=train_test_data$target[-sample_train_test$Resample1]  %>%
  as.character() %>% as.numeric()



xgb.train = xgb.DMatrix(data=trainx,label=trainy)

xgb.test = xgb.DMatrix(data=testx,label=testy)


watchlist <- list(train=xgb.train, test=xgb.test)

params <- list(booster = "gbtree", objective = "multi:softmax", num_class = 3, eval_metric = list("mlogloss","accuracy"),max.depth=100)

modxgb<- xgb.train(data=xgb.train, max.depth=10,gamma = 0.1, eta=0.2, nrounds=100,print_every_n = 100,metric="accuracy",num_class=3,watchlist=watchlist, objective = "multi:softmax")

predxgb_valid=as.factor(predict(modxgb,as.matrix(valid_data[,-ncol(valid_data)])))

confusionMatrix(predxgb_valid,as.factor(valid_data$target))


predxgb_valid


predxgb_train=as.factor(predict(modxgb,trainx))
nrow(as.data.frame(predxgb_train))

datata = data_mod2
dim(datata)
trai=as.matrix(datata[,-ncol(datata)])
dim(trai)
predxgb_tra=as.factor(predict(modxgb,trai))
nrow(as.data.frame(predxgb_tra))


predxgb_tra


new_data_df_base <- read_excel(file.choose())
new_data_df <- new_data_df_base
new_data_df$sentence <- new_data_df$Commentaires
new_data_df <- new_data_df[-1]
new_data_df <- new_data_df[-1]


new_data_df_corpus <- VCorpus(VectorSource(new_data_df$sentence))
new_data_df_corpus <- tm_map(new_data_df_corpus, removeNumbers) #enlever les nombre
new_data_df_corpus <- tm_map(new_data_df_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractC(res de ponctuation
new_data_df_corpus <- tm_map(new_data_df_corpus, content_transformer(tolower)) #mettre en minuscule
new_data_df_corpus <- tm_map(new_data_df_corpus, removeWords, stopwords("french")) # ici cela va supprimer automatiquement une bonne partie des mots franC'ais "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
new_data_df_corpus <- tm_map(new_data_df_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides
new_data_df_corpus <- tm_map(new_data_df_corpus, stemDocument, language = "french") #on cherche les radicaux des termes

new_data_tdm_td<-DocumentTermMatrix(new_data_df_corpus)
data_new_mod=as.matrix(new_data_tdm_td) %>%as.data.frame()

# create a document-term sparse matrix directly from the corpus

new_data_dtm <- DocumentTermMatrix(new_data_df_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter", language = "french")
))

new_data_dtm_tf<- apply(new_data_dtm, 1, term_frequency)
new_data_dtm_idf<- apply(new_data_dtm, 2, inverse_document_frequency)
new_data_dtm_tfidf<- apply(new_data_dtm_tf,2, term_freq_inv_doc_freq, inv_doc_f = new_data_dtm_idf)
new_data_dtm_tfidf<- t(new_data_dtm_tfidf)
incomplete.cases<- which(!complete.cases(new_data_dtm_tfidf))
new_data_dtm_tfidf[incomplete.cases,]<- rep(0.0, ncol(new_data_dtm_tfidf))


new_data_tdm_td<-DocumentTermMatrix(new_data_df_corpus)
new_data_tdm_td2=removeSparseTerms(new_data_tdm_td, sparse = 0.9997)
data_new_mod=as.matrix(new_data_tdm_td2) %>%as.data.frame()

datata = data_new_mod
final_data =as.matrix(datata)

predic=as.factor(predict(modxgb,final_data))

levels(predic) = list(Negative = "0", Neutre = "1",  Positive = "2")

new_data_df$Sentiment = predic
new_data_df


### Best model until now
#modxgb<- xgb.train(data=xgb.train, max.depth=100,gamma = 0.1, eta=0.2, nrounds=100,print_every_n = 100,metric="accuracy",num_class=3,watchlist=watchlist, objective = "multi:softmax")