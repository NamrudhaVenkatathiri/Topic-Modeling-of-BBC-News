bbc_folder <- ‘C:/Users/namru.NAMUS-ENVY/Downloads/bbc/bbc/’
bbcsports_folder<-‘C:/Users/namru.NAMUS-ENVY/Downloads/bbc/bbcsport/’bc_
bbc_source <-paste(bbc_folder, “bbc.mtx”,sep=””)
bbc_source_terms<-paste(bbc_folder,”bbc.terms”,sep=””)
bbc_source_docs<-paste(bbc_folder,”bbc.docs”,sep=””)
bbcsports_source <-paste(bbcsports_folder,”bbcsport.mtx”,sep=””)
bbcsports_source_terms <-paste(bbcsports_folder,”bbcsport.terms”,sep=””)
bbcsports_source_docs <-paste(bbcsports_folder,”bbcsport.docs”,sep=””)

library(“tm”)
library(“Matrix”)
bbc_matrix<-readMM(bbc_source)
bbc_tdm<-as.TextDocumentMatrix(bbc_matrix,weightTf)
bbcsports_matrix<-readMM(bbcsports_source)
bbcsports_tdm<-as.TermDocumentMatrix(bbcsports_matrix,weightTf)

bbc_rows<-scan(bbc_source_terms,what=”character”)
bbc_cols<-scan(bbc_source_docs,what=”character”)

bbcsports_rows<-scan(bbcsports_source_terms,what=”character”)
bbcsports_cols<-scan(bbcsports_source_docs,what=”character”)

bbc_tdm$dimnames$Terms <-bbc_rows
bbc_tdm$dimnames$Docs <-bbc_cols
(bbc_dtm <-t(bbc_tdm))

bbcsports_tdm$dimnames$Terms <-bbcsports_rows
bbcsports_tdm$dimnames$Docs <-bbcsports_cols
(bbcsports_dtm<-t(bbcsports_tdm))
bbc_cols[1:5]
bbcsports_cols[1:5]

bbc_gold_topics<-sapply(bbc_cols,function(x) substr(x,1,nchar(x)-4))
bbc_gold_factor<-factor(bbc_gold_topics)

bbcsports_gold_topics<-sapply(bbcsports_cols,function(x) substr(x,1,nchar(x)-4))
bbcsports_gold_factor<-factor(bbcsports_gold_topics)

summary(bbc_gold_factor)
summary(bbcsports_gold_factor)

compute_model_list <- function (k, topic_seed, myDtm)
{ LDA_VEM <- LDA(myDtm, k = k, control = list(seed = topic_seed))
LDA_VEM_a <- LDA(myDtm, k = k, control = list(estimate.alpha = FALSE, seed = topic_seed))
LDA_GIB <- LDA(myDtm, k = k, method = "Gibbs", 
control = list(seed = topic_seed, burnin = 1000, thin = 100, iter = 1000)) 
CTM_VEM <- CTM(myDtm, k = k, control = list(seed = topic_seed, 
var = list(tol = 10^-4), em = list( tol = 10^-3))) 
return(list(LDA_VEM = LDA_VEM, LDA_VEM_a = LDA_VEM_a, 
LDA_GIB = LDA_GIB, CTM_VEM = CTM_VEM))
 }

install.packages(“topicmodels”)
library(“topicmodels”)
k <- 5 
topic_seed <- 5798252 
bbc_models <- compute_model_list(k, topic_seed,bbc_dtm) 
 bbcsports_models <- compute_model_list(k, topic_seed, bbcsports_dtm)
model_topics <- topics(bbc_models$LDA_VEM)
table(model_topics, bbc_gold_factor)

compute_topic_model_accuracy <- function(model, gold_factor) {
 model_topics <- topics(model) 
model_table <- table(model_topics, gold_factor)
 model_matches <- apply(model_table, 1, max)
 model_accuracy <- sum(model_matches) / sum(model_table) 
return(model_accuracy) 
}

sapply(bbc_models, function(x) 
compute_topic_model_accuracy(x, bbc_gold_factor)) 
sapply(bbcsports_models, function(x) 
compute_topic_model_accuracy(x, bbcsports_gold_factor))

sapply(bbc_models, logLik)
sapply(bbcsports_models, logLik)

seeded_bbc_models <- lapply(5798252 : 5798256, 
function(x) compute_model_list(k, x, bbc_dtm))
seeded_bbcsports_models <- lapply(5798252 : 5798256, 
function(x) compute_model_list(k, x, bbcsports_dtm))

seeded_bbc_models_acc <- sapply(seeded_bbc_models,
 function(x) sapply(x, function(y)
 compute_topic_model_accuracy(y, bbc_gold_factor)))
seeded_bbc_models_acc

seeded_bbcsports_models_acc <- sapply(seeded_bbcsports_models, 
function(x) sapply(x, function(y) 
compute_topic_model_accuracy(y, bbcsports_gold_factor)))
 seeded_bbcsports_models_acc



compute_model_list_r <- function (k, topic_seed, myDtm, nstart) { 
seed_range <- topic_seed : (topic_seed + nstart - 1) 
LDA_VEM <- LDA(myDtm, k = k, control = list(seed = seed_range, 
nstart = nstart)) 
LDA_VEM_a <- LDA(myDtm, k = k, control = list(estimate.alpha = 
FALSE, seed = seed_range, nstart = nstart)) 
LDA_GIB <- LDA(myDtm, k = k, method = "Gibbs", control = 
list(seed = seed_range, burnin = 1000, thin =
 100, iter = 1000, nstart = nstart))
 CTM_VEM <- CTM(myDtm, k = k, control = list(seed = seed_range, 
var = list(tol = 10^-4), em = list(tol = 10^-3), 
nstart = nstart))
 return(list(LDA_VEM = LDA_VEM, LDA_VEM_a = LDA_VEM_a, 
LDA_GIB = LDA_GIB, CTM_VEM = CTM_VEM)) 
}

nstart <- 5 
topic_seed <- 5798252 
nstarted_bbc_models_r <- compute_model_list_r(k, topic_seed, bbc_dtm, nstart) 
nstarted_bbcsports_models_r <- compute_model_list_r(k, topic_seed, bbcsports_dtm, nstart) 
 sapply(nstarted_bbc_models_r, function(x) 
compute_topic_model_accuracy(x, bbc_gold_factor)) 
sapply(nstarted_bbcsports_models_r, function(x) 
compute_topic_model_accuracy(x, bbcsports_gold_factor))

bbc_models[[1]]@alpha
bbc_models[[2]]@alpha
bbcsports_models[[1]]@alpha
bbcsports_models[[2]]@alpha

options(digits = 4)
head(posterior(bbc_models[[1]])$topics)

histogram=apply(posterior(bbc_models[[1]])$topics,1,max)
hist(max.post)

histogram_LDA_VEM=apply(posterior(bbc_models[[1]])$topics,1,max)
hist(histogram_LDA_VEM,main=”Max Probabilities forLDA_VEM”,xlab=”Probability of most likely topic”,ylab=”Frequency”,col=”red”,border=”grey”,breaks=30)

histogram_LDA_GIB=apply(posterior(bbc_models[[2]])$topics,1,max)
hist(histogram_LDA_GIB,main=”Histogram of Max Probabilities forLDA_GIB”,xlab=”Probability of most likely topic”,ylab=”Frequency”,col=”blue”,border=”grey”,breaks=30)

histogram_LDA_VEM_a=apply(posterior(bbc_models[[3]])$topics,1,max)
hist(histogram_LDA_VEM_a,main=”Histogram of Max Probabilities forLDA_VEM_a”,xlab=”Probability of most likely topic”,ylab=”Frequency”,col=”red”,border=”grey”,breaks=30)

histogram_CTM_VEM=apply(posterior(bbc_models[[4]])$topics,1,max)
hist(histogram_ CTM_VEM,main=”Histogram of Max Probabilities for CTM_VEM”,xlab=”Probability of most likely topic”,ylab=”Frequency”,col=”blue”,border=”grey”,breaks=30)

compute_entropy <- function(probs) 
{ return(- sum(probs * log(probs))) 
} 
compute_model_mean_entropy <- function(model) 
{ topics <- posterior(model)$topics
 return(mean(apply(topics, 1, compute_entropy)))
 }

sapply(bbc_models, compute_model_mean_entropy)
sapply(bbcsports_models, compute_model_mean_entropy)

GIB_bbc_model <- bbc_models[[3]]
terms(GIB_bbc_model, 10)

plot_wordcloud <- function(model, myDtm, index, numTerms)
 { model_terms <- terms(model,numTerms) 
model_topics <- topics(model) 
terms i <- model_terms[,index]
topic_i <- model_topics == index 
dtm_i <- myDtm[topic_i, terms_i] 
frequencies_i <- colSums(as.matrix(dtm_i)) 
wordcloud(terms_i, frequencies_i, min.freq = 0) }
cloud1=plot_wordcloud(GIB_bbc_model,bbc_dtm,1,40)
cloud2=plot_wordcloud(GIB_bbc_model,bbc_dtm,2,40)
cloud3=plot_wordcloud(GIB_bbc_model,bbc_dtm,3,40)
cloud4=plot_wordcloud(GIB_bbc_model,bbc_dtm,4,40)
cloud5=plot_wordcloud(GIB_bbc_model,bbc_dtm,5,40)

