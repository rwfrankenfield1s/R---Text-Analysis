install.packages('dplyr')
install.packages('ggplot2')
install.packages('stringr')
install.packages('udpipe')
install.packages('lattice')
install.packages('tabulizer')
install.packages('pdftools')
install.packages('igraph')
install.packages('ggraph')
install.packages('topicmodels')
install.packages('qgraph')
install.packages('textrank')
install.packages('wordcloud')
install.packages('leaflet')
install.packages('shiny')
install.packages('htmlwidgets')


library(dplyr)
library(ggplot2)
library(stringr)
library(udpipe)
library(lattice)
library(tabulizer)
library(pdftools)
library(igraph)
library(ggraph)
library(topicmodels)
library(qgraph)
library(textrank)
library(wordcloud)
library(leaflet)
library(shiny)
library(htmlwidgets)


xx <- read.delim2('C:/Users/rwfra/Desktop/Data Science/WordWeb/WordNovel.txt')
View(xx)

model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
                                     
oo <- udpipe_annotate(udmodel_english, xx$THE.PLAN.John.Francis.Kinsella)
View(oo)

ooo <- data.frame(oo)

#
##POS
#
stats <- txt_freq(ooo$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


#
##Nouns
#
stats <- subset(ooo, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

#
##Adj
#
stats <- subset(ooo, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring adjectives", xlab = "Freq")


#
##RAKE
#
stats <- keywords_rake(x = ooo, term = "lemma", group = "doc_id", 
                       relevant = ooo$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


#
##Collocation
#
ooo$word <- tolower(ooo$token)
stats <- keywords_collocation(x = ooo, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")




#
##Phrasemachine
#
ooo$phrase_tag <- as_phrasemachine(ooo$upos, type = "upos")
stats <- keywords_phrases(x = ooo$phrase_tag, term = tolower(ooo$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")


#
##Cooccurrence
#
cooc <- cooccurrence(x = subset(ooo, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")



cooc <- cooccurrence(ooo$lemma, relevant = ooo$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")


#
##
#
ooo$id <- unique_identifier(ooo, fields = c("sentence_id", "doc_id"))
dtm <- subset(ooo, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)


## Define the identifier at which we will build a topic model
ooo$topic_level_id <- unique_identifier(ooo, fields = c("doc_id", "paragraph_id", "sentence_id"))
## Get a data.frame with 1 row per id/lemma
dtf <- subset(ooo, upos %in% c("NOUN"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
head(dtf)

## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
head(dtm_colsums(dtm_clean))

## Remove nouns which you really do not like (mostly too common nouns)
dtm_clean <- dtm_remove_terms(dtm_clean, terms = c("appartement", "appart", "eter"))
## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
dtm_clean <- dtm_remove_tfidf(dtm_clean, top = 50)




m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))


scores <- predict(m, newdata = dtm, type = "topics", 
                  labels = c("labela", "labelb", "labelc", "xyz"))



predict(m, type = "terms", min_posterior = 0.05, min_terms = 3)
str(scores)



## Build document term matrix on nouns/adjectives only
dtf <- subset(ooo, upos %in% c("NOUN", "ADJ") & 
                !lemma %in% c("appartement", "appart", "eter", "tres"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
dtm <- document_term_matrix(x = dtf)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
## Build topic model + get topic terminology
m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
topicterminology <- predict(m, type = "terms", min_posterior = 0.025, min_terms = 5)
scores <- predict(m, newdata = dtm, type = "topics")



x_topics <- merge(ooo, scores, by.x="topic_level_id", by.y="doc_id")
wordnetwork <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
wordnetwork <- cooccurrence(wordnetwork, group = c("topic_level_id"), term = "lemma")
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink")  +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words in topic 1 ", subtitle = "Nouns & Adjective cooccurrence")



topicterminology <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 10)
termcorrs <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
termcorrs <- document_term_frequencies(termcorrs, document = "topic_level_id", term = "lemma")
termcorrs <- document_term_matrix(termcorrs)
termcorrs <- dtm_cor(termcorrs)
termcorrs[lower.tri(termcorrs)] <- NA
diag(termcorrs) <- NA

qgraph(termcorrs, layout = "spring", labels = colnames(termcorrs), directed = FALSE,
       borders = FALSE, label.scale = FALSE, label.cex = 1, node.width = 0.5)



topicterminology <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 10)
termcorrs <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
termcorrs <- document_term_frequencies(termcorrs, document = "topic_level_id", term = "lemma")
termcorrs <- document_term_matrix(termcorrs)
termcorrs <- dtm_cor(termcorrs)
termcorrs[lower.tri(termcorrs)] <- NA
diag(termcorrs) <- NA

qgraph(termcorrs, layout = "spring", labels = colnames(termcorrs), directed = FALSE,
       borders = FALSE, label.scale = FALSE, label.cex = 1, node.width = 0.5)





## Find keywords with RAKE 
keyw_rake <- keywords_rake(ooo, 
                           term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), 
                           relevant = ooo$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 3, n_min = 5)
## Find simple noun phrases
ooo$phrase_tag <- as_phrasemachine(ooo$upos, type = "upos")
keyw_nounphrases <- keywords_phrases(ooo$phrase_tag, term = ooo$token, 
                                     pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, 
                                     detailed = FALSE)
keyw_nounphrases <- subset(keyw_nounphrases, ngram > 1)

## Recode terms to keywords
ooo$term <- ooo$token
ooo$term <- txt_recode_ngram(ooo$term, 
                           compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
ooo$term <- txt_recode_ngram(ooo$term, 
                           compound = keyw_nounphrases$keyword, ngram = keyw_nounphrases$ngram)
## Keep keyword or just plain nouns
ooo$term <- ifelse(ooo$upos %in% "NOUN", ooo$term,
                 ifelse(ooo$term %in% c(keyw_rake$keyword, keyw_nounphrases$keyword), ooo$term, NA))

## Build document/term/matrix
dtm <- document_term_frequencies(ooo, document = "topic_level_id", term = "term")
dtm <- document_term_matrix(x = dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)

m <- LDA(dtm, k = 3, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

topicterminology <- predict(m, type = "terms", min_posterior = 0.10, min_terms = 3)
topicterminology



## Recode tokens to keywords, if it is not in the list of tokens, set to NA
ooo$mwe <- txt_recode_ngram(ooo$token, compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
ooo$mwe <- ifelse(ooo$mwe %in% keyw_rake$keyword, ooo$mwe, NA)

## nouns
ooo$term_noun <- ifelse(ooo$upos %in% "NOUN", ooo$token, NA)

## Build document/term/matrix 
dtm <- document_term_frequencies(ooo, document = "topic_level_id", term = c("term_noun", "mwe"))
dtm <- document_term_matrix(x = dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 3)
m <- LDA(dtm, k = 3, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

topicterminology <- predict(m, type = "terms", min_posterior = 0.10, min_terms = 3)
topicterminology


#####################################################################################################
## Collocation (words following one another)
stats <- keywords_collocation(x = ooo, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(ooo, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = ooo$lemma, 
                      relevant = ooo$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = ooo$lemma, 
                      relevant = ooo$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)



wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")



stats <- textrank_keywords(ooo$lemma, 
                           relevant = ooo$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
wordcloud(words = stats$keyword, freq = stats$freq)



## Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
ooo$phrase_tag <- as_phrasemachine(ooo$upos, type = "upos")
stats <- keywords_phrases(x = ooo$phrase_tag, term = ooo$token, 
                          pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, ngram_max = 4, detailed = FALSE)
head(subset(stats, ngram > 2))





stats <- merge(ooo, ooo, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
stats <- txt_freq(stats$term)

wordcloud(words = stats$key, freq = stats$freq, min.freq = 3, max.words = 100,
          random.order = FALSE, colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))
