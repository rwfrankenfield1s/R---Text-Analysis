install.packages('tm')
install.packages('tmap')
install.packages('NLP')
install.packages('udpipe')
install.packages('dplyr')
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("rlang")
install.packages("magrittr")
install.packages("stringr")
install.packages("lattice")
install.packages('data.table')
install.packages("caret")

library(tm)
library(tmap)
library(NLP)
library(udpipe)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rlang)
library(magrittr)
library(stringr)
library(lattice)
library(data.table)
library(caret)

### Load and Observe Data
data <- read.csv('C:/--------/Desktop/Grp Process/Flight_TS_2015-10.29.18.csv', header=T, stringsAsFactors = F)
View(data)
str(data)


gc()

### Rearrange data if needed 
subset <- data[,c(1,2,3,4,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,5,6)]
View(subset)
str(subset)



### Observe the distribution and freq of trips
subset %>% group_by(Arrival_Date) %>% count() %>% arrange(desc(n))
subset %>% group_by(Arrival_Date) %>% count() %>% ggplot() + geom_line(aes(Arrival_Date,n, group = 1))
View(subset)


### Break the "Arrival_Date" column up for simple filtering
subset_more <- subset %>% mutate(year = str_sub(Arrival_Date,1,4),
                                 month = str_sub(Arrival_Date,6,7),
                                 date = str_sub(Arrival_Date,9,10))
View(subset_more)

### Index the data for fucture join with POS and other annotations
subset_more <- subset_more %>% mutate(doc_id1 = 'doc')
subset_more <- subset_more %>% mutate(doc_id2 = row_number())
subset_more$doc_id <- paste(subset_more$doc_id1, subset_more$doc_id2, sep = "")
View(subset_more)

### Observe the distribution of trips based on year... etc
subset_more %>% group_by(year) %>% count()  %>% ggplot() + geom_bar(aes(year,n), stat ='identity')


### Load pre-trained model from udpipe
# Use '??' To see pre-trained languages
??udpipe_download_model
model <- udpipe_download_model(language = "english")
# Use 'str' to see file name for pre-trained data
str(model)
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')


### Filter data on the columns derived from the 'Arrival_Date' column
subset_anno <- subset_more %>% filter(year==2018 & month=='03')
View(subset_anno)

subset_anno2 <- as.character(subset_anno$Header_Description)


### Annotate the text collumn in question
s <- udpipe_annotate(udmodel_english, subset_anno$Header_Description)
View(s)


# Set dataframe
x <- data.frame(s)
View(x)

newX <- as.data.frame(s)
View(newX)
####################### ^ END ^ #######################



##
############### Observe UPOS, Etc. 
##


# UPOS
UPOS <- txt_freq(x$upos)
UPOS$key <- factor(UPOS$key, levels = rev(UPOS$key))
barchart(key ~ freq, data = UPOS, col = "blue",
         main = "Universal Parts of Speech\n Frequency of Occurrence", 
         xlab = "Frequency")
View(UPOS)

# NOUNS
NOUNS <- subset(x, upos %in% c ("NOUN"))
NOUNS <- txt_freq(NOUNS$token)
NOUNS$key <- factor(NOUNS$key, levels = rev(NOUNS$key))
barchart(key ~ freq, data = head(NOUNS, 30), col = "cadetblue", 
         main = "Most Occurring Nouns", xlab = "Frequency")
View(NOUNS)


# ADJECTIVES
ADJECTIVES <- subset(x, upos %in% c("ADJ")) 
ADJECTIVES <- txt_freq(ADJECTIVES$token)
ADJECTIVES$key <- factor(ADJECTIVES$key, levels = rev(ADJECTIVES$key))
barchart(key ~ freq, data = head(ADJECTIVES, 30), col = "purple", 
         main = "Most Occurring Adjectives", xlab = "Frequency")
View(ADJECTIVES)


# VERBS
VERBS <- subset(x, upos %in% c("VERB")) 
VERBS <- txt_freq(VERBS$token)
VERBS$key <- factor(VERBS$key, levels = rev(VERBS$key))
barchart(key ~ freq, data = head(VERBS, 30), col = "gold", 
         main = "Most Occurring Verbs", xlab = "Frequency")
View(VERBS)

####################### ^ END ^ #######################



##
############### Exploring Collocation and Co-Occurrences
##


## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id"),
                              ngram_max = 3)
head(stats)
jstats <- within(stats, join <- paste(stats$right, stats$left, sep='_'))
View(jstats)
##jstats <- subset(jstats[,c(4,3)])
jstats <- as.data.frame(jstats)
barchart(join ~ freq_right, data = head(jstats, 30), xlab = "Freq Same Sentance Word Follow Occure",
         ylab = "Freq Word Follow in Sentance",
         main = "Occurance")


## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id"))
head(stats)
jstats <- within(stats, join <- paste(stats$term1, stats$term2, sep=' '))
jstats <- subset(jstats[,c(4,3)])
View(jstats)
barchart(join ~ cooc, data = head(jstats, 30), xlab = "Freq Same Sentance Word Occure",
         ylab = "Freq Word in Sentance",
         main = "Occurance")


## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))
head(stats)
jstats <- within(stats, join <- paste(stats$term1, stats$term2, sep='_'))
jstats <- subset(jstats[,c(4,3)])
View(jstats)
jstats <- as.data.frame(jstats)
barchart(join ~ cooc, data = head(jstats, 30), xlab = "Freq Same Sentance Word Follow Occure",
         ylab = "Freq Word Follow in Sentance",
         main = "Occurance")


## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
View(stats)
head(stats)
jstats <- within(stats, join <- paste(stats$term1, stats$term2, sep='_'))
jstats <- subset(jstats[,c(4,3)])
View(jstats)
jstats <- as.data.frame(jstats)
barchart(join ~ cooc, data = head(jstats, 30), xlab = "Freq Same Sentance Word Follow Occure",
         ylab = "Freq Word Follow in Sentance (Skip 2)",
         main = "Occurance")


## Co-occurrences: How frequent do words follow one another even if we would skip 1 words in between
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)

xxx <- as.data.frame(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)


head(stats)
View(stats)
jstats <- within(stats, join <- paste(stats$term1, stats$term2, sep='_'))
jstats <- subset(jstats[,c(4,3)])
View(jstats)
jstats <- as.data.frame(jstats)
barchart(join ~ cooc, data = head(jstats, 30), xlab = "Freq Same Sentance Word Follow Occure",
         ylab = "Freq Word Follow in Sentance (Skip 1)",
         main = "Occurance")




##
############### RAKE = [Rapid Automatic Keyword Extraction]
##



# KEYWORD EXTRACTION:RAKE (ML) -- NOUN TO ADJ
stats <- keywords_rake(x=x, term = "lemma", group = "sentence",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 30), col
         = "red",
                  main = "Keywords Identified by RAKE(NOUN TO ADJ)",
                  xlab = "Rake")
View(stats)
#View(x$upos)


# KEYWORD EXTRACTION:RAKE (ML) -- VERB TO NOUN
stats <- keywords_rake(x=x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("VERB", "NOUN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 30), col
         = "red",
         main = "Keywords Identified by RAKE(VERB TO NOUN)",
         xlab = "Rake")
View(stats)
# KEYWORD EXTRACTION:RAKE (ML) -- NOUN TO NOUN
stats <- keywords_rake(x=x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("VERB", "NOUN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 30), col
         = "red",
         main = "Keywords Identified by RAKE(NOUN TO NOUN)",
         xlab = "Rake")


# Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 30), col = "magenta", 
         main = "Simple Noun Phrases", xlab = "Frequency")




#########################################################################
#########################################################################
  

xyz <- x %>% filter(doc_id == 'doc1')
View(xyz)

stats <- keywords_rake(x=xyz, term = "lemma", group = "sentance",
                       relevant = xyz$upos %in% c("NOUN", "NOUN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 0), 30), col
         = "red",
         main = "Keywords Identified by RAKE(NOUN TO ADJ)",
         xlab = "Rake")

View(stats)


###########################################################################################
################################## Corpus Test for Meta Save ##############################

### Create corpus indedx (saved in memory) for Header Decsription

sub_sub_anno <- subset_anno[,c(30,23)]
View(sub_sub_anno)

# Change Collumn names to correct names for algo
df_title <- data.frame(doc_id=row.names(sub_sub_anno),
                       text=sub_sub_anno$Header_Description)
View(df_title)

mycorpus <- Corpus(DataframeSource(df_title))

mycorpus <- data.frame(text = sapply(mycorpus, as.character), stringsAsFactors = FALSE)

View(mycorpus)

# Manually keep ID information from https://stackoverflow.com/a/14852502/1036500
for (i in 1:length(mycorpus)) {
  attr(mycorpus[[i]], "ID") <- df_title$doc_id[i]
}

View(mycorpus[c(1:18924)]$content)

skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(content_transformer(tolower), removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(mycorpus, FUN = tm_reduce, tmFuns = funcs)
mydtm <- DocumentTermMatrix(a, control = list(wordLengths = c(3,10)))
inspect(mydtm)

################################## Corpus Test for Meta Save ##############################
###########################################################################################





################################### KEY ###################################################
#<
xxx <- as.data.table(x)
xxx <- xxx[, token_bigram := txt_nextgram(token, n = 2), by = list(doc_id, sentence_id)]
xxx <- xxx[, token_trigram := txt_nextgram(token, n = 3), by = list(doc_id, sentence_id)]
xxx <- document_term_frequencies(x = xxx, 
                               document = "doc_id", 
                               term = c("token", "token_bigram", "token_trigram"))
dtm <- document_term_matrix(xxx)
dtm()
View(xxx)

#>
################################### KEY ###################################################



################################### KEY KEY KEY KEY ###################################################
#<;

xxx <- as.data.table(x)
xxx <- subset(xxx, xpos %in% c("NN", "VB"))
xxx <- xxx[, cooccurrence(lemma, order = TRUE), by = list(doc_id)]
View(xxx)

xxxd <- as.data.table(xxx)


# perform the join, eliminating not matched rows from Right
MERGED2 <- merge(x = subset_more, y = xxxd, by = "doc_id", all = FALSE)

View(MERGED)
View(MERGED2)




w <- as.data.table(x)
w <- subset(w, xpos %in% c("NN", "VB"))
w <- w[, cooccurrence(lemma, order = TRUE), by = list(doc_id)]
w <- w %>%
  group_by(doc_id) %>%
  mutate(keywords = paste(term1, term2)) %>%
  summarize(keywords = paste(keywords, collapse = ", "))  
View(w)



#>
################################### KEY KEY KEY KEY ###################################################



############### RAKE KEY !!!!!! ############### 
#^
############ Anticipated ############

bbb <- x[,keywords_rake(term = "token", group = "doc_id", relevant = x$upos %in% c("NOUN", "ADJ")),by = list(doc_id)]
View(bbb)

lll <- keywords_rake(x=x, term = "token", group = "doc_id",
                           relevant = x$upos %in% c("NOUN", "ADJ"))
View(lll)

############ Anticipated ############
#^
############### RAKE KEY !!!!!! ############### 




############### TEST  ALMOST PERFECT ############### 
#^
x$doc_id <- x$doc_id #TEST FOR JOIN TO PHRASE

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
keyw_nounphrases <- keywords_phrases(x$phrase_tag, term = x$token, 
                                     pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = FALSE, 
                                     detailed = TRUE)
#keyw_nounphrases <- subset(keyw_nounphrases, ngram > 1)

View(x$doc_id)
View(x$phrase_tag)
View(keyw_nounphrases)

#^
############### TEST  ALMOST PERFECT ############### 




stats <- keywords_rake(x=x, term = "lemma", group = "sentence",
                       relevant = x$upos %in% c("NOUN", "ADJ"))


stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 30), col
         = "red",
         main = "Keywords Identified by RAKE(NOUN TO ADJ)",
         xlab = "Rake")
View(stats)


#####BOBO#########################################################################################################################################################################################


kw <- subset_anno2

kw <- as.data.frame(kw , stringsAsFactors = FALSE)

keywords <- vector(mode = "list", length = ncol(kw))

for(i in 1:ncol(kw)){
  temp <- as.data.frame(udpipe_annotate(udmodel_english, x = kw[, i]))
  keywords[[i]] <- temp$lemma[temp$upos == "NOUN"]
}


View(temp)

View(subset_anno2)
View(kw)
View(keywords[[1]])


###TEST NEW #

x$phrase_tag <- as_phrasemachine(temp$upos, type = "upos")

for(i in 1:ncol(kw)){
  temp <- as.data.frame(udpipe_annotate(udmodel_english, x = kw[, i]))
  keywords[[i]] <- keywords_phrases(x$phrase_tag, term = temp$token, 
                                    pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, 
                                    detailed = TRUE)
}


keywords_phrases(x$phrase_tag, term = x$token, 
                 pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = FALSE, 
                 detailed = TRUE)
gc()




##########################################################################################

#NEW CHILL TEST


y <- as.data.table(x)
#y <- subset(y, xpos %in% c("NN", "JJ"))
y <- subset(y)
y <- y[, cooccurrence(lemma, order = FALSE), by = list(doc_id)]
head(y)
View(y)



##############
