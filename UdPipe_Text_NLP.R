#Fantasy

install.packages('dplyr')
install.packages('ggplot2')
install.packages('stringr')
install.packages('udpipe')
install.packages('lattice')
install.packages()
install.packages()
install.packages()
install.packages()

library(dplyr)
library(ggplot2)
library(stringr)
library(udpipe)
library(lattice)
library()
library()
library()
library()

gc()

stats <- read.csv('C:/Users/rwfra/Desktop/Data Science/Fantasy/pbp-main.csv', header = T, stringsAsFactors = F)

stats %>% group_by(GameDate) %>% count() %>% arrange(desc(n))

stats %>% group_by(GameDate) %>% count() %>% ggplot() + geom_line(aes(GameDate,n, group = 1))

stats_more <- stats %>% mutate(year = str_sub(GameDate,1,4),
                               month = str_sub(GameDate,6,7),
                               day = str_sub(GameDate,9,10))

stats_more %>% group_by(year) %>% count()  %>% ggplot() + geom_bar(aes(year,n), stat ='identity')




model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

stats_more_NYG <- stats_more %>% filter(OffenseTeam == 'NYG' & DefenseTeam == 'SF' & GameDate >= as.Date(2017-01-01))
stats_more_SF <- stats_more %>% filter(OffenseTeam == 'SF' & DefenseTeam == 'NYG' & GameDate >= as.Date(2017-01-01))

o <- udpipe_annotate(udmodel_english, stats_more_NYG$Description)
d <- udpipe_annotate(udmodel_english, stats_more_SF$Description)

oo <- data.frame(o)
View(oo)
dd <- data.frame(d)


par(mfrow = c(2, 2))


stats <- keywords_rake(x = oo, term = "lemma", group = "doc_id", 
                       relevant = oo$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 40), col = "red", 
         main = "Pair 1.1 Keywords identified by RAKE", 
         xlab = "Rake")

stats <- keywords_rake(x = dd, term = "lemma", group = "doc_id", 
                       relevant = dd$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 40), col = "red", 
         main = "Pair 1.2 Keywords identified by RAKE", 
         xlab = "Rake")


## Using a sequence of POS tags (noun phrases / verb phrases)
oo$phrase_tag <- as_phrasemachine(oo$upos, type = "upos")
stats <- keywords_phrases(x = oo$phrase_tag, term = tolower(oo$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 40), col = "magenta", 
         main = "Pair 1.1 Keywords - simple noun phrases", xlab = "Frequency")

## Using a sequence of POS tags (noun phrases / verb phrases)
dd$phrase_tag <- as_phrasemachine(dd$upos, type = "upos")
stats <- keywords_phrases(x = dd$phrase_tag, term = tolower(dd$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 40), col = "magenta", 
         main = "Pair 1.2 Keywords - simple noun phrases", xlab = "Frequency")


###############################

stats <- keywords_rake(x = oo, term = "lemma", group = "doc_id", 
                       relevant = oo$upos %in% c("NOUN","ADV","VERB","PROPN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 40), col = "red", 
         main = "Pair 1.1 Keywords identified by RAKE", 
         xlab = "Rake")

stats <- keywords_rake(x = dd, term = "lemma", group = "doc_id", 
                       relevant = dd$upos %in% c("NOUN","ADV","VERB","PROPN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 40), col = "red", 
         main = "Pair 1.2 Keywords identified by RAKE", 
         xlab = "Rake")


#################################

stats <- keywords_rake(x = oo, term = "lemma", group = "doc_id", 
                       relevant = oo$upos %in% c("NOUN","NOUN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 40), col = "red", 
         main = "Pair 1.1 Keywords identified by RAKE", 
         xlab = "Rake")

stats <- keywords_rake(x = dd, term = "lemma", group = "doc_id", 
                       relevant = dd$upos %in% c("NOUN","NOUN"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 40), col = "red", 
         main = "Pair 1.2 Keywords identified by RAKE", 
         xlab = "Rake")
