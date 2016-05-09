load('RFmodel/sf_train.Rda')
setwd('~/my_Git_repos/SF_Crime/')
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(doParallel)
library(RTextTools)
trim <- function(dirty) clean <- gsub('[[:punct:]]', '', dirty)

#########################################################################################################################
## some customized text preparation for text mining
### step1. split phrases into words, by each record
sf_train$Address <- as.character(sf_train$Address)
word_by_record <- strsplit(sf_train$Address, ' ')
word_by_record <- llply(word_by_record, trim)
# word_by_record <- llply(word_by_record, tolower)
# word_by_record <- llply(word_by_record, RTextTools::wordStem)
unique_words <- unique(unlist(word_by_record))
sort(unique_words)

### step 2. compute boolean term frequency
term_freq_bool <- llply(word_by_record, function(x){as.numeric(unique_words%in%x)})
term_freq_bool <- as.data.table(do.call(rbind, term_freq_bool))
colnames(term_freq_bool) <- toupper(unique_words)
save(term_freq_bool, file = '~/my_Git_repos/SF_Crime/term_freq_boolean_description.Rda')

### step 3. compute document frequency
system.time(doc_freq <- apply(term_freq_bool, 2, sum)) # 18 sec
system.time(doc_freq <- as.numeric(term_freq_bool[,lapply(.SD, sum, na.rm=TRUE)])) # 1.7 sec
doc_freq_by_word <- data.table(toupper(unique_words), doc_freq)
doc_freq_by_word[order(unique_words)[1:20]]
#### filter out numbers
sf_train[sf_train$``>0, 1:10, with = F]
word_by_record[sf_train$``>0]


### step 4. compute inverse document frequency log natual base.
idf_by_word <- data.table(toupper(unique_words),
                          idf = log(length(word_by_record)/doc_freq))
idf_by_word[order(idf)]

### step 5. compute tf-idf
tf_idf <- t(t(term_freq_bool) * idf_by_word$idf)
dim(tf_idf)
colnames(tf_idf) <- paste('fs_', toupper(unique_words), sep = '')
tf_idf[1:10, colnames(tf_idf)[1:10]]
save(tf_idf, file = 'tf_idf_description.Rda')
load('tf_idf_description.Rda')


na_by_col <- apply(tf_idf, 2, function(x)sum(is.na(x)))
sum(na_by_col)

### step 6. prepare data.table for modeling
sf_train <- data.table(sf_train, tf_idf)

#### remove redundant columns for modeling
sf_train[,c(1,3,10,14,16):=NULL]
dim(sf_train)
save(sf_train, file = '~/my_Git_repos/SF_Crime/sf_train_v2_with_tfidf.Rda')
load('sf_train_v2_with_tfidf.Rda')
colnames(sf_train)

