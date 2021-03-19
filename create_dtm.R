library(textstem)
library(dplyr)
library(quanteda)

setwd("/media/sebi/Storage/Documents/scraper_example")

# Adatok ------------------------------------------------------------------

# beolvassuk a metaadatokat
data <- read.table('data.csv', sep = ';', header = TRUE, encoding = 'utf-8', stringsAsFactors = FALSE, na.strings = '')

# Nézzünk bele
View(data)

# megnezzuk a strukturajat
str(data)

# kerunk egy kis osszefoglalot
summary(data)

# Az adatok eleje és vége
head(data, 10)
tail(data, 10)

# Mit is jelent a pipe ( %>% )?
data %>% head(10) # ugyan az mint az előző

# Alakítsunk át egy változót
data <- data %>% 
  mutate(is_question = ifelse(is_question == 'True', T, F))

# Alakítsuk át a szöveget is
data <- data %>% 
  mutate(text = ifelse(is_question == T, paste(title, text, sep = '\n'), text))

# Válasszunk ki pár sort
data %>% 
  filter(is_question == T) %>% 
  View()

# Válasszunk ki pár oszlopot
data %>% 
  select(c(text, title, id)) %>% 
  View()

# Korpusz -----------------------------------------------------------------

# Csináljunk korpuszt
corp <- corpus(data, text_field = "text")

# Nézzünk bele
summary(corp, 5)

# Tokenek -----------------------------------------------------------------

# Tokenizáljunk
toks <- tokens(corp, 
               remove_punct = T,
               remove_symbols = T,
               remove_numbers = T,
               remove_separators = T)

toks %>% head(10)

# Nézzünk csak kisbetűket
toks <- toks %>% 
  tokens_tolower()

toks %>% head(10)

# Szedjük ki a nagyon rövid tokeneket
toks <- tokens_select(toks, min_nchar = 3)

toks %>% head(10)

# Nézzük meg szerepel-e a Rajk
toks %>% 
  kwic('rajk', case_insensitive = T, valuetype = 'regex') %>% 
  head(30)

# Nézzük meg szerepel-e a Rajk
toks %>% 
  kwic(c('rajk', 'heller', 'bibó'), case_insensitive = T, valuetype = 'regex') %>% 
  head(15)

# Csináljunk n-gramokat a Rajkból
# Nézzük meg először a regexet
# Regex help: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
rajk_regex <- phrase(c('rajk (lászló)? szakk.*', 'rajk szakk.*'))

# Vondjuk össze a tokeneket
tokens_compound(toks, pattern = rajk_regex, valuetype = 'regex') %>% 
  kwic(pattern = 'rajk', valuetype = 'regex', window = 2)

# Mentés
toks <- tokens_compound(toks, pattern = rajk_regex, valuetype = 'regex')

# Nézzünk negációt
# Nézzük meg először a regexet
neg_regex <- phrase(c('(^nem)|(^nincs)|(^sem.{0,3}$) .+'))

# Vondjuk össze a tokeneket
tokens_compound(toks, pattern = neg_regex, valuetype = 'regex') %>% 
  kwic(pattern = '(^nem)|(^nincs)|(^sem.{0,3}$)', valuetype = 'regex', window = 2) %>% 
  head(10)

# Dobjuk ki a stopwordöket
toks <- toks %>% 
  tokens_select(pattern = stopwords("hu"), selection = "remove")

# Nézzünk rá ismét a negációra
tokens_compound(toks, pattern = neg_regex, valuetype = 'regex') %>% 
  kwic(pattern = '(^nem)|(^nincs)|(^sem.{0,3}$)', valuetype = 'regex', window = 2) %>% 
  head(10)
  
# Akkor most mi is a stopword?
# Első 50
stopwords('hu') %>% head(50)
# Ebből negáció
kwic(stopwords('hu'), '(^nem)|(^nincs)|(^sem.{0,3}$)', valuetype = 'regex')

# Egymás melletti tokenek
toks %>% 
  tokens_ngrams(n = 3:4) %>% 
  head(5)

# Egymás melletti tokenek kihagyással
toks %>% 
  tokens_ngrams(n = 3:4, skip = 0:1) %>% 
  head(5)

# Nézzük meg a potenciális ngramokat
textstat_collocations(toks, min_count = 10, tolower = T)

# Stemmelés
# Magyar lemmatizációhoz: https://github.com/oroszgy/awesome-hungarian-nlp
toks <- toks %>% 
  tokens_wordstem(language = 'hu')

# DTM ---------------------------------------------------------------------

# DFM generálása
dtm <- dfm(toks)

# Hány dokumentum?
ndoc(dtm)

# Hány feature?
nfeat(dtm)

# Feature-ök (oszlopok)
head(featnames(dtm), 20)

# Dokumentumok (sorok)
head(docnames(dtm), 20)

# Peremeloszlás
head(colSums(dtm), 10)
head(rowSums(dtm), 10)

# Leggyakoribb szavak
topfeatures(dtm, 10, scheme = 'count')
topfeatures(dtm, 10, scheme = 'docfreq')

# Súlyozás
# TF-IDF
dfm_tfidf(dtm)

# Lehet más is pl dokumentumokon belüli arány
dfm_weight(dtm, 'prop')
# További lehetőségek: c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave", "logsmooth")

# Dimenziócsökkentés
# Eredeti előfordulások eleje és vége
topfeatures(dtm, decreasing = T)
topfeatures(dtm, decreasing = F)

# Legalább kétszer szerepel egy szó
dfm_trim(dtm, min_termfreq = 2, termfreq_type = 'count') %>% 
  topfeatures(decreasing = F)

# Minimum a dokumentumok 10 százalékában benne van egy szó
dfm_trim(dtm, min_docfreq = .1, docfreq_type = 'prop') %>% 
  topfeatures(decreasing = F)


# Leíró statisztika -------------------------------------------------------

# Egyszerű statisztikák
# Így is meg lehet nézni az előfordulási statisztikákat
textstat_frequency(dtm) %>% head(10)
textstat_frequency(dtm) %>% tail(10)

# Szófelhő
set.seed(1969)
textplot_wordcloud(dtm, max_words = 50)

# Szófelhő összehasonlítás - kérdező <-> válaszoló
set.seed(1969)
dfm_group(dtm, 'is_question') %>% 
  textplot_wordcloud(comparison = TRUE, max_words = 50, min_size = 1.5)
