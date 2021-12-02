# library(corplingr) # see https://gederajeg.github.io/corplingr/
library(tidyverse) # see https://www.tidyverse.org/

# Casasanto et al's re-analysis (appearing in the INTRODUCTION of our paper) ========================

# If you have not installed corplingr, install it from github using the remotes package; uncomment the codes in lines 7, 8, and 9 below
# library(remotes)
# install_github("gederajeg/corplingr")
# library(corplingr)

# if you have not installed tidyverse, install it via: install.packages("tidyverse")

# generate concordance for "waktu panjang", "waktu banyak" (Casasanto et al 2004), and "banyak waktu" and "panjang waktu" from the corpus used in the paper (newscrawl 2016)
# mycorpus_filepath <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_newscrawl_2016_1M-sentences.txt"
# mysearch_regexes <- "(?<!\\-)\\b(waktu (banyak|panjang)|(panjang|banyak) waktu)\\b(?!\\-)"
# durations <- concord_leipzig(leipzig_path = mycorpus_filepath,
#                              pattern = mysearch_regexes)
# 
# # save the output to be shared
# write_tsv(durations, file = "data/durations_conc.txt")

# load the concordance output
durations <- readr::read_tsv("data/durations_conc.txt")

# create tabulation of the search phrases/patterns
durations_count <- durations %>% 
  mutate(node = str_to_lower(node)) %>% # lower case the nodeword
  count(node) %>% 
  mutate(grouping_key = if_else(str_detect(node, "waktu$"), "1", "2"))

# compare proportion of "waktu banyak" vs. "waktu panjang"
prop1 <- durations_count %>% 
  filter(str_detect(node, "^waktu")) %>% 
  mutate(perc = n/sum(n) * 100)
prop1
prop1 %>% select(phrases = node, n, perc) %>% write_tsv(file = "data/duration_waktu-banyak_vs_waktu-panjang.txt")

# chisqtest for prop1
phrases_group1 <- prop1$n
names(phrases_group1) <- prop1$node
chisq.test(phrases_group1)
chisq.test(phrases_group1)$p.value
chisq.test(phrases_group1)$p.value < 0.001
chisq.test(phrases_group1)$residuals
chisq.test(phrases_group1)$stdres



# compare proportion of "banyak waktu" vs. "waktu panjang"
prop2 <- durations_count %>% 
  filter(node %in% c("banyak waktu", "waktu panjang")) %>% 
  mutate(perc = n/sum(n) * 100)
prop2
prop2 %>% select(phrases = node, n, perc) %>% write_tsv(file = "data/duration_banyak-waktu_vs_waktu-panjang.txt")

# chisqtest for prop2
phrases_group2 <- prop2$n
names(phrases_group2) <- prop2$node
chisq.test(phrases_group2)
chisq.test(phrases_group2)$p.value
chisq.test(phrases_group2)$p.value < 0.001
chisq.test(phrases_group2)$residuals
chisq.test(phrases_group2)$stdres



# compare proportion of "banyak waktu" vs. "panjang waktu"
prop3 <- durations_count %>% 
  filter(node %in% c("banyak waktu", "panjang waktu")) %>% 
  mutate(perc = n/sum(n) * 100)
prop3
prop3 %>% select(phrases = node, n, perc) %>% write_tsv(file = "data/duration_banyak-waktu_vs_panjang-waktu.txt")

# chisqtest for prop3
phrases_group3 <- prop3$n
names(phrases_group3) <- prop3$node
chisq.test(phrases_group3)
chisq.test(phrases_group3)$p.value
chisq.test(phrases_group3)$p.value < 0.001
chisq.test(phrases_group3)$residuals
chisq.test(phrases_group3)$stdres



# compare proportion of "banyak waktu" vs. "waktu banyak"
prop4 <- durations_count %>% 
  filter(str_detect(node, "banyak")) %>% 
  mutate(perc = n/sum(n) * 100)
prop4
prop4 %>% select(phrases = node, n, perc) %>% write_tsv(file = "data/duration_banyak-waktu_vs_waktu-banyak.txt")

# chisqtest for prop4
phrases_group4 <- prop4$n
names(phrases_group4) <- prop4$node
chisq.test(phrases_group4)
chisq.test(phrases_group4)$p.value
chisq.test(phrases_group4)$p.value < 0.001
chisq.test(phrases_group4)$residuals
chisq.test(phrases_group4)$stdres



# google hits (13 Nov 2021)
## parameters:
### search pattern 1: "waktu banyak" site:.id -> about 91,400 hits (0.36 seconds)
### search pattern 2: "banyak waktu" site:.id -> about 885,000 hits (0.42 seconds)
### search pattern 3: "waktu panjang" site:.id -> about 147,000 hits (0.39 seconds)
### search pattern 4: "panjang waktu" site:.id -> about 25,800 hits (0.31 seconds)

gg_waktu_banyak <- 91400
gg_banyak_waktu <- 885000
gg_waktu_panjang <- 147000
gg_panjang_waktu <- 25800

# ratio "waktu banyak" to "banyak waktu" is 1 to 9.68
# so for every one occurrence of "waktu banyak", there are around 9.68 occurrences of "banyak waktu"
round(gg_banyak_waktu/gg_waktu_banyak, 2)

# ratio "waktu banyak" to "waktu panjang" is 1 to 1.61
# so for every one occurrence of "waktu banyak", there are around 1.61 occurrences of "waktu panjang"
round(gg_waktu_panjang/gg_waktu_banyak, 2)