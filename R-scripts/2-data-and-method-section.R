# Data and Method section =================
library(tidyverse)
temporal_terms <- readr::read_tsv(file = "data/freqlist_temporal_terms.txt")
filter(temporal_terms, match %in% c("masa", "waktu", "zaman"))

## n-gram snippet (Table 1)
df1 <- readr::read_tsv("data/masa_2grams.txt") %>% 
  filter(ngram %in% c("masa_mendatang", "pada_masa")) %>% 
  select(ngram, n)
df2 <- readr::read_tsv("data/waktu_3grams.txt") %>% 
  filter(ngram %in% c("beberapa_waktu_lalu", "dalam_waktu_dekat")) %>% 
  select(ngram, n)
df3 <- readr::read_tsv("data/zaman_4grams.txt") %>% 
  filter(ngram %in% c("seiring_dengan_perkembangan_zaman", 
                      "zaman_yang_semakin_maju")) %>% 
  select(ngram, n)
(df_all <- bind_rows(df1, df2, df3))