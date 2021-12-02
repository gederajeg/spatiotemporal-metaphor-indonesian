# Linguistic Data Analyses =================
library(tidyverse)
my_corpus_path <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_newscrawl_2016_1M-sentences.txt"

## check the frequency for some motion verbs with "masa" in the 2-gram database of "masa"
masa_2gr <- read_tsv("data/masa_2grams.txt")
mot_verb_masa_2gr <- masa_2gr %>% 
  filter(str_detect(ngram, "(lewat|(?<!ter)masuk|datang|(?<!se)lalui?|eluar(?!ga)|pergi|jalan|lintas|[tn]inggalk?an)")) %>%
  mutate(across(where(is_character), ~str_replace_all(., "me-masuki", "memasuki")),
         across(where(is_character), ~str_replace_all(., "me-lewati", "melewati")),
         across(where(is_character), ~str_replace_all(., "men-datang", "mendatang")),
         across(where(is_character), ~str_replace_all(., "jalani-lah", "jalanilah")),
         across(where(is_character), ~str_replace_all(., "men-jalani", "menjalani")),
         across(where(is_character), ~str_replace_all(., "masa-masa", "masa"))) %>% # replace plural, reduplicated masa into singular
  group_by(ngram, w1, w2) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  as.data.frame() %>% 
  # remove 'masa' referring to 'mass of people'
  filter(!ngram %in% c("masa_kemasukan", "masa_masukan", "masa_masuk"))
mot_verb_masa_2gr <- mot_verb_masa_2gr %>% 
  # remove other irrelevant pattern after checking the concordance of full sentences
  filter(ngram != "dimasukkan_masa", 
         # use the following code to check the concordance -> corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)dimasukkan[^a-zA-Z-]+masa\\b")$node_sentences
         ngram != "masa_perjalanan", 
         # use the following code to check the concordance -> corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)masa[^a-zA-Z-]+perjalanan\\b")$node_sentences
         ngram != "masa_menjalani", 
         # use the following code to check the concordance -> corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)masa[^a-zA-Z-]+menjalani\\b")$node_sentences
         ngram != "jalan_masa", 
         # use the following code to check the concordance -> corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)jalan[^a-zA-Z-]+masa\\b")$node_sentences
         ngram != "lalu_masa") # use the following code to check the concordance -> corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)lalu[^a-zA-Z-]+masa\\b")$node_sentences
moving_ego_masa_2gr_regex <- "(^(me)?masuki?|^(me)?lewat(i|kan)|jalan(i(lah)?|kan)|lalui|lintas|[tn]iggalkan)_masa$"
moving_time_masa_2gr <- mot_verb_masa_2gr %>% filter(!str_detect(ngram, moving_ego_masa_2gr_regex))
(moving_time_masa_2gr_freq <- unlist(rename(tally(moving_time_masa_2gr, n), moving_time = n)))
moving_ego_masa_2gr <- mot_verb_masa_2gr %>% filter(str_detect(ngram, moving_ego_masa_2gr_regex))
(moving_ego_masa_2gr_freq <- unlist(rename(tally(moving_ego_masa_2gr, n), moving_ego = n)))

## check the frequency for some motion verbs with "zaman" in the 2-gram database of "zaman"
zaman_2gr <- read_tsv("data/zaman_2grams.txt")
mot_verb_zaman_2gr <- zaman_2gr %>% 
  filter(str_detect(ngram, "(lewat|(?<!ter)masuk|datang|(?<!se)lalui?|eluar(?!ga)|pergi|jalan|lintas|[tn]inggalk?an)")) %>% 
  as.data.frame()
(moving_time_zaman_2gr_freq <- unlist(rename(tally(filter(mot_verb_zaman_2gr, ngram %in% c("berjalannya_zaman", "datangnya_zaman", "ketinggalan_zaman", "ditinggalkan_zaman")), n), moving_time = n)))
(moving_ego_zaman_2gr_freq <- unlist(select(rename(filter(mot_verb_zaman_2gr, ngram %in% c("melewati_zaman")), moving_ego = n), moving_ego)))

## check the frequency for some motion verbs with "waktu" in the 2-gram database of "waktu"
waktu_2gr <- read_tsv("data/waktu_2grams.txt")
mot_verb_waktu_2gr <- waktu_2gr %>% 
  filter(str_detect(ngram, "(lewat|(?<!ter)masuk|datang|(?<!se)lalui?|eluar(?!ga)|pergi|jalan|lintas|[tn]inggalk?an)")) %>% 
  as.data.frame() %>%
  mutate(across(where(is_character), ~str_replace_all(., "ber-jalannya", "berjalannya")),
         across(where(is_character), ~str_replace_all(., "berjalanannya_waktu", "berjalannya_waktu")))
moving_time_waktu_2gr <- filter(mot_verb_waktu_2gr, ngram %in% c("waktu_lalu", "berjalannya_waktu", "waktu_mendatang", "waktu_berjalan", "lewat_waktu", "waktu_berlalu", "berjalan_waktu", "masuknya_waktu", "jalannya_waktu"))
(moving_time_waktu_2gr_freq <- unlist(rename(tally(moving_time_waktu_2gr, n), moving_time = n)))
moving_ego_waktu_2gr <- filter(mot_verb_waktu_2gr, ngram %in% c("pelintas_waktu", "menjalani_waktu", "meninggalkan_waktu", "melewatkan_waktu", "waktu_terlewat", "melalui_waktu", "melewati_waktu", "memasuki_waktu", "kelewat_waktu", "masuk_waktu"))
(moving_ego_waktu_2gr_freq <- unlist(rename(tally(moving_ego_waktu_2gr, n), moving_ego = n)))

## combine MOVING TIME and MOVING EGO across words
(moving_ego_ALL_2gr_freq <- sum(c(moving_ego_masa_2gr_freq, moving_ego_zaman_2gr_freq, moving_ego_waktu_2gr_freq)))
(moving_time_ALL_2gr_freq <- sum(c(moving_time_masa_2gr_freq, moving_time_zaman_2gr_freq, moving_time_waktu_2gr_freq)))
(ego_rp_deictic_model <- c(moving_time = moving_time_ALL_2gr_freq, moving_ego = moving_ego_ALL_2gr_freq))

moving_ego_ALL_2gr <- bind_rows(moving_ego_masa_2gr, moving_ego_waktu_2gr, filter(mot_verb_zaman_2gr, ngram %in% c("melewati_zaman"))) %>% 
  mutate(lu = str_replace(ngram, "_?(waktu|masa|zaman)_?", ""))
moving_ego_ALL_2gr_lu <- moving_ego_ALL_2gr %>% 
  group_by(lu) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  mutate(mapping = "moving_ego")
moving_ego_ALL_2gr_lu %>% slice_max(order_by = n, n = 5)

moving_time_ALL_2gr <- bind_rows(moving_time_masa_2gr, moving_time_waktu_2gr, filter(mot_verb_zaman_2gr, ngram %in% c("berjalannya_zaman", "datangnya_zaman", "ketinggalan_zaman", "ditinggalkan_zaman"))) %>% 
  mutate(lu = str_replace(ngram, "_?(waktu|masa|zaman)_?", ""))
moving_time_ALL_2gr %>% slice_max(order_by = n, n = 3)
moving_time_ALL_2gr_lu <- moving_time_ALL_2gr %>% 
  group_by(lu) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  mutate(mapping = "moving_time")
ego_rp_deictic_model_df <- bind_rows(moving_time_ALL_2gr_lu, moving_ego_ALL_2gr_lu) %>% 
  group_by(mapping) %>% 
  mutate(perc_lu = n/sum(n) * 100)
ego_rp_deictic_model_df

## statistical test for the frequency of Moving Ego and Moving Time for Deictic time
prop.table(ego_rp_deictic_model)
chisq.test(ego_rp_deictic_model)
binom.test(ego_rp_deictic_model)

## visualisation of moving time and moving ego in linguistic analyses
png(filename = "figures/fig-1-moving_ego_moving_time.png", width = 6.8, height = 6, units = "in", res = 300)
ego_rp_bp <- barplot(prop.table(ego_rp_deictic_model), 
                     names.arg = c("Moving Time", "Moving Ego"), 
                     main = "2-grams evoking the Moving Time and the Moving Ego models\n(combined data across the studied TIME words)", 
                     ylab = "Proportion",
                     col = "#D3DDDC",
                     xlab = "Axis",
                     sub = "Numbers inside the bars are the raw frequencies.")
text(ego_rp_bp, c(.2, .05), labels = paste("N=", prettyNum(ego_rp_deictic_model, big.mark = ","), sep = ""))
# abline(h = 1/2, col = "red", lty = 2)
# text(x = 1.9, y = 0.53, labels = "Baseline proportion", col = "red")
dev.off()

## DEPAN, BELAKANG, DATANG, MENDATANG, LALU ========
namahari <- c("senin", "selasa",
              "rabu", "kamis",
              "jumat", "sabtu",
              "minggu", "pekan")

namabulan <- c("januari", "january", "februari", "february",
               "maret", "march", "april", "mei", "may", "juni", "juli", "july", "august",
               "agustus", "september", "oktober", "october", "november",
               "desember", "december")

timenouns <- c(namahari, namabulan, "tahun", "bulan", "musim", "masa", "waktu")

timeunits <- c("tahun", "bulan", "hari", "era", "jaman", "zaman", "masa", "waktu", "saat", "jam", "menit", "detik", "sepekan", "pekan", "minggu", "periode", "period", "momen")

timewords_all <- unique(c(namahari, namabulan, timenouns, timeunits))

peekgram <- function(ngram = NULL, pattern = NULL) {
  tbl <- filter(ngram, if_any(starts_with("w"), ~str_detect(., pattern)))
  return(tbl)
}

# gr2 <- read_tsv("data_ngram_raw/ngram_2_raw.txt") # read the whole-corpus 2-gram data (387MB!)
# gr3 <- read_tsv("data_ngram_raw/ngram_3_raw.txt") # read the whole-corpus 3-gram data (540.2MB!)
# gr4 <- read_tsv("data_ngram_raw/ngram_4_raw.txt") # read the whole-corpus 4-gram data (666.5MB!)
# timewords_depan <- gr2 %>% filter(w1 %in% timewords_all, w2 == "depan")
# timewords_belakang <- gr2 %>% filter(w1 %in% timewords_all, w2 == "belakang")
# write_tsv(timewords_depan, file = "data/timewords_depan.txt")
# write_tsv(timewords_belakang, file = "data/timewords_belakang.txt")
timewords_depan <- read_tsv("data/timewords_depan.txt")
timewords_belakang <- read_tsv("data/timewords_belakang.txt")

# all 2-, 3-, 4-, and 5-gram DEPAN data
(depan_2gr <- read_tsv("data/depan_2grams.txt"))
(depan_3gr <- read_tsv("data/depan_3grams.txt"))
(depan_4gr <- read_tsv("data/depan_4grams.txt"))
(depan_5gr <- read_tsv("data/depan_5grams.txt"))

## Analysis on [TIME NOUN + depan]
timewords_depan %>% count(ngram, sort = TRUE) # masa depan 'period front; future' is the most frequent, followed by tahun depan 'year front; next year'
## check the full sentences
# mycorpus_filepath <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_newscrawl_2016_1M-sentences.txt"
# corplingr::concord_leipzig(mycorpus_filepath, pattern = "\\bhari\\sdepan\\b") %>% pull(node_sentences) %>% write_lines(file = "data/sent_hari_depan.txt")
(sent_hari_depan <- read_lines("data/sent_hari_depan.txt"))

# all 2-, 3-, 4-, and 5-gram BELAKANG data
(belakang_2gr <- read_tsv("data/belakang_2grams.txt"))
(belakang_3gr <- read_tsv("data/belakang_3grams.txt"))
(belakang_4gr <- read_tsv("data/belakang_4grams.txt"))
(belakang_5gr <- read_tsv("data/belakang_5grams.txt"))

## Analysis on [TIME NOUN + belakang]
timewords_belakang %>% count(ngram, sort = TRUE) # very low frequency (only three types [five tokens])
## check the full sentences
# mycorpus_filepath <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_newscrawl_2016_1M-sentences.txt"
# corplingr::concord_leipzig(mycorpus_filepath, pattern = "\\b(tahun|bulan|sepekan)\\sbelakang\\b") %>% pull(node_sentences) %>% write_lines(file = "data/sent_TIME_belakang.txt")
(sent_TIME_belakang <- read_lines("data/sent_TIME_belakang.txt"))

## checking 'akan mendatang' in 2-, 3-, and 4-gram data returns zero results (see below)
# > gr2 %>% filter(w2 == "mendatang", w1 == "akan")
# # A tibble: 0 x 3
# # … with 3 variables: ngram <chr>, w1 <chr>, w2 <chr>
# > gr3 %>% filter(w3 == "mendatang", w2 == "akan")
# # A tibble: 0 x 4
# # … with 4 variables: ngram <chr>, w1 <chr>, w2 <chr>, w3 <chr>
# > gr4 %>% filter(w4 == "mendatang", w3 == "akan")
# # A tibble: 0 x 5
# # … with 5 variables: ngram <chr>, w1 <chr>, w2 <chr>, w3 <chr>, w4 <chr>

## checking 'yang mendatang' produces two (2) results (see below based on the 4-gram data)
# > gr4 %>% filter(w4=='mendatang', w3 == 'yang')
# # A tibble: 2 x 5
# ngram                             w1     w2          w3    w4       
# <chr>                             <chr>  <chr>       <chr> <chr>    
# 1 Agenda_persidangan_yang_mendatang Agenda persidangan yang  mendatang
# 2 di_waktu_yang_mendatang           di     waktu       yang  mendatang

## checking 'X mendatang' in 2-gram data
# x_mendatang <- gr2 %>% filter(w2 == "mendatang")
# x_mendatang %>% write_tsv("data/x_mendatang.txt")
x_mendatang <- read_tsv("data/x_mendatang.txt")
x_mendatang %>% count(ngram, sort = TRUE)
# lower-case the first word
x_mendatang %>% 
  mutate(across(where(is_character), str_to_lower)) %>% 
  count(ngram, sort = TRUE)

## checking 'X X mendatang' in 3-gram data
# x_x_mendatang <- gr3 %>% filter(w3 == "mendatang")
# x_x_mendatang %>% write_tsv("data/x_x_mendatang.txt")
x_x_mendatang <- read_tsv("data/x_x_mendatang.txt")
x_x_mendatang %>% count(ngram, sort = TRUE)
# lower-case the first word
x_x_mendatang %>% 
  mutate(across(where(is_character), str_to_lower)) %>% 
  count(ngram, sort = TRUE)

## checking 'X X X mendatang' in 4-gram data
# x_x_x_mendatang <- gr4 %>% filter(w4 == "mendatang")
# x_x_x_mendatang %>% write_tsv("data/x_x_x_mendatang.txt")
x_x_x_mendatang <- read_tsv("data/x_x_x_mendatang.txt")
x_x_x_mendatang %>% count(ngram, sort = TRUE)
# lower-case the first word
x_x_x_mendatang %>% 
  mutate(across(where(is_character), str_to_lower)) %>% 
  count(ngram, sort = TRUE)

## checking 'datang'
# datang_gr2 <- gr2 %>% filter(if_any(starts_with("w"), ~str_detect(., "^datang$")))
# datang_gr2 %>% write_tsv("data/datang_2grams.txt")
# datang_gr3 <- gr3 %>% filter(if_any(starts_with("w"), ~str_detect(., "^datang$")))
# datang_gr3 %>% write_tsv("data/datang_3grams.txt")
# datang_gr4 <- gr4 %>% filter(if_any(starts_with("w"), ~str_detect(., "^datang$")))
# datang_gr4 %>% write_tsv("data/datang_4grams.txt")
datang_gr2 <- read_tsv("data/datang_2grams.txt") %>% 
  mutate(across(where(is_character), str_to_lower))
datang_gr3 <- read_tsv("data/datang_3grams.txt") %>% 
  mutate(across(where(is_character), str_to_lower))
datang_gr4 <- read_tsv("data/datang_4grams.txt") %>% 
  mutate(across(where(is_character), str_to_lower))

## count and filter the ngram where "datang" is the last word in the sequence
datang_gr2 %>% count(ngram, sort = TRUE) %>% filter(str_detect(ngram, "_datang$")) # not too revealing
datang_gr3 %>% count(ngram, sort = TRUE) %>% filter(str_detect(ngram, "_datang$")) # "yang akan datang" as the top pattern, but still can be anything.
datang_gr4 %>% count(ngram, sort = TRUE) %>% filter(str_detect(ngram, "_datang$")) # better!