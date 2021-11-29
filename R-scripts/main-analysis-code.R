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




# Data and Method section =================
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




# Linguistic Data Analyses =================
my_corpus_path <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_newscrawl_2016_1M-sentences.txt"

## check the frequency for some motion verbs with "masa" in the 2-gram database of "masa"
masa_2gr <- readr::read_tsv("data/masa_2grams.txt")
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
  filter(ngram != "dimasukkan_masa", # corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)dimasukkan[^a-zA-Z-]+masa\\b")$node_sentences
         ngram != "masa_perjalanan", # corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)masa[^a-zA-Z-]+perjalanan\\b")$node_sentences
         ngram != "masa_menjalani", # corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)masa[^a-zA-Z-]+menjalani\\b")$node_sentences
         ngram != "jalan_masa", # corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)jalan[^a-zA-Z-]+masa\\b")$node_sentences
         ngram != "lalu_masa") # corplingr::concord_leipzig(my_corpus_path, pattern = "\\b(?i)lalu[^a-zA-Z-]+masa\\b")$node_sentences
moving_ego_masa_2gr_regex <- "(^(me)?masuki?|^(me)?lewat(i|kan)|jalan(i(lah)?|kan)|lalui|lintas|[tn]iggalkan)_masa$"
moving_time_masa_2gr <- mot_verb_masa_2gr %>% filter(!str_detect(ngram, moving_ego_masa_2gr_regex))
(moving_time_masa_2gr_freq <- unlist(rename(tally(moving_time_masa_2gr, n), moving_time = n)))
moving_ego_masa_2gr <- mot_verb_masa_2gr %>% filter(str_detect(ngram, moving_ego_masa_2gr_regex))
(moving_ego_masa_2gr_freq <- unlist(rename(tally(moving_ego_masa_2gr, n), moving_ego = n)))

## check the frequency for some motion verbs with "zaman" in the 2-gram database of "zaman"
zaman_2gr <- readr::read_tsv("data/zaman_2grams.txt")
mot_verb_zaman_2gr <- zaman_2gr %>% 
  filter(str_detect(ngram, "(lewat|(?<!ter)masuk|datang|(?<!se)lalui?|eluar(?!ga)|pergi|jalan|lintas|[tn]inggalk?an)")) %>% 
  as.data.frame()
(moving_time_zaman_2gr_freq <- unlist(rename(tally(filter(mot_verb_zaman_2gr, ngram %in% c("berjalannya_zaman", "datangnya_zaman", "ketinggalan_zaman", "ditinggalkan_zaman")), n), moving_time = n)))
(moving_ego_zaman_2gr_freq <- unlist(select(rename(filter(mot_verb_zaman_2gr, ngram %in% c("melewati_zaman")), moving_ego = n), moving_ego)))

## check the frequency for some motion verbs with "waktu" in the 2-gram database of "waktu"
waktu_2gr <- readr::read_tsv("data/waktu_2grams.txt")
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
png(filename = "figures/moving_ego_moving_time_plot-1.png", width = 6.8, height = 6, units = "in", res = 300)
ego_rp_bp <- barplot(prop.table(ego_rp_deictic_model), 
                      names.arg = c("Moving Time", "Moving Ego"), 
                      main = "2-grams evoking Moving Time and Moving Ego\n(combined data across the studied TIME words)", 
                      ylab = "Proportion",
                      xlab = "Axis",
                      sub = "Numbers inside the bars are the raw frequencies.")
text(ego_rp_bp, c(.2, .05), labels = paste("N=", prettyNum(ego_rp_deictic_model, big.mark = ","), sep = ""))
abline(h = 1/2, col = "red", lty = 2)
text(x = 1.9, y = 0.53, labels = "Expected proportion", col = "red")
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






# Gesture Data Analyses ==========================
## load the gesture annotation notes
gesture_notes <- readr::read_lines("data/temporal_gesture_annotation_notes.txt")

## prepare the data
sequential_lateral <- length(grep("sequential-lateral", gesture_notes))
sequential_sagittal <- length(grep("sequential-sagittal", gesture_notes))
sequential_gestures <- sum(sequential_lateral, sequential_sagittal) # see cell D63 in the Excel Sheet file

deictic_lateral_future <- length(grep("deictic-lateral-future", gesture_notes))
deictic_lateral_present <- length(grep("deictic-lateral-present", gesture_notes))
deictic_lateral_past <- length(grep("deictic-lateral-past", gesture_notes))
deictic_lateral_gestures <- sum(deictic_lateral_future, deictic_lateral_present, deictic_lateral_past) # see cell E52 in Excel Sheet file

deictic_sagittal_future <- length(grep("deictic-sagittal-future", gesture_notes))
deictic_sagittal_present <- length(grep("deictic-sagittal-present", gesture_notes))
deictic_sagittal_past <- length(grep("deictic-sagittal-past", gesture_notes))
deictic_sagittal_gestures <- sum(deictic_sagittal_future, deictic_sagittal_present, deictic_sagittal_past) # see cell E46 in Excel Sheet file

deictic_gestures <- c(lateral_gestures = deictic_lateral_gestures, 
                      sagittal_gestures = deictic_sagittal_gestures) 
deictic_gestures
sum(deictic_gestures)
round(prop.table(deictic_gestures), 2)

## graphical (barplot) representation (Figure 1 in the paper)
png(filename = "figures/deictic_gestures_plot-1.png", width = 6, height = 5.5, units = "in", res = 300)
deictic_bp <- barplot(prop.table(deictic_gestures), 
                      names.arg = gsub("_gestures", "", names(deictic_gestures)), 
                      main = "Types of gestural axes for the deictic time", 
                      ylab = "Proportion",
                      xlab = "Axis",
                      sub = "Numbers inside the bars are the raw frequencies.")
text(deictic_bp, .2, labels = paste("N=", deictic_gestures, sep = ""))
abline(h = 1/2, col = "red", lty = 2)
text(x = 1.9, y = 0.53, labels = "Expected proportion", col = "red")
dev.off()

## statistical, chi-square test for the comparison of sub-type of the deictic gestures
chisq.test(deictic_gestures)
chisq.test(deictic_gestures)$p.value
chisq.test(deictic_gestures)$p.value < 0.05 # not significant different from chance
binom.test(deictic_gestures[1], sum(deictic_gestures))$p.value # not significant different from chance (two-tailed)

## distribution of the axes across the element of deictic time (i.e. past, present, future)
# deictic_times_in_gestures <- c(past = 20, # cell H45 in Excel Sheet file
#                                present = 7, # cell I45 in Excel Sheet file
#                                future = 3) # cell J45 in Excel Sheet file
# sagittal <- c(past = 9, # cell H43 in Excel Sheet file
#               present = 1, # cell I43 in Excel Sheet file
#               future = 1) # cell J43 in Excel Sheet file
# lateral <- c(past = 11, # cell H44 in Excel Sheet file
#              present = 6, # cell I43 in Excel Sheet file
#              future = 2) # cell J43 in Excel Sheet file
# (deictic_across_axes <- t(rbind(lateral, sagittal)))
deictic_across_axes <- cbind(past = c(deictic_sagittal_past, deictic_lateral_past), 
                             present = c(deictic_sagittal_present, deictic_lateral_present), 
                             future = c(deictic_sagittal_future, deictic_lateral_future))
(rownames(deictic_across_axes) <- c("sagittal", "lateral"))
(deictic_across_axes <- t(deictic_across_axes)[, c(2, 1)])
prop.table(deictic_across_axes, 2)

## two-tailed fisher exact
(deictic_pfye <- round(fisher.test(deictic_across_axes)$p.value, 3))

## graphical (barplot) representation (Figure 2 in the paper)
png(filename = "figures/deictic_gestures_plot-2.png", width = 6.5, height = 5, units = "in", res = 300)
deictic_bp1 <- barplot(prop.table(deictic_across_axes, 2), 
                       beside = TRUE, 
                       legend.text = TRUE, 
                       args.legend = list(x = "topleft", bty = "n"), 
                       ylab = "Proportion",
                       xlab = "Axis",
                       ylim = c(0, .9),
                       sub = "Numbers above the bars are the raw frequencies.",
                       main = "Distribution of the deictic times across axes")
text(deictic_bp1, 
     prop.table(deictic_across_axes, 2)+.03, 
     labels = paste("N=", deictic_across_axes, sep = ""), 
     cex = .9)
dev.off()

## distribution of the deictic time in relation to the front-back vs. left-right axes
# axis <- c(rep("sagittal", 4), rep("lateral", 4))
# time <- c("past", "past", "future", "present", "present", "future", "past", "past")
# position <- c("behind", "front", "front", "centre", "centre", "right", "left", "right")
# n <- c(8, 1, 1, 1, 6, 2, 9, 2)
# (deictic_position <- data.frame(axis, time, position, n))
deictic_position_rgx <- "(sagittal|lateral)-(past|future|present)-(behind|front|centre|left|right)"
m <- stringr::str_extract_all(gesture_notes, deictic_position_rgx, simplify = TRUE)
m <- m[nzchar(m)]
m_df <- as.data.frame(table(m))
deictic_position <- m_df %>% 
  arrange(desc(m)) %>% 
  rename(search_pattern = m, n = Freq) %>% 
  extract(col = search_pattern, 
          into = c("axis", "time", "position"), 
          regex = "([^-]+)-([^-]+)-([^-]+)",
          remove = TRUE)
deictic_position

### Binomial for PAST IS BEHIND vs. PAST IS FRONT
(past_behind_front <- pull(filter(deictic_position, time == "past", axis == "sagittal"), n))
(names(past_behind_front) <- pull(filter(deictic_position, time == "past", axis == "sagittal"), position))
round(prop.table(past_behind_front), 2)
round(prop.table(past_behind_front)*100, 2)
chisq.test(past_behind_front)
chisq.test(past_behind_front)$p.value < 0.01
binom.test(past_behind_front)
binom.test(past_behind_front)$p.value < 0.05

### added from the gesture that talks about the PRESENT but before that moves their hands laterally.
past_left <- length(grep("\\<past-left\\/\\>", gesture_notes, perl = TRUE))
past_right <- length(grep("\\<past-right\\/\\>", gesture_notes, perl = TRUE))

### combine all past lateral gestures
past_left_all <- past_left + pull(filter(deictic_position, time == "past", position == "left"), n)
names(past_left_all) <- "leftward"
past_right_all <- past_right + pull(filter(deictic_position, time == "past", position == "right"), n)
names(past_right_all) <- "rightward"

c(past_left_all, past_right_all)
round(prop.table(c(past_left_all, past_right_all)), 2)

### two-tailed chi-square for goodness-of-fit for the distribution of past in left-vs-right axis
chisq.test(c(past_left_all, past_right_all))
chisq.test(c(past_left_all, past_right_all))$p.value < 0.01

### two-tailed binomial test for the distribution of past in left-vs-right axis
round(binom.test(past_left_all, sum(past_left_all, past_right_all))$p.value, 3)
round(binom.test(past_left_all, sum(past_left_all, past_right_all))$p.value, 3) < 0.01

### graphical (barplot) representation (Figure 3 in the paper)
lateral_past_data <- c(past_left_all, past_right_all)
(lateral_past_data_prop <- prop.table(lateral_past_data))

png(filename = "figures/deictic_gestures_plot-3.png", width = 6.5, height = 5, units = "in", res = 300)
lateral_past_bp <- barplot(height = lateral_past_data_prop,
                           ylab = "Proportion",
                           sub = "Numbers inside the bars are the raw frequencies.",
                           xlab = "Directionality",
                           main = "Lateral gestures for PAST")
text(lateral_past_bp, c(.4, .1), labels = paste("N=", lateral_past_data, sep = ""))
abline(h = 1/2, col = "red", lty = 2)
text(x = 1.9, y = 0.53, labels = "Expected proportion", col = "red")
dev.off()

## distribution of deictic vs. sequential
c(sum(deictic_gestures), sequential_gestures)
round(prop.table(c(sum(deictic_gestures), sequential_gestures)), 2)
binom.test(c(sum(deictic_gestures), sequential_gestures))$p.value
binom.test(c(sum(deictic_gestures), sequential_gestures))$p.value < 0.001
chisq.test(c(sum(deictic_gestures), sequential_gestures))
chisq.test(c(sum(deictic_gestures), sequential_gestures))$p.value < 0.001
