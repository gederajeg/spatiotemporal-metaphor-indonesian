library(corplingr)
library(tidyverse)

# Casasanto et al's re-analysis (appearing in the INTRODUCTION of our paper) ========================

# If you have not installed corplingr, install it from github using the remotes package; uncomment the codes in lines 5, 6, and 7 below
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



# compare proportion of "banyak waktu" vs. "waktu banyak"
prop3 <- durations_count %>% 
  filter(str_detect(node, "banyak")) %>% 
  mutate(perc = n/sum(n) * 100)
prop3
prop3 %>% select(phrases = node, n, perc) %>% write_tsv(file = "data/duration_banyak-waktu_vs_waktu-banyak.txt")

# chisqtest for prop3
phrases_group3 <- prop3$n
names(phrases_group3) <- prop3$node
chisq.test(phrases_group3)
chisq.test(phrases_group3)$p.value < 0.001
chisq.test(phrases_group3)$residuals
chisq.test(phrases_group3)$stdres



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




# Gesture Data Analyses ==========================
## prepare the data
sequential_gestures <- 6 # cell D62 in the Excel Sheet file
deictic_gestures <- c(lateral_gestures = 19, # cell E52 in Excel Sheet file
                      sagittal_gestures = 11) # cell E46 in Excel Sheet file
deictic_gestures
round(prop.table(deictic_gestures), 2)

## distribution of deictic vs. sequential
round(prop.table(c(sum(deictic_gestures), sequential_gestures)), 2)
binom.test(c(sum(deictic_gestures), sequential_gestures))$p.value
binom.test(c(sum(deictic_gestures), sequential_gestures))$p.value < 0.001
chisq.test(c(sum(deictic_gestures), sequential_gestures))
chisq.test(c(sum(deictic_gestures), sequential_gestures))$p.value < 0.001

## graphical (barplot) representation (Figure 1 in the paper)
png(filename = "figures/deictic_gestures_plot-1.png", width = 6, height = 5, units = "in", res = 300)
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
deictic_times_in_gestures <- c(past = 20, # cell I45 in Excel Sheet file
                               present = 7, # cell J45 in Excel Sheet file
                               future = 3) # cell K45 in Excel Sheet file
sagittal <- c(past = 9, # cell I43 in Excel Sheet file
              present = 1, # cell J43 in Excel Sheet file
              future = 1) # cell K43 in Excel Sheet file
lateral <- c(past = 11, # cell I44 in Excel Sheet file
             present = 6, # cell J43 in Excel Sheet file
             future = 2) # cell K43 in Excel Sheet file
(deictic_across_axes <- t(rbind(lateral, sagittal)))
prop.table(deictic_across_axes, 2)

## two-tailed fisher exact
(deictic_pfye <- round(fisher.test(deictic_across_axes)$p.value, 3))

## graphical (barplot) representation (Figure 2 in the paper)
png(filename = "figures/deictic_gestures_plot-2.png", width = 6.5, height = 5, units = "in", res = 300)
deictic_bp1 <- barplot(prop.table(deictic_across_axes, 2), 
                       beside = TRUE, 
                       legend.text = TRUE, 
                       args.legend = list(x = "topright", bty = "n"), 
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
axis <- c(rep("sagittal", 4), rep("lateral", 4))
time <- c("past", "past", "future", "present", "present", "future", "past", "past")
position <- c("behind", "front", "front", "centre", "centre", "right", "left", "right")
n <- c(8, 1, 1, 1, 6, 2, 9, 2)
(deictic_position <- data.frame(axis, time, position, n))

### Binomial for PAST IS BEHIND vs. PAST IS FRONT
(past_behind_front <- pull(filter(deictic_position, time == "past", axis == "sagittal"), n))
(names(past_behind_front) <- pull(filter(deictic_position, time == "past", axis == "sagittal"), position))
binom.test(past_behind_front)
binom.test(past_behind_front)$p.value < 0.05

### added from the gesture that talks about the PRESENT but before that moves their hands laterally.
past_left <- 4
past_right <- 1

### combine all past lateral gestures
past_left_all <- past_left + pull(filter(deictic_position, time == "past", position == "left"), n)
names(past_left_all) <- "leftward"
past_right_all <- past_right + pull(filter(deictic_position, time == "past", position == "right"), n)
names(past_right_all) <- "rightward"

round(prop.table(c(past_left_all, past_right_all)), 2)

### two-tailed binomial test for the distribution of past in left-vs-right axis
round(binom.test(past_left_all, sum(past_left_all, past_right_all))$p.value, 3)
round(binom.test(past_left_all, sum(past_left_all, past_right_all))$p.value, 3) < 0.05

### two-tailed chi-square for goodness-of-fit for the distribution of past in left-vs-right axis
chisq.test(c(past_left_all, past_right_all))
chisq.test(c(past_left_all, past_right_all))$p.value < 0.05

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