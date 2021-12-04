# Gesture Data Analyses ==========================
library(tidyverse)
library(RColorBrewer)

## load the gesture annotation notes
gesture_notes <- readr::read_lines("data/temporal_gesture_annotation_notes.txt")

## 0. gesture about deictic vs. sequential times
### gestures for talking about sequential time
(sequential_lateral <- sum(str_count(gesture_notes, "<sequential-lateral")))
(sequential_sagittal <- sum(str_count(gesture_notes, "<sequential-sagittal")))
(sequential_gestures_n <- sum(sequential_lateral, sequential_sagittal)) # see cell D63 in the Excel Sheet file
(sequential_directionality_type <- str_extract(unlist(str_extract_all(gesture_notes, "<sequential-[ls].+\\/>")), "[a-z-]+ward"))

sequential_df <- str_subset(gesture_notes, "<sequential-[sl]") %>% 
  str_extract("<.+\\/>") %>% 
  str_replace_all("(^<sequential-|\"\\/>$|\")", "") %>% 
  str_replace_all("(\\/><(speaker=)?)", "_") %>% 
  tibble(origin = .) %>% 
  separate(origin, c("axes", "handedness", "directionality", "congruency", "speaker"), sep = "_")

### proportion of lateral and sagittal axes in sequential time & directionality count
(sequential_axes <- c(sequential_lateral, sequential_sagittal))
(names(sequential_axes) <- c("lateral", "sagittal"))
sequential_axes
round(prop.table(sequential_axes) * 100)
rev(sort(table(sequential_directionality_type)))
sequential_directionality_df <- tibble(directionality = sequential_directionality_type) %>% 
  count(directionality, sort = TRUE) %>% 
  mutate(axes = if_else(directionality == "toward", "sagittal", "lateral"),
         temporal_reference = "sequential") %>% 
  select(temporal_reference, axes, directionality, n)
sequential_directionality_df

### statistical tests for the sagittal vs. lateral axes in sequential time
binom.test(x = sequential_lateral, n = sequential_gestures_n, alternative = "greater")
binom.test(x = sequential_lateral, n = sequential_gestures_n, alternative = "greater")$p.value < 0.05 # TRUE

### gestures for talking about deictic time
(deictic_gestures_n <- sum(str_count(gesture_notes, "<deictic-")))
# 37 gestures accompanying utterances about deictic time (PAST, PRESENT, FUTURE)

### proportion of deictic vs. sequential time gestures
(deictic_vs_sequential <- c(deictic_gestures_n, sequential_gestures_n))
names(deictic_vs_sequential) <- c("deictic", "sequential")
round(prop.table(deictic_vs_sequential) * 100)

### statistical tests for deictic time vs. sequential time
binom.test(x = deictic_gestures_n, n = sum(deictic_vs_sequential), p = 0.5)
binom.test(x = deictic_gestures_n, n = sum(deictic_vs_sequential), p = 0.5)$p.value < 0.001

# DEICTIC TIME
## 1. axes: 
### 1.1 lateral (leftward-rightward), 
axes_deictic_lateral_sum <- str_subset(gesture_notes, "<deictic-") %>% 
  str_count("<leftward/>|<rightward/>") %>% sum()
axes_deictic_lateral_sum
axes_deictic_lateral_type <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("<leftward/>|<rightward/>") %>% 
  str_extract("(?<=<)leftward(?=\\/>)|(?<=<)rightward(?=\\/>)")
table(axes_deictic_lateral_type)
axes_deictic_lateral_type_df <- tibble(directionality = axes_deictic_lateral_type) %>% 
  mutate(temporal_reference = "deictic", axes = "lateral") %>% 
  count(temporal_reference, axes, directionality) %>% 
  arrange(desc(n))
axes_deictic_lateral_type_df

### 1.2 sagittal (forward-backward), 
axes_deictic_sagittal_sum <- str_subset(gesture_notes, "<deictic-") %>% 
  str_count("<backward/>|<forward/>|<downward/>") %>% sum()
axes_deictic_sagittal_sum
axes_deictic_sagittal_type <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("<backward/>|<forward/>|<downward/>") %>% 
  str_extract("(?<=<)backward(?=\\/>)|(?<=<)forward(?=\\/>)|(?<=<)downward(?=\\/>)")
sort(table(axes_deictic_sagittal_type), decreasing = TRUE)
axes_deictic_sagittal_type_df <- tibble(directionality = axes_deictic_sagittal_type) %>% 
  mutate(temporal_reference = "deictic", axes = "sagittal") %>% 
  count(temporal_reference, axes, directionality) %>% 
  arrange(desc(n))
axes_deictic_sagittal_type_df

### 1.3 combined
axes_deictic_combined_sum <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<rightward/>|<leftward/>|<forward/>|<backward/>|<downward/>)", negate = TRUE) %>% 
  length()
axes_deictic_combined_sum
axes_deictic_combined_type <- str_subset(gesture_notes, "<deictic-") %>% 
  str_extract("(?<=<)[a-z]+ward\\-[a-z]+ward(?=\\/>)") %>% 
  .[!is.na(.)]
sort(table(axes_deictic_combined_type), decreasing = TRUE)
axes_deictic_combined_type_df <- tibble(directionality = axes_deictic_combined_type) %>% 
  mutate(temporal_reference = "deictic", axes = "combined") %>% 
  count(temporal_reference, axes, directionality) %>% 
  arrange(desc(n))
axes_deictic_combined_type_df

# multinomial test for the directionality in the combined axes
dmultinom(sort(table(axes_deictic_combined_type), decreasing = TRUE), prob = rep(1/length(table(axes_deictic_combined_type)), length(table(axes_deictic_combined_type))))
dmultinom(sort(table(axes_deictic_combined_type), decreasing = TRUE), prob = rep(1/length(table(axes_deictic_combined_type)), length(table(axes_deictic_combined_type)))) < 0.001

### 1.3.1 participants of the combined-axes
axes_deictic_combined_speakers <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<rightward/>|<leftward/>|<forward/>|<backward/>|<downward/>)", negate = TRUE) %>% 
  str_extract("(?<=<speaker\\=.)([a-zA-Z-]+)(?=.)")
table(axes_deictic_combined_speakers)
round(prop.table(table(axes_deictic_combined_speakers))*100)

axes_deictic_all <- c(lateral = axes_deictic_lateral_sum, 
                      sagittal = axes_deictic_sagittal_sum, 
                      combined = axes_deictic_combined_sum)
(axes_deictic_all <- sort(axes_deictic_all, decreasing = TRUE))

### 1.4 proportion of axes for deictic all
axes_deictic_all
axes_deictic_all_proportion <- round(prop.table(axes_deictic_all) * 100)

### 1.5 statistical test for the axes in the deictic time
axes_deictic_all
dmultinom(axes_deictic_all, prob = rep(1/length(axes_deictic_all), length(axes_deictic_all)))
dmultinom(axes_deictic_all, prob = rep(1/length(axes_deictic_all), length(axes_deictic_all))) < 0.01

### 1.7 for PRESENT
present_expressions_gesture <- str_subset(gesture_notes, "<deictic-[a-z]+-present-[a-z]+")
present_position_construal <- str_extract(present_expressions_gesture, "(?<=present-)([a-z]+)")
present_reached_from <- str_extract(present_expressions_gesture, "(?<=past-)[a-z]+")
present_handedness <- str_extract(present_expressions_gesture, "(?<=<)[a-z-]+hands?(?=\\/>)")
present_speaker <- str_extract(present_expressions_gesture, "(?<=<speaker..)[^\"]+(?=\")")
present_gesture_type <- str_extract(present_expressions_gesture, "(?<=<)(sequential|isolated)-gesture")

present_df <- tibble(position = present_position_construal, reached_from = present_reached_from, handedness = present_handedness, speaker = present_speaker, gesture_type = present_gesture_type)
present_df

### 1.8 data for visualisation of TEMPORAL-REFERENCE * AXIS * DIRECTIONALITY
plot_df1 <- 
bind_rows(axes_deictic_combined_type_df, 
          axes_deictic_lateral_type_df, 
          axes_deictic_sagittal_type_df, 
          sequential_directionality_df) %>%
  mutate(directionality = factor(directionality,
                                 levels = c("backward-leftward", "forward-leftward", "forward-rightward", "backward-rightward", "leftward", "rightward", "backward", "forward", "downward", "toward")))
plot_df1
plot_df1 %>% 
  ggplot(aes(x = axes, y = n, fill = directionality)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(.9), vjust = -.25) +
  facet_wrap(~temporal_reference, scales = "free_x",
             labeller = labeller(temporal_reference = c(deictic = paste("DEICTIC (N=", deictic_gestures_n, ")", sep = ""), sequential = paste("SEQUENTIAL (N=", sequential_gestures_n, ")", sep = "")))) +
  labs(y = "Token frequency", x = "Axes", fill = "Directionality") +
  scale_fill_brewer(type = "div", palette = "Spectral") +
  scale_y_continuous(limits = c(0, 11.15), 
                     breaks = c(0, 2, 4, 6, 8, 10)) + 
  theme_bw() +
  ggsave("figures/fig-2-directionality-and-axes-for-temporal-gestures.png", width = 8, height = 4, units = "in", dpi = 300)

### freq. of sagittal gestures in deictic vs. sequential time
(sagittal_in_deictic <- axes_deictic_all[names(axes_deictic_all) == "sagittal"])
(sagittal_in_sequential <- sequential_axes[names(sequential_axes) == "sagittal"])
binom.test(c(sagittal_in_deictic, sagittal_in_sequential), alternative = "greater")
binom.test(c(sagittal_in_deictic, sagittal_in_sequential), alternative = "greater")$p.value < 0.05

### freq of lateral vs. sagittal in deictic and sequential time
lateral_all <- sequential_axes[names(sequential_axes) == "lateral"] + axes_deictic_all[names(axes_deictic_all) == "lateral"]
sagittal_all <- sequential_axes[names(sequential_axes) == "sagittal"] + axes_deictic_all[names(axes_deictic_all) == "sagittal"]
(all_lateral_vs_sagittal <- c(lateral_all, sagittal_all))
round(prop.table(all_lateral_vs_sagittal)*100)
binom.test(all_lateral_vs_sagittal)
binom.test(all_lateral_vs_sagittal)$p.value < 0.05


## 2. handedness: left-hand, right-hand, bimanual
handedness_deictic <- str_subset(gesture_notes, "<deictic-") %>% 
  str_extract("(right|left|both)hands?")
(handedness_deictic_count <- sort(table(handedness_deictic), decreasing = TRUE))
round(prop.table(handedness_deictic_count) * 100)
dmultinom(handedness_deictic_count, prob = rep(1/length(handedness_deictic_count), length(handedness_deictic_count)))
dmultinom(handedness_deictic_count, prob = rep(1/length(handedness_deictic_count), length(handedness_deictic_count))) < 0.001
chisq.test(handedness_deictic_count)$p.value < 0.01

handedness_sequential <- str_subset(gesture_notes, "<sequential-[sl]") %>% 
  str_extract("(right|left|both)hands?")
(handedness_sequential_count <- sort(table(handedness_sequential), decreasing = TRUE))
round(prop.table(handedness_sequential_count) * 100)


## 3. (in)congruence (across axes) for deictic time:
### 3.1 (in)congruence-lateral
congruence_lateral <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<rightward/>|<leftward/>)") %>% 
  str_extract("(?<=<)([a-z-]+)?congr[a-z]+")
(congruence_lateral <- sort(table(congruence_lateral), decreasing = TRUE))
tapply(congruence_lateral, str_detect(names(congruence_lateral), "incongruent", negate = TRUE), sum)
sum(tapply(congruence_lateral, str_detect(names(congruence_lateral), "incongruent", negate = TRUE), sum))
round(prop.table(congruence_lateral) * 100, 2)

congruence_lateral_with_time_reference_directionality <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<leftward/>|<rightward/>)") %>% 
  str_extract_all("<.+\\/>") %>% 
  unlist()
congruence_lateral_with_time_reference_directionality <- congruence_lateral_with_time_reference_directionality %>% 
  str_replace_all("(past|future).+?([a-z-]+hands?).+?([a-z-]+ward).+?<(([a-z-]+)?congr[a-z]+).+speaker..([a-zA-Z]+).+", "\\1_\\2_\\3_\\4_\\6") %>% 
  tibble(time_reference = .) %>% 
  separate(time_reference, c("time", "handedness", "directionality", "congruency", "speaker"), sep = "_", remove = FALSE) %>% 
  mutate(time = str_replace(time, "<deictic-lateral", ""),
         time = if_else(str_detect(time, "<past$"),
                        str_extract(time, "(?<=<)past$"),
                        str_extract(time, "(?<=\\-)[a-z]+"))) %>% 
  mutate(handedness = if_else(is.na(handedness),
                              str_extract(time_reference, "(?<=<)([a-z-]+hands?)"),
                              handedness),
         directionality = if_else(is.na(directionality),
                                  str_extract(time_reference, "(?<=<)([a-z-]+ward)"),
                                  directionality),
         congruency = if_else(is.na(congruency),
                              str_extract(time_reference, "(?<=<)(([a-z-]+)?congr[a-z]+)"),
                              congruency),
         speaker = if_else(is.na(speaker),
                           str_extract(time_reference, "(?<=speaker..)[A-Za-z]+"),
                           speaker)) %>% 
  mutate(congruency_simple = if_else(congruency == "incongruent", "incongruent", "congruent"),
         axis = "lateral")
congruence_lateral_with_time_reference_directionality

### 3.2 (in)congruence-sagittal
congruence_sagittal <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<backward/>|<forward/>|<downward/>)") %>% 
  str_extract("(?<=<)([a-z-]+)?congr[a-z]+")
(congruence_sagittal <- sort(table(congruence_sagittal), decreasing = TRUE))
round(prop.table(congruence_sagittal) * 100, 2)

congruence_sagittal_with_time_reference_directionality <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<backward/>|<forward/>|<downward/>)") %>% 
  str_extract_all("<.+\\/>") %>% 
  unlist()
congruence_sagittal_with_time_reference_directionality <- congruence_sagittal_with_time_reference_directionality %>% 
  str_replace_all("<.+?(past|future|present).+?([a-z-]+hands?).+?([a-z-]+ward).+?<(([a-z-]+)?congr[a-z]+).+speaker..([a-zA-Z]+).+", "\\1_\\2_\\3_\\4_\\6") %>% 
  tibble(time_reference = .) %>% 
  separate(time_reference, c("time", "handedness", "directionality", "congruency", "speaker"), sep = "_", remove = FALSE) %>% 
  mutate(congruency_simple = if_else(congruency == "incongruent", "incongruent", "congruent"),
         axis = "sagittal")


### 3.3 (in)congruence-combined
congruence_combined <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(<rightward/>|<leftward/>|<forward/>|<backward/>|<downward/>)", negate = TRUE) %>% 
  str_extract("(?<=<)([a-z-]+)?congr[a-z]+")
congruence_combined
(congruence_combined <- sort(table(congruence_combined), decreasing = TRUE))
(congruence_combined_sum <- c(congruence = sum(congruence_combined[1:2]), congruence_combined[3]))
round(prop.table(congruence_combined_sum) * 100, 2)

congruence_combined_with_time_reference_directionality <- str_subset(gesture_notes, "<deictic-") %>% 
  str_subset("(?<=<)[a-z]+ward\\-[a-z]+ward(?=\\/>)") %>% 
  str_extract_all("<.+\\/>") %>% 
  unlist()
congruence_combined_with_time_reference_directionality <- congruence_combined_with_time_reference_directionality %>% 
  str_replace_all("<.+?(past|future|present).+?([a-z-]+hands?).+?([a-z-]+ward\\-[a-z-]+ward).+?<(([a-z-]+)?congr[a-z]+).+speaker..([a-zA-Z]+).+", "\\1_\\2_\\3_\\4_\\6") %>% 
  tibble(time_reference = .) %>% 
  separate(time_reference, c("time", "handedness", "directionality", "congruency", "speaker"), sep = "_", remove = FALSE) %>% 
  mutate(congruency_simple = if_else(congruency == "incongruent", "incongruent", "congruent"),
         axis = "combined")
congruence_combined_with_time_reference_directionality

congruency_all <- bind_rows(congruence_combined_with_time_reference_directionality, congruence_lateral_with_time_reference_directionality, congruence_sagittal_with_time_reference_directionality) %>% 
  mutate(time = factor(time, levels = c("past", "present", "future")),
         handedness = factor(handedness, levels = c("righthand", "lefthand", "bothhands")),
         axis = factor(axis, levels = c("sagittal", "lateral", "combined")),
         congruency = factor(congruency, levels = c("congruent", "doubly-congruent", "singly-congruent", "incongruent")))
congruency_all

### congruency overall
congruency_all %>% 
  count(congruency_simple) %>% 
  mutate(perc = round(n/sum(n) * 100))
congruency_all %>% count(axis, congruency)
congruency_all %>% count(congruency_simple, axis) %>% arrange(axis)

### 3.4 congruency mapping for the deictic-time reference, directionality, and axes
plot_df <- congruency_all %>% 
  filter(time != "present") %>%
  mutate(axis = fct_rev(axis)) %>% 
  count(time, directionality, axis) %>% 
  mutate(directionality = factor(directionality,
                                 levels = c("backward-leftward", "forward-leftward", "backward-rightward", "forward-rightward", "leftward", "rightward", "backward", "forward"))) %>% 
  arrange(axis, directionality)
plot_df %>% 
  ggplot(aes(x = time, y = n, fill = directionality)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(.9), vjust = -.5, size = 2.85) + 
  facet_wrap(~axis, labeller = labeller(axis = c(combined = "COMBINED", lateral = "LATERAL", sagittal = "SAGITTAL"))) +
  labs(y = "Token frequency", x = "Deictic times", fill = "Directionality") + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 11.15), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_fill_brewer(type = "div", palette = "Spectral") +
  ggsave("figures/fig-3-directionality-of-deictic-time.png", width = 8, height = 3.5, units = "in", dpi = 300)

### past vs. future utterances
plot_df %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(perc = round(n/sum(n) * 100))

### multinomial test for directionality of PAST in combined axes
past_combined <- pull(filter(plot_df, axis == "combined", time == "past"), n)
(names(past_combined) <- pull(filter(plot_df, axis == "combined", time == "past"), directionality))
past_combined
prop.table(past_combined)
past_combined_prop <- round(prop.table(past_combined)*100)
dmultinom(past_combined, prob = rep(1/length(past_combined), length(past_combined)))
dmultinom(past_combined, prob = rep(1/length(past_combined), length(past_combined))) < 0.001

## handedness of BACKWARD-LEFTAWRD - PAST gesture
congruency_all %>% 
  count(directionality, handedness, time, axis) %>% 
  filter(axis == "combined", directionality == "backward-leftward") %>% 
  mutate(perc = round(n/sum(n)*100))
handedness_backwardleftward <- congruency_all %>% 
  count(directionality, handedness, time, axis) %>% 
  filter(axis == "combined", directionality == "backward-leftward") %>% 
  pull(n)
names(handedness_backwardleftward) <- congruency_all %>% 
  count(directionality, handedness, time, axis) %>% 
  filter(axis == "combined", directionality == "backward-leftward") %>% 
  pull(handedness)
handedness_backwardleftward
binom.test(handedness_backwardleftward)

## handedness and speakers for BACKWARD-LEFTWARD - PAST gesture
handedness_backwardleftward_speakers <- congruency_all %>% 
  filter(axis == 'combined', directionality == 'backward-leftward') %>% 
  count(speaker, handedness) %>% 
  mutate(perc = round(n/sum(n)*100)) %>% 
  arrange(desc(perc))

## binomial test for directionality of PAST in lateral axis
past_lateral <- pull(filter(plot_df, axis == "lateral", time == "past"), n)
(names(past_lateral) <- pull(filter(plot_df, axis == "lateral", time == "past"), directionality))
past_lateral
prop.table(past_lateral)
past_lateral_prop <- round(prop.table(past_lateral)*100)
binom.test(past_lateral, alternative = "greater")
binom.test(past_lateral, alternative = "greater")$p.value < 0.05

## binomial test for directionality of PAST in sagittal axis
past_sagittal <- pull(filter(plot_df, axis == "sagittal", time == "past"), n)
(names(past_sagittal) <- pull(filter(plot_df, axis == "sagittal", time == "past"), directionality))
prop.table(past_sagittal)
past_sagittal
(past_sagittal_prop <- round(prop.table(past_sagittal)*100))
binom.test(past_sagittal, alternative = "greater")
binom.test(past_sagittal, alternative = "greater")$p.value < 0.05

backward_past_all <- sum(c(sum(past_combined[str_detect(names(past_combined), "backward")]), past_sagittal[str_detect(names(past_sagittal), "backward")]))
backward_past_all
c(backward_past_all, past_sagittal[str_detect(names(past_sagittal), "forward")])
binom.test(c(backward_past_all, past_sagittal[str_detect(names(past_sagittal), "forward")]), alternative = "greater")$p.value
binom.test(c(backward_past_all, past_sagittal[str_detect(names(past_sagittal), "forward")]), alternative = "greater")$p.value < 0.001

## Analyses for PRESENT for the directionality of the gestures
present_df %>% 
  count(gesture_type) %>% 
  arrange(desc(n)) %>% 
  mutate(perc = round(n/sum(n) * 100))
present_df %>% 
  count(reached_from) %>% 
  arrange(desc(n)) %>% 
  filter(reached_from != "downward") %>% 
  mutate(perc = round(n/sum(n) * 100))
