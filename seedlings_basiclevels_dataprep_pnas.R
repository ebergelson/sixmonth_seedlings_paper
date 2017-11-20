# this script aggregates the seedlings basiclevels at the recording-level, and adds some relevant statistics.
# it runs over the all_basiclevel_feather files that are made by concatenating 
# each file's annotations lab internally
# for the pnas paper, we only care about month 6
# so at the end of this script we write out the subset of all_basiclevel that's month 6, 
# and the aggregated version of just the month six subset (and the overall agg file for other purposes)
# you don't need to run this script to recreate our analysis since sixmonth_only_stats_pnas.R is self-contained

#first let's get our libraries and our functions

library(tidyverse)
library(feather)
library(forcats)
library(entropy)
options(tibble.width = Inf)
options(tibble.print_max = 200, tibble.print_min = 100)

all_basiclevel_home_data <- read_feather("data/all_basiclevel_new_09-26-17.feather")
all_basiclevel_home_data <- all_basiclevel_home_data%>%
  #adding in noun onset, i.e. when the child said their first noun
    filter(speaker == "CHI")%>%
  group_by(subj)%>%
  summarise(noun_chi_onset = min(as.numeric(as.character(month))))%>%
  right_join(all_basiclevel_home_data)


summary(all_basiclevel_home_data, maxsum=50)
levels(all_basiclevel_home_data$speaker)

#should produce no rows.
all_basiclevel_home_data%>%
  filter(nchar(as.character(speaker))!=3)

subset(all_basiclevel_home_data, is.na(utterance_type))
 

# aggregating the home data -----------------------------------------------

#we make lots of little dataframes first
#the majority of these aggregate over subj, month, and audio_video


# num words in 6 month experiment -----------------------------------------
#note: established possible basic levels for other versions of these words
# #by crawling like so:
 # all_basiclevel_home_data %>%
 #    distinct(basic_level)%>%
 #    filter(grepl("^[b]", x = basic_level))%>%
 #     arrange(basic_level)%>%as.data.frame()


basic_level_tested_words <- c("baby","babe","baby+doll",
  "ball","bally",
  "blanket","blankey","blanky",
  "book","books",
  "bottle","baba","ba",
  "car", "car+car",
  "diaper","diape","diapey","diapers","diatee","didey","diadey",
  "foot","footsy","footy","feet","feetsie","footsie","feetsy","feety",
  "hair","hairs",
  "hand",
  "juice","juices","juice+box","juice+boxes","juicey",
  "milk","milkies","milky","milk+water","milk+jug","milks",
  "mouth",
  "nose","nosey",
  "spoon","spoony",
  "stroller")
  
num_experimentwords <- all_basiclevel_home_data %>%
  filter(basic_level %in% basic_level_tested_words) %>%
  group_by(subj, month, audio_video)%>%
  summarise(num_exp_tokens = n(),
            num_exp_types = n_distinct(basic_level))

         
# MOT and FAT count -------------------------------------------------------
six_to_seventeen_home_FAT_MOT_count <- all_basiclevel_home_data %>%
  filter(speaker %in%c("MOT","FAT"))%>%
  group_by(subj, month, audio_video, speaker)%>%
  tally()%>%
  spread(speaker, n)

# utterance type count ----------------------------------------------------
six_to_seventeen_home_utt_count <-  all_basiclevel_home_data %>%
  filter(utterance_type %in%c("d","i","q","r","s","n"))%>%
  group_by(subj, month, audio_video, utterance_type)%>%
  tally()%>%
  spread(utterance_type, n)


# object present count ----------------------------------------------------
six_to_seventeen_home_op <- all_basiclevel_home_data %>%
  filter(object_present %in% c("n","y"))%>%
  group_by(subj, month, audio_video, object_present)%>%
  tally()%>%
  spread(object_present, n)%>%
  mutate(prop_op = y/(n+y))%>%
  rename(y_op = y,
         n_op = n)

six_to_seventeen_home_op_exp <- all_basiclevel_home_data %>%
  filter(basic_level %in% basic_level_tested_words &
           object_present %in% c("n","y"))%>%
  group_by(subj, month, audio_video, object_present)%>%
  tally()%>%
  spread(object_present, n)%>%
  mutate(prop_op_exp = y/(n+y))%>%
  rename(y_op_exp = y,
         n_op_exp = n)

  
# device and toy use count ------------------------------------------------
six_to_seventeen_home_device_count <-  all_basiclevel_home_data %>%
  filter(speaker %in% c("TOY","TVN","TVF", "TVM","TVS","TVB"))%>%
  group_by(subj, month, audio_video, speaker)%>%
  tally()%>%
  spread(speaker, n)


# few versions of kid talk info -------------------------------------------

#chi tokens
six_to_seventeen_home_chi_count <-  all_basiclevel_home_data %>%
  filter(speaker %in% c("CHI"))%>%
  group_by(subj, month, audio_video, speaker)%>%
  tally()%>%
  spread(speaker, n)

#chi types
six_to_seventeen_home_chi_type_count <-  all_basiclevel_home_data %>%
  filter(speaker %in% c("CHI"))%>%
  group_by(subj, month,audio_video)%>%
  dplyr::select(subj, month, basic_level)%>%
  distinct(basic_level)%>%
  tally()%>%
  rename(CHItypes = n)

# noun production onset age
six_to_seventeen_home_noun_chi_onset <- all_basiclevel_home_data %>%
  dplyr::select(subj, noun_chi_onset) %>% 
  distinct()
  

# finally, big aggregation of our little datasets -------------------------

all_basiclevel_home_data_agg <- all_basiclevel_home_data %>%
  group_by(subj, month, SubjectNumber, audio_video)%>%
  summarise(numspeakers = n_distinct(speaker),
            numtokens = n(),
            numtypes = n_distinct(basic_level))%>%
  left_join(six_to_seventeen_home_FAT_MOT_count)%>%
  left_join(num_experimentwords)%>%
  left_join(six_to_seventeen_home_utt_count)%>%
  left_join(six_to_seventeen_home_device_count)%>%
  left_join(six_to_seventeen_home_op)%>%
  left_join(six_to_seventeen_home_op_exp)%>%
  left_join(six_to_seventeen_home_chi_count)%>%
  left_join(six_to_seventeen_home_chi_type_count)%>%
  mutate_each(funs(replace(., which(is.na(.)), 0)))%>%
  group_by(subj, month, SubjectNumber, audio_video)%>%
  mutate(prop_mom = MOT/numtokens,
         prop_dad = FAT/numtokens,
         prop_parent = prop_mom+prop_dad,
         prop_tech = (TVN+TVF+TVS+TVM+TOY+TVB)/numtokens,
         tech = (TVN+TVF+TVS+TVM+TOY+TVB),
         propd = d/numtokens,
         propi = i/numtokens,
         propn = n/numtokens,
         propq = q/numtokens,
         propr = r/numtokens,
         props = s/numtokens,
         type_token_ratio = numtypes/numtokens,
         exp_type_ratio = num_exp_types/numtypes,
         exp_token_ratio = num_exp_tokens/numtokens,
         ent_subj_av = entropy(c(d/numtokens,
                                 q/numtokens,
                                 s/numtokens,
                                 r/numtokens,
                                 n/numtokens,
                                 i/numtokens),unit = "log2"),
         sum_prop_ut = round(sum(c(d/numtokens,
                                   q/numtokens,
                                   s/numtokens,
                                   r/numtokens,
                                   n/numtokens,
                                   i/numtokens)),2))%>%
  dplyr::select(-TVF, -TVM, -TVS, -TVF, -TVN, -TVB)%>%
  left_join(six_to_seventeen_home_noun_chi_onset)%>%
  mutate(posttalk =  ifelse(as.numeric(as.character(month))<noun_chi_onset|
                              is.na(noun_chi_onset),F,T))

summary(all_basiclevel_home_data_agg, maxsum=50)


#overall agg feather
write_feather(all_basiclevel_home_data_agg, "data/all_basiclevel_home_data_agg_feather9-26-17")

# two feathers, agg, and not-agg, six month only, for pnas
write_feather(all_basiclevel_home_data_agg %>% filter(month=="06"), "data/sixmonth_basiclevel_home_data_agg_feather")
write_feather(all_basiclevel_home_data%>% filter(month=="06"), "data/sixmonth_basiclevel_home_data_feather")