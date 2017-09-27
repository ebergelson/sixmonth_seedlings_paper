#This script does various aggregation and merging needed for stats for the main .rmd file
# for Bergelson&Aslin 2017, PNAS
#it needs the following files in order to run:
#**sixmonth_basiclevel_home_data_agg_feather** (created by seedlings_basiclevels_dataprep_pnas.R)
#**sixmonth_seedlings_monthsixonly_feather** (created by sixmonthseedlings_dataprep.R)
#**video_06_reliability.csv** (created by in-house python scripts, step 4 https://osf.io/cxwyz/wiki/Video%20Reliability%20Checks/)
#**audio_06_reliability.csv** (created by in-house python scripts, step 6 https://osf.io/cxwyz/wiki/Audio%20Reliability%20Checks/)

library(feather)
library(tidyverse)
library(MASS)
library(broom)
library(knitr)
options(dplyr.width = Inf)

videorel <- read_csv("data/video_06_reliability.csv") %>% filter(new_utt_type!="o")
audiorel <- read_csv("data/audio_06_reliability.csv") %>% filter(new_utt_type!="o")
sixmonth_seedlings_test <- read_feather("data/sixmonth_seedlings_monthsixonly_feather")
sixmonth_basiclevel_home_data_agg <- read_feather("data/sixmonth_basiclevel_home_data_agg_feather")

#for graphs later
tt_labeller <- c(
  between = "unrelated",
  within = "related"
)


listofsubs <- sixmonth_seedlings_test%>%
  filter(month=="06")%>%
  distinct(SubjectNumber)

listofsubsswithdata <- sixmonth_seedlings_test%>%
  filter(month=="06" & lowdata ==F)%>%
  distinct(SubjectNumber)

nslistofsubs <- sixmonth_seedlings_test%>%
  filter(month=="06" & startsWith(as.character(subj),"ns"))%>%
  distinct(SubjectNumber)

seedlingslistofsubs <- sixmonth_seedlings_test%>%
  filter(month=="06" & !startsWith(as.character(subj),"ns"))%>%
  distinct(SubjectNumber)

#when did we stop the experiment
lasttrialpersub <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber)%>%
  #filter(lowdata==F & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  filter(month=="06")%>%
  summarise(maxtrial = max(TrialNumber))

subswhostoppedearly <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber)%>%
  #filter(lowdata==F & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  filter(month=="06")%>%
  summarise(maxtrial = max(TrialNumber),
            missedtrials_endedearly = 36-maxtrial)%>%
  filter(maxtrial<36)


lastnonlowtrialpersub <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber)%>%
  #filter(lowdata==F & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  filter(lowdata==F & month=="06")%>%
  summarise(maxtrial = max(TrialNumber))

subswhostoppedearly_inlowdata <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber)%>%
  #filter(lowdata==F & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  filter(lowdata==F & month=="06")%>%
  summarise(maxtrial = max(TrialNumber),
            numtrials = n_distinct(TrialNumber))%>%
  filter(maxtrial<36)


#1596
pretrials <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber, TrialNumber)%>%
  filter(whichwin == "pre" & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  dplyr::select(SubjectNumber, TrialNumber) %>% 
  distinct()

#1626
wintrials <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber, TrialNumber)%>%
  filter(whichwin == "long" & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  dplyr::select(SubjectNumber, TrialNumber) %>% 
  distinct()

#trials with win data but no pre data
trials_win_no_pre <- setdiff(wintrials, pretrials)
nrow(trials_win_no_pre)#65



#818 trials
lowdatatrials <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber, TrialNumber)%>%
  filter(lowdata==T & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  dplyr::select(SubjectNumber, TrialNumber) %>% 
  distinct()
nrow(lowdatatrials)
#808
nonlowdatatrials <- sixmonth_seedlings_test %>%
  group_by(SubjectNumber, TrialNumber)%>%
  filter(lowdata==F & month=="06" & SubjectNumber %in%listofsubsswithdata$SubjectNumber)%>%
  dplyr::select(SubjectNumber, TrialNumber) %>% 
  distinct()

#trials with no pre and lowdata
nopre_andlow<- dplyr::intersect(trials_win_no_pre, lowdatatrials)
#max number of trials we could have gotten from 54 subjects (excluding 4 subjects with NO usable trials) = 54*32 = 1728



# overall subject means across trials, then words -------------------------------------
sixmonthseedlings_propt_wins.melt_x <- sixmonth_seedlings_test %>%
  filter(lowdata==F, whichwin !="neither")%>%
  group_by(SubjectNumber,  subj, month, target, whichwin,TrialNumber)%>%# agg trials,
  summarise(propt = mean(propt, na.rm=T))%>%
  group_by(SubjectNumber,  subj, month, target, whichwin)%>% #agg word
  summarise(propt = mean(propt, na.rm=T))%>%
  spread(whichwin, propt)%>%
  mutate(proptcorr = long - pre)%>%
  rename(propt_long = long,
         propt_pre = pre)

sixmonthseedlings_propt_wins.melt_x%>%
  group_by(SubjectNumber, month)%>%
  summarise(missing_trials = length(propt_long),
            missing_predata = sum(is.na(propt_pre)))%>%
  group_by(month)%>%
  summarise(sum(missing_trials),
            sum(missing_predata),
            length(SubjectNumber))

sixmonthseedlings_propt_Ss <- sixmonthseedlings_propt_wins.melt_x %>%
  group_by (subj, month, SubjectNumber)%>%
  summarise(cross_item_mean_propt = mean(propt_long, na.rm=T),
            cross_item_mean_proptcorr = mean(proptcorr, na.rm=T),
            cross_item_mean_proptpre = mean(propt_pre, na.rm=T))%>%
  group_by(month)%>%
  mutate(outlier = (cross_item_mean_proptcorr > 
                      (mean(cross_item_mean_proptcorr, na.rm=T) + 
                         3*(sd(cross_item_mean_proptcorr, na.rm=T))) |
                      cross_item_mean_proptcorr <
                      (mean(cross_item_mean_proptcorr, na.rm=T) - 
                         3*(sd(cross_item_mean_proptcorr, na.rm=T)))))

# ggplot(sixmonthseedlings_propt_Ss,
#        aes(month, cross_item_mean_proptcorr, shape= outlier))+
#   geom_jitter(size=.5,width = .2, height = 0)+
#   stat_summary(fun.data=mean_cl_boot, color = "red")+
#   geom_hline(yintercept = 0)

filter(sixmonthseedlings_propt_Ss, outlier==T)


sixmonthseedlings_propt_Ss%>%
  filter(month=="06")%>%
  summarise(mean_of_means = mean(cross_item_mean_proptcorr),
            sd_of_means = sd(cross_item_mean_proptcorr))
#.03+(.14*5) 5sd
#getting rid of the outliers
sixmonthseedlings_propt_Ss <- sixmonthseedlings_propt_Ss%>%
  filter(outlier==F)


# now by trial type -------------------------------------------------------
#nb that when agg by trial and word, if you keep trial type you get 32 trials

# we want to spread this out by trial-type
sixmonthseedlings_proptTT_wins.melt_x <- sixmonth_seedlings_test %>%
  filter(lowdata==F, whichwin !="neither")%>%
  group_by(SubjectNumber,  subj, month, target, trialtype, whichwin,TrialNumber)%>%# agg trials,
  summarise(propt = mean(propt, na.rm=T))%>%
  group_by(SubjectNumber,  subj, month, target, trialtype, whichwin)%>% #agg word
  summarise(propt = mean(propt, na.rm=T))%>%
  spread(whichwin, propt)%>%
  mutate(proptcorrTT = long - pre)%>%
  rename(propt_longTT = long,
         propt_preTT = pre)

sixmonthseedlings_proptTT_wins.melt_x%>%
  group_by(SubjectNumber, month)%>%
  summarise(missing_trials = length(propt_longTT),
            missing_predata = sum(is.na(propt_preTT)))%>%
  group_by(month)%>%
  summarise(sum(missing_trials),
            sum(missing_predata),
            length(SubjectNumber))

#adding in numtrials, which is max 32 total, 16 for each TT 
# nb in a propT only analyses, would want to bring the relevant rows back in.  
sixmonthseedlings_proptTT_wins.melt <- sixmonthseedlings_proptTT_wins.melt_x%>%
  filter(!is.na(proptcorrTT))%>%
  group_by(SubjectNumber)%>%
  mutate(numtrials = length(SubjectNumber))%>%
  group_by(SubjectNumber, trialtype)%>%
  mutate(numtrials_TT = length(proptcorrTT))

sixmonthseedlings_proptTT_wins.melt%>%
  group_by(month, trialtype)%>%
  summarise(mean(numtrials),
            mean(numtrials_TT),
            mean(proptcorrTT))


sixmonthseedlings_proptTT_Ss <- sixmonthseedlings_proptTT_wins.melt %>%
  group_by (subj, month, SubjectNumber, trialtype, numtrials,numtrials_TT)%>%
  summarise(cross_item_mean_proptTT = mean(propt_longTT, na.rm=T),
            cross_item_mean_proptcorrTT = mean(proptcorrTT, na.rm=T),
            cross_item_mean_proptpreTT = mean(propt_preTT, na.rm=T))%>%
  group_by(month)%>%
  mutate(outlier = (cross_item_mean_proptcorrTT > 
                      (mean(cross_item_mean_proptcorrTT) + 
                         3*(sd(cross_item_mean_proptcorrTT))) |
                    cross_item_mean_proptcorrTT <
                      (mean(cross_item_mean_proptcorrTT) - 
                         3*(sd(cross_item_mean_proptcorrTT)))))
sixmonthseedlings_proptTT_Ss_withoutliers <- sixmonthseedlings_proptTT_wins.melt %>%
  group_by (subj, month, SubjectNumber, trialtype, numtrials,numtrials_TT)%>%
  summarise(cross_item_mean_proptTT = mean(propt_longTT, na.rm=T),
            cross_item_mean_proptcorrTT = mean(proptcorrTT, na.rm=T),
            cross_item_mean_proptpreTT = mean(propt_preTT, na.rm=T))%>%
  group_by(month)%>%
  mutate(outlier = (cross_item_mean_proptcorrTT > 
                      (mean(cross_item_mean_proptcorrTT) + 
                         3*(sd(cross_item_mean_proptcorrTT))) |
                      cross_item_mean_proptcorrTT <
                      (mean(cross_item_mean_proptcorrTT) - 
                         3*(sd(cross_item_mean_proptcorrTT)))))

filter(sixmonthseedlings_proptTT_Ss_withoutliers, outlier==T)

sixmonthseedlings_proptTT_Ss <- sixmonthseedlings_proptTT_Ss%>%
  filter(outlier==F)

cor.test(sixmonthseedlings_proptTT_Ss$numtrials, 
         sixmonthseedlings_proptTT_Ss$cross_item_mean_proptcorrTT,
         method = "spearman")

# just six month Ss, TT_Ss and TT_Items -----------------------------------

sixmonthseedlings_proptTT_Ss06 <- sixmonthseedlings_proptTT_Ss %>%
  filter(month=="06")

sixmonthseedlings_propt_Ss06 <- sixmonthseedlings_propt_Ss%>%
  filter(month=="06")

# Subject Missing 1 of the Trial Types ------------------------------------

# subj 05 and 06 only have data in one condition
missingTT_Ss <- sixmonthseedlings_proptTT_Ss06%>%
  group_by(subj)%>%
  filter(!is.na(cross_item_mean_proptcorrTT))%>% # no NAs anyway
  tally()%>%
  filter(n!=2)
missingTT_Ssall <- sixmonthseedlings_proptTT_Ss%>%
  group_by(subj, month, SubjectNumber)%>%
  filter(!is.na(cross_item_mean_proptcorrTT))%>% # no NAs anyway
  tally()%>%
  filter(n!=2)

# by TT over items --------------------------------------------------------

sixmonthseedlings_proptTT_items <- sixmonthseedlings_proptTT_wins.melt %>%
  filter(!SubjectNumber %in% missingTT_Ssall$SubjectNumber) %>% 
  group_by(target ,month, trialtype)%>%
  summarise(cross_item_mean_proptTT = mean(propt_longTT, na.rm=T),
            cross_item_mean_proptcorrTT = mean(proptcorrTT, na.rm=T),
            cross_item_mean_proptpreTT = mean(propt_preTT, na.rm=T))%>%
  group_by(month, trialtype)%>%
  mutate(outlier = (cross_item_mean_proptcorrTT > 
                      (mean(cross_item_mean_proptcorrTT) + 
                         3*(sd(cross_item_mean_proptcorrTT))) |
                      cross_item_mean_proptcorrTT <
                      (mean(cross_item_mean_proptcorrTT) - 
                         3*(sd(cross_item_mean_proptcorrTT)))))%>%
  left_join(distinct(sixmonth_seedlings_test, trialtype, pair, target))

filter(sixmonthseedlings_proptTT_items, outlier==T)
#no outliers anyway, over items by TT
sixmonthseedlings_proptTT_items <- sixmonthseedlings_proptTT_items%>%
  filter(outlier==F)

# just six month TT_Items -----------------------------------

sixmonthseedlings_proptTT_items06 <- sixmonthseedlings_proptTT_items%>%
  filter(month=="06")


# adding in the home data -------------------------------------------------
video_homestats <- sixmonth_basiclevel_home_data_agg%>%
  filter(audio_video=="video")%>%
  summarise(recording_type = "vid",
    numtokens_avg = paste(round(min(numtokens),1),"-",
                                  round(max(numtokens),1),",",
                                  round(mean(numtokens, na.rm=T),1),"(",
                                  round(sd(numtokens, na.rm=T),1),")",
                                  sep = ""),
            numtypes_avg = paste(round(min(numtypes),1),"-",
                                 round(max(numtypes),1),",",
                                 round(mean(numtypes, na.rm=T),1),"(",
                                 round(sd(numtypes, na.rm=T),1),")",
                                 sep = ""),
            numspeakers_avg = paste(round(min(numspeakers),1),"-",
                                    round(max(numspeakers),1),",",
                                    round(mean(numspeakers, na.rm=T),1),"(",
                                    round(sd(numspeakers, na.rm=T),1),")",
                                    sep = ""),
            prop_mom_avg = paste(round(min(prop_mom),1),"-",
                                 round(max(prop_mom),1),",",
                                 round(mean(prop_mom, na.rm=T),1),"(",
                                 round(sd(prop_mom, na.rm=T),1),")",
                                 sep = ""),
            prop_op_avg = paste(round(min(prop_op),1),"-",
                                round(max(prop_op),1),",",
                                round(mean(prop_op, na.rm=T),1),"(",
                                round(sd(prop_op, na.rm=T),1),")",
                                sep = ""),
    subj_entropy = paste(round(min(ent_subj_av),1),"-",
                         round(max(ent_subj_av),1),",",
                         round(mean(ent_subj_av, na.rm=T),1),"(",
                         round(sd(ent_subj_av, na.rm=T),1),")",
                         sep = ""),
    prop_n = paste(round(min(propn),1),"-",
                   round(max(propn),1),",",
                   round(mean(propn, na.rm=T),1),"(",
                   round(sd(propn, na.rm=T),1),")",
                   sep = ""
    ))
  
# simpler_table <- sixmonth_basiclevel_home_data_agg%>%
#   group_by(audio_video)%>%
#   dplyr::select(numtokens, numtypes, numspeakers, prop_mom, prop_op, ent_subj_av, propn) %>% 
#   #summarise_all(., funs(round(min(.),1),round(max(.),1),round(mean(.,na.rm=T),1), round(sd(.,na.rm=T),1)))
#   summarise_all(funs(min, max, mean, sd)) 

audio_homestats <- sixmonth_basiclevel_home_data_agg%>%
  filter(audio_video=="audio")%>%
  summarise(recording_type = "aud",
            numtokens_avg = paste(round(min(numtokens),1),"-",
                                  round(max(numtokens),1),",",
                                  round(mean(numtokens, na.rm=T),1),"(",
                                  round(sd(numtokens, na.rm=T),1),")",
                                  sep = ""),
            numtypes_avg = paste(round(min(numtypes),1),"-",
                                 round(max(numtypes),1),",",
                                 round(mean(numtypes, na.rm=T),1),"(",
                                 round(sd(numtypes, na.rm=T),1),")",
                                 sep = ""),
            numspeakers_avg = paste(round(min(numspeakers),1),"-",
                                    round(max(numspeakers),1),",",
                                    round(mean(numspeakers, na.rm=T),1),"(",
                                    round(sd(numspeakers, na.rm=T),1),")",
                                    sep = ""),
            prop_mom_avg = paste(round(min(prop_mom),1),"-",
                                 round(max(prop_mom),1),",",
                                 round(mean(prop_mom, na.rm=T),1),"(",
                                 round(sd(prop_mom, na.rm=T),1),")",
                                 sep = ""),
            prop_op_avg = paste(round(min(prop_op),1),"-",
                                round(max(prop_op),1),",",
                                round(mean(prop_op, na.rm=T),1),"(",
                                round(sd(prop_op, na.rm=T),1),")",
                                sep = ""),
            subj_entropy = paste(round(min(ent_subj_av),1),"-",
                                round(max(ent_subj_av),1),",",
                                round(mean(ent_subj_av, na.rm=T),1),"(",
                                round(sd(ent_subj_av, na.rm=T),1),")",
                                sep = ""),
            prop_n = paste(round(min(propn),1),"-",
                           round(max(propn),1),",",
                           round(mean(propn, na.rm=T),1),"(",
                           round(sd(propn, na.rm=T),2),")",
                           sep = ""
            ))


audio_homestats
video_homestats
kable(rbind(audio_homestats, video_homestats))

# binding with in lab data ------------------------------------------------
#and bind it to the in-lab just 6-month data
sixmonth_inlab_athome_agg <- sixmonthseedlings_proptTT_Ss %>%
  filter(month=="06")%>%
  dplyr::select(-cross_item_mean_proptTT, 
                -cross_item_mean_proptpreTT,
                -outlier,
                -numtrials_TT)%>%
  spread(trialtype, cross_item_mean_proptcorrTT)%>%
  right_join(sixmonth_basiclevel_home_data_agg)%>%
  left_join(sixmonthseedlings_propt_Ss)%>%
  mutate(subj = factor(subj),
         SubjectNumber = factor(SubjectNumber),
         cross_item_mean_proptcorr = ifelse(is.na(between)|is.na(within), NA, cross_item_mean_proptcorr),
         cross_item_mean_proptpre = ifelse(is.na(between)|is.na(within), NA, cross_item_mean_proptpre),
         cross_item_mean_propt = ifelse(is.na(between)|is.na(within), NA, cross_item_mean_propt))
#the above 3 lines ensure the subjects with data in only one condition are not included in the cross-item outcomes which avg. conditions


# #s for correlations lab-home --------------------------------------------

audsixmonth <- sixmonth_inlab_athome_agg %>% 
  filter(audio_video=="audio")


vidsixmonth <- sixmonth_inlab_athome_agg %>% 
  filter(audio_video=="video")

propvarssixmonth <- sixmonth_inlab_athome_agg%>%
  filter(subj%in%vidsixmonth$subj & subj%in%audsixmonth$subj)%>% #remove 1subject who lacked video data from pooled props
  dplyr::select(subj,
                month,
                SubjectNumber,
                prop_op,
                prop_mom,
                propn,
                ent_subj_av,
                cross_item_mean_proptcorr,
                audio_video,
                between,
                within)%>%
  group_by(subj, SubjectNumber, month)%>%
  summarise(prop_op_avg = mean(prop_op, na.rm=T),
            prop_mom_avg = mean(prop_mom, na.rm=T),
            ent_subj_avg = mean(ent_subj_av, na.rm=T),
            prop_n_avg = mean(propn, na.rm=T),
            cross_item_mean_proptcorr = mean(cross_item_mean_proptcorr, na.rm=T),
            between = mean(between, na.rm=T),
            within = mean(within, na.rm=T))


