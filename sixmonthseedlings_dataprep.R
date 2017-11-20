# this is the data prep file for the seedlings six month study (done at 6, 12, and 18 months)
# what we need for this is the message report and the fixation report from dataview (eyetracker output)
# running this file in its entirety will produce the the following files:
# the BINNED file
# the diffscore at the subject pair level, excluding trials with <1/3 of the window of interest 
# the propt, proptpre, and proptcorr at the subject item level, 
# you don't need to run this script to recreate our analysis since sixmonth_only_stats_pnas.R is self-contained

#first let's get our libraries and our functions

# bc this file has dependencies on other functions and unshared spreadsheets, needs to be run in local folder
setwd("~/Desktop/backed_up_byduke/studies/sixmonth_seedlings/sixmonthonly_rproject/")
library(tidyverse)
library(feather)
library(forcats)
options(tibble.width = 100)
options(dplyr.width = 50)


source("function/helper_functions.R")

#here's the fix rep and mes rep files
sixmonth_seedlings <- read_tsv("data/fix_rep_seedlings_6month_7-26-2016.xls",  na = character())
sixmonth_seedlings_mesrep <- read_tsv("data/mes_rep_seedlings_6month_7-26-2016.xls", na = character())

sixmonth_seedlings <- sixmonth_seedlings %>%
  filter(!is.na(audiotarget))# this gets rid of any rows where exp ended before the study was over 

# we really just need the button press column and time, trial, and subj  (called "EL_BUTTON_CRIT_WORD" in the mesrep)
sixmonth_seedlings_mesrep <- sixmonth_seedlings_mesrep %>%
  filter(CURRENT_MSG_TEXT == "EL_BUTTON_CRIT_WORD")%>%
  mutate(RT = as.numeric(as.character(RT)))%>%
  dplyr::select(RECORDING_SESSION_LABEL,CURRENT_MSG_TIME, TRIAL_INDEX)

#And then let's convert the fixations into 20ms bins
sixmonth_seedlings_bin <- sixmonth_seedlings %>%
  filter(practice=="n") %>% # to take out the warmup trials
  do(binifyFixations(sixmonth_seedlings, keepCols=c("RECORDING_SESSION_LABEL",#subject number
                          "CURRENT_FIX_INTEREST_AREA_LABEL",#TARGET or DISTRACTOR
                          "TRIAL_INDEX",#1-36
                          "RT",#from images showing up to target onset
                          "TRIAL_START_TIME", 
                          "audiotarget",#name of sound file, e.g. where_stroller_75db.wav
                          "carrier",#can, do, look, where
                          "distractorimage","targetimage", # image file name e.g. apple.jpg
                          "distractorloc","targetloc", #location of target and distractor [320,512] or [960,512] for the test trials
                          "pair",
                          "targetside","trial","trialtype"))) #L or R, 1-32, between or within

#write.table(sixmonth_seedlings_bin, "sixmonth_seedlings_bin_5-6-16.txt", row.names =F)
dim(sixmonth_seedlings_bin) #1150381 x18


#here are a handful of trials with a keypress issue (35 trials)
sixmonth_seedlings_bin%>%
  filter(RT == -1 & !trial%in% c("p4","p3","p2","p1") )%>%
  group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, audiotarget)%>%
  distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, RT, audiotarget)

summary(sixmonth_seedlings_bin)
dim(sixmonth_seedlings_mesrep)
summary(sixmonth_seedlings_mesrep)

# now we can merge the message report file with the fixation report file so we get the target onset times, 
# and then rename some things
sixmonth_seedlings_test <- left_join(sixmonth_seedlings_bin, sixmonth_seedlings_mesrep)%>%
  filter(!trial%in% c("p4","p3","p2","p1") & 
           !is.na(CURRENT_MSG_TIME) &
          !trialtype==".")%>%#get rid of the practice trial data and target-press-less-trials
  droplevels() %>% #drop any empty levels 
  rename(SubjectNumber = RECORDING_SESSION_LABEL, TargetOnset = CURRENT_MSG_TIME, 
         gaze = CURRENT_FIX_INTEREST_AREA_LABEL,
          TrialNumber = TRIAL_INDEX)%>%
  ungroup()%>%
  mutate(SubjectNumber = factor(SubjectNumber),
         propt = ifelse(gaze == "TARGET", 1,ifelse(gaze == "DISTRACTOR",0, NA)),
         target = as.factor(gsub(".jpg","", targetimage)),
         trialtype = factor(trialtype),
         Nonset = (((timeBin)-(floor(TargetOnset/20)))*20),
         prewin = factor(ifelse(Time<= TargetOnset, "Y","N")),
         longwin = factor(ifelse((timeBin >= ((TargetOnset/20)+18) & # this is a 367-5s bin bc 5000/20 = 250
                                   timeBin <= ((TargetOnset/20)+250)),"Y","N")),
         whichwin = factor(ifelse(prewin=="Y","pre",
                           ifelse(longwin=="Y","long","neither"))),
         gaze = fct_recode(gaze, NULL = "."),
         SubjectNumber = fct_recode(SubjectNumber,
                                    "01_18_6"= "01_06_18",
                                    "11_18_6" =  "11_06_18",
                                    "16_18_6" =  "16_18_18",
                                    "18_18_6" =  "18_18_18",
                                    "21_12_6" =  "21_12_2",
                                    "39_12_6" =  "39_12",
                                    "ns01_06" =  "ns01",
                                    "ns02_06" =  "ns02",
                                    "ns03_06" =  "ns03",
                                    "ns04_06" =  "ns04",
                                    "ns05_06" =  "ns05",
                                    "ns06_06" =  "ns06",
                                    "ns07_06" =  "ns07",
                                    "ns08_06" =  "ns08",
                                    "ns09_06" =  "ns09",
                                    "ns10_06" =  "ns10",
                                    "ns12_06" =  "ns12"))%>%
  separate(SubjectNumber, c("subj","month"), sep = c("_"), remove = F, extra="drop")%>%
  mutate(month = factor(month),
         subj = factor(subj)) %>%
  separate(pair, c("first","second"), sep = "_", remove = F, extra="drop")%>%
  mutate(first = factor(first),
         second = factor(second),
         num = factor(ifelse(as.character(second) == as.character(target),"two","one")),
         pair = factor(pair),
         targetside = factor(targetside),
         audiotarget = factor(audiotarget),
         carrier = factor(carrier),
         distractorimage = factor(distractorimage),
         targetimage = factor(targetimage),
         targetloc = factor(targetloc),
         distractorloc = factor(distractorloc),
         gaze = factor(gaze))%>%
  dplyr::select(-trial)

dim(sixmonth_seedlings_test) #1039510 x 29
summary(sixmonth_seedlings_test)
summary(sixmonth_seedlings_test$pair)

#removing low data#####
# this could be put into a 'do' command up higher; it's currently based on the 367-5000 bin,
# fix this if you change the window
# (5000-367)/20 = 232 max bins, and 1/3 of that has to be there so
# (1/3)* (5000-367 )= 1540 has to be there, (5000-367)- ((1/3)* (5000-367 )) and 3089 can be missing
sixmonth_seedlings_test <- removeLowData(sixmonth_seedlings_test, "longwin", 
                                         maxBins = 232, maxMissing = 3089)


write_feather(sixmonth_seedlings_test, path = "data/sixmonth_seedlings_feather")
write_feather(sixmonth_seedlings_test %>% filter(month=="06"), path = "data/sixmonth_seedlings_monthsixonly_feather")
summary(sixmonth_seedlings_test)
