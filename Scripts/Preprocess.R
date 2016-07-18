# Author: Christina Bergmann (chbergma 'at' gmail)
# Last modified: 29.06.2016
# Description: Preprocess, including enforcing a minumum number of tokens for each speaker/contrast/context combination.
# Analyzing between-/within-speaker discriminability of sound contrasts in the Buckeye corpus (Zerospeech challenge subset)

# Note: This step is no longer needed. 


library(dplyr)

threshold = 20
#The minimum number of observations per cell, to be aligned with MArtin et al. 2015

#The following code was generated and generously shared by Page Piccinini (page.piccinini 'at' gmail.com and http://pagepiccinini.com/)
rawdata_within_sum = rawdata_within %>%
  group_by(contrasts, context) %>%
  summarise(n_min = min(n)) %>%
  ungroup()

rawdata_within_clean = rawdata_within %>%
  inner_join(rawdata_within_sum) %>%
  filter(n_min >= threshold)

rawdata_between_sum = rawdata_between %>%
  group_by(contrasts, context) %>%
  summarise(n_min = min(n)) %>%
  ungroup()

rawdata_between_clean = rawdata_between %>%
  inner_join(rawdata_between_sum) %>%
  filter(n_min >= threshold)
