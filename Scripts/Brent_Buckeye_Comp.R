# Author: Alejandrina Cristia (alecristia 'at' gmail)
# Last modified: 25.12.2017
# Description: Calculate model to decide whether talker effects are larger or smaller than contrast or corpus effects


read.table("Supplementary/both_buckeye.txt",header=T)->buck
buck$corpus="buckeye"

read.table("../ABX-Brent/Supplementary/both_brent.txt",header=T)->brent
brent$corpus="brent"

all=rbind(buck,brent)
levels(brent$contrasts)[!(levels(brent$contrasts) %in% levels(buck$contrasts))]
#problem: not only speakers in one corpus are not in the other corpus but also slight differences in contrast coverage

inbothcor=rownames(table(all$contrasts,all$corpus))[table(all$contrasts,all$corpus)[,1]>0 & table(all$contrasts,all$corpus)[,2]>0]

all_ol=subset(all,contrasts %in% inbothcor)

whole=lm((mean_score_temp-5)~contrasts+corpus+speakerpairs,data=all_ol)
wo_sp=lm((mean_score_temp-5)~contrasts+corpus,data=all_ol)
wo_contrasts=lm((mean_score_temp-5)~speakerpairs+corpus,data=all_ol)
wo_corpus=lm((mean_score_temp-5)~speakerpairs+contrasts,data=all_ol)

summary(whole) #49% of var explained

anova(whole,wo_sp) #effect of speaker removal F 10.7
summary(wo_sp) #41% var explained

anova(whole,wo_contrasts) #effect of contrast removal F 121.7
summary(wo_contrasts) #20% var explained

anova(whole,wo_corpus) #effect of corpus removal NA
summary(wo_corpus) #49% var explained

whole=lm((mean_score_temp-5)~contrasts*corpus+speakerpairs,data=all_ol)
wo_sp=lm((mean_score_temp-5)~contrasts*corpus,data=all_ol)
summary(whole) #60% of var explained

anova(whole,wo_sp) #effect of speaker removal F 13.6
summary(wo_sp) #51% var explained

Anova(whole)

