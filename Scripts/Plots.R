# Author: Christina Bergmann (chbergma 'at' gmail)
# Last modified: 29.06.2016
# Description: Generate figures. 
# Analyzing between-/within-speaker discriminability of sound contrasts in the Buckeye corpus (Zerospeech challenge subset)


library(ggplot2)

#### Within vs between scores. ####

#The so-called Banana-plot

par(pty="s")
plot(c(.45,1.), c(.45, 1.), type="n",xlab="Within-Speaker ABX Score",ylab="Across-Speaker ABX Score",main = "ABX scores", xlim=c(0.45,.99),ylim=c(0.45,.99), frame.plot = FALSE)
axis(side = 1)

for (thiscontrast in levels(rawdata_within$contrasts)) {
  between_score = rawdata_between$score[rawdata_between$contrasts == thiscontrast]
  within_score = rawdata_within$score[rawdata_within$contrasts == thiscontrast]
  points(mean(within_score), mean(between_score), pch = 1, cex =1)
}


lines(c(0.45,1.),c(0.45,1.),col="red",lty=3)


#### Null distribution vs actual difference score. ####


hist(diff_results, xlim =c(-.1, .1))
abline(v=difference_score, col = "red")


#### Density plots of within and across speakerpairs averaged across contrasts ####

density_speakers_t = within_perspeaker %>%
  mutate(cond = "within") %>%
  select(mean_score, cond) 


density_speakers_t2 = between_perspeaker %>%
  mutate(cond = "between") %>%
  select(mean_score, cond) 

density_speakers = bind_rows(density_speakers_t, density_speakers_t2)

remove(density_speakers_t2, density_speakers_t)

density_speaker_plot = ggplot(density_speakers, aes(x=mean_score, colour = cond, fill = cond)) + geom_density(alpha  = .5)  +
  xlim(.5, 1) +
  theme_classic() +
  theme(axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(),
        text=element_text(size=14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())
density_speaker_plot


#### Density plots mean scores per contrasts within and across averaged across speakers ####

density_contrasts_t = within_percontrast %>%
  mutate(cond = "within") %>%
  select(mean_score, cond) 


density_contrasts_t2 = between_percontrast %>%
  mutate(cond = "between") %>%
  select(mean_score, cond) 

density_contrasts = bind_rows(density_contrasts_t, density_contrasts_t2)

remove(density_contrasts_t2, density_contrasts_t)

density_contrast_plot = ggplot(density_contrasts, aes(x=mean_score, colour = cond, fill = cond)) + geom_density(alpha  = .5)  +
  xlim(.5, 1) +
  theme_classic() +
  theme(axis.line.y = element_line(), legend.position = "none", legend.key = element_blank(),
        text=element_text(size=14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())
density_contrast_plot

