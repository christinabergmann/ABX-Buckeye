# Author: Christina Bergmann (chbergma 'at' gmail)
# Last modified: 29.06.2016
# Description: Generate figures. 
# Analyzing between-/within-speaker discriminability of sound contrasts in the Buckeye corpus (Zerospeech challenge subset)


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

