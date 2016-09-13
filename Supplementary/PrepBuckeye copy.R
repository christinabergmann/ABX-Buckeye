# Separate the full corpus item file into vowels and consonants and impose a minimum threshold per speaker / context combination


buck = read.table("Supplementary/triphones.item")


#in the actual item file, theheader is:
##file onset offset #phone prev-phone next-phone speaker
#Unfortunately "#" has different meaning in R, so we omit them for now and only add them back in when writing the file
names(buck) = c("file", "onset", "offset", "phone", "p_context", "n_context", "talker")

buck$talker = as.factor(buck$talker)

buck$context <- paste(buck$p_context, buck$n_context, sep = "-")
buck$context = as.factor(buck$context)

buck = subset(buck, select = -c(p_context, n_context))

# We need both since there are additional symbols in the corpus, like silence, or noise
vowels = c("aa", "ae", "ah", "ao", "aw", "ay", "eh", "er", "ey", "ih", "iy", "ow", "oy", "uh", "uw")
consonants = c("b", "ch", "d", "dh", "dx", "el", "em", "en", "eng", "f", "g", "hh", "jh", "k", "l", "m", "n", "ng", "nx", "p", "r", 
               "s", "sh", "t", "th", "tq", "v", "w", "y", "z", "zh")

#Now we create two subsets
buck.vowels = droplevels(subset(buck, phone %in% vowels))
buck.cons = droplevels(subset(buck, phone %in% consonants))


#Here we create the two files in the suitable format for ABXpy
vowfile = "Buckeye_vowels.item"
consfile = "Buckeye_cons.item"
file.create(vowfile)
file.create(consfile)
write("#file onset offset #phone context talker", file = vowfile)
write("#file onset offset #phone context talker", file = consfile)


#Next we want to remove those elements where a context does not appear in all talkers per phone

talkers = levels(buck$talker)
contexts = levels(buck$context)


#This is a very roundabout way and I am sure in dplyr there are better solutions.
for(phone in levels(buck.cons$phone)){
  for(context in contexts){
    subset = buck.cons[buck.cons$phone==phone & buck.cons$context==context, ]
    if(length(unique(subset$talker))==length(talkers)){
      write.table(subset, append = TRUE, sep = " ", col.names= FALSE, row.names = FALSE, quote = FALSE, file = consfile)
    }
  }
}

for(phone in levels(buck.vowels$phone)){
  for(context in contexts){
    subset = buck.vowels[buck.vowels$phone==phone & buck.vowels$context==context, ]
    if(length(unique(subset$talker))==length(talkers)){
      write.table(subset, append = TRUE, sep = " ", col.names= FALSE, row.names = FALSE, quote = FALSE, file = vowfile)
    }
  }
}

