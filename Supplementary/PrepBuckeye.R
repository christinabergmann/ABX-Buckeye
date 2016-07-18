# Separate the zerospeech item file into vowels and consonants and impose a minimum threshold per speaker / context combination


# Download the file from:

buck = read.table("english.item")

vowels = c("aa", "ae", "ah", "aw", "ay", "eh", "er", "ey", "ih", "iy", "ow", "oy", "uh", "uw")

# These are consequently the consonants: "b"  "ch" "d"  "dh" "el" "en" "f"  "g"  "hh" jh" "k"  "l"  "m"  "n"  "ng" "p"  "r"  "s"  "sh" "t"  "th" "z"  "zh" "v"  "w"  "y"

#in the actual item file, the header is:
#file onset offset #phone context talker
#Unfortunately "#" has different meaning in R, so we omit them for now and only add them back in when writing the file
names(buck) = c("file", "onset", "offset", "phone", "context", "talker")


buck$talker = as.factor(buck$talker)

#Now we create two subsets
buck.vowels = droplevels(subset(buck, phone %in% vowels))
buck.cons = droplevels(subset(buck, !(phone %in% vowels)))


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

