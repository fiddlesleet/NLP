library(readr)
library(stringr)
library(syuzhet)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)

##########
# GET DATA
##########

# download UTF-8 data from: https://www.gutenberg.org/ebooks/1342

file.path <- 'pp.txt'
txt.raw <- read_lines(file.path, skip = 30, n_max = 13032)
pp <- character()

# concatenate 70-char lines into chunks of 10, so NLP on 700 chars 
for (i in seq_along(txt.raw)) {
  if (i%%10 == 1) pp[ceiling(i/10)] <- str_c(txt.raw[i], 
                                                txt.raw[i+1],
                                                txt.raw[i+2],
                                                txt.raw[i+3],
                                                txt.raw[i+4],
                                                txt.raw[i+5],
                                                txt.raw[i+6],
                                                txt.raw[i+7],
                                                txt.raw[i+8],
                                                txt.raw[i+9], sep = " ")
}

# NRC Word-Emotion Association Lexicon, created via Mechanical Turk
get_nrc_sentiment("Nobody can tell what I suffer! But it is always so. Those who do not complain are never pitied.")
get_nrc_sentiment("And your defect is to hate everybody.")
get_nrc_sentiment("You must allow me to tell you how ardently I admire and love you.")

nrc.pp <- cbind(linenumber = seq_along(pp), get_nrc_sentiment(pp))
# inspect
str(nrc.pp)
head(nrc.pp)
summary(nrc.pp)

########################################################################
# DIVIDE THE VOLUMES
# - Pride and Prejudice contains 61 chapters divided into three volumes: 
# - Volume I is Chapters 1-23, 
# - Volume II is Chapters 24-42
# - Volume III is Chapters 43-61
########################################################################

# find volume breaks 
grep("Chapter 1", pp) # 1 
grep("Chapter 24", pp) # 452
grep("Chapter 43", pp) # 806

# create [volume] as factor; restart linenumber at start of each volume
nrc.pp$volume <- "Volume I"
#inspect
str(nrc.pp)

# update volume values
nrc.pp[grep("Chapter 24", pp):length(pp), 'volume'] <- 'Volume II'
nrc.pp[grep("Chapter 43", pp):length(pp), 'volume'] <- 'Volume III'
#inspect
str(nrc.pp)
tail(nrc.pp)
head(nrc.pp)

# convert volume from chr to factor
nrc.pp$volume <- as.factor(nrc.pp$volume)
#inspect
str(nrc.pp)

nrc.pp$linenumber[nrc.pp$volume == "Volume II"] <- seq_along(pp)
nrc.pp$linenumber[nrc.pp$volume == "Volume III"] <- seq_along(pp)

###################
# EXAMINE SENTIMENT
###################

# calculate overall negative sentiment
nrc.pp$negative <- -nrc.pp$negative

# a df containing positive and negative scores for each line
pos_vs_neg <- nrc.pp %>% 
  select(linenumber, volume, positive, negative) %>%
  melt(id = c("linenumber", "volume"))
#inspect
str(pos_vs_neg)
head(pos_vs_neg)
tail(pos_vs_neg)

names(pos_vs_neg) <- c("linenumber", "volume", "sentiment", "value")

### MAUNUALLY MATCH KEY EVENTS TO VOLUMES & LINE NUMBERS
key_event_line_num <- c(114, 211, 307, 183, 91, 415)
annotated_text <- data.frame(x = key_event_line_num, y = rep(18.3, 6),
                             label = c("Jane's illness", "Mr. Collins arrives", 
                                       "Ball at Netherfield", "Mr. Darcy proposes", 
                                       "Lydia elopes", "Mr. Darcy proposes again"),
                             volume = factor(c("Volume I", "Volume I", 
                                               "Volume I", "Volume II", 
                                               "Volume III", "Volume III"),
                                             levels = c("Volume I", "Volume II", "Volume III")))
                             
annotated_arrow <- data.frame(x = key_event_line_num, 
                            y1 = rep(17, 6), 
                            y2 = c(11.2, 10.7, 11.4, 13.5, 10.5, 11.5), 
                            volume = factor(c("Volume I", "Volume I", 
                                              "Volume I", "Volume II", 
                                              "Volume III", "Volume III"),
                                            levels = c("Volume I", "Volume II", "Volume III")))

####################################
# PLOT POSITIVE & NEGATIVE SENTIMENT
####################################

ggplot(data = pos_vs_neg, aes(x = linenumber, y = value, color = sentiment)) +
  facet_wrap(~volume, nrow = 3) + 
  geom_point(size = 4, alpha = .5) + 
  theme_minimal() + 
  ylab("Sentiment") + 
  theme(legend.title=element_blank()) + 
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) + 
  geom_text(data = annotated_text, aes(x, y, label = label), 
            hjust = 0.5, size = 3, inherit.aes = FALSE) + 
  geom_segment(data = annotated_arrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE) + 
  theme(legend.justification = c(1, 1), legend.position = c(1, 0.71)) +
  scale_color_manual(values = c("aquamarine3", "midnightblue")) + 
  ggtitle("Positive and Negative Sentiment in Pride and Prejudice")

# ANALYZE: We can see positive scores overall much higher than negative scores 

##################################################
# PLOT POSITIVE & NEGATIVE SENTIMENT, AS BAR CHART
##################################################
ggplot(data = pos_vs_neg, aes(x = linenumber, y = value, color = sentiment, fill = sentiment)) +
  facet_wrap(~volume, nrow = 3) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  ylab("Sentiment") + 
  theme(legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        legend.justification=c(1,1), 
        legend.position=c(1, 0.71)) + 
  geom_text(data = annotated_text, aes(x, y, label = label), 
            hjust = 0.5, size = 3, inherit.aes = FALSE) + 
  geom_segment(data = annotated_arrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE) + 
  theme(legend.justification = c(1, 1), legend.position = c(1, 0.71)) +
  scale_color_manual(values = c("aquamarine3", "midnightblue")) + 
  scale_fill_manual(values = c("aquamarine3", "midnightblue")) + 
  ggtitle("Positive and Negative Sentiment in Pride and Prejudice")

##########################################################
# FOURIER TRANSFORM: OVERALL POSITIVE - NEGATIVE SENTIMENT
##########################################################
df.overall_sentiment <- tbl_df(cbind(linenumber = seq_along(nrc.pp),
                                     sentiment = get_sentiment(nrc.pp, method = "npc")))
