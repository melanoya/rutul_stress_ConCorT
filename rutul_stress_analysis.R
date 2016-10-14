library(dplyr)
library(ggplot2)

setwd("/home/agricolamz/_DATA/OneDrive1/_Work/_Students/2016_Martynova_rutul")

x <- read.csv("data.csv")

x$Stress.hearing <- factor(x$Stress.hearing)
x$stressed.syl <- factor(x$stressed.syl)



# vowels ------------------------------------------------------------------

x%>%
  filter(F1 < 1000, F1 >230, Filename == "mitNR_nouns") %>% 
  ggplot(aes(F2, F1, label = Segment.label, color = Segment.label))+
  geom_text()+
  scale_y_reverse() +
  scale_x_reverse() +
  theme_bw()+
  ggtitle("Formants of Rutul vowels")

# phonetic model ----------------------------------------------------------

x %>% 
  filter(Filename == "mitNR_nouns", Stress.hearing != "2")  %>% 
  mutate(range = Maximum.pitch..Hz. - Minimum.pitch..Hz.) ->
  mitNR

fit <- glm(Stress.hearing~
             Duration + 
             Pitch.mean+
             range,
           data = mitNR,
           family = "binomial")

summary(fit)

# phonological model ------------------------------------------------------

x %>% 
  filter(Filename == "mitNR_nouns", Stress.hearing != "2")  %>% 
  mutate(range = Maximum.pitch..Hz. - Minimum.pitch..Hz.) ->
  mitNR

fit <- glm(Stress.hearing~
             Num.of.Sylable + 
             Sylable.strukture+
             Segment.label+
             stressed.syl,
           data = mitNR,
           family = "binomial")

summary(fit)

mydf <- aggregate(Stress.hearing~
                    Num.of.Sylable+
                    Sylable.strukture+
                    Segment.label+
                    stressed.syl,
                  length, data = mitNR)

mydf %>% 
  arrange(desc(Stress.hearing)) ->
  mydf

mydf %>% 
  select(-Stress.hearing) %>% 
  predict(fit)

mydf$prob <- round(predict(fit, mydf[,-5], type = "response"), 2)

mydf


# barplot 1-----------------------------------------------------------------

x %>% 
  ggplot(aes(Sylable.strukture, fill = Stress.hearing))+
  geom_bar(position = "dodge")+
  theme_bw()+
  ggtitle("Barplot: stress hearing vs. syllable structure")+
  xlab("syllable structure")+
  ylab("count") -> 
  bp

bp + scale_fill_discrete(name="stress hearing",
                         breaks=c("0", "1", "2"),
                         labels=c("unstressed", "stressed", "ambiguous"))


# barplot 2 ---------------------------------------------------------------

x %>% 
  ggplot(aes(Num.of.Sylable, fill = Stress.hearing))+
  geom_bar(position = "dodge")+
  theme_bw()+
  ggtitle("Barplot: stress hearing vs. number of syllables")+
  xlab("number of syllables")+
  ylab("count") -> 
  bp

bp + scale_fill_discrete(name="stress hearing",
                         breaks=c("0", "1", "2"),
                         labels=c("unstressed", "stressed", "ambiguous"))


# barplot 3 ---------------------------------------------------------------

x %>% 
  ggplot(aes(Segment.label, fill = Stress.hearing))+
  geom_bar(position = "dodge")+
  theme_bw()+
  ggtitle("Barplot: stress hearing vs. vowels")+
  xlab("vowels")+
  ylab("count") -> 
  bp

bp + scale_fill_discrete(name="stress hearing",
                         breaks=c("0", "1", "2"),
                         labels=c("unstressed", "stressed", "ambiguous"))

# barplot 4 ---------------------------------------------------------------

x %>% 
  ggplot(aes(stressed.syl, fill = Stress.hearing))+
  geom_bar(position = "dodge")+
  theme_bw()+
  ggtitle("Barplot: stress hearing vs. stressed syllable")+
  xlab("stressed syllable")+
  ylab("count") -> 
  bp

bp + scale_fill_discrete(name="stress hearing",
                         breaks=c("0", "1", "2"),
                         labels=c("unstressed", "stressed", "ambiguous"))


