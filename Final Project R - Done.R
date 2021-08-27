rm(list = ls())
library(dplyr)
library(ggplot2)
library(gapminder)

setwd("~/Meghana's Stuff/College/Sophomore Classes/Poli 281")
file <- read.csv("cces18.csv")
View(file)

###PART TWO
table(file$wait)
prop.table(table(file$wait))
#WITH 6
mean(file$wait, na.rm=TRUE)
median(file$wait, na.rm=TRUE)
#WITHOUT 6
without6 <- file%>%
  filter(wait<=5) 
mean(without6$wait)
median(without6$wait)

#more10
newfile <- file%>%
  mutate(more10 = wait==3 | wait== 4 | wait==5)
View(newfile)

###PART THREE
states <- newfile%>%
  group_by(state)%>%
  summarise(ratio = mean(more10))%>%
  arrange(desc(ratio))
  
View(states)
states$state <- factor(states$state, states$state)
graphOne <- ggplot(states, aes(state, ratio)) + geom_col() + theme(axis.text.x = element_text(angle=90))
graphOne
View(states)

###PART FOUR
regions<- newfile%>%
  group_by(state, region)%>%
  summarise(ratio = mean(more10))%>%
  arrange(desc(ratio))

regions$state <- factor(regions$state, regions$state)
graphTwo <- ggplot(regions, aes(state, ratio, color=region)) + geom_col() + theme(axis.text.x = element_text(angle=90)) + ylab("Proportion that Waited Over 10 Minutes") +
  xlab("States")
graphTwo

###PART FIVE
parties <- newfile%>%
  filter(vote2016 == 1 | vote2016==2)%>%
  mutate(conserv_vote = vote2016==1)%>%
  group_by(conserv_vote)%>%
  summarise(ratio = mean(more10, na.rm=TRUE))
View(parties)

parties2 <- newfile%>%
  filter(vote2016 == 1 | vote2016==2)%>%
  mutate(conserv_vote = vote2016==1)
View(parties2)

table(parties2$conserv_vote, parties2$wait)
prop.table(table(parties2$conserv_vote, parties2$wait))

table(parties$ratio, parties$conserv_vote)
prop.table(table(parties$ratio, parties$conserv_vote))

###PART SIX
newfile$race_groups
newfile$race_groups[newfile$race == 1 ] <- "White Non-Hispanic"
newfile$race_groups[newfile$race == 2 ] <- "Black"
newfile$race_groups[newfile$race == 3 ] <- "Hispanic"
newfile$race_groups[newfile$race == 4 ] <- "Asian"
newfile$race_groups[newfile$race == 5 ] <- "Other"
newfile$race_groups[newfile$race == 6 ] <- "Other"
newfile$race_groups[newfile$race == 7 ] <- "Other"
newfile$race_groups[newfile$race == 8 ] <- "Other"
View(newfile)
race_5 <- newfile %>%
  group_by(race_groups) %>%
  summarise(ratio = mean(more10, na.rm=TRUE))
View(race_5)

race_5_graph <- ggplot(race_5, aes(race_groups, ratio)) +
  geom_col() +
  ylab("Proportion that Waited Over 10 Minutes") +
  xlab("Race")
race_5_graph

###PART SEVEN
newfile$faminc_4 <- NA
newfile$faminc_4[newfile$faminc <=3] <- "Lower Class"
newfile$faminc_4[newfile$faminc >3 & newfile$faminc <= 6] <- "Lower Middle Class"
newfile$faminc_4[newfile$faminc >6 & newfile$faminc <= 13] <- "Upper Middle Class"
newfile$faminc_4[newfile$faminc > 13 & newfile$faminc <= 16] <- "Upper Class"
View(newfile)

table(newfile$faminc_4)
prop.table(table(newfile$faminc_4))

faminc_groups <- newfile %>%
  group_by(faminc_4) %>%
  summarise(ratio = mean(more10, na.rm=TRUE))

faminc_4_graph <- ggplot(faminc_groups, aes(faminc_4, ratio)) +
  geom_col() +
  ylab("Proportion that Waited Over 10 Minutes") +
  xlab("Income")
faminc_4_graph


###PART EIGHT

lowincome <- newfile %>%
  filter(faminc_4 == "Lower Class") %>%
  group_by(race_groups) %>%
  summarise(ratio = mean(more10, na.rm=TRUE))

low_income_graph <- ggplot(lowincome, aes(race_groups, ratio)) +
  geom_col() + 
  ylab("Proportion of Respondents") +
  xlab("Race") 
  
low_income_graph

lowmiddleclass <- newfile %>%
  filter(faminc_4 == "Lower Middle Class") %>%
  group_by(race_groups) %>%
  summarise(ratio = mean(more10, na.rm=TRUE))

lowmiddleclassgraph <- ggplot(lowmiddleclass, aes(race_groups, ratio)) +
  geom_col() + 
  ylab("Proportion of Respondents") +
  xlab("Race") 
  
lowmiddleclassgraph

uppermiddleclass <- newfile %>%
  filter(faminc_4 == "Upper Middle Class") %>%
  group_by(race_groups) %>%
  summarise(ratio = mean(more10, na.rm=TRUE))

uppermiddleclass_graph <- ggplot(uppermiddleclass, aes(race_groups, ratio)) +
  geom_col() + 
  ylab("Proportion of Respondents") +
  xlab("Race") 

uppermiddleclass_graph

upperclass <- newfile %>%
  filter(faminc_4 == "Upper Class") %>%
  group_by(race_groups) %>%
  summarise(ratio = mean(more10, na.rm=TRUE))

upperclass_graph <- ggplot(upperclass, aes(race_groups, ratio)) +
  geom_col() + 
  ylab("Proportion of Respondents") +
  xlab("Race") 

upperclass_graph

###PART NINE

income_histogram <- ggplot(newfile, aes(income_county)) +
  geom_histogram()
income_histogram

filewithpd <- newfile %>%
  mutate(population_density = ((county_pop / land_area) / 1000)) 
View(filewithpd)

black <- newfile$race_groups=='Black'
hispanic <- newfile$race_groups=='Hispanic'
asian <- newfile$race_groups =='Asian'
other <- newfile$race_groups =='Other'
wait_reg<-newfile$wait
wait_reg[wait_reg ==6] <- NA
faminc_reg <- newfile$faminc
faminc_reg[faminc_reg == 97] <- NA

model1 <- lm(wait_reg ~ black + hispanic + asian + other, data = newfile)
model2 <- lm(wait_reg ~ black + hispanic + asian + other + faminc_reg + income_county + population_density, data = filewithpd)

summary(model1)
summary(model2)
nobs(model1)
nobs(model2)
