library(tidyverse)
library(tibble)
library(bnlearn)
library(lattice)
library(Rgraphviz)

##### Ładowanie danych ####
df <- read.csv("datasets/students_adaptability_level_online_education.csv")

df %>% 
  count(Gender)

ggplot(df, aes(`Education.Level`)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

#### Przygotowanie danych pod sieć ####
col_names = names(df)
df[col_names] <- lapply(df[col_names], as.factor)

#### Nauka struktury sieci #####
blacklist <- set2blacklist(c("Gender", "Age", "Location"))
blacklist <- rbind(blacklist, 
                   set2blacklist(c("Gender", "Education.Level")))
blacklist <- rbind(blacklist, 
                   set2blacklist(c("Age", "Class.Duration")))

dag <- pc.stable(df, blacklist=blacklist)
#dag <- set.arc(dag, "Location", "Internet.Type")

graphviz.plot(dag, layout='dot')

#### Nauka parametróœ sieci ####
fit <- bn.fit(dag, df)
fit
fit$Education.Level

bn.fit.barchart(fit$IT.Student)

data.frame(matrix(c("XD", "XD"), ncol = 2, byrow = TRUE))

