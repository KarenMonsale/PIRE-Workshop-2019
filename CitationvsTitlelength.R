setwd("C:/Users/User/Documents/PhilippinesR_Workshop/CSB/r/data")
rm(list=ls())

library(tidyverse)
Journals <- read.csv("C:/Users/User/Documents/PhilippinesR_Workshop/CSB/r/data/Letchford2015_data.csv", sep=",")
ggplot(data = Journals)+
  aes(x = Title_length, y = Cites) +
  geom_col()

pl2 <- ggplot(data = Journals %>%
                filter(Year == 2013,
                       Journal == "Nature")) +
                aes(x = Title_length, y = Cites) +
  geom_col()
pl2

pl3 <- ggplot(data = Journals %>%
                   filter(Year == 2013,
                          Journal == "Science")) +
  aes(x = Title_length, y = Cites) +
  geom_col()             

pl4 <- ggplot(data = Journals %>%
                filter(Year == 2013,
                       Journal == "Cell")) +
  aes(x = Title_length, y = Cites) +
  geom_col()                 
pl4
 pl5 <- ggplot(data = Journals %>%
   filter(Year == 2013,
            Journal == "The Lancet")) +
  aes(x = Title_length, y = Cites) +
  geom_col()             
pl5 

pl6 <-ggplot(data = Journals %>%
              filter(Journal %in% c("Nature", "Cell", "Science", "The Lancet"),
                     Year %in% c(2013))) +
  aes(x = Title_length, y = Cites) +
  geom_col()
facet_grid(~Year, scales = "free")
pl6
