getwd()
setwd ("C:/Users/User/Documents/PhilippinesR_Workshop/CSB/data_wrangling/sandbox")
rm(list=ls())

#load the library
library(tidyverse)
install.packages("tidyverse")
install.packages("colorspace")
popsize <- read_tsv("../data/FauchaldEtAl2017/pop_size.csv")
ndvi <- read_tsv("../data/FauchaldEtAl2017/ndvi.csv")
seaice <- read_tsv("../data/FauchaldEtAl2017/sea_ice.csv")
snow <- read_tsv("../data/FauchaldEtAl2017/snow.csv")



seaice
seaice <- seaice %>% gather(Month, Cover, 3:14)
seaice

ggplot(data = popsize)

ggplot(data = popsize) +
  aes(x = Year, y = Pop_Size,
      colour = Herd) 


#ggplot line graph
ggplot(data = popsize) +
  aes(x = Year, y = Pop_Size,
      colour = Herd) +
  geom_point() +
  geom_line()
#ggplot histogram
ggplot(data = ndvi) +
  aes(x = NDVI_May) +
  geom_histogram()

#ggplot nesting
ggplot(data = ndvi) +
  aes(x = NDVI_May) +
  geom_histogram()
ggplot(data = ndvi, aes(x = NDVI_May)) +
  geom_histogram()
ggplot(data = ndvi) +
  geom_histogram(aes(x = NDVI_May))

#density plot
ggplot(data = ndvi) +
  aes(x = NDVI_May) +
  geom_density()

# Box and violin plots
p1 <- ggplot(data = ndvi) +
  aes(x = Herd, y = NDVI_May)
p1 + geom_boxplot()
p1 + geom_violin()

p1 + geom_boxplot() + aes(fill = Herd)
p1 + geom_violin() + aes(fill = Herd)

#bar plot
seaice$Month <- factor(seaice$Month,
                       month.abb)
ggplot(data = seaice %>%
         filter(Herd == "WAH",
                Year == 1990)) +
  aes(x = Month, y = Cover) +
  geom_col()

ggplot(data = seaice %>%
         filter(Herd == "WAH")) +
  aes(x = Year) +
  geom_bar()

#scatter Plots
pl <- ggplot(data = popsize %>%
               filter(Herd == "WAH")) +
  aes(x = Year, y = Pop_Size) +
  geom_point()

show (pl)
pl + geom_smooth()
pl + geom_smooth(method = "lm")
pl + geom_smooth(method = "lm",
                 formula = y ~ poly(x, 3),
                 se= FALSE)


#Plotting experimental errors
stats <- popsize %>%
  filter(Herd %in% c("GRH", "PCH")) %>%
  group_by(Herd) %>%
  summarise(
    meanPopSize = mean(Pop_Size),
    SD = sd(Pop_Size),
    N = n(),
    SEM = SD/sqrt(N),
    CI = SEM * qt(0.975, N-1))
stats

ggplot(data = stats) +
  aes(x = Herd, y = meanPopSize) +
  geom_col()

limits <- aes(ymax = stats$meanPopSize + stats$CI,
              ymin = stats$meanPopSize - stats$CI)

ggplot(data = stats) +
  aes(x = Herd, y = meanPopSize) +
  geom_col() +
  geom_errorbar(limits, width = .3)

#mind expander 1-1
pl <- ggplot(data = snow) +
  aes(x = Week_snowmelt,y = Perc_snowcover, colour = as.factor(Year)) +
  geom_point()
show(pl)

#mindexpander 1-2
snowstats <- snow %>%
  filter(Year %in% c("1970" : "2014"))%>%
  group_by(Year) %>%
  summarise(meanPerc_snowcover = mean(Perc_snowcover), 
             meanWeek_snowmelt = mean(Week_snowmelt))
stats    

#mindexpander 1-3
ggplot(data = snowstats, aes(x = meanWeek_snowmelt, y = meanPerc_snowcover)) + geom_point()

#mindexpander 1-4
p3 <- ggplot(data = popsize) +
  aes(x = Herd, y = Pop_Size)
p3 + geom_boxplot()

#mindexpander 1-5
tibble_pop_stats <- popsize %>%
  filter(Year %in% c("2008": "2014")) %>%
  group_by(Year) %>%
  summarise(meanPopSize = mean(Pop_Size),
    SD = sd(Pop_Size))
tibble_pop_stats

limits <- aes(ymax = tibble_pop_stats$meanPopSize + tibble_pop_stats$CI,
              ymin = tibble_pop_stats$meanPopSize - tibble_pop_stats$CI)

ggplot(data = tibble_pop_stats) +
  aes(x = Year, y = meanPopSize) +
  geom_col() +
  geom_errorbar(limits, width = .3)

#scales: color brewer
pl1 <- ggplot(data = popsize,
              aes(x = Herd, y = Pop_Size,
                  fill = Herd)) +
  geom_boxplot()
show(pl1)

pl1 + scale_fill_brewer(palette = "Set3")
pl1 + scale_fill_hue()
pl1 + scale_fill_manual(values = rainbow(11),
                        name = "aaa")

pl2 <- ggplot(data =seaice %>%
                filter(Herd == "BEV")) +
  aes(x = Year, y = Month, colour = Cover,
      size= Cover) +
  geom_point()
show(pl2)

pl2 + scale_color_gradient(high = "violet",
                           low = "blue")

#RcolorBrewer
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

#Faceting
ggplot(data = seaice %>%
         filter(Herd %in% c("WAH", "BAT"),
                Year %in% c(1980, 1990,
                            2000, 2010))) +
  aes(x = Month, y = Cover) +
  geom_col() +
  facet_grid(Year~Herd)

ggplot(data = seaice %>%
         filter(Year == 2010)) +
  aes(x = Month, y = Cover) +
  geom_col() +
  facet_wrap(~Herd)


ggplot(data = seaice %>%
         filter(Year == 2010)) +
  aes(x = Month, y = Cover) +
  geom_col() +
  facet_wrap(~Herd, scales = "free")

#labels
pl <- ggplot(data = popsize) +
  aes(x = Year, y = Pop_Size) +
  geom_point()
pl + xlab("Years")
pl + ylab("Population Size")
pl + ggtitle("Population Dynamics")

#Legends
pl <- ggplot(data = popsize) +
  aes(x = Herd, y = Pop_Size, fill = Herd) +
  geom_boxplot()
show(pl)

pl + theme(legend.position = "bottom")
pl + theme(legend.position = "top")
pl + theme(legend.position = "none")

pl <- ggplot(data = popsize) +
  aes(x = Year, y = Pop_Size, colour = Herd,
      alpha = sqrt(Pop_Size)) +
  geom_point()
show(pl)
pl + guides(color = guide_legend(nrow = 4,
                                 title = "herd"),
            alpha = guide_legend(direction = "horizontal",
                                 title = "al"))
pl +guides(colour = "none")

#themes
pl <- ggplot(data = snow %>%
               filter(Herd == "CAH"),
             aes(y = Week_snowmelt,
                 x = Perc_snowcover)) +
  geom_point()
show(pl)
pl + theme_bw()
pl + theme_classic()
pl + theme_linedraw()
pl + theme_minimal()

#setting a feature
pl <- ggplot(data = popsize) +
  aes(x = Year, y = Pop_Size, colour = Herd) +
  geom_point()
pl

pl <- ggplot(data= popsize) +
  aes(x = Year, y = Pop_Size) +
  geom_point(colour = "red")
pl

#ggsave as pdf
ggsave(filename = "test.pdf", plot = pl,
       width = 3, height = 4)

pdf(file="test3.pdf", width = 3, height = 4)
  pl; pl1; pl2
dev.off()

#geomtile
pl <- ggplot(data = snow) +
      aes (x = Year, y = Herd, fill = Week_snowmelt) +
  geom_tile

