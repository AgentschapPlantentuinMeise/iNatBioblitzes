library("tidyverse")

iNatProject <- read.csv("~/notebooks/iNatBioblitzes/iNatProject.csv", encoding="UTF-8", comment.char="#")

#Get rid of projects with 1 or zero observers

projects <- filter(iNatProject, observers > 1)
projects <- filter(projects, identifiers > 0)

ggplot(data=projects) +
  geom_point(mapping = aes(x=log(observers),y=log(observations)))
             

ggplot(data=projects) +
  geom_point(mapping = aes(x=log(hours/observers),y=log(observations)))


ggplot(data=projects) +
  geom_point(mapping = aes(x=log(observations),y=log(species)))


#Species found verses effort
ggplot(data=projects) +
  geom_point(mapping = aes(x=hours*observers/1000,y=species, color=project_type)) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  theme_classic()

ggplot(projects, aes(hours*observers/1000, species)) + geom_point() + geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab('Observer effort (length of project (hours) x number of observers)') +
  ylab('Number of species observed') +
  theme_classic()


ggplot(data=projects) +
  geom_point(mapping = aes(x=observers,y=identifiers, color=project_type)) + geom_smooth(method = "lm") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 


ggplot(projects, aes(observers, identifiers)) + geom_point() + geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') 


ggplot(projects, aes(hours, observers)) + geom_point() + geom_smooth(method = "lm", se=TRUE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  theme_classic()

#What is the average number of observations per bioblitz
projects %>% summarise(mean_size = mean(observations, na.rm = TRUE))

#What is the average number of observers per bioblitz
projects %>% summarise(mean_size = mean(observers, na.rm = TRUE))

#What is the average number of species recorded per bioblitz
projects %>% summarise(mean_size = mean(species, na.rm = TRUE))

#What is the average number of identifiers involved in the bioblitz
projects %>% summarise(mean_size = mean(identifiers, na.rm = TRUE))

#what is the average length of the bioblitzes in our sample
projects %>% summarise(mean_size = mean(hours, na.rm = TRUE))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#The most common length of a bioblitz
Mode(projects$hours)

# A count of all the projects used
tally(projects)

ggplot(projects, aes(month(projects$month), observers))

# Observations histogram
p1 <- ggplot(data=projects, aes(x=projects$observations)) + 
  geom_histogram(breaks=seq(0, 2000, by=10), 
                 col="black", 
                 fill="green", 
                 alpha = .2) + 
  theme_bw() +
  labs(title="", x="Number of observations", y="Count")

# Observations histogram
p2 <- ggplot(data=projects, aes(x=projects$species)) + 
  geom_histogram(breaks=seq(0, 5000, by=50), 
                 col="black", 
                 fill="green", 
                 alpha = .2) + 
  theme_bw() +
  labs(title="", x="Number of species recorded", y="")

# Observations histogram
p3 <- ggplot(data=projects, aes(x=projects$observers)) + 
  geom_histogram(breaks=seq(0, 1000, by=10), 
                 col="black", 
                 fill="green", 
                 alpha = .2) + 
  theme_bw() +
  labs(title="", x="Number of observers", y="Count")

# Observations histogram
p4 <- ggplot(data=projects, aes(x=projects$identifiers)) + 
  geom_histogram(breaks=seq(0, 1000, by=10), 
                 col="black", 
                 fill="green", 
                 alpha = .2) + 
  theme_bw() +
  labs(title="", x="Number of identifiers", y="")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


