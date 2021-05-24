library(tidyverse)
library(dslabs)
data(gapminder)
max(gapminder$year)
names(gapminder)
years <- c(1970,2015)
fertility_vs_life<-gapminder %>% 
  filter(year %in% years & !is.na(life_expectancy) & !is.na(fertility)) %>%
  group_by(year) %>%
  ggplot(aes(fertility,life_expectancy, size = population, color=continent))+
  geom_point() +
  facet_grid(~year)
fertility_vs_life
  
dpd <- gapminder %>% filter(!is.na(gdp)) %>% mutate(dollars_per_day = (gdp/population)/365, weight = population/sum(population)) %>%
  data.frame()

years <- c(1970,1990,2010)
dpd %>% filter(year %in% years & !is.na(dollars_per_day)) %>%
  group_by(year) %>%
  ungroup() %>%
  ggplot(aes(x=dollars_per_day, fill = continent, weight = weight)) +
  scale_x_continuous(trans = "log2", limits = c(0.125,300))+
  geom_density(alpha = 0.2, position = "stack", bw = 0.75)+
  facet_grid(year~.) + 
  xlab("Dolares al Dia")+
  ylab("Porcentaje de poblacion mundial")
  
options(digits = 3)
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare ) %>%
  mutate(Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex))

titanic %>% group_by(Sex) %>% ggplot(aes(Age, color = Sex)) + 
  geom_density()
sum(ifelse(titanic$Sex == "female",1,0))
sum(ifelse(titanic$Sex == "male",1,0))

sum(ifelse(titanic$Age >=40 & titanic$Age<41 & titanic$Age & !is.na(titanic$Age) & titanic$Sex == "female",1,0))
sum(ifelse(titanic$Age >=40 & titanic$Age<41 & titanic$Age & !is.na(titanic$Age) & titanic$Sex== "male",1,0))

titanic %>% filter(Age == max(titanic$Age, na.rm = TRUE)) %>% select(Sex)

params <- titanic %>% filter(!is.na(Age)) %>% summarise(mean = mean(Age), sd = sd(Age))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age)) + 
  geom_qq(dparams = params) +
  geom_abline()

titanic %>% group_by(Sex) %>% ggplot(aes(Survived, fill = Sex)) +
  geom_bar()

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,color = Survived)) +
  geom_density(alfa = 0.2)

titanic %>% filter(Fare>=0) %>% ggplot(aes(Survived, Fare, color = Survived)) +
  geom_boxplot(alfa = 0.2) + 
  geom_jitter() +
  scale_y_continuous(trans = "log2")

titanic %>% ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()
titanic %>% ggplot(aes(Pclass, fill = Survived)) +
geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>% ggplot(aes(Age,..count..,fill = Survived)) +
  geom_density(alpha = 0.2, ) + 
  facet_grid(Sex~Pclass)

library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) 

names(stars)
stars %>% ggplot(aes(magnitude)) +
  geom_density()

stars %>% ggplot(aes(temp)) +
  geom_density()

stars %>% ggplot(aes(temp, magnitude,color = type ,label = star)) +
  geom_point(size = 5) + 
  scale_y_reverse()

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%  filter(!is.na(carbon_emissions)) %>%.$year %>% max() 
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% min() 
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year) 

maxYear <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max()
minYear <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% min()
data <- temp_carbon %>% filter(year==maxYear | year==minYear) %>% select(year, carbon_emissions)
data <- data[order(data$year),]
proportion <- data$carbon_emissions[2]/data$carbon_emissions[1]

minYear <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% select(year) %>% min()
maxYear <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% select(year) %>% max()
data <- temp_carbon %>% filter(year==maxYear | year==minYear) %>% select(year, temp_anomaly)
data <- data[order(data$year),]
proportion <- data$temp_anomaly[2]-data$temp_anomaly[1]

p<- temp_carbon %>% filter(!is.na(temp_anomaly) & !is.na(land_anomaly)& !is.na(ocean_anomaly)) %>% ggplot(aes(year)) +
  geom_line(aes(y= temp_anomaly)) +
  geom_line(aes(y= ocean_anomaly), col = "blue") + 
  geom_line(aes(y= land_anomaly), col = "red") 
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")+
  geom_text(aes(x = 2000, y=.05, label = "20th century mean"), col= "black")+
  geom_hline(aes(yintercept=0), col = "black")



temp_carbon %>% filter(!is.na(temp_anomaly) & year>=1900 & year<2000) %>% summarise(avg = mean(temp_anomaly))

greenhouse_gases %>%
  ggplot(aes(x=year,y=concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept=1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(year, carbon_emissions))+
  geom_line()

co2_time <- historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(year,co2, color = source))+
  geom_line() + 
  xlim(-3000,2018)


#Creating a grid
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

comRunners=combinations(8,3)
mean(comRunners[,1]<4 & comRunners[,2]<4 & comRunners[,3]<4 )
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
NbSim <- 10000
results<-replicate(NbSim,{
  medallist <- sample(runners,3)
  (medallist[1]=="Jamaica" & medallist[2]=="Jamaica" & medallist[3]=="Jamaica")
})
 mean(results) 
 
mealsPosible <- function(choices){
  6*dim(combinations(choices,2))[1]*3
}
sapply(2:12,mealsPosible)

length(levels(esoph$agegp))*length(levels(esoph$alcgp))*length(levels(esoph$tobgp))

all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)

newData <- esoph%>%filter(esoph$alcgp =="120+")
sum(newData$ncases)/(sum(newData$ncontrols)+sum(newData$ncases))

newData <- esoph%>%filter(esoph$alcgp =="0-39g/day")
sum(newData$ncases)/(sum(newData$ncontrols)+sum(newData$ncases))


newData <- esoph%>%filter(esoph$tobgp == "30+" | esoph$alcgp == "120+")
sum(newData$ncontrols)/all_controls

avg_S<-20.9
sd_S<-5.7
set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000, avg, sd)
sd(act_scores)
x<-seq(1,36,1)
f_x<-dnorm(x, avg, sd)
z_scores<-(act_scores-mean(act_scores))/sd(act_scores)
avg_G<-mean(act_scores)
sd_G<-sd(act_scores)

CDF <- function(value){
  pnorm(value, avg_G, sd_G)
}
CDF(x)
p<-seq(0.01, 0.99,0.01)
sample_quantiles<-quantile(act_scores, probs = p)
teoretical_quantiles<-qnorm(p,avg_S, sd_S)
