library(readxl)
setwd("~/Documents/WM_NCSRP_selection")

WM <- read_excel("WM-2015-2016-2017_fixingproductnames.xlsx")

class(WM)

colnames(WM
)
str(WM)

WM$Field <- (as.factor(WM$Field))
WM$Field
plot(WM$Field)
library(dplyr)
####filterring by 2015 for ne-survey
WM1S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="NE",Year=="2015" )
# that have at least 10 sclerotia 
NE_S_15 <- summary(WM1S$Field)[summary(WM1S$Field)>9]
barplot(WM1S)
barplot(sort(summary(WM1S$Field)))
str(WM)

length(sort(summary(WM$Field)))-1


sobarplot(sort(summary(WM$Field))[ 1:length(sort(summary(WM$Field)))-1])
####filterring by 2016 for ne-survey, and there are nothing
WM2S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="NE",Year=="2016" )
# that have at least 10 sclerotia and there are nothing, correct!

NE_S_16 <- summary(WM2S$Field)[summary(WM2S$Field)>9]

## Filterring by 2017 for ne-survey , correct!
WM3S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="NE",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

NE_S_17 <- summary(WM3S$Field)[summary(WM3S$Field)>9]

## Filterring by 2015 for MI-survey, correct!
WM4S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="MI",Year=="2015" )
# that have at least 10 sclerotia, correct!

MI_S_15 <- summary(WM4S$Field)[summary(WM4S$Field)>9]
## Filterring by 2016 for MI-survey and there are nothing, correct!
WM5S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="MI",Year=="2016" )
# that have at least 10 sclerotia and there are nothing, correct!

MI_S_16 <- summary(WM5S$Field)[summary(WM5S$Field)>9]

## Filterring by 2017 for MI-survey and there are nothing, correct!
WM6S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="MI",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

MI_S_17 <- summary(WM6S$Field)[summary(WM6S$Field)>9]

## Filterring by 2015 for IA-survey, correct!
WM7S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="IA",Year=="2015" )
# that have at least 10 sclerotia and, correct!


IA_S_15 <- summary(WM7S$Field)[summary(WM7S$Field)>9]
## Filterring by 2016 for IA-survey and there are nothing, correct!
WM8S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="IA",Year=="2016" )
# that have at least 10 sclerotia and there are nothing, correct!
IA_S_16 <- summary(WM8S$Field)[summary(WM8S$Field)>9]

## Filterring by 2017 for IA-survey and there are nothing, correct!
WM9S <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="IA",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

IA_S_17 <- summary(WM9S$Field)[summary(WM9S$Field)>9]

#### Filterring by 2015 for WI-survey, correct!
WM10S<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="WI",Year=="2015" )
# that have at least 10 sclerotia and there are nothing, correct!

WI_S_15 <- summary(WM10S$Field)[summary(WM10S$Field)>9]


##### Filterring by 2016 for WI-survey,  and there are nothing, correct!

WM11<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="WI",Year=="2016" )
# that have at least 10 sclerotia, and there are nothing, correct!

WI_S_16 <- summary(WM11S$Field)[summary(WM11S$Field)>9]

##### Filterring by 2017 for WI-survey and there are nothing, correct!

WM12S<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="WI",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

WI_S_17 <- summary(WM12S$Field)[summary(WM12S$Field)>9]


#############NOW TRIALS###
####filterring by 2015 for ne-survey
WM1_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="NE",Year=="2015" )
# that have at least 10 sclerotia 
NE_T_15 <- summary(WM1_T$Field)[summary(WM1_T$Field)>9]


sobarplot(sort(summary(WM$Field))[ 1:length(sort(summary(WM$Field)))-1])
####filterring by 2016 for ne-survey, and there are nothing
WM2_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="NE",Year=="2016" )
# that have at least 10 sclerotia and there are nothing, correct!

NE_T_16 <- summary(WM2_T$Field)[summary(WM2_T$Field)>9]

## Filterring by 2017 for ne-survey , correct!
WM3_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="NE",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

NE_T_17 <- summary(WM3_T$Field)[summary(WM3_T$Field)>9]

## Filterring by 2015 for MI-survey, correct!
WM4_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="MI",Year=="2015" )
# that have at least 10 sclerotia, correct!

MI_T_15 <- summary(WM4_T$Field)[summary(WM4_T$Field)>9]
## Filterring by 2016 for MI-survey and there are nothing, correct!
WM5_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="MI",Year=="2016" )
# that have at least 10 sclerotia and there are nothing, correct!

MI_T_16 <- summary(WM5_T$Field)[summary(WM5_T$Field)>9]

## Filterring by 2017 for MI-survey and there are nothing, correct!
WM6_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="MI",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

MI_T_17 <- summary(WM6_T$Field)[summary(WM6_T$Field)>9]

###
## Filterring by 2015 for IA-survey, correct!
WM7_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="IA",Year=="2015" )
# that have at least 10 sclerotia and, correct!


IA_T_15 <- summary(WM7_T$Field)[summary(WM7_T$Field)>9]
## Filterring by 2016 for IA-survey and there are nothing, correct!
WM8_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="IA",Year=="2016" )
# that have at least 10 sclerotia and there are nothing, correct!
IA_T_16 <- summary(WM8_T$Field)[summary(WM8_T$Field)>9]

## Filterring by 2017 for IA-survey and there are nothing, correct!
WM9_T <- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="IA",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

IA_T_17 <- summary(WM9_T$Field)[summary(WM9_T$Field)>9]

#### Filterring by 2015 for WI-survey, correct!
WM10_T<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="WI",Year=="2015" )
# that have at least 10 sclerotia and there are nothing, correct!

WI_T_15 <- summary(WM10_T$Field)[summary(WM10_T$Field)>9]


##### Filterring by 2016 for WI-survey,  and there are nothing, correct!

WM11_T<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="WI",Year=="2016" )
# that have at least 10 sclerotia, and there are nothing, correct!

WI_T_16 <- summary(WM11_T$Field)[summary(WM11_T$Field)>9]

##### Filterring by 2017 for WI-survey and there are nothing, correct!

WM12_T<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey", State=="WI",Year=="2017" )
# that have at least 10 sclerotia and there are nothing, correct!

WI_T_17 <- summary(WM12_T$Field)[summary(WM12_T$Field)>9]



###########################

# WM7 <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="trial", State=="IA",Year=="2016" )
# IA_16 <- summary(WM7$Field)[summary(WM7$Field)>9]
# IA_16
# 
# ##
# 
# WM8 <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="trial", State=="IA",Year=="2017" )
# IA_17 <- summary(WM8$Field)[summary(WM8$Field)>9]
# IA_17
# 
# 
# 
# 
# WM10<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="trial", State=="WI",Year=="2016" )
# WI_16 <- summary(WM10$Field)[summary(WM10$Field)>9]
# WI_16
# 
# #####when its trial first it was confirmed first Survey for eah one
# 
# WM11<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="trial", State=="WI",Year=="2017" )
# WI_17 <- summary(WM11$Field)[summary(WM11$Field)>9]
# WI_17

# just filterring by fields SURVEY that have at least 10 sclerotia BY YEAR 2015
WM12<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey",Year=="2015" )
year2015<- summary(WM12$Field)[summary(WM12$Field)>9]
prod1 <- unique(WM13$Products_Soybean)[6]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[1]

prod1 <- unique(WM13$Products_Soybean)[7]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[1]

prod1 <- unique(WM13$Products_Soybean)[9]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[4]

prod1 <- unique(WM13$Products_Soybean)[12]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[3]

prod1 <- unique(WM13$Products_Soybean)[9]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[4]

prod1 <- unique(WM13$Products_Soybean)[5]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[3]

prod1 <- unique(WM13$Products_Soybean)[10]
loc11<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- "Approach"


levels(WM13$Products_Soybean) <- unique(WM13$Products_Soybean)
WM13$Products_Soybean <- as.factor(WM13$Products_Soybean)

summary

prodlist<-unique(WM13$Products_Soybean)
loc2 <- grep(prodlist[1], WM13$Products_Soybean)
newdat <- WM12[loc2,]
priaxor <- summary(newdat$Field)[summary(newdat$Field)>9]
###  2 stratego, 4 = TM, 5 = Domark, 6=Quilt, 7=Pri+App,
### 8=TM --empty?, 9 = Quilt --empty?, 10 = approach -- empty??
loc2 <- grep(prodlist[2], WM13$Products_Soybean)
newdat <- WM12[loc2,]
stratego <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[4], WM13$Products_Soybean)
newdat <- WM12[loc2,]
TM <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[5], WM13$Products_Soybean)
newdat <- WM12[loc2,]
Domark <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[6], WM13$Products_Soybean)
newdat <- WM12[loc2,]
Quilt <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[7], WM13$Products_Soybean)
newdat <- WM12[loc2,]
pri.app <- summary(newdat$Field)[summary(newdat$Field)>9]

priaxor
stratego
TM
Domark
Quilt
pri.app

# priaxor Fluxapryroxad + Piraclos
# stratego TRi +Propi
# TM
# Domark Tetraconaz
# Quilt  AZ + Propi



summary(WM13$Products_Soybean)


length(year2015)


############

WM13<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
  filter(Origin=="Survey",Year=="2015" )
year20155<- summary(WM13$Field)[summary(WM13$Field)>9]
prod11 <- unique(WM13$Products_Soybean)[6]
loc11 <- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[1]


prod11 <- unique(WM13$Products_Soybean)[7]
loc11<- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[1]

prod11 <- unique(WM13$Products_Soybean)[9]
loc11<- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[4]

prod11 <- unique(WM13$Products_Soybean)[12]
loc11<- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[3]

prod11 <- unique(WM13$Products_Soybean)[9]
loc11<- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[4]

prod11 <- unique(WM13$Products_Soybean)[5]
loc11<- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[3]

prod11 <- unique(WM13$Products_Soybean)[10]
loc11<- grep(prod11, WM13$Products_Soybean) 
WM13$Products_Soybean[loc11] <- "Approah"


levels(WM13$Products_Soybean) <- unique(WM13$Products_Soybean)
WM13$Products_Soybean <- as.factor(WM13$Products_Soybean)

summary

prodlistt<-unique(WM13$Products_Soybean)
loc22 <- grep(prodlistt[1], WM13$Products_Soybean)
newdatt <- WM13[loc22,]
priaxorrr <- summary(newdatt$Field)[summary(newdatt$Field)>9]
###  2 stratego, 4 = TM, 5 = Domark, 6=Quilt, 7=Pri+App,
### 8=TM --empty?, 9 = Quilt --empty?, 10 = approach -- empty??
loc22 <- grep(prodlist[2], WM13$Products_Soybean)
newdat <- WM12[loc2,]
stratego <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[4], WM13$Products_Soybean)
newdat <- WM12[loc2,]
TM <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[5], WM13$Products_Soybean)
newdat <- WM12[loc2,]
Domark <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[6], WM13$Products_Soybean)
newdat <- WM12[loc2,]
Quilt <- summary(newdat$Field)[summary(newdat$Field)>9]
loc2 <- grep(prodlist[7], WM13$Products_Soybean)
newdat <- WM12[loc2,]
pri.app <- summary(newdat$Field)[summary(newdat$Field)>9]

priaxor
stratego
TM
Domark
Quilt
pri.app
Headline

# priaxor Fluxapryroxad + Piraclos
# stratego TRi +Propi
# TM
# Domark Tetraconaz
# Quilt  AZ + Propi
#Pyraclostrobin



summary(WM13$Products_Soybean)


length(year2015)

################################
WM14<- WM %>% 
  group_by(`Collection ID`, Field, `Form ID`, State, Products, History) %>%   
  filter(Origin=="Survey",Year=="2016" )
year2016<- summary(WM14$Field)[summary(WM14$Field)>9]
prod1_1 <- unique(WM13$Products_Soybean)[6]
loc11_1<- grep(prod1, WM13$Products_Soybean) 
WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[1]

