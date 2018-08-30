library(gdata)
library(ggmap)
library(maps)
library(mapdata)
#library(xlsx)
#library(readxl)
library(misc3d)
library(ptinpoly)
library(tidyverse)
library(ggplot2)
library(ezec)
library(NRES803)
library(broom)
library(dplyr)

WM <- read.csv("data/WM-2015-2016-2017_fixingproductnames.csv", stringsAsFactors = TRUE)
levels(WM$Origin )[levels(WM$Origin ) == "trial"] <- "Fungicide field trials"
levels(WM$Origin )[levels(WM$Origin ) == "Survey"] <- "Farmer fields"
levels(WM$DNA.Extraction )[levels(WM$DNA.Extraction ) == ""] <- "No DNA extraction"
levels(WM$DNA.Extraction )[levels(WM$DNA.Extraction ) == "No"] <- "No DNA extraction"
levels(WM$DNA.Extraction )[levels(WM$DNA.Extraction ) == "Yes"] <- "Yes DNA extraction"

WM$Year <- as.factor(WM$Year)
WM$Collection_ID <- as.numeric(WM$Collection_ID)  
WM$Collection_ID <- as.character(WM$Collection_ID)  

# class(WM)
# 
# colnames(WM
# )
# str(WM)
# 
#WM$Field <- (as.factor(WM$Field))
# WM$Field
# plot(WM$Field)



wm1 <-
  WM %>% select(c(Collection_ID, State, Field, Year, Origin, long, lat, hyphal_tip..ht., DNA.Extraction))
#ggplot(wm1, aes(x = State, y = Collection ID, fill= Origin))
#wm1 <- na.omit(wm1)
#wm1$Collection_ID <- as.numeric(wm1$Collection_ID)  
#wm1$Collection_ID <- as.character(wm1$Collection_ID)  

#mexican_isolates <- c(1857:1940)
#noyear <- c(2115,2392)
tested <- c("1","118", "123", "12B", "129", "20", "21", "449", "461", "467", "475", "558", "564", "568", "581", "645", "800", "667", "74SS1", "8", "87", "318", "413", "419", "62-02", "62-03", "62-04", "78-01", "78-02", "78-05", "H-01", "H-03", "H-04", "I-20", "S-01", "W212", "1025", "1026", "1027", "1029", "1032","1033", "1870","1872", "1884", "1885", "54C", "65B", "51C", "71B", "64D", "53B", "60A","698", "699", "710", "711", "724", "725", "731", "732", "738", "739", "746", "751", "755", "756", "757", "764", "765", "771", "772", "786", "787", "811", "812", "813", "814", "817", "818", "851", "852", "853", "855", "858", "859",  "860", "861", "862", "867", "870", "871", "877", "878", "884", "885", "891", "892", "896", "897", "901", "902", "905", "906", "908", "909", "911", "912", "914","274", "307", "504", "505", "2384", "2385", "2386","2388","2407", "2408", "2383", "1058", "1081", "1087", "1109", "1127","1128","1134", "1135", "1139", "1175", "1026","1027","1029", "1328", "1329", "1330", "1331","1332","1340", "1345", "1365", "1366","1392", "1327", "1502", "1582", "1620", "1622", "1541", "1671", "1672", "1691", "1692", "1712","1713","1721", "1722", "1731", "1732","1791", "1","118", "123", "20", "74SS1", "8",  "419", "78-02", "H-01", "H-03")
mexican_isolates <- c("1857":"1940")

# wm1_ <-  wm1 %>%
#   filter(!Collection_ID %in% mexican_isolates &!Collection_ID %in% noyear) %>% 
# arrange(State = "NE", "IA", "WI", "MI")

# wm1_ <-  wm1 %>%
#   mutate(Status = ifelse(Collection_ID  %in% tested,
#                          "tested", "no_tested")) %>%
#   filter(!Collection_ID %in% mexican_isolates) %>%
#   group_by(Field) %>%
#   filter(n() > 9) %>%
#   ungroup()
#
wm1_ <-  wm1 %>%
  mutate(Status = ifelse(Collection_ID  %in% tested,
                         "tested", "no_tested")) %>%
  filter(!Collection_ID %in% mexican_isolates) %>%
  group_by(Field) %>% mutate (num_isolates_per_field = ifelse(n() > 9, ">9isol", 
                                                              ifelse(n() > 2, "2>isol<9", "<2isol"))) %>% ungroup()
wm1_$num_isolates_per_field <- as.factor(wm1_$num_isolates_per_field)


###

# wm2_ <-  wm1 %>%
# mutate(Status = ifelse(Collection_ID  %in% tested,
# "tested", "no_tested")) %>%
# filter(!Collection_ID %in% mexican_isolates) %>%
# group_by(Field) %>%
# filter(n() <= 9) %>% ungroup()

##

# wm2_ <-  wm1 %>%
#    mutate(Status = ifelse(Collection_ID  %in% tested,  "tested", "no_tested")) %>%
#   group_by(Field) %>% mutate (number_of_isolates = ifelse(n() >9), "good", "bad")



# wm2_ <-  wm1 %>%
#   group_by(State)  %>% count(Field, State)
# 
# wm3_ <-  wm1 %>%
#   group_by(State)  %>% frequency(Field)
#wm1_$Origin <- (as.factor(wm1_$Origin))

#wm1_ <-  wm1_ %>%
#filter(!Collection_ID %in% mexican_isolates &!Collection_ID %in% noyear) %>%arrange(match(State, c("NE", "IA", "WI", "MI"))) 

#wm1$long <- as.numeric( as.character(wm1$long))
#wm1$long <- (wm1$long)*-1

wm1_$long <- as.numeric( as.character(wm1_$long))
wm1_$long <- (wm1_$long)*-1

# wm2_$long <- as.numeric( as.character(wm2_$long))
# wm2_$long <- (wm2_$long)*-1

#h <- ggplot(wm1_, aes(State, fill= Origin))
#h + geom_bar() +facet_wrap(~Year) + scale_fill_brewer (palette = "Set1") +   scale_x_discrete(limits=c("NE", "IA", "WI", "MI"))+  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Verdana"), axis.title= element_text(size = 14, face = "bold", hjust = 0.5), axis.text = element_text(face = "bold",size = 14, family = "Verdana"),panel.background = element_rect(fill = "white", colour = "grey50")) + labs(x ="States", y = "# Isolates" )

#h <- ggplot(wm1_, aes(State, fill= Origin))+ geom_bar()+ geom_text(stat='count', aes(label=..count..), vjust=2, size= 3.5)+ facet_wrap(~Year) + scale_fill_brewer (palette = "Set1") +   scale_x_discrete(limits=c("NE", "IA", "WI", "MI"))+ theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Verdana"), axis.title= element_text(size = 14, face = "bold", hjust = 0.5), axis.text = element_text(face = "bold",size = 14, family = "Verdana"),panel.background = element_rect(fill = "white", colour = "grey50", size = 2)) + labs(title = "Current collection ", x ="States", y = "# Isolates" )+ theme(strip.text = element_text(face="bold", size=14,lineheight=5.0))


library(sp)
library(ggmap)
#SpatialPoints(Location_of_field, wm1_)



usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")

#mid_west <- subset(states, region %in% c("nebraska", "iowa", "michigan", "wisconsin"))
#mid_west_df <- subset(states, region == "nebraska"|region == "wisconsin"|region == "michigan"|region == "iowa")
mid_west_county <- subset(counties, region == "nebraska"|region == "wisconsin"|region == "michigan"|region == "iowa")

bra <- map_data("worldHires", "Brazil")
mexico <- map_data("worldHires", "Mexico")
Mozambique <- map_data("worldHires", "Mozambique")
# ggplot(data = mid_west) +
#   geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")
#
#ggplot(data = mid_west) +
 # geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
  #coord_fixed(1.3)

# labs <- data.frame(
#   long = c(-100, -101),
#   lat = c(42, 42.1),
#   names = c("SWFSC-FED", "NWFSC"),
#   stringsAsFactors = FALSE)  

# wm2 <- wm1 %>% select(long, lat)
# wm2$lat <- as.numeric(wm2$lat)
# wm2$long <- as.numeric( as.character(wm1$long))
# wm2$long <- (wm1$long)*-1

#wm2$lat

# ggplot(data = mid_west) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
#   coord_fixed(1.3) + geom_point(data = labs, aes(x = long, y = lat), color = "red", size = 2) 


# ggplot(data = mid_west) +
#   geom_polygon(aes(x = long, y = lat, group = group),
#                fill = "palegreen",
#                color = "black") +
#   coord_fixed(1.3) + geom_point(data = wm2,
#                                 aes(x = lat, y = long),
#                                 color = "red",
#                                 size = 2)




wm1_ <- wm1_[-970,]
mid_west_base <-
  ggplot(data = mid_west_county,
         mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey") 
mexico_base <-
  ggplot(data = mexico,
         mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey") 
#mid_west_base + theme_nothing()
 #mexico_base+ theme_nothing()

# mid_west_good <-  mid_west_base + theme_nothing() +
#   geom_polygon(data = mid_west_county, fill = "grey", color = "white") +
#   geom_polygon(color = "black", fill = NA)
# mid_west_good
#
# mid_west_base +
#   geom_polygon(data = mid_west_county, fill = NA, color = "white") +
#   geom_polygon(color = "black", fill = NA)  # get the state border back on top
#
# mid_west_base + theme_nothing() +
#   geom_polygon(data = mid_west_county, fill = NA, color = "white") +
#   geom_polygon(color = "black", fill = NA) + geom_point()

# mid_west_good <-  mid_west_base + theme_nothing() +
#   geom_polygon(data = mid_west_county, fill = NA, color = "white") +
#   geom_polygon(color = "green", fill = NA)
# mid_west_good
# ggplot(data = mid_west) +
#   geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")
# ggplot(data = mid_west) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
#   coord_fixed(1.3)

###

# mid_west_base+ geom_point(data = wm11, aes(x = long, y = lat),inherit.aes = FALSE, color = "red", size = 2)
# wm11$long <- (wm11$long)*-1
# wm11 <-(wm1)[1:5,]
# wm11$long
# wm11$long <- as.numeric( as.character(wm11$long))
# wm11$long


pdf("r.plot.pdf", onefile=T) #paper='A4r')
a <-  mid_west_base + 
  geom_point(data = wm1_, aes(x = long, y = lat),inherit.aes = FALSE, color = "red", size = 0.3, na.rm = TRUE, shape=22, stroke= 0.5, fill= "white")+ labs(title = "Sclerotinia sclerotiorum Soybean inventory from Midwest") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))
a

# mid_west_base + geom_point(data = wm2_, aes(x = long, y = lat),inherit.aes = FALSE, color = "red", size = 0.3)
#by origin
b <- mid_west_base + geom_point(data = wm1_, aes(x = long, y = lat, color= Origin),inherit.aes = FALSE, size = 1, na.rm = TRUE, shape=22, stroke= 0.5, fill= "white") + theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Midwest") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))
b

# mid_west_base + geom_point(data = wm2_, aes(x = long, y = lat, color= Origin),inherit.aes = FALSE, size = 1, na.rm = TRUE)
#wm1_$Year <- as.numeric(wm1_$Year)
#wm1_$Year <- as.factor(wm1_$Year)
#by year
c <- mid_west_base + geom_point(data = wm1_, aes(x = long, y = lat, color= Year),inherit.aes = FALSE, size = 1,  na.rm = TRUE, shape=22, stroke= 0.5, fill= "white")+ theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Midwest") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))
c
#by origin wrap it by year
d <- mid_west_base + geom_point(data = wm1_, aes(x = long, y = lat, color= Origin),inherit.aes = FALSE, size = 1, na.rm = TRUE, shape=22, stroke= 0.5, fill= "white")+ facet_wrap(~Year)+ theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Midwest") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))

d
#wm2 <- wm1 %>% group_by(State) %>% summarise(count(Collection_ID))
#wm2 <- wm1 %>% group_by(State, Year, Origin) %>% summarise(isolates= sum(Collection_ID, na.rm = TRUE))
##
#farmer fields

WMfarmerfields <- wm1_ %>%
group_by(State,Year, Origin) %>%
filter(Origin=="Farmer fields")


e <-  mid_west_base + geom_point(data = WMfarmerfields, aes(x = long, y = lat, color= Year),inherit.aes = FALSE, size = 1, na.rm = TRUE, shape=22, stroke= 0.5, fill= "white") + theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum of Farmer Field Soybean from Midwest") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))
e


#fungicide field trials
WMfungicide <- wm1_ %>% 
  group_by(State,Year, Origin) %>%   
  filter(Origin=="Fungicide field trials")

#WMfungicide$Year <- as.numeric(WMfungicide$Year)
#WMfungicide$Year <- as.factor(WMfungicide$Year)
f <- mid_west_base + geom_point(data = WMfungicide, aes(x = long, y = lat, color= Year),inherit.aes = FALSE, size = 1, na.rm = TRUE, shape=22, stroke= 0.5, fill= "white") + theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum of Fungicide Field Trials Soybean from Midwest") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))
f


##
####filterring by NE
WM1S <- wm1_ %>% 
  group_by(State, Origin, Status) %>%   
  filter(Origin=="Farmer fields", State=="NE" ) %>% 
ungroup()
#WM1S$Year <- as.numeric(WM1S$Year)
#WM1S$Year <- as.factor(WM1S$Year)
WM1S$Status = factor(WM1S$Status , levels = c("tested", "no_tested"))

NE_county <- subset(counties, region == "nebraska")
NE_county_base <-
  ggplot(data = NE_county,
         mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey")
#colour <- c("white", "yellow")
g <- NE_county_base + geom_point(
  data = WM1S,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  na.rm = TRUE,
  size = 0.8
) + theme(legend.position = "bottom")+ facet_wrap(~Status) +
scale_color_manual(values = c("red", "blue")) + theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Nebraska by already tested/no tested") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
))

g
#By DNA extraction

h <- NE_county_base + geom_point(
  data = WM1S,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction ,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  na.rm = TRUE,
  size = 0.8
) + theme(legend.position = "bottom")+ facet_wrap(~Status) +
  scale_color_manual(values = c("red", "blue"))+ theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Nebraska by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
h
# As we can noticed here it's been  for the furthest places


#MAKE A MAP FOR THE PRINCIPAL COUNTIES FOR NEBRASKA
NE_counties <- counties %>% group_by(region) %>% filter( region == "nebraska") %>% ungroup %>% filter(subregion=="antelope"|subregion=="madison"|subregion=="cedar"|subregion=="cuming"|subregion=="dodge"|subregion=="boone"|subregion=="holt"|subregion=="valley"|subregion=="greeley"|subregion=="sherman"|subregion=="howard")                                                                                          

centroid <- aggregate(data=NE_counties, 
                      cbind(long,lat) ~ subregion, 
                      FUN= mean)

NE_counties_base <-  ggplot(data = NE_counties,
         mapping = aes(x = long, y = lat, group= subregion)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey") +  geom_text(data = centroid, aes(label = subregion, x = long, y = lat), check_overlap = TRUE, size= 3)

# ######NEW
# Edgar_counties <- NE_counties %>% 
#   select(long, lat)
# 
# Edgar_counties <- as.matrix(Edgar_counties)
# 
# Edgar_queries <- wm1_ %>% 
#   filter(State == "NE") %>% 
#   select(long, lat)
# 
# Edgar_queries <- as.matrix(Edgar_queries)
# 
# Edgar_ans <- pip2d(Edgar_counties,Edgar_queries)
# 
# ######NEW

i <- NE_counties_base + geom_point(
  data = WM1S,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 0.8
) + theme(legend.position = "bottom")+ facet_wrap(~Status) +
  scale_color_manual(values = c("red", "blue"))+ theme(legend.position = "bottom") + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Nebraska counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
i
### By DNA extraction
j <- NE_counties_base + geom_point(
  data = WM1S,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 0.8
) + theme(legend.position = "bottom")+ labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Nebraska counties by already tested/no tested") + theme(plot.title = element_text(
  size = 10,
  face = "bold",
  hjust = 0.5,
  family = "Helvetica"
)) + facet_wrap(~Status) +
  scale_color_manual(values = c("red", "blue"))
j
##IOWA

WM_IA <- wm1_ %>% 
  #group_by(  Origin, Status) %>%   
  filter( State=="IA" )

# WM_IA_tested <- wm1_ %>%
#   group_by(State,  Origin, Status) %>%
#   filter( State=="IA" & Status == "tested" )

#WM_IA$Year <- as.numeric(WM_IA$Year)
#WM_IA$Year <- as.factor(WM_IA$Year)
#WM_IA$Status <- as.factor(WM_IA$Status)
WM_IA$Status = factor(WM_IA$Status , levels = c("tested", "no_tested"))

IA_county <- subset(counties, region == "iowa")
IA_county_base <- ggplot(data = IA_county, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "white")

k <- IA_county_base + geom_point(
  data = WM_IA,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
     
  ),
  inherit.aes = FALSE,
  size = 1.5
)  + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Iowa by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
k
### By DNA extraction
l <- IA_county_base + geom_point(
  data = WM_IA,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
    
  ),
  inherit.aes = FALSE,
  size = 1.5
)  + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Iowa by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
l
###
m <- IA_county_base + geom_point(
  data = WM_IA,
   aes(
   x = long,
   y = lat,
   color = Status,
   shape = num_isolates_per_field
    ),
   inherit.aes = FALSE,
  size = 1.5) +
   theme(legend.position = "bottom")+ 
   scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ DNA.Extraction) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Iowa by already DNA extraction") + theme(plot.title = element_text(
     size = 10,
     face = "bold",
     hjust = 0.5,
     family = "Helvetica"
   ))

m
#MAKE A MAP FOR THE PRINCIPAL COUNTIES FOR IOWA

IA_counties <- counties %>% group_by(region) %>% filter( region == "iowa") %>% ungroup %>% filter(subregion=="floyd"|subregion=="butler"|subregion=="chickasaw"|subregion=="bremer"|subregion=="shelby"|subregion=="boone"|subregion=="polk"|subregion=="bremer"|subregion=="benton"|subregion=="tama")
centroid_iowa <- aggregate(data=IA_counties, cbind(long,lat) ~ subregion, FUN= mean)

IA_counties_base <-  ggplot(data = IA_counties,
                            mapping = aes(x = long, y = lat, group = subregion)) + 
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey") +  geom_text(data = centroid_iowa, aes(label = subregion, x = long, y = lat), check_overlap = TRUE, size= 3)




n <- IA_counties_base + geom_point(
  data = WM_IA,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 0.5
) + theme(legend.position = "bottom")+ facet_wrap(~Status) +
  scale_color_manual(values = c( "blue", "chartreuse4", "red")) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Iowa counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
n
##
o <- IA_counties_base + geom_point(
  data = WM_IA,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 0.5
) + theme(legend.position = "bottom")+ facet_wrap(~Status) +
  scale_color_manual(values = c( "blue", "chartreuse4", "red")) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Iowa counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))

o
##
p <- IA_counties_base + geom_point(
  data = WM_IA,
  aes(
    x = long,
    y = lat,
    color = Status,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 0.5
) + theme(legend.position = "bottom")+ facet_wrap(~DNA.Extraction) +
  scale_color_manual(values = c( "chartreuse4","blue", "red")) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Iowa counties by already DNA extraction") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
p


# IA_county_base + geom_point(
#   data = WM_IA_tested,
#   aes(
#     x = long,
#     y = lat,
#     color = Status,
#     shape = Year
#   ),
#   inherit.aes = FALSE,
#   size = 2, color="blue"
# )


### WISCONSIN
WM_WI <- wm1_ %>% 
  #group_by(State,  Origin, Status) %>%   
  filter( State=="WI" )

# WM_WI_tested <- wm1_ %>% 
#   #group_by(State,  Origin, Status) %>%   
#   filter( State=="WI" & Status== "tested")
#WM_WI$Year <- as.numeric(WM_WI$Year)
#WM_WI$Year <- as.factor(WM_WI$Year)
#WM_WI$Status <- as.factor(WM_WI$Status)
WM_WI$Status = factor(WM_WI$Status , levels = c("tested", "no_tested"))

WM_WIcounty <- subset(counties, region == "wisconsin")
WI_county_base <- ggplot(data = WM_WIcounty, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "white")

q <- WI_county_base + geom_point(
  data = WM_WI,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 1.5
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Wisconsin by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))#Status  


q
### By DNA extraction
r <- WI_county_base + geom_point(
  data = WM_WI,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
    
  ),
  inherit.aes = FALSE,
  size = 1.5
)+ theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Wisconsin by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
r
##

s <- WI_county_base + geom_point(
  data = WM_WI,
  aes(
    x = long,
    y = lat,
    color = Status,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 1.5
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ DNA.Extraction) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Wisconsin by already DNA extraction") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
s
#

#MAKE A MAP FOR THE PRINCIPAL COUNTIES FOR WI

WI_counties <- counties %>% group_by(region) %>% filter( region == "wisconsin") %>% ungroup %>% filter(subregion=="walworth"|subregion=="grant"|subregion=="lafayette"|subregion=="waushara"|subregion=="adams"|subregion=="dane")
centroid_wisconsin <- aggregate(data=WI_counties, cbind(long,lat) ~ subregion, FUN= mean)

WI_counties_base <-  ggplot(data = WI_counties,
                            mapping = aes(x = long, y = lat, group = subregion)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey") +  geom_text(data = centroid_wisconsin, aes(label = subregion, x = long, y = lat), check_overlap = TRUE, size= 3)


t <- WI_counties_base + geom_point(
  data = WM_WI,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 1
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Wisconsin counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
t
##
### By DNA extraction
u <- WI_counties_base + geom_point(
  data = WM_WI,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
    
  ),
  inherit.aes = FALSE,
  size = 1.5
)  + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Wisconsin counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
u
##

w <- WI_counties_base + geom_point(
  data = WM_WI,
  aes(
    x = long,
    y = lat,
    color = Status,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 1.5
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ DNA.Extraction) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Wisconsin counties by already DNA extraction") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
w
##


# WM_county_base + geom_point(
#   data = WM_WI_tested,
#   aes(
#     x = long,
#     y = lat,
#     color = Status,
#     shape = Year
#   ),
#   inherit.aes = FALSE,
#   size = 2, color= "blue"
# )





###Michigan
WM_MI <- wm1_ %>% 
  #group_by(State, Origin, Origin) %>%   
  filter( State=="MI" )
# WM_MI_tested <- wm1_ %>% 
  #group_by(State, Origin, Origin) %>%   
  # filter( State=="MI" )
#WM_MI$Year <- as.numeric(WM_MI$Year)
#WM_MI$Year <- as.factor(WM_MI$Year)

WM_MI$Status = factor(WM_MI$Status , levels = c("tested", "no_tested"))

WM_MIcounty <- subset(counties, region == "michigan")
WM_county_base <- ggplot(data = WM_MIcounty, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "white")

x <-   WM_county_base + geom_point(
  data = WM_MI,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 2
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Michigan by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))#Status  
x  
###
y <- WM_county_base + geom_point(
  data = WM_MI,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 1.5
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Michigan by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))

y
### By DNA extraction
z <- WM_county_base + geom_point(
  data = WM_MI,
  aes(
    x = long,
    y = lat,
    color = Status,
    shape = num_isolates_per_field
    
  ),
  inherit.aes = FALSE,
  size = 1.5
)  + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ DNA.Extraction) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from Michigan by already DNA extraction") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))
z
##
#MAKE A MAP FOR THE PRINCIPAL COUNTIES FOR MI
MI_counties <- counties %>% group_by(region) %>% filter( region == "michigan") %>% ungroup %>% filter(subregion=="montcalm"|subregion=="ingham"|subregion=="allegan"|subregion=="ionia"|subregion=="joseph"|subregion=="hillsdale"|subregion=="sanilac")
centroid_michigan <- aggregate(data=MI_counties, cbind(long,lat) ~ subregion, FUN= mean)
MI_counties_base <-  ggplot(data = MI_counties,
                            mapping = aes(x = long, y = lat, group = subregion)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey")+  geom_text(data = centroid_michigan, aes(label = subregion, x = long, y = lat), check_overlap = TRUE, size= 3)

aa <- MI_counties_base + geom_point(
  data = WM_MI,
  aes(
    x = long,
    y = lat,
    color = Year,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 1
) + theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Michigan counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  

aa
### By DNA extraction
bb <- MI_counties_base + geom_point(
  data = WM_MI,
  aes(
    x = long,
    y = lat,
    color = DNA.Extraction,
    shape = num_isolates_per_field
    
  ),
  inherit.aes = FALSE,
  size = 1.5
)+ theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ Status) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Michigan counties by already tested/no tested") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
bb
##

cc <- MI_counties_base + geom_point(
  data = WM_MI,
  aes(
    x = long,
    y = lat,
    color = Status,
    shape = num_isolates_per_field
  ),
  inherit.aes = FALSE,
  size = 0.5
)+ theme(legend.position = "bottom")+ 
  scale_color_manual(values = c("red", "blue", "chartreuse4")) + facet_wrap(~ DNA.Extraction) + labs(title = "Sclerotinia sclerotiorum Soybean inventory from representative Michigan counties by already DNA extraction") + theme(plot.title = element_text(
    size = 10,
    face = "bold",
    hjust = 0.5,
    family = "Helvetica"
  ))  
#dev.off()




#######

# pdf("r.plot.pdf", onefile = TRUE) 
# plot(a) 
# plot(b)
# plot(c) 
# plot(d)
# plot(e) 
# plot(f)
# plot(g) 
# plot(h)
# plot(i) 
# plot(j)
# plot(k) 
# plot(l)
# plot(m) 
# plot(n)
# plot(o) 
# plot(p)
# plot(q) 
# plot(r)
# plot(s) 
# plot(t)
# plot(w) 
# plot(x)
# plot(y) 
# plot(z)
# plot(aa) 
# plot(bb)
# plot(bb)
# plot(cc)
# dev.off()




#     axis.title = element_text(size = 14, face = "bold", hjust = 0.5),
# axis.text = element_text(
#   face = "bold",
#   size = 14,
#   family = "Verdana"
# ),
# panel.background = element_rect(fill = "white", colour = "grey50")
# ) + labs(title = "Boscalid", x = "Isolates", y = "EC50 (ppm)")
# grid.arrange( t, p,b, ncol = 1)
# grid.arrange( t, p, b)
# 

# WM_county_base + geom_point(
#   data = WM_MI_tested,
#   aes(
#     x = long,
#     y = lat,
#     color = Status,
#     shape = Year
#   ),
#   inherit.aes = FALSE,
#   size = 2 , color= "blue"
# )
# 
# + theme(
#   plot.title = element_text(
#     size = 14,
#     face = "bold",
#     hjust = 0.5,
#     family = "Verdana"
#   ),
# axis.title = element_text(size = 14, face = "bold", hjust = 0.5),
# axis.text = element_text(
#   face = "bold",
#   size = 14,
#   family = "Verdana"
# ),
# panel.background = element_rect(fill = "white", colour = "grey50")
# ) + labs(title = "Boscalid", x = "Isolates", y = "EC50 (ppm)")
# grid.arrange( t, p,b, ncol = 1)
# grid.arrange( t, p, b)
# 
# 
# 
# that have at least 10 sclerotia 
# NE_S_15 <- summary(WM1S$Field)[summary(WM1S$Field)>9]
# barplot(WM1S)
# barplot(sort(summary(WM1S$Field)))
# str(WM)
# 
# length(sort(summary(WM$Field)))-1
# 
# 
# barplot(sort(summary(WM$Field))[ 1:length(sort(summary(WM$Field)))-1])
# ####filterring by 2016 for ne-survey, and there are nothing
# WM2S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="NE",Year=="2016" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# NE_S_16 <- summary(WM2S$Field)[summary(WM2S$Field)>9]
# 
# ## Filterring by 2017 for ne-survey , correct!
# WM3S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="NE",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# NE_S_17 <- summary(WM3S$Field)[summary(WM3S$Field)>9]
# 
# ## Filterring by 2015 for MI-survey, correct!
# WM4S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="MI",Year=="2015" )
# 
# # that have at least 10 sclerotia, correct!
# 
# MI_S_15 <- summary(WM4S$Field)[summary(WM4S$Field)>9]
# ## Filterring by 2016 for MI-survey and there are nothing, correct!
# WM5S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="MI",Year=="2016" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# MI_S_16 <- summary(WM5S$Field)[summary(WM5S$Field)>9]
# 
# ## Filterring by 2017 for MI-survey and there are nothing, correct!
# WM6S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="MI",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# MI_S_17 <- summary(WM6S$Field)[summary(WM6S$Field)>9]
# 
# ## Filterring by 2015 for IA-survey, correct!
# WM7S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="IA",Year=="2015" )
# # that have at least 10 sclerotia and, correct!
# 
# 
# IA_S_15 <- summary(WM7S$Field)[summary(WM7S$Field)>9]
# ## Filterring by 2016 for IA-survey and there are nothing, correct!
# WM8S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="IA",Year=="2016" )
# # that have at least 10 sclerotia and there are nothing, correct!
# IA_S_16 <- summary(WM8S$Field)[summary(WM8S$Field)>9]
# 
# ## Filterring by 2017 for IA-survey and there are nothing, correct!
# WM9S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="IA",Year=="2017" )# that have at least 10 sclerotia and there are nothing, correct!
# 
# IA_S_17 <- summary(WM9S$Field)[summary(WM9S$Field)>9]
# 
# #### Filterring by 2015 for WI-survey, correct!
# WM10S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="WI",Year=="2015" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# WI_S_15 <- summary(WM10S$Field)[summary(WM10S$Field)>9]
# 
# 
# ##### Filterring by 2016 for WI-survey,  and there are nothing, correct!
# 
# WM11S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="WI",Year=="2016" )
# # that have at least 10 sclerotia, and there are nothing, correct!
# 
# WI_S_16 <- summary(WM11S$Field)[summary(WM11S$Field)>9]
# 
# ##### Filterring by 2017 for WI-survey and there are nothing, correct!
# 
# WM12S <- wm1_ %>% 
#   group_by(State, Field, Year, Origin) %>%   
#   filter(Origin=="Farmer fields", State=="WI",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# WI_S_17 <- summary(WM12S$Field)[summary(WM12S$Field)>9]
# 
# 
# #############NOW TRIALS#####
# ####filterring by 2015 for ne-survey
# WM1_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="NE",Year=="2015" )
# # that have at least 10 sclerotia 
# NE_T_15 <- summary(WM1_T$Field)[summary(WM1_T$Field)>9]
# 
# 
# sobarplot(sort(summary(WM$Field))[ 1:length(sort(summary(WM$Field)))-1])
# ####filterring by 2016 for ne-survey, and there are nothing
# WM2_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="NE",Year=="2016" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# NE_T_16 <- summary(WM2_T$Field)[summary(WM2_T$Field)>9]
# 
# ## Filterring by 2017 for ne-survey , correct!
# WM3_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="NE",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# NE_T_17 <- summary(WM3_T$Field)[summary(WM3_T$Field)>9]
# 
# ## Filterring by 2015 for MI-survey, correct!
# WM4_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="MI",Year=="2015" )
# # that have at least 10 sclerotia, correct!
# 
# MI_T_15 <- summary(WM4_T$Field)[summary(WM4_T$Field)>9]
# ## Filterring by 2016 for MI-survey and there are nothing, correct!
# WM5_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="MI",Year=="2016" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# MI_T_16 <- summary(WM5_T$Field)[summary(WM5_T$Field)>9]
# 
# ## Filterring by 2017 for MI-survey and there are nothing, correct!
# WM6_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="MI",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# MI_T_17 <- summary(WM6_T$Field)[summary(WM6_T$Field)>9]
# 
# ###
# ## Filterring by 2015 for IA-survey, correct!
# WM7_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="IA",Year=="2015" )
# # that have at least 10 sclerotia and, correct!
# 
# 
# IA_T_15 <- summary(WM7_T$Field)[summary(WM7_T$Field)>9]
# ## Filterring by 2016 for IA-survey and there are nothing, correct!
# WM8_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="IA",Year=="2016" )
# # that have at least 10 sclerotia and there are nothing, correct!
# IA_T_16 <- summary(WM8_T$Field)[summary(WM8_T$Field)>9]
# 
# ## Filterring by 2017 for IA-survey and there are nothing, correct!
# WM9_T <- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="IA",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# IA_T_17 <- summary(WM9_T$Field)[summary(WM9_T$Field)>9]
# 
# #### Filterring by 2015 for WI-survey, correct!
# WM10_T<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="WI",Year=="2015" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# WI_T_15 <- summary(WM10_T$Field)[summary(WM10_T$Field)>9]
# 
# 
# ##### Filterring by 2016 for WI-survey,  and there are nothing, correct!
# 
# WM11_T<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="WI",Year=="2016" )
# # that have at least 10 sclerotia, and there are nothing, correct!
# 
# WI_T_16 <- summary(WM11_T$Field)[summary(WM11_T$Field)>9]
# 
# ##### Filterring by 2017 for WI-survey and there are nothing, correct!
# 
# WM12_T<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey", State=="WI",Year=="2017" )
# # that have at least 10 sclerotia and there are nothing, correct!
# 
# WI_T_17 <- summary(WM12_T$Field)[summary(WM12_T$Field)>9]
# 
# 
# 
# ###########################
# 
# # WM7 <- WM %>% 
# #   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
# #   filter(Origin=="trial", State=="IA",Year=="2016" )
# # IA_16 <- summary(WM7$Field)[summary(WM7$Field)>9]
# # IA_16
# # 
# # ##
# # 
# # WM8 <- WM %>% 
# #   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
# #   filter(Origin=="trial", State=="IA",Year=="2017" )
# # IA_17 <- summary(WM8$Field)[summary(WM8$Field)>9]
# # IA_17
# # 
# # 
# # 
# # 
# # WM10<- WM %>% 
# #   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
# #   filter(Origin=="trial", State=="WI",Year=="2016" )
# # WI_16 <- summary(WM10$Field)[summary(WM10$Field)>9]
# # WI_16
# # 
# # #####when its trial first it was confirmed first Survey for eah one
# # 
# # WM11<- WM %>% 
# #   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
# #   filter(Origin=="trial", State=="WI",Year=="2017" )
# # WI_17 <- summary(WM11$Field)[summary(WM11$Field)>9]
# # WI_17
# 
# # just filterring by fields SURVEY that have at least 10 sclerotia BY YEAR 2015
# WM12<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey",Year=="2015" )
# year2015<- summary(WM12$Field)[summary(WM12$Field)>9]
# prod1 <- unique(WM12$Products_Soybean)[6]
# loc1<- grep(prod1, WM12$Products_Soybean) 
# WM12$Products_Soybean[loc1] <- unique(WM12$Products_Soybean)[1]
# 
# prod1 <- unique(WM13$Products_Soybean)[7]
# loc11<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[1]
# 
# prod1 <- unique(WM13$Products_Soybean)[9]
# loc11<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[4]
# 
# prod1 <- unique(WM13$Products_Soybean)[12]
# loc11<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[3]
# 
# prod1 <- unique(WM13$Products_Soybean)[9]
# loc11<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[4]
# 
# prod1 <- unique(WM13$Products_Soybean)[5]
# loc11<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[3]
# 
# prod1 <- unique(WM13$Products_Soybean)[10]
# loc11<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- "Approach"
# 
# 
# levels(WM13$Products_Soybean) <- unique(WM13$Products_Soybean)
# WM13$Products_Soybean <- as.factor(WM13$Products_Soybean)
# 
# summary
# 
# prodlist<-unique(WM13$Products_Soybean)
# loc2 <- grep(prodlist[1], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# priaxor <- summary(newdat$Field)[summary(newdat$Field)>9]
# ###  2 stratego, 4 = TM, 5 = Domark, 6=Quilt, 7=Pri+App,
# ### 8=TM --empty?, 9 = Quilt --empty?, 10 = approach -- empty??
# loc2 <- grep(prodlist[2], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# stratego <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[4], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# TM <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[5], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# Domark <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[6], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# Quilt <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[7], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# pri.app <- summary(newdat$Field)[summary(newdat$Field)>9]
# 
# priaxor
# stratego
# TM
# Domark
# Quilt
# pri.app
# 
# # priaxor Fluxapryroxad + Piraclos
# # stratego TRi +Propi
# # TM
# # Domark Tetraconaz
# # Quilt  AZ + Propi
# 
# 
# 
# summary(WM13$Products_Soybean)
# 
# 
# length(year2015)
# 
# 
# ############
# 
# WM13<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History, Products_Soybean) %>%   
#   filter(Origin=="Survey",Year=="2015" )
# year20155<- summary(WM13$Field)[summary(WM13$Field)>9]
# prod11 <- unique(WM13$Products_Soybean)[6]
# loc11 <- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[1]
# 
# 
# prod11 <- unique(WM13$Products_Soybean)[7]
# loc11<- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[1]
# 
# prod11 <- unique(WM13$Products_Soybean)[9]
# loc11<- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[4]
# 
# prod11 <- unique(WM13$Products_Soybean)[12]
# loc11<- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[3]
# 
# prod11 <- unique(WM13$Products_Soybean)[9]
# loc11<- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[4]
# 
# prod11 <- unique(WM13$Products_Soybean)[5]
# loc11<- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- unique(WM13$Products_Soybean)[3]
# 
# prod11 <- unique(WM13$Products_Soybean)[10]
# loc11<- grep(prod11, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc11] <- "Approah"
# 
# 
# levels(WM13$Products_Soybean) <- unique(WM13$Products_Soybean)
# WM13$Products_Soybean <- as.factor(WM13$Products_Soybean)
# 
# summary
# 
# prodlistt<-unique(WM13$Products_Soybean)
# loc22 <- grep(prodlistt[1], WM13$Products_Soybean)
# newdatt <- WM13[loc22,]
# priaxorrr <- summary(newdatt$Field)[summary(newdatt$Field)>9]
# ###  2 stratego, 4 = TM, 5 = Domark, 6=Quilt, 7=Pri+App,
# ### 8=TM --empty?, 9 = Quilt --empty?, 10 = approach -- empty??
# loc22 <- grep(prodlist[2], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# stratego <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[4], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# TM <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[5], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# Domark <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[6], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# Quilt <- summary(newdat$Field)[summary(newdat$Field)>9]
# loc2 <- grep(prodlist[7], WM13$Products_Soybean)
# newdat <- WM12[loc2,]
# pri.app <- summary(newdat$Field)[summary(newdat$Field)>9]
# 
# priaxor
# stratego
# TM
# Domark
# Quilt
# pri.app
# Headline
# 
# # priaxor Fluxapryroxad + Piraclos
# # stratego TRi +Propi
# # TM
# # Domark Tetraconaz
# # Quilt  AZ + Propi
# #Pyraclostrobin
# 
# 
# 
# summary(WM13$Products_Soybean)
# 
# 
# length(year2015)
# 
# ################################
# WM14<- WM %>% 
#   group_by(`Collection ID`, Field, `Form ID`, State, Products, History) %>%   
#   filter(Origin=="Survey",Year=="2016" )
# year2016<- summary(WM14$Field)[summary(WM14$Field)>9]
# prod1_1 <- unique(WM13$Products_Soybean)[6]
# loc11_1<- grep(prod1, WM13$Products_Soybean) 
# WM13$Products_Soybean[loc1] <- unique(WM13$Products_Soybean)[1]
# 
