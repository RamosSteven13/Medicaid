#http://ldh.la.gov/assets/medicaid/MedicaidEnrollmentReports/MedicaidbyParish/Medicaid_by_Parish3.2018.pdf
medicaid <- read.table("D:/Medicaid/LA.txt", header=T)
library(ggplot2)
library(ggmap)
library(dplyr)
library(stringr)
library(maps)
library(mapdata)
#Dont give up
usa <- map_data("usa")
states <- map_data("state")
la_df <- subset(states, region == "louisiana")
counties <- map_data("county")
la_county <- subset(counties, region == "louisiana")
unique(la_county$subregion)
medicaid$County <- tolower(medicaid$County)
#Changing all those annoying names
medicaid <- medicaid %>% 
  mutate(County=str_replace(County,"desoto", "de soto")) %>% 
  mutate(County=str_replace(County,"eastfeliciana", "east feliciana")) %>%
  mutate(County=str_replace(County,"east_baton_rouge", "east baton rouge")) %>%
  mutate(County=str_replace(County,"east_carroll", "east carroll")) %>%
  mutate(County=str_replace(County,"jeff_davis", "jefferson davis")) %>%
  mutate(County=str_replace(County,"lasalle", "la salle")) %>%
  mutate(County=str_replace(County,"pointe_coupee" , "pointe coupee")) %>%
  mutate(County=str_replace(County,"red_river" , "red river")) %>%
  mutate(County=str_replace(County,"st_bernard" , "st bernard")) %>%
  mutate(County=str_replace(County,"st_charles"  , "st charles")) %>%
  mutate(County=str_replace(County,"st_helena"  , "st helena")) %>%
  mutate(County=str_replace(County,"st_james"  , "st james")) %>%
  mutate(County=str_replace(County,"st_john"  , "st john the baptist")) %>%
  mutate(County=str_replace(County,"st_landry"  , "st landry")) %>%
  mutate(County=str_replace(County,"st_martin"  , "st martin")) %>%
  mutate(County=str_replace(County,"st_mary"  , "st mary")) %>%
  mutate(County=str_replace(County,"st_tammany"  , "st tammany")) %>%
  mutate(County=str_replace(County,"west_feliciana"  , "west feliciana")) %>%
  mutate(County=str_replace(County,"west_baton_rouge"  , "west baton rouge")) %>%
  mutate(County=str_replace(County,"west_carroll"  , "west carroll"))
  

#The mean inner join
colnames(medicaid)[1] <- "subregion"
La <- dplyr::inner_join(la_county, medicaid, by = "subregion")




#To do; Fix County names in 'medicaid', change all data types to numeric that \
#should be numeric
#just LA base

la_base <-ggplot(data = la_df,
         mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
la_base

ditch_the_axes <- theme(
  legend.key = element_rect(fill = "purple"),
  legend.background = element_rect(fill="purple"),
  panel.background = element_rect(fill = "purple"),
  plot.background = element_rect(fill = "purple"),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title =  element_text(size=40)
)
la_base +
  geom_polygon(data = la_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)+
  ditch_the_axes
#need to change vars into correct types

La$Blind <- as.numeric(La$Blind)
La$Children <- as.numeric(gsub(",", "", as.character(La$Children)))
La$Parents <- as.numeric(gsub(",", "", as.character(La$Parents)))

#blind LA's
Blind <- la_base +
  geom_polygon(data = La, aes(fill =La$Blind ), color = "white") +
  geom_polygon(color = "white", fill = NA) +
  scale_fill_gradient( name = "Number of Blind") +
  ditch_the_axes+
  labs(title="Louisiana by Parish", caption="http://ldh.la.gov/assets/medicaid/MedicaidEnrollmentReports/MedicaidbyParish/Medicaid_by_Parish3.2018.pdf")
Blind


sapply(La, class)
