pacman::p_load(tidyverse,httr,ggmap,jsonlite,tidyr,dplyr,kableExtra,ggplot2,MASS,plotly,ggmap,car,psych,leaflet,htmltools,tidytext,wordcloud,wordcloud2,reshape2,kableExtra)
#Data subsetting

#read data, json file is way too big! 
yelp_business <- stream_in(file("business.json"),verbose= F)
yelp_business <- flatten(yelp_business)
yelp_business <- as_tibble(yelp_business)

#select variables that are need in here, and only intrested in Las Vegas 
yelp_business2 <- yelp_business %>%
  select(c(business_id,name,address,city,state,postal_code,latitude,longitude)) %>% 
  filter(city %in% "Las Vegas")

#only interested in numbers top 50 stores in Las Vegas
table1 <- yelp_business2 %>% 
  group_by(name) %>%
  summarise(no_rows = length(name)) %>% 
  arrange(desc(no_rows)) %>% 
  slice(1:50) 

list1 <-as.vector(table1$name)

yelp_businessfinal <- yelp_business2 %>%
  filter(name %in% list1)

#write this csv out avoid big data read in
write.csv(yelp_businessfinal,"~/Desktop/MA615/Final Project/yelp_businessfinal.csv")
