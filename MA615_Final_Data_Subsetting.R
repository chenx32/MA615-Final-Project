pacman::p_load(tidyverse,httr,ggmap,jsonlite,tidyr,dplyr,kableExtra,ggplot2,MASS,plotly,ggmap,car,psych,leaflet,htmltools,tidytext,wordcloud,wordcloud2,reshape2,kableExtra)
#Data subsetting, Part of this is done in SCC, because the json file is too big to read in my own laptop.

#read data, json file is way too big! 
yelp_review  <- stream_in(file("/project/mssphw1/yelpmssp/review.json"))
yelp_review  <- flatten(yelp_review)
yelp_review  <- as_tibble(yelp_review)

yelp_business <- stream_in(file("business.json"))
yelp_business <- flatten(yelp_business)
yelp_business <- as_tibble(yelp_business)

#select variables that are need in here, and only intrested in Las Vegas 
yelp_business2 <- yelp_business %>%
  dplyr::select(c(business_id,name,address,city,state,postal_code,latitude,longitude,categories)) %>% 
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

write.csv(yelp_businessfinal,"~/Desktop/MA615/Final Project/yelp_businessfinal.csv")

review <-merge(x=yelp_businessfinal,y=yelp_review,by="business_id",all.x=TRUE)

review <- review %>%
  dplyr::select(business_id, name, city, text, categories)

write.csv(review,"/usr4/mssphw1/chenx32/Desktop/review.csv")

