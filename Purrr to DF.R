library(tidyverse)
library (repurrrsive)

#Create sw.people which is a named list with each character's name as the list's [[id]]
sw.people <- sw_people %>% set_names(map_chr(sw_people, "name"))

starship_name <- map(sw_starships, "name") %>% set_names(map(sw_starships,"url"))
starship_class <- map(sw_starships, "starship_class") %>% set_names(map(sw_starships, "url"))

#join sw.people$url with sw_starship information
shipname <- map(sw.people, ~starship_name[.x$starships])
shipclass <- map(sw.people, ~starship_class[.x$starships])

#Create the count of starthips, built a data.frame and order decreasing
sw_char <- map_int(sw.people, ~length(.x[["starships"]]))
sw_char <- data.frame (sw_char)
sw_char$names <- row.names (sw_char)
row.names (sw_char) <- 1:87
colnames(sw_char) <- c("Starships","People")
sw_char <- sw_char[with(sw_char, order(Starships, People, decreasing = TRUE)),]

#Take the top5 pilots
top5 <- head(sw_char,5)
top5 <- top5[order (top5$People),]

#Plot with ascending Y axis
ggplot(top5, aes(Starships, People, colour = Starships)) +
  geom_point(size = 5, shape = 10) +
  labs(caption = "Top Pilot: Obi-Wan Kenobi
                             Jedi starfighter
                             Trade Federation cruiser
                             Naboo star skiff
                             Jedi Interceptor
                             Belbullab-22 starfighter") +
  scale_y_discrete(limits = rev(levels(factor(top5$People)))) + 
  xlim(0,5)
