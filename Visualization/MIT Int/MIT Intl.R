# Read data
intl = read.csv("intl.csv")
str(intl)
# Load ggplot2
library(ggplot2)
# Bar plot
ggplot(intl, aes(x= Region, y= PercentOfIntl)) + geom_bar(stat = "identity")
ggplot(intl, aes(x= Region, y= PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label=PercentOfIntl))
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
ggplot(intl, aes(x= Region, y= PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label=PercentOfIntl))
intl$PercentOfIntl = intl$PercentOfIntl * 100
ggplot(intl, aes(x= Region, y= PercentOfIntl)) + geom_bar(stat = "identity", fill = "blue") + geom_text(aes(label=PercentOfIntl), vjust = 0.4) + ylab("Percentage of International Student") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
# World map
# Load ggmap
library(ggmap)
intlall = read.csv("intlall.csv")
head(intlall)
# Replace NA to 0
intlall[is.na(intlall)] = 0
head(intlall)
# Load the world map
world_map = map_data("world")
str(world_map)
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")
table(intlall$Citizenship)
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"
world_map = merge(map_data("world"), intlall, by.x ="region",by.y = "Citizenship")
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") + coord_map("ortho", orientation=c(20, 30, 0))