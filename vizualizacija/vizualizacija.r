# 3. faza: Vizualizacija podatkov

source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")
library(RColorBrewer)


#Primerjava cene in zivljenski standard v nekoliko drzavah
countries <- filter(cene, Country %in% c("Bulgaria", "Romania", "Finland", "Spain", "United Kingdom", "Switzerland"))
nb.cols <- 6
mycolours <- colorRampPalette(brewer.pal(7, "Paired"))(nb.cols)

graf_cene <- ggplot(mapping = aes(x=Year, y=Value, colour=Country)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8), plot.title = element_text(size = 20)) +
  geom_line(linetype="solid", data=countries) +
  geom_line(linetype="dashed", data=gdp_per_capita, show.legend = TRUE) + 
  labs(x="Leto", y="Cen in BDP - indeksi cen (referenca EU28=100)", title="Sprememba cen in BDP prebivalca po letih 2008-2018 capita", colour="Države") +
  scale_x_discrete(limits=c(2008:2018))+
  scale_colour_manual(values = mycolours)


#Proizvodnja alkohola v EU
vrednosti <- sort(unique(production$`Type of alcohol`))
graf_proizvodnja <- ggplot(data=production, aes(x=Year, y=Production, fill=`Type of alcohol`)) +
  labs(x="Leto", y="Litrov alkohola", title = "Proizvodnja alkohola v EU po kategorijah") + 
  theme_minimal() +
  geom_bar(stat = "identity", position=position_dodge(), size=.3) +
  scale_x_discrete(limits=c(2008:2018))+
  scale_y_continuous(labels=comma_format(big.mark=" ", scale = 0.00001)) +
  scale_fill_manual(labels = c("Pivo", "Gin", "Rum", "Vodka", "Viski"), values= mycolours, breaks=vrednosti) +
  theme(axis.text.x = element_text(angle = 90, size = 8), legend.text = element_text(size=8), 
        legend.position = "bottom",  plot.title = element_text(size = 20)) +
  labs(fill="Tip alkohol") +
  coord_flip()


#Zemljevid

zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                               
                              "ne_50m_admin_0_countries",mapa="./zemljevidi") %>% fortify()
  
#Zemljevid: kolicina alkohola per capita
zemljevid_kolicina_alkohola <- ggplot() + 
  geom_polygon(data=left_join(zemljevid, kolicina_pc %>% group_by(Countries) %>% summarise(`Total alcohol`), by=c("SOVEREIGNT"="Countries")), 
               aes(x=long, y=lat, group=group, fill=`Total alcohol`), size=0.1) +
  labs(x="", y="", fill="Litrov alkohola na prebivalca", title = "Konzumacija alkohola") + 
  theme_map(base_size = 20) +
  theme(legend.position = "bottom")
  

#Zemljevid: kolicina spirits, pivo, vino per capita
s1 <- left_join(zemljevid, kolicina_pc %>% group_by(Countries) %>% summarise(`Spirits(%)`), by=c("SOVEREIGNT"="Countries"))
s1 <- mutate(s1, type = c("Spirits"))
s2 <- left_join(zemljevid, kolicina_pc %>% group_by(Countries) %>% summarise(`Wine(%)`), by=c("SOVEREIGNT"="Countries"))
s2 <- mutate(s2, type = c("Wine"))
s3 <- left_join(zemljevid, kolicina_pc %>% group_by(Countries) %>% summarise(`Beer(%)`), by=c("SOVEREIGNT"="Countries"))
s3 <- mutate(s3, type = c("Beer"))

zemljevid_kolicina <- ggplot() + 
  geom_polygon(data=s1, 
               aes(x=long, y=lat, group=group, fill=`Spirits(%)`), size=0.1) +
  geom_polygon(data=s2, 
               aes(x=long, y=lat, group=group, fill=`Wine(%)`), size=0.1) +
  geom_polygon(data=s3, 
               aes(x=long, y=lat, group=group, fill=`Beer(%)`), size=0.1) +
  facet_grid(type~., labeller = as_labeller(c("Beer"="Pivo", "Spirits" = "Zganje", "Wine"="Vino"))) +
  labs(x="", y="", fill="Litrov na prebivalca") + 
  ggtitle("Konzumacija različnih pijačah na prebivalca") +
  theme_map(base_size = 20)


#Zemljevid: kolicina alkohola na mladi ljudi 
alcohol_young_people <- kolicina %>% filter(Sex=="Total") %>% filter(Frequency=="Every month") %>% group_by(Country) %>% summarise(Total)

zemljevid_kolicina_alkohola_mlade <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Europe"), alcohol_young_people, by=c("SOVEREIGNT"="Country")),
               aes(x=long, y=lat, group=group, fill=Total), size=0.1) +
  labs(x="", y="", fill="Kolicina alkohola\n na osebo (15-24) let") +   
  ggtitle("Konzumacija alkohola v Evropi s strani mladih (15-24 let)") +
  coord_cartesian(xlim=c(-27, 50), ylim=c(25, 80), expand = TRUE)  +
  theme_map(base_size = 20)


