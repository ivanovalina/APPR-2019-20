# 4. faza: Analiza podatkov

source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")

nb.cols <- 6
mycolours <- colorRampPalette(brewer.pal(6, "Paired"))(nb.cols)

#Analiza po spol, izbrane drzave

podatki <- kolicina %>% 
  filter(Frequency == "Every week", Country %in% c("Romania", "Cyprus", "Ireland", "Norway"), Sex %in% c("Females", "Males")) %>%
  select(Country, Sex, `From 25 to 64 years`) 

graf_spol <- ggplot(data=podatki, aes(x=Country, y=`From 25 to 64 years`)) + 
  geom_col(aes(fill=Sex), position = "dodge") +
  scale_fill_manual(values=c("steelblue3", "orangered2"), labels = c("Ženske", "Moški")) +
  theme_minimal() +
  labs(x="Država", y="Kolicina alkohola v procentih",
       title="Analiza kolicina alkohola po spolu v izbranih državah za ljudi 25-64 let",
       fill = "Spol") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, size = 8),  plot.title = element_text(size = 20))

#Analiza za Slovenija
podatki_slovenija <- kolicina %>% filter(Country == "Slovenia") %>%
  select(-Total, -`From 15 to 24 years`) %>% gather("Years", "From 25 to 64 years", 4:5)
names(podatki_slovenija)[5] <- "Value"
oznake <- c("Females" = "Ženske", "Males" = "Moški", "Total" = "Skupaj", "From 25 to 64 years" = "25-64 let", "65 years or over" = "65+")

graf_slovenija <- ggplot(data=podatki_slovenija, mapping=aes(x=Years, y = get("Value") ,group=Frequency, fill=Frequency)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("steelblue4", "steelblue3", "steelblue1", "skyblue", "lightblue1"),
                    labels = c("Vsak dan", "Vsak mesec", "Vsak teden", "Manj kot enkrat na mesec", "Nikoli ali ne v zadnjih 12 mesecih")) +
  labs(x="Let", y="Kolicina alkohola v procentih", color="Frequency", title = "Konzumacija alkohola v Sloveniji") +
  facet_grid(.~Sex, labeller = as_labeller(oznake)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, size = 8),  plot.title = element_text(size = 20))


#Regression

#Data frames
t_bulgaria1 <- table_production %>% filter(Country == "Bulgaria", Year %in% c(2008:2013)) %>% select(Country, Year, Beer)
t_bulgaria2 <- table_production %>% filter(Country == "Bulgaria", Year %in% c(2008:2013)) %>% select(Country, Year, Whisky)
t_finland1 <- table_production %>% filter(Country == "Finland", Year %in% c(2008:2013)) %>% select(Country, Year, Beer)
t_finland2 <- table_production %>% filter(Country == "Finland", Year %in% c(2008:2013)) %>% select(Country, Year, Whisky)
t_lithuania1 <- table_production %>% filter(Country == "Lithuania", Year %in% c(2008:2013)) %>% select(Country, Year, Beer)
t_lithuania2 <- table_production %>% filter(Country == "Lithuania", Year %in% c(2008:2013)) %>% select(Country, Year, Whisky)

t_all1 <- bind_rows(t_bulgaria1, t_finland1, t_lithuania1)
t_all2 <- bind_rows(t_bulgaria2, t_finland2, t_lithuania2)

test_future1 <- table_production %>% 
  filter(Country %in% c("Bulgaria", "Finland", "Lithuania"), Year %in% c(2014:2018)) %>% 
  select(Country, Year, Beer)
test_future2 <- table_production %>% 
  filter(Country %in% c("Bulgaria", "Finland", "Lithuania"), Year %in% c(2014:2018)) %>% 
  select(Country, Year, Whisky)

#Models
p_bulgaria1 <- lm(data= t_bulgaria1, Beer ~ Year)
p_bulgaria2 <- lm(data= t_bulgaria2, Whisky ~ Year)
p_finland1 <- lm(data = t_finland1, Beer ~ Year)
p_finland2 <- lm(data = t_finland2, Whisky ~ Year)
p_lithuania1 <- lm(data = t_lithuania1, Beer ~ Year)
p_lithuania2 <- lm(data = t_lithuania2, Whisky ~ Year)

#Prediction
future <- data.frame(Year=c(2014:2020))
whole_interval <- data.frame(Year=c(2007:2020))

napoved_bulgaria1 <- mutate(whole_interval, Beer=predict(p_bulgaria1, whole_interval), Country="Bulgaria")
napoved_bulgaria2 <- mutate(whole_interval, Whisky=predict(p_bulgaria2, whole_interval), Country="Bulgaria")
napoved_finland1 <- mutate(whole_interval, Beer=predict(p_finland1, whole_interval), Country="Finland")
napoved_finland2 <- mutate(whole_interval, Whisky=predict(p_finland2, whole_interval), Country="Finland")
napoved_lithuania1 <- mutate(whole_interval, Beer=predict(p_lithuania1, whole_interval), Country="Lithuania")
napoved_lithuania2 <- mutate(whole_interval, Whisky=predict(p_lithuania2, whole_interval), Country="Lithuania")

napoved1 <- bind_rows(napoved_bulgaria1, napoved_finland1, napoved_lithuania1)
napoved2 <- bind_rows(napoved_bulgaria2, napoved_finland2, napoved_lithuania2)

#Plot
graf_regresija_pivo <- ggplot() + 
  geom_point(data = t_all1, mapping = aes(x=Year, y=Beer), color = "black") +
  geom_point(data = test_future1, mapping = aes(x=Year, y=Beer), color = "orangered2") +
  geom_smooth(data = napoved1 , mapping = aes(x=Year, y=Beer), method=lm, color="steelblue3") +
  labs(x="Leto", y="Litrov proizvedena piva") +
  ggtitle("Napoved provizvodnja piva") +
  facet_wrap(.~Country, nrow=3, scales="free") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

graf_regresija_viski <- ggplot() + 
  geom_point(data = t_all2, mapping = aes(x=Year, y=Whisky), color = "black") +
  geom_point(data = test_future2, mapping = aes(x=Year, y=Whisky), color = "orangered2") +
  geom_smooth(data = napoved2 , mapping = aes(x=Year, y=Whisky), method=lm, color="steelblue3") +
  labs(x="Leto", y="Litrov proizveden viski") +
  ggtitle("Napoved provizvodnja viskija") +
  facet_wrap(.~Country, nrow=3, scales="free") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))



