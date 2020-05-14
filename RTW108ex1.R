pacman::p_load(dplyr,ggplot2,tidyr,plotly,stringr,googleVis)
load("RiTW108.rdata")
table(DemoY$year)

Y = DemoY %>% select(
  year, vid=村里代碼, city=縣市名稱, county=鄉鎮市區名稱, 
  vname=村里名稱, house=戶數, pop=人口數, fm.ratio=性比例, 
  sp.ratio=扶養比, elderly=老化指數) %>% 
  filter(! city %in% c("連江縣","金門縣","澎湖縣")) %>% 
  mutate(city = case_when(
    city == "臺北縣" ~ "新北市",
    city == "桃園縣" ~ "桃園市",
    city == "高雄縣" ~ "高雄市",
    city == "臺南縣" ~ "臺南市",
    city == "臺中縣" ~ "臺中市",
    TRUE ~ city ))

table(Y$city) %>% sort %>% tail(10) %>% 
  barplot(horiz=T,las=2)
BIG6 = c("桃園市","臺北市","臺中市","臺南市","高雄市","新北市")

Y %>% is.na %>% colSums
Y %>% group_by(year, city) %>% 
  summarise(Pop = sum(pop)) %>%
  ggplot(aes(x=year, y=Pop, col=factor(city)) ) + 
  geom_line(size=1) -> g; ggplotly(g)

scale1 = function(mp=0) scale_color_gradient2(
  midpoint=mp, low="firebrick2", mid="wheat2", 
  high="seagreen4")

df = Y %>% group_by(city, year) %>% summarise(
  性比例 = sum(fm.ratio*pop)/sum(pop),
  扶養比 = sum(sp.ratio*pop)/sum(pop),
  老化指數 = sum(elderly*pop)/sum(pop),
  人口 = sum(pop)
  ) %>% filter(year %in% seq(99,108,3)) 

df %>% #filter(city %in% BIG6) %>% 
  ggplot(aes(x=扶養比, y=老化指數, col=性比例, 
             size=人口, label=year)) +
  geom_point(alpha=1) + scale1(100) + theme_bw() +
  facet_wrap(~city) -> g; ggplotly(g)

df %>% #filter(city %in% BIG6) %>% 
  ggplot(aes(x=扶養比, y=老化指數, col=性比例, 
             size=人口, label=city)) +
  geom_point(alpha=1) + scale1(100) + theme_bw() +
  facet_wrap(~year) -> g; ggplotly(g)

###############################################################
names(DemoQ)

Q = DemoQ %>% transmute(
  year = str_remove(資料時間,"Y.*$") %>% as.integer,
  qtr = str_remove(資料時間,"^\\d+Y"),
  vid=村里代碼, city=縣市名稱, county=鄉鎮市區名稱, vname=村里名稱,
  born=出生數, death=死亡數, marriage=結婚對數, devorce=離婚對數, 
  time=資料時間
  ) %>% 
  filter(! city %in% c("連江縣","金門縣","澎湖縣")) %>% 
  mutate(city = case_when(
    city == "臺北縣" ~ "新北市",
    city == "桃園縣" ~ "桃園市",
    city == "高雄縣" ~ "高雄市",
    city == "臺南縣" ~ "臺南市",
    city == "臺中縣" ~ "臺中市",
    TRUE ~ city )) %>% 
  mutate_at(vars(born:devorce), ~replace_na(as.integer(.), 0))

group_by(Q, qtr) %>%
  summarise_at(vars(born:devorce), sum) %>%
  gather("event", "n", -1) %>%
  ggplot(aes(x=qtr, y=n)) + geom_bar(stat="identity") +
  facet_wrap(~event, nrow=1)

group_by(Q, time) %>% 
  summarise_at(vars(born:devorce), sum) %>% 
  gather("event", "n", -1) %>% 
  ggplot(aes(x=time, y=n, group=event, col=event)) + 
  geom_line() -> g ; ggplotly(g) 

group_by(Q, year) %>% 
  summarise_at(vars(born:devorce), sum) %>% 
  gather("event", "n", -1) %>% 
  ggplot(aes(x=year, y=n, col=event)) + 
  geom_line() + geom_point() -> g ; ggplotly(g) 

