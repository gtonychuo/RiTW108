pacman::p_load(readr, dplyr, sf, tmap, stringr)

###############################################################
load("GeoTW108.rdata")
plot(twCounty[,"M_F_RAT"])

county = st_simplify(twCounty,dTolerance=100)
county[,"M_F_RAT"] %>%  plot
county[,c(8:10,14)] %>%  plot

tm_shape(county) + tm_fill(col="M_F_RAT") + tm_borders()

islands = c("連江縣","金門縣","澎湖縣")
subset(county, ! COUNTY %in% islands) %>% tm_shape + 
  tm_fill(col="FLD4") + tm_borders(col="gray") +
  tm_text(text="COUNTY",size=0.65)

###############################################################
# Read in the 高雄捷運輕軌 data
L = st_read("kaoh/light_rail/LRT_1090102.shp", crs=3826)
L = subset(L,LRTSYS=="高雄捷運")
st_crs(L)
L = L %>% mutate_if(is.factor,as.character)

# Read in the house price data 房產時價登錄資料
load("kaoh/house_price.rdata")
A <- st_as_sf(D, coords=c("E","N"), crs=4326) %>% 
  st_transform(crs=3826)

# calculate distance
dx = st_distance(A, L)
dim(dx)                # 49898 2  
A$dx = apply(dx,1,min)
range(A$dx) %>% round  # 0 87104

# PLOT
Houses = A %>% filter(dx < 2000) %>% sample_n(1000)
Town = twTown[st_intersects(st_union(L), twTown)[[1]],]

subset(twVill, TOWN_ID %in% Town$TOWN_ID) %>% 
  tm_shape(name="人口密度(里)") +
  tm_fill(col="P_DEN",title="人口密度",alpha=0.65,palette="Greens") +
  tm_borders(col='lightgray') +
  tm_shape(Houses,name="時價登錄") + 
  tm_dots(col='type',title='房屋種類',size=0.06,shape=16) +
  tm_shape(L,name="高捷輕軌") + 
  tm_lines(col='LRTID',title.col="輕軌路線",lwd=3,
           palette=c("blue","magenta")) +
  tm_shape(Town,name="區界") + 
  tm_borders(col="firebrick") +
  tm_text(text="TOWN",col="firebrick")

save_tmap(filename="kaohLR.html")

