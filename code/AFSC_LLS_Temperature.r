library(tidyverse)
library(RODBC)

# Pull data from Access DB
channel <- odbcConnectAccess2007("data/LLsurvey_Temperature.accdb")
prof <- sqlQuery(channel,
                "SELECT *
               FROM [Temperature Depth Profiles];")

close(channel)
rm(channel)

##################
# First, generic region temperature trends for PT and Cruise Report
# With GOA combined contribution for the Sablefish ESP
# These use NPFMC sablefish management areas
##################

prof <- prof %>% mutate(stn.yr.z = paste0(Station_Number, ".", Year, ".", TDR_Depth)) # unique id for each stn/depth by set

pro1 <- prof %>% filter(Set == "One") # preference given to set 1 which is consistent through time series
pro2 <- prof %>% filter(Set == "Two") %>% 
  filter(!stn.yr.z %in% pro1$stn.yr.z) # when there is a depth in set 2 that is not in set 1

pro <- rbind(pro1, pro2) # combine the two for the most complete version

# Getting values for mean temp from 246 to 255 meters
AllYr <- pro %>% filter( TDR_Depth>245 & TDR_Depth<256 ) %>% 
  group_by(Year,NPFMC_Sablefish_Mgmt_Area, ESR_Region, Geographic_Area_Name, Station_Number, Set) %>%
  summarize(num = n(), temp=mean(Temperature), tempSD = sd(Temperature))

AllYr2 <- AllYr %>% filter(num==10) # Remove any that did not completely sample the 246 to 255 meter range to avoid any bias

area.wts <- read.csv(file="data/GeoWts.csv")
area.wts <- area.wts[,c(2:3)]
area.wts$Geographic_Area_Name <- area.wts$Geographic

AllYr2 <- transform(AllYr2, NPFMC_Sablefish_Mgmt_Area = factor(NPFMC_Sablefish_Mgmt_Area, levels=c("Bering Sea", "Aleutians", "Western Gulf of Alaska", "Central Gulf of Alaska", "Eastern Gulf of Alaska")))

AllYr3 = AllYr2 %>% 
  group_by(Year, NPFMC_Sablefish_Mgmt_Area, Geographic_Area_Name) %>% 
  summarize(annGeoT = mean(temp))

AllYrWt = merge(AllYr3, area.wts, by=c("Geographic_Area_Name"))

AllYrWt$WtTemp = AllYrWt$annGeoT * AllYrWt$Weight

AllYrWt2 = AllYrWt %>% 
  group_by(Year, NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(WtTemp = sum(WtTemp), TotWt=sum(Weight))

AllYrWt2$Temp = AllYrWt2$WtTemp / AllYrWt2$TotWt

AllYrWt2 = transform(AllYrWt2, NPFMC_Sablefish_Mgmt_Area = factor(NPFMC_Sablefish_Mgmt_Area, levels=c("Bering Sea", "Aleutians", "Western Gulf of Alaska", "Central Gulf of Alaska", "Eastern Gulf of Alaska")))

WtMean <- AllYrWt2 %>% 
  group_by(NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(Mean = mean(Temp))

goa.esp = AllYrWt2 %>% filter( !NPFMC_Sablefish_Mgmt_Area=="Bering Sea" & !NPFMC_Sablefish_Mgmt_Area=="Aleutians")

ggplot(AllYr2, aes(Year, temp))+ #, col=Geographic_Area_Name
  geom_hline(data = WtMean, aes(yintercept = Mean), col = "darkolivegreen", lty = 2) +
  geom_point(size = 0.5, col = "gray20") +
  geom_line(data = goa.esp, aes(x = Year, y = Temp), col = "blue") +
  geom_point(data = AllYrWt2, aes(x = Year, y = Temp), col = "red") +
  ylab(expression(paste('Temperature (',~degree,'C)',sep=''))) +
  facet_grid(~NPFMC_Sablefish_Mgmt_Area) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave("output/PT_cruise_Report_Fig.png", width=18, height=4.5)

esp.mean <- goa.esp %>% 
  group_by(NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(Area_Mean = mean(Temp))

esp.anom = left_join(goa.esp, esp.mean, by="NPFMC_Sablefish_Mgmt_Area")
esp.anom$anom = esp.anom$Temp - esp.anom$Area_Mean

plot = esp.anom %>%
  group_by(Year) %>%
  summarize(AnAnom = mean(anom), AnSD = sd(anom), AnN = n())

plot$ci95 = 1.96 * plot$AnSD/sqrt(plot$AnN)

ggplot(plot) +
  geom_line(aes(x=Year,y=AnAnom),size=1.5,colour="blue") +
  geom_point(aes(x=Year,y=AnAnom),size=5,colour="red")+
  geom_ribbon(aes(x=Year, ymin=AnAnom-ci95, ymax=AnAnom+ci95), fill="blue", alpha=0.2) +
  geom_hline(yintercept=0,colour="dark green",linetype=5,size=1.3,alpha=0.5)+
  theme_bw(base_size=18)

plot$Ind  = "Summer_Temperature_250m_GOA_Survey"
dat = plot[ , c(1,6,2) ]
names(dat) = c("YEAR", "INDICATOR_NAME", "DATA_VALUE")
write.csv(dat, file="output/Summer_Temperature_250m_GOA_Survey.csv" , row.names=FALSE)

##################
# Now, ESR contributions, which use ESR regions which are slightly different
##################
AllYr3_esr = AllYr2 %>% 
  group_by(Year, ESR_Region, Geographic_Area_Name) %>% 
  summarize(annGeoT = mean(temp), annGeoSD = sd(temp)) %>% 
  mutate(annGeoSD = ifelse(is.na(annGeoSD), 0.5, annGeoSD))

AllYrWt_esr = merge(AllYr3_esr, area.wts, by=c("Geographic_Area_Name"))

AllYrWt_esr$WtTemp = AllYrWt_esr$annGeoT * AllYrWt_esr$Weight

AllYrWt_esr$WtSD = AllYrWt_esr$annGeoSD * AllYrWt_esr$Weight

AllYrWt2_esr = AllYrWt_esr %>% 
  group_by(Year, ESR_Region) %>% 
  summarize(WtTemp = sum(WtTemp), WtSD = sum(WtSD), TotWt=sum(Weight))

AllYrWt2_esr$Temp = AllYrWt2_esr$WtTemp / AllYrWt2_esr$TotWt
AllYrWt2_esr$SD = AllYrWt2_esr$WtSD / AllYrWt2_esr$TotWt

##################
# Aleutian Islands ESR contribution
##################
AI = pro %>% 
  filter(Day_of_Year < 186, Station_Number > 22, Station_Number < 76)
AI = transform(AI, ESR_Region = factor(ESR_Region, levels=c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Western Gulf of Alaska")))

AI$Chain = ifelse(AI$Station_Number<53 | AI$Station_Number==54, "North","South")

AIplot = AI %>% group_by(ESR_Region, Chain, Station_Number) %>% 
  summarize(Latitude = mean(Latitude), Longitude = mean(Longitude))

ggplot(AIplot, aes(Longitude, Latitude, col=ESR_Region, shape=Chain, label=Station_Number)) +
  geom_point(size=2) +
  geom_text(nudge_x = .2, nudge_y=.2) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(),
        panel.grid.major = element_blank(), strip.text=element_text(size =12)) 

# North V. South
ns_comp = AI %>% filter(TDR_Depth==200, ESR_Region=="Central Aleutians" | ESR_Region=="Western Aleutians" | ESR_Region=="Eastern Aleutians") %>% 
  group_by(Year, ESR_Region, Chain) %>% 
  summarize(T200=mean(Temperature))

ns_comp$reg.yr = paste0(ns_comp$ESR_Region,'.',ns_comp$Year)
# only years with both north and south

north = ns_comp %>% filter(Chain=="North")
north$reg.yr = paste0(north$ESR_Region, '.', north$Year)
ns_comp = ns_comp %>% filter(reg.yr %in% north$reg.yr)

ggplot() +
  geom_point(data=ns_comp, aes(x=Year, y=T200, col=Chain), size=2) +
  facet_wrap(~ESR_Region) +
  ylab(expression(paste('Temperature (',~degree,'C)',sep=''))) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(),
        panel.grid.major = element_blank(), strip.text=element_text(size =12)) 

ggsave(file="output/AI/AI_N_S_COMP.png", height=6, width=12, dpi = 600)

# Heat Map for South of Chain to WGOA
nmfs.col <- c("#00467F", "#0093D0", "#ACDDF1", "#E8E8E8", "#FF4438",
              "#D02C2F", "#B2292E") # From Ivonne's Fisheries Brand Colors list

# library(rcolors)
ggplot(AI %>% filter(TDR_Depth > 100, TDR_Depth < 301, Chain == "South"), 
       aes(Longitude, TDR_Depth, color=Temperature)) +
  geom_point(size=1) +
  scale_color_gradientn(colors=nmfs.col) + # colors=viridis(10)
  scale_y_reverse() +
  ylab("Depth (m)") +
  labs(color = expression("Temperature ("*degree*"C)")) +
  facet_wrap(~Year, ncol = 4) +
  geom_vline(aes(xintercept=-170), lty=2, size=1) +
  geom_vline(aes(xintercept=-164), lty=2, size=1) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), 
        panel.grid.minor = element_blank(), strip.text=element_text(size =12),
        panel.spacing.x = unit(1.5, "lines")) 

ggsave(file = "output/AI/AI_Heat_Map.png", width=14, height = 10, dpi=600)

############################
# GOA ESR contribution
############################
WGOA = AllYrWt2_esr %>% filter(ESR_Region=="Western Gulf of Alaska")
write.csv(WGOA[ ,c(1,2,6,7) ], file="output/GOA/wgoa_LL_temp_index.csv")

ggplot(WGOA, aes(Year, Temp)) +
  geom_ribbon(aes(x=Year,ymin=Temp-(SD), ymax=Temp+(SD)),fill="grey",alpha=0.5) +
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_hline(aes(yintercept=mean(Temp)), size=1, lty=2) + 
  ylab(expression(paste('Temperature (',~degree,'C)',sep=''))) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(),
        panel.grid.major = element_blank(), strip.text=element_text(size =12)) 

ggsave("output/GOA/WGOA_TEMP_INDEX.png", height = 5, width = 7.5, dpi = 600)

EGOA = AllYrWt2_esr %>% filter(ESR_Region=="Eastern Gulf of Alaska")
write.csv(EGOA[ ,c(1,2,6,7) ], file="output/GOA/egoa_LL_temp_index.csv")

ggplot(EGOA, aes(Year, Temp)) +
  geom_ribbon(aes(x=Year,ymin=Temp-(SD), ymax=Temp+(SD)),fill="grey",alpha=0.5) +
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_hline(aes(yintercept=mean(Temp)), size=1, lty=2) + 
  ylab(expression(paste('Temperature (',~degree,'C)',sep=''))) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(),
        panel.grid.major = element_blank(), strip.text=element_text(size =12)) 

ggsave("output/GOA/EGOA_TEMP_INDEX.png", height = 5, width = 7.5, dpi = 600)

############################
# Eastern Bering Sea ESR contribution
############################
EBS = AllYrWt2_esr %>% 
  filter(ESR_Region == "Southeastern Bering Sea")

write.csv(EBS[ ,c(1,2,6,7) ], file="output/EBS/ebs_LL_temp_index.csv")

ggplot(EBS, aes(Year, Temp)) +
  geom_ribbon(aes(x=Year,ymin=Temp-(SD), ymax=Temp+(SD)),fill="grey",alpha=0.5) +
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_hline(aes(yintercept=mean(Temp)), size=1, lty=2) + 
  ylab(expression(paste('Temperature (',~degree,'C)',sep=''))) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(),
        panel.grid.major = element_blank(), strip.text=element_text(size =12)) 

ggsave("output/EBS/EBS_TEMP_INDEX.png", height = 5, width = 7.5, dpi = 600)
