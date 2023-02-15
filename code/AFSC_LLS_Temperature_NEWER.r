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
AllStn <- pro %>% filter( TDR_Depth > 245 & TDR_Depth < 256 ) %>% 
  group_by(Year, NPFMC_Sablefish_Mgmt_Area, ESR_Region, Geographic_Area_Name, Station_Number, Set) %>%
  summarize(num = n(), T_i = mean(Temperature)) %>% 
  filter(num == 10) %>%  # Remove any that did not completely sample the 246 to 255 meter range to avoid any bias
  mutate(NPFMC_Sablefish_Mgmt_Area = factor(NPFMC_Sablefish_Mgmt_Area, levels = c("Bering Sea", "Aleutians", "Western Gulf of Alaska", "Central Gulf of Alaska", "Eastern Gulf of Alaska")))

AllGeo <- AllStn %>%
  group_by(Year, NPFMC_Sablefish_Mgmt_Area, Geographic_Area_Name) %>% 
  summarize(T_g = mean(T_i), V_g = var(T_i))
  
area.wts <- read.csv(file="data/GeoWts.csv") %>% 
  mutate(Geographic_Area_Name = Geographic, W_g = Weight) %>% 
  select(Geographic_Area_Name, W_g)

AllGeo2 <- left_join(AllGeo, area.wts, by=c("Geographic_Area_Name")) %>% 
  mutate(T_g_W_g = T_g * W_g) 

AllReg <- AllGeo2 %>%  
  group_by(Year, NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(T_r_numer = sum(T_g_W_g), V1 = sum(W_g),
            V2 = sum(W_g ^ 2))  %>% 
  mutate(T_r = T_r_numer / V1)

AllGeo3 <- left_join(AllGeo2, AllReg, by = c("Year", "NPFMC_Sablefish_Mgmt_Area")) %>% 
  mutate(To_sum = W_g * (T_g - T_r) ^ 2)  

AllReg2 <- AllGeo3 %>%  
  group_by(Year, NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(T_r = mean(T_r), V1 = mean(V1), V2 = mean(V2),
            V_sum = sum(To_sum), V_g = mean(V_g)) %>% 
  mutate(V_r = ifelse(V_sum == 0, V_g, 
                      V1 / (V1^2 - V2) * V_sum),
         sd_r = sqrt(V_r)) # Because only 1 geo in WGOA region, uses the variance treats each station equally

WtMean <- AllReg2 %>% 
  group_by(NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(Mean = mean(T_r))

goa.esp <- AllReg2 %>% 
  filter(!NPFMC_Sablefish_Mgmt_Area == "Bering Sea" & !NPFMC_Sablefish_Mgmt_Area == "Aleutians")

ggplot() +
  geom_hline(data = WtMean, aes(yintercept = Mean), col = "darkolivegreen", lty = 2) +
  geom_point(data = AllStn, aes(x = Year, y = T_i), size = 0.5, col = "gray80") +
  geom_ribbon(data = AllReg2, aes(x = Year, ymin = (T_r - sd_r), ymax = (T_r + sd_r)), fill = "blue", alpha = 0.2) +
  geom_line(data = goa.esp, aes(x = Year, y = T_r), col = "blue") +
  geom_point(data = AllReg2, aes(x = Year, y = T_r), col = "red") +
  ylab(expression(paste('Temperature (',~degree,'C)',sep =''))) +
  facet_grid(~NPFMC_Sablefish_Mgmt_Area) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        strip.background = element_blank(), panel.grid.minor = element_blank()) 

ggsave("output/PT_cruise_Report_Fig.png", width = 18, height = 4.5, dpi = 600)

esp.mean <- goa.esp %>% 
  group_by(NPFMC_Sablefish_Mgmt_Area) %>% 
  summarize(Area_Mean = mean(T_r))

esp.anom <- left_join(goa.esp, esp.mean, by="NPFMC_Sablefish_Mgmt_Area") %>% 
  mutate(anom = T_r - Area_Mean) %>% 
  group_by(Year) %>%
  summarize(AnAnom = mean(anom), AnSD = sd(anom), AnN = n()) %>% 
  mutate(YEAR = Year, INDICATOR_NAME = "Summer_Temperature_250m_GOA_Survey",
         DATA_VALUE = AnAnom) %>% 
  select(YEAR, INDICATOR_NAME, DATA_VALUE) %>% 
  write.csv(file="output/Summer_Temperature_250m_GOA_Survey.csv" , row.names=FALSE)
  
##################
# Now, ESR contributions, which use ESR regions which are slightly different
##################

##################
# Aleutian Islands ESR contribution, specifically requested by Ivonne Ortiz
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
# GOA ESR contribution, more similar to above, just using ESR region instead of NPFMC
############################
AllGeo_esr <- AllStn %>%
  group_by(Year, ESR_Region, Geographic_Area_Name) %>% 
  summarize(T_g = mean(T_i), V_g = var(T_i))

AllGeo2_esr <- left_join(AllGeo_esr, area.wts, by=c("Geographic_Area_Name")) %>% 
  mutate(T_g_W_g = T_g * W_g) 

AllReg_esr <- AllGeo2_esr %>%  
  group_by(Year, ESR_Region) %>% 
  summarize(T_r_numer = sum(T_g_W_g), V1 = sum(W_g),
            V2 = sum(W_g ^ 2))  %>% 
  mutate(T_r = T_r_numer / V1)

AllGeo3_esr <- left_join(AllGeo2_esr, AllReg_esr, by = c("Year", "ESR_Region")) %>% 
  mutate(To_sum = W_g * (T_g - T_r) ^ 2)  

AllReg2_esr <- AllGeo3_esr %>%  
  group_by(Year, ESR_Region) %>% 
  summarize(T_r = mean(T_r), V1 = mean(V1), V2 = mean(V2),
            V_sum = sum(To_sum), V_g = mean(V_g)) %>% 
  mutate(V_r = ifelse(V_sum == 0, V_g, 
                      V1 / (V1^2 - V2) * V_sum),
         sd_r = sqrt(V_r)) # Because only 1 geo in WGOA region, uses the variance treats each station equally

WtMean_esr <- AllReg2_esr %>% 
  group_by(ESR_Region) %>% 
  summarize(Mean = mean(T_r))
           
WGOA = AllReg2_esr %>% 
  filter(ESR_Region == "Western Gulf of Alaska") %>% 
  mutate(Temperature = T_r, SD = sd_r) %>% 
  select(Year, ESR_Region, Temperature, SD)

write.csv(WGOA, file="output/GOA/wgoa_LL_temp_index.csv")

ggplot(WGOA, aes(Year, Temperature)) +
  geom_ribbon(aes(x = Year, ymin = Temperature - SD, ymax = Temperature + SD), fill = "grey", alpha = 0.5) +
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  geom_hline(aes(yintercept = mean(Temperature)), size = 1, lty = 2) + 
  ylab(expression(paste('Temperature (', ~degree, 'C)', sep = ''))) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        strip.background = element_blank(), strip.text = element_text(size = 12))   

ggsave("output/GOA/WGOA_TEMP_INDEX.png", height = 5, width = 7.5, dpi = 600)

EGOA = AllReg2_esr %>% 
  filter(ESR_Region == "Eastern Gulf of Alaska") %>% 
  mutate(Temperature = T_r, SD = sd_r) %>% 
  select(Year, ESR_Region, Temperature, SD)

write.csv(EGOA, file="output/GOA/egoa_LL_temp_index.csv")

ggplot(EGOA, aes(Year, Temperature)) +
  geom_ribbon(aes(x = Year, ymin = Temperature - SD, ymax = Temperature + SD), fill = "grey", alpha = 0.5) +
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  geom_hline(aes(yintercept = mean(Temperature)), size = 1, lty = 2) + 
  ylab(expression(paste('Temperature (', ~degree, 'C)', sep = ''))) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        strip.background = element_blank(), strip.text = element_text(size = 12))   

ggsave("output/GOA/EGOA_TEMP_INDEX.png", height = 5, width = 7.5, dpi = 600)

############################
# Eastern Bering Sea ESR contribution
############################
EBS = AllReg2_esr %>% 
  filter(ESR_Region == "Southeastern Bering Sea") %>% 
  mutate(Temperature = T_r, SD = sd_r) %>% 
  select(Year, ESR_Region, Temperature, SD)

write.csv(EBS, file="output/EBS/ebs_LL_temp_index.csv")

ggplot(EBS, aes(Year, Temperature)) +
  geom_ribbon(aes(x = Year, ymin = Temperature - SD, ymax = Temperature + SD), fill = "grey", alpha = 0.5) +
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  geom_hline(aes(yintercept = mean(Temperature)), size = 1, lty = 2) + 
  ylab(expression(paste('Temperature (', ~degree, 'C)', sep = ''))) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        strip.background = element_blank(), strip.text = element_text(size = 12))

ggsave("output/EBS/EBS_TEMP_INDEX.png", height = 5, width = 7.5, dpi = 600)
