library(tidyverse)
library(RODBC)

# dir <- "C:/Users/kevin.siwicke/Work/Github/AFSC_LLSurvey_Temperatyre"
# Load pre coded layouts for graphics
source("Plot_Layout.R")

# Pull data from DB
channel <- odbcConnectAccess2007("data/LLsurvey_Temperature.accdb")
prof <- sqlQuery(channel,
                "SELECT *
               FROM [Temperature Depth Profiles];")

close(channel)
rm(channel)

prof <- prof %>% mutate(stn.yr.z = paste0(Station_Number, ".", Year, ".", TDR_Depth)) # unique id for each stn/depth by set

pro1 <- prof %>% filter(Set == "One") # preference given to set 1 which is consistent through time series
pro2 <- prof %>% filter(Set == "Two") %>% 
  filter(!stn.yr.z %in% pro1$stn.yr.z) # when there is a depth in set 2 that is not in set 1

pro <- rbind(pro1, pro2) # combine the two for the most complete version

# Getting values for mean temp from 246 to 255 meters
AllYr <- pro %>% filter( TDR_Depth>245 & TDR_Depth<256 ) %>% 
  group_by(Year,NPFMC_Sablefish_Mgmt_Area, Geographic_Area_Name, Station_Number, Set) %>%
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
