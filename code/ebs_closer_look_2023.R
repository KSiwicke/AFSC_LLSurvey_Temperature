look <- pro %>% 
  filter(ESR_Region == "Southeastern Bering Sea", TDR_Depth > 245, TDR_Depth < 256) %>% 
  group_by(Year, Station_Number, Geographic_Area_Name) %>% 
  summarize(temp = mean(Temperature))

ggplot(look, aes(Year, temp, col = Geographic_Area_Name)) + 
  geom_point() + 
  # scale_color_gradientn(colors=viridis(9)) +
  facet_wrap(~Station_Number)

ebs_wts <- AllGeo2_esr %>% filter(ESR_Region == "Southeastern Bering Sea")

area.wts
