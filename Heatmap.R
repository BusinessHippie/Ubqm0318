yy %>%
  setNames(c("Date","Value")) %>%
  dplyr::mutate(
    Year=lubridate::year(Date),
    Month=lubridate::month(Date),
    # I use factors here to get plot ordering in the right order
    # without worrying about locale
    MonthTag=factor(Month,levels=as.character(1:12),
                    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE),
    # week start on Monday in my world. Per tant week_start=1
    Wday=lubridate::wday(Date,week_start=1),
    # the rev reverse here is just for the plotting order ("de baix a dalt en aquest axis")
    WdayTag=factor(Wday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE),
    Week=as.numeric(format(Date,"%W"))
  ) %>%
  # ok here we group by year and month and then calculate the week of the month 
  # we are currently in
  dplyr::group_by(Year,Month) %>% 
  #Aleshores crearem una nova columna Wmonth que estara relacionada amb el seu mes dins del any
  #Per tant, si Week =52 es posible que hi hagi una 49,50,51, 52 i 53 en el seu mes i any
  #Així que aquest Wmonth es 1+52-49= 4 que vol dir quarta semana de desembre del any que sigui
  dplyr::mutate(Wmonth=1+Week-min(Week)) %>%
  #Desagrupo any i mes, per no fastidiar el plot
  dplyr::ungroup() %>% 
  ggplot(aes(x=Wmonth, y=WdayTag, fill = Value)) + 
  geom_tile(colour = "white") + 
  facet_grid(Year~MonthTag) + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(x="Week of Month", y=NULL) +
  labs(title="Everything at a glance - Global Active Energy [Wh]") +
  theme(axis.title.y = element_text( size=2, face=2))

# End of Heatmap plot