# C:\Pablo UK\43 R projects 2020\04 My Shiny app\04 Mycovid19 app\07 METHODOLOGY
# File: 08 Barplots using PLOTLY.R

# EXAMPLE OF A PLOTLY BARPLOT  


Tabledesc1 <- POP_POPULATED  %>%
              select(Confirmed,Country,date) %>% 
              mutate(Max_date = max(POP_POPULATED$date)) %>% 
              mutate(Flag_max_date = ifelse(Max_date == date,1,0)) %>% 
              filter(Flag_max_date==1) %>% 
              arrange(desc(Confirmed)) %>% 
              group_by(date) %>% 
              slice(1:10) %>% 
              ungroup()


TabledesC <- Tabledesc1 %>% 
              arrange(Confirmed)

# This is a trick  to display counts in descending order in Plotly bar charts

# This is the way of producing a plot in DESCENDING order
Top10_D <- TabledesC


Top10_D <- data.frame(Tabledesc2,stringsAsFactors = FALSE)
Top10_D$Country

# The trick is to use decresing = FALE
Top10_D$Country <- factor(Top10_C$Country, 
                          levels = unique(Top10_D$Country)[order(Top10_D$Confirmed, decreasing = FALSE)])
          
plot_ly(Top10_D, x = ~Confirmed, y = ~Country,
        type = 'bar') %>%
        layout(title="Top 10 countries by Confirmed  Covid19 cases")
  
  
          








