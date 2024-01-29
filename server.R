shinyServer(function(input,output,session){
  
  plot <- reactive({
    
    Plot <- PopulationinbroadAgeGroups %>%
      group_by(age_groups,nationality) %>% 
      summarise(Population = sum(population)) %>% 
      mutate(`Age Group` = age_groups) %>%
      
      ggplot(aes(x = `Age Group`, y = Population, fill = nationality))+
      geom_bar(position = "dodge", stat='identity')+
      coord_flip()+
      theme_classic() +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
      theme(plot.margin = margin(2,0.5,0.5,0.5,"cm"),plot.title = element_text(size = 12)) +
      xlab("Age Groups") +
      ylab("Population (in Millions)") +
      scale_fill_discrete(name = "Nationality")
    
    ggplotly(Plot,tooltip = c("x", "y")) %>%
      layout(title = list(text = paste0("Saudi Arabia's Population by Age Groups and nationality",
                                        '<br>',
                                        '<sup>',
                                        'Source: portal.saudicensus.sa','</sup>')))
  })
  
  # Arabic page
  output$plot1 <- renderPlotly({
    plot()
  })
  
  # English page
  output$plot2 <- renderPlotly({
    plot()
  })
  
})
  