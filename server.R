
server <- function(input, output) {
  
  output$distPlot <- renderPlot({  
    
    mapcolor<- c("#FADBD8", "#D4E6F1", "#D5F5E3", "#FAD7A0", "#E8DAEF")
    names(mapcolor)<- c("Reds", "Blues", "Greens", "Oranges", "Purples")

    ggplot(data=state_list, aes(x=state_lat,y= state_lon, size =Job_Count,color=Job_Count)) +
      labs(title = "Number of Jobs in States",x="latitude",y="Longitude",colours="size",
           size = "Number of Jobs", color = "Number of Jobs")+
      borders("state",fill = mapcolor[input$PlotColor]) +
      geom_point() +
      coord_quickmap() +
      scale_colour_viridis()+
      theme_minimal()+ theme(panel.grid = element_blank())+
      theme(plot.title = element_text(size = 25, hjust = 0.5))

  })
  
  output$distPlot1 <- renderPlot({
    
    slices1<- c(Full_Time,Part_Time,Per_Diem)
    pct1 <- ceiling(slices/sum(slices)*100)
    lbls1<- c("Full Time", "Part Time", "Per Diem")
    pie1<- data.frame(lbls1, pct1, slices1)
    pie1 %<>%
      arrange(desc(lbls1)) %>%
      mutate(ypos1= cumsum(pct1) - 0.5*pct1) %>%
      mutate(lbls1= paste0(lbls1, " (", pct1, "%)"))
    
    plot1 <- ggplot(pie1, aes(x="", y= pct1, fill= lbls1)) +
      geom_bar(stat= "identity", width = 1, color= "white") +
      labs(title = "Job Period") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_brewer(palette = input$PlotColor) +
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), legend.position = "none")+
      geom_label_repel(aes(y= ypos1, label= lbls1))
    
    slices2<- c(employee, temporary, seasonal)  
    pct2 <- ceiling(slices/sum(slices)*100)       # To find percentages
    lbls2<- c("Employee", "Temporary", "Seasonal")
    pie2<- data.frame(lbls2, pct2, slices2)
    pie2 %<>%
      arrange(desc(lbls2)) %>%
      mutate(ypos2= cumsum(pct2) - 0.5*pct2) %>%
      mutate(lbls2= paste0(lbls2, " (", pct2, "%)"))
    
    plot2 <- ggplot(pie2, aes(x="", y= pct2, fill= lbls2)) +
      geom_bar(stat= "identity", width = 1, color= "white") +
      labs(title = "Contract Type") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_brewer(palette = input$PlotColor) +
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), legend.position = "none")+
      geom_label_repel(aes(y= ypos2, label= lbls2))
    plot_grid(plot1, plot2)
  })
  
  output$distPlot2 <- renderPlot({
     
    if (input$sorts == 1) {
      ggplot(head(job_categories, input$bars), aes(head(job_category, input$bars), head(jobs_count, input$bars), fill = head(job_category, input$bars))) +
        geom_bar(stat = 'identity', orientation = "x")+
        labs(title = "Comparison of Job Categories", x = "Job Categories", y = "Jobs count", fill= "Job Category") +
        theme(plot.title = element_text(size = 25, hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line()) +
        scale_fill_brewer(palette = input$PlotColor, direction = -1)
    } else if (input$sorts == 2) {
      ggplot(head(job_categories, input$bars), aes(x= reorder(head(job_category, input$bars), head(jobs_count, input$bars)), y= head(jobs_count, input$bars), fill = head(job_category, input$bars))) +
        geom_bar(stat = 'identity', orientation = "x")+
        labs(title = "Comparison of Job Categories", x = "Job Categories", y = "Jobs count", fill= "Job Category") +
        theme(plot.title = element_text(size = 25, hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line()) +
        scale_fill_brewer(palette = input$PlotColor, direction = -1)
    } else {
      ggplot(head(job_categories, input$bars), aes(x= reorder(head(job_category, input$bars), -head(jobs_count, input$bars)), y= head(jobs_count, input$bars), fill = head(job_category, input$bars))) +
        geom_bar(stat = 'identity', orientation = "x")+
        labs(title = "Comparison of Job Categories", x = "Job Categories", y = "Jobs count", fill= "Job Category") +
        theme(plot.title = element_text(size = 25, hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line()) +
        scale_fill_brewer(palette = input$PlotColor, direction = -1)
    }

  })
  
  output$distPlot3 <- renderPlot({
    
    ggplot(Plot_sectors, aes(x="", y=sectors_count, fill=plot_sector)) +
      geom_bar(stat="identity", width=1, color="white") +
      labs(title = input$PlotTitle, fill= "Job Sectors")+
      coord_polar("y", start=0) +
      theme_void() +
      scale_fill_brewer(palette = input$PlotColor)+
      theme(plot.title = element_text(size = 25, hjust = 0.5))
    
  })
  
}