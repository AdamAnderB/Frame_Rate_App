# Define UI for application that draws a histogram
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(shinyWidgets)
library(tidyr)
library(ggExtra)
library(forcats)


#load data
actual_distance=.3


#"#F7FBFF"
#build a GUI
ui <- navbarPage("TheArtOfWrangling: Frame Rate", 
                 setBackgroundColor(
                   color = c("#E0E0E0","#E0E0E0"),
                   gradient = "linear",
                   direction = "bottom" ),
                 sidebarPanel(
                   p("PLEASE WAIT: DATA IS BEING WRANGLED IN REAL TIME"),
                   p(style = "font-style: italic; font-weight: bold;",
                     "Citation: Bramlett, A. A. & Wiener, S. (2024). ",
                     tags$em("The Art of Wrangling: Working with Web-based Eye-tracking Data in Language Research. "),
                     "Linguistic Approaches to Bilingualism."),
                   p("Input for all visualizations and dataframes is reactive (they change with the sliders). 
                     The visuals aim to assist the reader in understanding the relationship between frame rate 
                     as a metric to capture effects in the VWP."),
                   p("Vertical blue lines represent the times of interest in the study. 
                     Sentences are centered on the verb at time zero."),
                   p("In the center plot: grey violins represent item aggregate frame rates, 
                     while colored boxplots and points represent participant aggregates."),
                   #p("Preset loading standards are the same as standards used in the Art of Wrangling"),
                   #p("•Red verticle lines indicate the approximate beginning and end of sentences."),
                   #p("•Blue verticle lines indicate the time of interest. (-400 to 800 with 0 being verb offset)"),
                   #p(tags$a(href="https://www.andrew.cmu.edu/user/swiener/lapp/", "See our lab here")),
                   #p(tags$a(href="https://www.researchgate.net/publication/369090199_An_eye-tracking_replication_without_an_eye_tracker_Capturing_predictive_sentence_processing_of_accented_speech_via_the_internet
                   #", "See the poster here")),
                   #p(tags$a(href="https://www.adamabramlett.com/", "See Adam's personal site here")),
                   #sliderInput("bin_size", label = h3("Bin size"), min = 10, max = 300, value = c(0, 60)),
                   sliderInput("filter_median_hz", label = h3("Kept participants by Hz"), min = 0, max = 40, value = c(0, 100)),
                   sliderInput("time_mech", label = h3("Change: time frame"), min = -2400, max = 2400, value = c(-400, 800)),
                   #p("Notice how increasing the internal removal has huge implications for how much data is removed. 0 is no removal, .3 is maximal removal"),
                   sliderInput("bin_size", label = h3("Change: bin size"), min = 10, max = 400, value = 50),
                   #p("Notice how increasing the outside removal has a very small effect on how much data is removed until close to the borders of the actual image. -1 is minimal removal, 0 is maximal removal"),
                   radioButtons("line_choice",label = "Choose: Line Type", 
                   choices = c("Smooth" = "gam",
                               "Linear" = "lm"))
                 ),
                 tabPanel("Visualizations",mainPanel(
                   plotOutput("plot", brush = "plot_brush", dblclick = "plot_reset"),
                   plotOutput("plot2", brush = "plot_brush", dblclick = "plot_reset"),
                   plotOutput("plot3", brush = "plot_brush", dblclick = "plot_reset")
                 )),
                 #tabPanel("other visuals"),
                 navbarMenu("Data",
                            tabPanel("All data (post-wrangling-raw)",
                                     DTOutput(outputId = "tabledata")),
                            tabPanel("Individual Participant Frame Rates by Trial",
                                     DTOutput(outputId = "fr_parts")),
                            tabPanel("Time Binned Data Aggregates across Participants",
                                     DTOutput(outputId = "agg_data_part_table")),
                            tabPanel("Time Binned Data Aggregates across Items",
                                     DTOutput(outputId = "agg_data_item_table")),
                            tabPanel("Emp Logit Data for Visualization",
                                     DTOutput(outputId = "smooth_data_table"))
                            ),
)
#build server
server <- function(input, output, session) {
  data <- reactive({
    read.csv("shiny_binning_data.csv",
             header=TRUE, row.names=1)
  })
  output$tabledata <- renderDT(data())
  center=.5
  time_start = -500
  time_end = 900
  time_bin_size<-400
  actual_screen=.5
  distance=0
  beyond_screen=-.5
  
  
  all_data <- reactive({
    data()%>%
      mutate(image_viewing=case_when(x_pred_normalised <= center-distance & 
                                       y_pred_normalised >= center+distance ~ image_1,
                                     x_pred_normalised >= center+distance & 
                                       y_pred_normalised >= center+distance ~ image_2,
                                     x_pred_normalised <= center-distance & 
                                       y_pred_normalised <= center-distance ~ image_3,
                                     x_pred_normalised >= center+distance & 
                                       y_pred_normalised <= center-distance ~ image_4))%>%
      filter(!is.na(image_viewing))%>%
      mutate(target = if_else(image_viewing == img_1_file, 1, 0), 
             comp_1 = if_else(image_viewing == img_2_file, 1, 0), 
             comp_2 = if_else(image_viewing == img_3_file, 1, 0), 
             dist = if_else(image_viewing == img_4_file, 1, 0))%>%
      filter(x_pred_normalised>center-abs(beyond_screen)-.5&
               x_pred_normalised<center+abs(beyond_screen)+.5&
               y_pred_normalised>center-abs(beyond_screen)-.5&
               y_pred_normalised<center+abs(beyond_screen)+.5)
  })
  
  
  #data manipulation
  #looks
  frame_rates_parts <- reactive({
    all_data()%>%
      group_by(Participant.Private.ID,subject_img_file,verb_type,talker,Participant.Browser,Participant.OS,Participant.Device,Participant.Viewport.Size,Participant.Monitor.Size,Trial.Number)%>%
      summarize(count = n(),
                max_time = max(time_elapsed),
                frame_rate = count/max_time*1000)%>%
      ungroup()%>%
      group_by(Participant.Private.ID)%>%
      mutate(median_frame_rate = median(frame_rate),
             frame_rate_cats = case_when(median_frame_rate>=30~"high",
                                         median_frame_rate<30&median_frame_rate>=5~"medium",
                                         median_frame_rate<5~"low"),
             frame_rate_cats=as.factor(frame_rate_cats))%>%
      ungroup()%>%
      group_by(Participant.Private.ID)%>%
      filter(median_frame_rate>input$filter_median_hz[1] & median_frame_rate<input$filter_median_hz[2])
  })
  #creates table for viewing
  output$fr_parts <- renderDT(frame_rates_parts())
  
  #for overall_mean
  overall_mean_rate <- reactive({
    mean(frame_rates_parts()$frame_rate)
  })
  
  #plot 1
  output$plot <- renderPlot({
    ggMarginal(frame_rates_parts()%>%
                 ggplot(aes(y=frame_rate,
                            x=fct_reorder(as.factor(Participant.Private.ID), median_frame_rate),
                            group=as.factor(Participant.Private.ID),color =frame_rate_cats))+
                 geom_jitter(alpha=.3)+
                 geom_violin()+
                 geom_boxplot(alpha=.2)+
                 geom_point(aes(y=median_frame_rate),color ="white")+
                 geom_hline(yintercept = overall_mean_rate(),linetype=3)+
                 theme_minimal()+
                 scale_color_manual(values = c("#009647","#FDB515","#EF3A47"))+
                 theme(axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),
                       panel.grid.major.x = element_blank(),
                       legend.position = c(.1,.75),
                       legend.direction="vertical",
                       legend.title = element_blank())+
                 xlab("Participants Ordered by Median Frame Rate")+
                 ylab("Frame Rate (Hz)"), groupColour = TRUE, groupFill = TRUE)
    
  }, res = 96)
  
  
  frame_rate_cats_part <- reactive({
    frame_rates_parts()%>%
      select(frame_rate_cats,Participant.Private.ID,subject_img_file)%>%
      distinct(subject_img_file,frame_rate_cats)
  })
  frame_rate_cats_item <- reactive({
    frame_rates_parts()%>%
    select(frame_rate_cats,Participant.Private.ID,subject_img_file)%>%
      distinct(Participant.Private.ID,frame_rate_cats)
  })
  #creates table for viewing
  output$agg_data_item_table <- renderDT(agg_data_item())

  agg_data_item <- reactive({
    all_data()%>%
      filter(Participant.Private.ID %in%frame_rate_cats_part()$Participant.Private.ID)%>%
      mutate(time_elapsed=time_elapsed-object_start-200)%>%
      mutate(time_elapsed_rounded=input$bin_size*round((time_elapsed)/input$bin_size))%>% 
      group_by(time_elapsed_rounded,subject_img_file) %>%
      summarise(target_n=n())%>%
      filter(time_elapsed_rounded>input$time_mech[1] & time_elapsed_rounded<input$time_mech[2])
  })
  #creates table for viewing
  output$agg_data_item_table <- renderDT(agg_data_item())
  
  agg_data_part <- reactive({
    all_data()%>%
      mutate(time_elapsed=time_elapsed-object_start-200)%>%
      mutate(time_elapsed_rounded=input$bin_size*round((time_elapsed)/input$bin_size))%>% 
      group_by(time_elapsed_rounded,Participant.Private.ID) %>%
      summarise(target_n=n())%>%
      filter(time_elapsed_rounded>input$time_mech[1] & time_elapsed_rounded<input$time_mech[2])%>%
      right_join(frame_rate_cats_part())
      })
  #creates table for viewing
  output$agg_data_part_table <- renderDT(agg_data_part())
  #plot 1
  output$plot2 <- renderPlot({
    ggplot()+
      geom_vline(xintercept = -400,color="#007BC0")+
      geom_vline(xintercept = 800,color ="#007BC0")+
      geom_jitter(data=agg_data_part(),aes(x=time_elapsed_rounded,y=target_n,color = frame_rate_cats),alpha=.02)+
      geom_boxplot(data=agg_data_part(),aes(x=time_elapsed_rounded,
                                          y=target_n,
                                          alpha=.1,
                                          group =interaction(time_elapsed_rounded,frame_rate_cats),
                                          color=frame_rate_cats,
                                          fill=frame_rate_cats),
                   alpha=.4)+
      geom_jitter(data=agg_data_item(),aes(x=time_elapsed_rounded,
                                         y=target_n),
                  alpha=.02,
                  color = "grey")+
      geom_violin(data=agg_data_item(),aes(x=time_elapsed_rounded,
                                         y=target_n,
                                         alpha=.1,
                                         group=time_elapsed_rounded),
                  color = "grey",
                  fill= "grey",alpha=.1)+
      scale_fill_manual(values = c("#009647","#FDB515","#EF3A47"))+
      scale_color_manual(values = c("#009647","#FDB515","#EF3A47"))+
      scale_x_continuous(limits = c(-2200,2400),breaks=seq(-2400,2400, 400))+
      theme_minimal()+
      theme(axis.ticks.x=element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = c(.8,.75),
            legend.direction="vertical",
            legend.title = element_blank())+
      xlab("Adjusted Time")+
      ylab("Total Looks Captured")
  }, res = 96)
  
  
  
  #for outcomes
  smooth_data <- reactive({
    
    all_data()%>%
    group_by(Participant.Private.ID,subject_img_file,verb_type,talker)%>%
      mutate(count = n(),
             max_time = max(time_elapsed),
             frame_rate = count/max_time*1000)%>%
      ungroup()%>%
      group_by(Participant.Private.ID)%>%
      mutate(median_frame_rate = median(frame_rate))%>%
      filter(median_frame_rate>=input$filter_median_hz[1] & median_frame_rate<=input$filter_median_hz[2])%>%
      mutate(time_elapsed=time_elapsed-object_start-200)%>% 
      mutate(time_elapsed_rounded=input$bin_size*round((time_elapsed)/input$bin_size))%>%
      filter(time_elapsed_rounded>input$time_mech[1] & time_elapsed_rounded<input$time_mech[2])%>%
      group_by(time_elapsed_rounded,verb_type, talker)%>%
      summarise(target_looks = mean(target), 
                comp_1_looks = mean(comp_1),
                comp_2_looks = mean(comp_2),
                dist_looks = mean(dist),
                target_n=n())%>%
      mutate(emp_logit =log((target_looks+(0.5/target_n))/
                              (1-(target_looks+(0.5/target_n)))))
  })
  
  #creates table for viewing
  output$plot3 <- renderPlot({
    smooth_data()%>%
      ggplot(aes(x=time_elapsed_rounded, y=emp_logit, 
               linetype = verb_type, color= talker))+
             geom_smooth(method = input$line_choice)+
             geom_vline(xintercept = -400,color="#007BC0")+
             geom_vline(xintercept = 800,color ="#007BC0")+
             scale_x_continuous(limits = c(input$time_mech[1],input$time_mech[2]),breaks=seq(input$time_mech[1],input$time_mech[2], abs(input$time_mech[1])))+
             scale_linetype_discrete(labels=c('Non-Restricting','Restricting'))+
             scale_color_manual(values=c("#007BC0", "#FDB515"),labels=c('Native','Non-Native'))+
             ylab("Average empirical logit looks to object")+
             xlab("Time (ms)")+
             theme(legend.position = c(.2, .7))+
             theme_minimal()+
             labs(title = "Online Replication from Poretta et al's (2020)")+
             labs(color='Talker',
                  linetype="Verb Type")+
             theme(legend.position = c(.2,.7),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   legend.key=element_blank())+
             guides(color = guide_legend(override.aes = list(fill = NA)),
                    linetype = guide_legend(override.aes = list(fill = NA)))
 
  }, res = 96)
  
}
shinyApp(ui, server)


