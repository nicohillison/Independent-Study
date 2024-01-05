library(shiny)
library(plotly)
library(ggplot2)
library(readr)
library(dplyr)
library(shinydashboard)
library(scales)
hud_data2 <- read.csv("hud_data2.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("My Dashboard",
             fluidRow(h1("Predicting If A Company Will Go Public, Survive During A Recession, Or Go To The Next Funding Round.")),
             fluidRow(box(title = "Data Description: ",
                          width = 10,
                          solidHeader = TRUE,
                          "")),
             fluidRow(box(title = "More Information: ",
                          width = 10,
                          solidHeader = TRUE,
                          "")),
             fluidRow(box(title = "Credit: ",
                          width = 10,
                          solidHeader = TRUE,
                          ""))
    ),
    tabPanel("Investments and Funding Amounts",
             fluidRow(h1("Investments and Funding Amounts")),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "State",
                   label = "Select State",
                   choices = list("ALL STATES", "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
                                  "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                                  "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND", "NE", 
                                  "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", 
                                  "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", 
                                  "WY")
                 ),
                 box(title = "Average Investment Considering the Total Funding Amount: ",
                     width = 15,
                     solidHeader = TRUE,
                     "Put in Description.")
               ),
               box(plotlyOutput(outputId = "scatter2Plot"))),
               sidebarLayout(
               sidebarPanel(
                 box(title = "Other Question??: ",
                     width = 15,
                     solidHeader = TRUE,
                     "Put in Description.")
               ),
               box(plotlyOutput(outputId = "scatter1Plot"))
             )
    ),
    tabPanel("Funding Round",
             fluidRow(h1("Funding Round")),
             sidebarLayout(
               sidebarPanel(
                 box(title = "Dollar Amount Dictating the Next Funding Round: ",
                     width = 15,
                     solidHeader = TRUE,
                     "Olivia's text.")
               ),
               box(plotlyOutput(outputId = "bar1Plot"))),
             sidebarLayout(
               sidebarPanel(
                 box(title = "Other Question??: ",
                     width = 15,
                     solidHeader = TRUE,
                     "Put in Description.")
               ),
               box(plotlyOutput(outputId = "bar2Plot"))
             )
    ),
    tabPanel("IPO Status", 
             fluidRow(h1("IPO Status")),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: Slider for the number of bins ----
                 selectInput(inputId = "housing_type",
                             label = "Select Housing Type",
                             choices = list("Street Outreach" = "so", 
                                            "Emergency Shelter" = "es",
                                            "Temporary Housing" = "th",
                                            "Safe Haven" = "sh",
                                            "Public Housing" = "ph")
                 ),
                 box(title = "Question ",
                     width = 15,
                     solidHeader = TRUE,
                     "Put in Description."),
                 box(title = "Question ",
                     width = 15,
                     solidHeader = TRUE,
                     "Put in Description.")
               ),
               # Main panel for displaying outputs ----
               mainPanel(
                 # Output: Bar chart of percent that successfully exit street outreach ----
                 plotlyOutput(outputId = "map3Plot"),
                 plotlyOutput(outputId = "map1Plot")
               )
             )
             
    )
  ))


server <- function(input, output) {
  
  output$bar1Plot <- renderPlotly({
    # Create data table containing average percentages across states for percent of people that exit street outreach
    hud_data2$perc_successfully_exit_so <- as.numeric(gsub("[%]","",hud_data2$perc_successfully_exit_so))
    
    so_data <- hud_data2 %>%
      select(state, total_num_exit_so, num_exit_so_to_temp, num_exit_so_to_perm, perc_successfully_exit_so) %>%
      mutate(perc_exit_so_to_temp = 100*num_exit_so_to_temp/total_num_exit_so,
             perc_exit_so_to_perm = 100*num_exit_so_to_perm/total_num_exit_so)
    
    so_data_by_state <- aggregate(.~state,data=so_data,FUN=mean)
    
    fig <- plot_ly(so_data_by_state, x = ~state, y = ~perc_exit_so_to_temp, type = 'bar', name = "Exit to Temporary Housing")
    fig <- fig %>% add_trace(y = ~perc_exit_so_to_perm, name = 'Exit to Permanent Housing')
    fig <- fig %>% layout(yaxis = list(title = 'Percent of People that Successfully Exit Street Outreach'), xaxis = list(title = 'State'), barmode = 'stack')
    
    fig
    
  })
  
  output$scatter1Plot <- renderPlotly({
    # Convert currency to numeric variable
    hud_data2$coc_award_amount <- as.numeric(gsub('[$,]','', hud_data2$coc_award_amount))
    
    # Convert percentage to numeric value
    hud_data2$perc_return_12_all <- as.numeric(gsub("[%]","",hud_data2$perc_return_12_all))
    
    coc_award_data <- hud_data2 %>%
      select(state, coc_award_amount, perc_return_12_all)
    
    coc_award_data_by_state <- aggregate(.~state,data=coc_award_data,FUN=mean)
    
    fig <- plot_ly(coc_award_data_by_state, x = ~coc_award_data_by_state$coc_award_amount, y = ~coc_award_data_by_state$perc_return_12_all, text = ~paste("State: ", coc_award_data_by_state$state, "<br>Average Award Amount: $", coc_award_data_by_state$coc_award_amount, "<br>% Return:", coc_award_data_by_state$perc_return_12_all))
    fig <- fig %>% layout(xaxis = list(title = 'Average CoC Award Amount ($)'), yaxis = list(title = 'Percent that Return to a Shelter in 12 Months'))
    
    fig
  })
  
  output$map1Plot <- renderPlotly({
    stayhud.df <- hud_data2[,c(1,4,6,13,14,15,16,20,27,34,41,51,52)]
    stayhud.df$perc_return_6_all <- as.numeric(gsub('[%,]', '', stayhud.df$perc_return_6_all))
    stayhud.df$coc_award_amount <- as.numeric(gsub('[$,]', '', stayhud.df$coc_award_amount))
    
    avgagghud <- aggregate(. ~ state, stayhud.df, mean, na.omit = TRUE)
    
    agghud <- aggregate(. ~ state, stayhud.df, sum, na.omit = TRUE)
    
    avgagghud$hover2 <- with(agghud, paste(state, '<br>', "CoC Award Amount", coc_award_amount, '<br>', "Beds Available", total_beds, "<br>",
                                           "Count of Persons Returning in 24 Mo", num_return_24_all))
    # give state boundaries a white border
    l2 <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g2 <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig2 <- plot_geo(avgagghud, locationmode = 'USA-states')
    fig2 <- fig2 %>% add_trace(
      z = ~perc_return_6_all, text = ~hover2, zmin=0, zmax=30,locations = ~state,
      color = ~perc_return_6_all, colors = 'Blues'
    )
    fig2 <- fig2 %>% colorbar(title = "Percent Return")
    fig2 <- fig2 %>% layout(
      title = 'Percent Return to Shelter within 24 Months in 2019<br>(Hover for breakdown)',
      geo = g2
    )
    
    fig2
  })
  
  
  
  output$map3Plot <- renderPlotly({
    stayhud2.df <- hud_data2[,c(1,4,6,13,14,15,16,18, 25, 32, 39 ,46)]
    #stayhud2.df$perc_return_6_all <- as.numeric(gsub('[%,]', '', stayhud2.df$perc_return_6_all))
    stayhud2.df$perc_return_12_so <- as.numeric(gsub('[%,]', '', stayhud2.df$perc_return_12_so))
    stayhud2.df$perc_return_12_es <- as.numeric(gsub('[%,]', '', stayhud2.df$perc_return_12_es))
    stayhud2.df$perc_return_12_th <- as.numeric(gsub('[%,]', '', stayhud2.df$perc_return_12_th))
    stayhud2.df$perc_return_12_sh <- as.numeric(gsub('[%,]', '', stayhud2.df$perc_return_12_sh))
    stayhud2.df$perc_return_12_ph <- as.numeric(gsub('[%,]', '', stayhud2.df$perc_return_12_ph))
    stayhud2.df$coc_award_amount <- as.numeric(gsub('[$,]', '', stayhud2.df$coc_award_amount))
    
    avgagghud2 <- aggregate(. ~ state, stayhud2.df, mean)
    
    agghud2 <- aggregate(. ~ state, stayhud2.df, sum)
    
    avgagghud2$hover4 <- with(avgagghud2, paste(state, '<br>', "CoC Award Amount:", coc_award_amount, '<br>', "Beds Available:", total_beds, "<br>"))
    # give state boundaries a white border
    l4 <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g4 <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    var <- paste("perc_return_12_", input$housing_type, sep="")
    
    fig4 <- plot_geo(avgagghud2, locationmode = 'USA-states')
    fig4 <- fig4 %>% add_trace(
      z = as.formula(paste0('~',var)), text = ~hover4, locations = ~state,
      color = as.formula(paste0('~',var)), colors = 'Blues'
    )
    fig4 <- fig4 %>% colorbar(title = "Percent Exiting")
    fig4 <- fig4 %>% layout(
      title = 'Percent of Persons Returning by Housing Type in 2019 <br>(Default is Street Outreach)',
      geo = g4
    )
    
    fig4
  })
  
  output$bar2Plot <- renderPlotly({
    hud_data2$perc_successfully_exit_es_th_sh_phrrh_to_perm <- as.numeric(gsub("[%]","",hud_data2$perc_successfully_exit_es_th_sh_phrrh_to_perm))
    hud_data2$unsuccessful_exit <- 100 - hud_data2$perc_successfully_exit_es_th_sh_phrrh_to_perm
    so_data <- hud_data2 %>%
      select(state, total_num_exit_es_th_sh_phrrh, num_exit_es_th_sh_phrrh_to_perm, perc_successfully_exit_es_th_sh_phrrh_to_perm) %>%
      mutate(perc_so_exit_perm = 100*hud_data2$perc_successfully_exit_es_th_sh_phrrh_to_perm/hud_data2$num_exit_es_th_sh_phrrh_to_perm)
    so_data <- so_data %>% 
      rename(state = state, 
             total_exit = total_num_exit_es_th_sh_phrrh, 
             tot_exit_to_perm = num_exit_es_th_sh_phrrh_to_perm,
             perc_successfully_exit = perc_successfully_exit_es_th_sh_phrrh_to_perm)
    so_data_by_state <- aggregate(.~state,data=so_data,FUN=mean)
    so_data_by_state$unsuccesful <- 100 - so_data_by_state$perc_successfully_exit
    fig <- plot_ly(so_data_by_state, x = ~state, y = ~perc_successfully_exit, type = 'bar', name = 'Successful Exit')
    fig <- fig %>% layout(yaxis = list(title = 'Percentage of Exit'), xaxis = list(title = "States"), barmode = 'stack')
    p3 <- fig
    
    ggplotly(p3)
  })
  
# First graph in first pannel
  output$scatter2Plot <- renderPlotly({
    hud <- hud_data2
    if(input$State != "ALL STATES"){
      hud <- subset(hud, (state == input$State))
    }
    fig <- plot_ly(data = subset(hud, !rural_urban == ""), x = ~es_sh_th_avg_days, y = ~avg_pct_wage_inc, color = ~rural_urban, text = ~paste("State: ", state))
    fig <- fig %>% layout(yaxis = list(title = "Average Percent Increase in Wage"), xaxis = list(title = 'Average Days in the Shelter'))
    
    fig
  })
  
}

shinyApp(ui = ui, server = server)

