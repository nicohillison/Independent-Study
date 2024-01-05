library(shiny)
library(plotly)
library(ggplot2)
library(readr)
library(dplyr)
library(shinydashboard)
library(scales)
hud_data2 <- read.csv("data.csv")
hud_data2$averageAmountOfMoneyByInvestors <- (hud_data2$Total.Funding.Amount.Currency..in.USD. / hud_data2$Number.of.Investors)
ui <- fluidPage(
  tabsetPanel(
    tabPanel("My Dashboard",
             fluidRow(h1("A Company's Investments and Funding")),
             fluidRow(box(title = "Data Description: Using Crunchbase data, I leveraged my skills to effectively analyze and present
                          real-life information in a visually compelling manner. The primary emphasis of this project centered on funding
                          amounts and types, allowing for clear and accessible explanations. To achieve this, I utilized the programming
                          language R to construct an interactive analysis report. Rather than opting for a conventional paper format, 
                          I envisioned a more visually appealing application format to showcase the data. By organizing the content
                          into distinct tabs, each with its own section and focused topic, I ensured seamless navigation and 
                          facilitated effortless transitions between subjects.",
                          width = 10,
                          solidHeader = TRUE,
                          "")),
             fluidRow(box(title = "Setbacks: Initially, my intention was to conduct a predictive study; however, I encountered a
                          challenge as all the required data had already been collected, leaving me with insufficient information
                          for making predictions. Nevertheless, I embarked on the task of creating visualizations to offer insights
                          into potential future trends. During the process, I encountered several challenges, particularly in configuring
                          the application setup and organizing the gathered information effectively. It was crucial to select appropriate
                          graphs and optimize their visual appeal. To ensure the success of this project, I dedicated time to reacquaint
                          myself with R programming and identify the most suitable packages for the task at hand. Through collaborative
                          efforts with my teachers and colleagues, I managed to develop a dynamic and engaging report that not only elucidates
                          the visualizations but also breathes life into them, resulting in an enjoyable and interactive experience.",
                          width = 10,
                          solidHeader = TRUE,
                          "")),
             fluidRow(box(title = "Credit: Stephanie Taylor, Jerzy Wizorsky, Susana Chicano, Crunchbase Company",
                          width = 10,
                          solidHeader = TRUE,
                          ""))
    ),
    tabPanel("Investments and Funding Amounts",
             fluidRow(h1("Investments and Funding Amounts")),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "Organization.Name",
                   label = "Select Organization",
                   choices = c("ALL ORGANIZATIONS", hud_data2$Organization.Name)
                 ),
                 box(title = "Average Investment Considering the Total Funding Amount: ",
                     width = 15,
                     solidHeader = TRUE,
                     "The presented graph illustrates the hypothetical value of investments 
                     if the initial stakeholders had retained their shares and other assets 
                     within the respective companies. This concept prompts contemplation regarding
                     the potential outcomes that might have materialized. It is plausible that certain
                     investors made premature or delayed sales decisions. The graph, at first glance,
                     may not appear highly informative, hence the inclusion of a dropdown menu. This
                     interactive feature allows users to select any desired organization from the
                     available dataset and observe the specific results pertaining to that particular company.
                     The dropdown menu facilitates easy searching and locating of desired companies, enhancing
                     user convenience.")
               ),
               box(plotlyOutput(outputId = "scatter2Plot"))),
             sidebarLayout(
               sidebarPanel(
                 box(title = "Logarithmic Compression",
                     width = 15,
                     solidHeader = TRUE,
                     "In a manner reminiscent of the previous graph, this histogram applies a logarithmic
                     transformation to the average funding amounts. This logarithmic compression of the
                     axis results in a non-linear representation. Consequently, the histogram employs width
                     bins to capture the average funding amounts, while the y-axis depicts their corresponding density.
                     The logarithmic transformation, employed to reflect the density, gives rise to a distorted curve,
                     deviating from the previous bimodal distribution characterized by twin peaks.")
               ),
               box(plotlyOutput(outputId = "scatter1Plot"))
             )
    ),
    tabPanel("Status",
             fluidRow(h1("Status")),
             sidebarLayout(
               sidebarPanel(
                 box(title = "Status of each Company",
                     width = 15,
                     solidHeader = TRUE,
                     "This graph serves as an introductory overview for the subsequent section, namely, Funding Status.
                     It presents a straightforward bar chart that provides insights into the status of each company, primarily
                     categorized as either having achieved IPO status or not. The labels public and private are assigned accordingly:
                     public denotes companies that have successfully completed the IPO process, while private represents those currently
                     in the IPO stage but yet to finalize it. It is noteworthy that the majority of companies fall into the private category,
                     as attaining IPO status remains a formidable achievement for any organization. Additionally, there exists a category
                     labeled delisted, signifying companies that have ceased operations and failed to sustain themselves within the dataset.")
               ),
               box(plotlyOutput(outputId = "bar1Plot"))),
             
    ),
    tabPanel("Funding Types", 
             fluidRow(h1("Funding Types")),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 box(title = "Amount of Funding Rounds to its Status",
                     width = 15,
                     solidHeader = TRUE,
                     "The initial graph in this section offers an insightful visualization of the relationship between the number of
                     funding rounds and the attainment of a specific funding milestone. The x-axis represents the number of funding
                     rounds, while the y-axis depicts the count of companies that have reached the targeted status. The color variation
                     in the graph corresponds to different funding types. By hovering over the data points, users can gain clearer
                     visibility into specific counts, the precise number of rounds, and the corresponding funding types. It is important
                     to note that this visualization focuses solely on the final funding type for each company. Accordingly, a given
                     company is represented only once in the stacked bar plot, depending on its last funding type and corresponding
                     funding round.
"),
                 box(title = "Funding Amount in Last Funding Type",
                     width = 15,
                     solidHeader = TRUE,
                     "While bearing some resemblance to the initial graph, this one features slight variations in its axes.
                     The previous graph on the second tab focused on the overall average funding amount, whereas this graph specifically
                     highlights the last funding amount for each company. The x-axis corresponds to the company names, while the color
                     distinguishes the various last funding types, encompassing multiple categories. Similar to the other graphs, this
                     interactive visualization allows users to hover over data points, revealing specific details pertaining to each company's
                     last average funding amount.")
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
    fig <- plot_ly(x = crunch_data$IPO.Status)
    fig <- fig %>% layout(yaxis = list(title = "Count of Companies"), xaxis = list(title = 'Company Status'))
    fig
    
  })
  
  output$scatter1Plot <- renderPlotly({
    hud <- hud_data2
    fig <- plot_ly(x = log(crunch_data$averageAmountOfMoneyByInvestors), type = "histogram")
    fig
  })
  
  output$map1Plot <- renderPlotly({
    hud <- hud_data2
    fig <- plot_ly(data = crunch_data, x = ~crunch_data$Organization.Name, y = ~crunch_data$Last.Funding.Amount.Currency..in.USD., color = crunch_data$Last.Funding.Type)
    fig <- fig %>% layout(yaxis = list(title = "Last Funding Amount Currency"), xaxis = list(title = 'Organization Name'))
    fig
  })
  
  
  
  output$map3Plot <- renderPlotly({
    fig <- ggplot(crunch_data, aes(x = Number.of.Funding.Rounds, fill = Funding.Status)) + 
      geom_bar() +
      labs(x = "Number of Funding Rounds",
           y = "Count",
           fill = "Funding Status")
    fig
  })
  
  # First graph in first panel
  output$scatter2Plot <- renderPlotly({
    hud <- hud_data2
    if(input$Organization.Name != "ALL ORGANIZATIONS"){
      hud <- subset(hud, (Organization.Name == input$Organization.Name))
    }
    fig <- plot_ly(data = hud, x = ~hud$Organization.Name, y = ~hud$averageAmountOfMoneyByInvestors, text = ~paste("Last Funding Type:", Last.Funding.Type))
    fig <- fig %>% layout(yaxis = list(title = "average Amount Of Money By Investors"), xaxis = list(title = 'Organization Name'))
    
    fig
  })
  
}

shinyApp(ui = ui, server = server)
