library(shiny)
library(shinyWidgets)


#######
#Shiny App
#######

ui <- fluidPage(
    titlePanel("U.S. Data by State"),
    fluidRow(
      column(width = 1,  
             br(),
             selectInput(inputId = "s2",
                         "Statistics", choices = c("Population", 
                                                   "Income", "Illiteracy", "Life Exp",
                                                   "HS Grad", "Frost",   "Area")),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             selectInput(inputId = "s1",
                         "Crime (per 100,000)", choices = c("Murder", "Assault", "Rape"))

      ),
      column(width = 4,
             plotOutput(outputId = "map2"),
             plotOutput(outputId = "map1")

      ),
      column(width = 6,
             plotOutput(outputId = "plot2"),
             plotOutput(outputId = "plot1")
      )

    
    ),
    setBackgroundColor("whitesmoke")
)


server <- function(input,output){
  
  library(tidyverse)
  library(maps)
  library(lubridate)
  
  #######
  #Loading in Crime data
  #######
  
  #Crime data in the U.S. by State (in units of 100,000)
  crimedata <- USArrests  %>% 
    mutate(State = state.abb) %>% 
    select(-UrbanPop)
  
  statedata <- state.x77 %>% as.data.frame() %>% select(-Murder)
  
  USdata <- bind_cols(crimedata, statedata)
  
  
  #Initializing map data
  mdata <- map_data("state") %>% mutate(region = str_to_title(region)) %>% 
    mutate(region = state.abb[match(region,state.name)])
  
  
  mdata <- mdata %>% 
    left_join(USdata, by = c("region"="State"))
  
  color_table <- data.frame(
    Type = c("Murder", "Assault", "Rape", "Population", 
             "Income", "Illiteracy", "Life Exp",
             "HS Grad", "Frost",   "Area"),
    Color = c("orange", "darkgreen", "red", "blue", "purple", "yellow", "brown", "tomato3", "black", "green")
  )
  
  color_vector <- as.vector(unlist(color_table[1]))
  
  color_match_1 <- reactive({
    color_table$Color[match(input$s1, color_vector)]
  })
  
  color_match_2 <- reactive({
    color_table$Color[match(input$s2, color_vector)]
  })
  
  
  rt <- list("Murder"= USdata$Murder, "Assault" = USdata$Assault, "Rape" = USdata$Rape)
  
   fill_1  <- reactive({
     as.numeric(unlist(mdata %>% select(toString(input$s1))))
   })
   
   fill_2  <- reactive({
     as.numeric(unlist(mdata %>% select(toString(input$s2))))
   })
  
   bar_1 <- reactive({
     as.numeric(unlist(USdata %>% select(toString(input$s1)))) %>% sort(decreasing = TRUE)
   })
  
   bar_2 <- reactive({
     as.numeric(unlist(USdata %>% select(toString(input$s2)))) %>% sort(decreasing = TRUE)
   })
   

 
  
  output$map1 <- renderPlot({
    ggplot(mdata, aes(x = long, y = lat, group=group)) +
      geom_polygon(color = "grey", aes(fill= fill_1())) +
      scale_fill_gradientn(colours = c("oldlace",color_match_1())) +
      labs(fill = input$s1) +
      theme_classic() +
      theme_void()
    
  })
  output$map2 <- renderPlot({
    ggplot(mdata, aes(x = long, y = lat, group=group)) +
      geom_polygon(color = "grey", aes(fill= fill_2())) +
      scale_fill_gradientn(colours = c("oldlace",color_match_2())) +
      labs(fill = input$s2) +
      theme_classic()  +
      theme_void()
    
  })
  output$plot1 <- renderPlot({
    ggplot(USdata %>% arrange(desc(!!as.name(input$s1))), aes(x = reorder(State,!!as.name(input$s1)), y = !!as.name(input$s1))) +
      geom_bar(color= "grey",stat = "identity", fill = color_match_1()) +
      xlab("State") +
      theme_classic()
    
  })
  output$plot2 <- renderPlot({
    ggplot(USdata %>% arrange(desc(!!as.name(input$s2))), aes(x = reorder(State,!!as.name(input$s2)), y = !!as.name(input$s2))) +
      geom_bar(color= "grey",stat = "identity", fill = color_match_2()) +
      xlab("State") +
      theme_classic()
    
  })
  
}

shinyApp(ui=ui,server=server)