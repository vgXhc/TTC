#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(networkD3)
library(tidyverse)

joined <- readRDS("www/data/pre_and_post_joined.RDS")


title_list <- joined %>% 
  distinct(title.pre) %>% 
  arrange(title.pre) %>% 
  pull()
# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("From old titles to new: How were positions mapped during TTC?"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "titles_old",
            "Which pre-TTC titles do you want to look at? (just start typing)",
            title_list,
            multiple = T,
            selected = c("RESEARCHER", "ASST RESEARCHER", "ASSOC RESEARCHER")
          ),
          p("Job titles often (but not always) use abbreviations such as Sr for Senior, Acad for Academic or Re for Research. Use the slider below to limit the number of new titles shown to the most common combinations."),
          sliderInput(
            "top_n",
            "Limit combinations shown to",
            1,
            100,
            30,
            round = T
          ),
          p(a(img(src="img/UFAS_logo.png", width = '250px'), href = "http://ufas.wi.aft.org/"))
        , width = 3),

        # Show a plot of the generated distribution
        mainPanel(
           sankeyNetworkOutput("jobPlot"),
           p("The diagram shows how UW-Madison employees were mapped from their pre-TTC titles to new titles. Hover over the links or nodes to show counts. You can click and drag to move nodes."),
           p("Are you a member of", a("United Faculty & Academic Staff Local 223", href="http://ufas.wi.aft.org/join-union"), "yet? Without our union, we wouldn't have these data."),
           p("App development: Harald Kliems", a("@HaraldKliems", href="https://twitter.com/HaraldKliems")),
           width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$jobPlot <- renderSankeyNetwork({
      df <- joined %>% 
        filter(title.pre %in% input$titles_old) %>% 
        select(title.pre, title.post) %>% 
        mutate(title.pre = str_to_title(title.pre)) %>% 
        group_by(title.pre, title.post) %>% 
        summarize(n = n()) %>% 
        arrange(desc(n)) %>% 
        head(input$top_n) %>% 
        mutate(title.pre = fct_reorder(title.pre, n),
               title.post = as.factor(title.post)) %>% 
        as.data.frame()
      
      nodes <- data.frame(
        name=c(as.character(df$title.pre), as.character(df$title.post)) %>% 
          unique()
      )
      
      df$IDsource <- match(df$title.pre, nodes$name)-1 
      df$IDtarget <- match(df$title.post, nodes$name)-1
      
      sankeyNetwork(Links = df, Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "n", NodeID = "name", 
                    sinksRight=FALSE,
                    fontSize = 15)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
