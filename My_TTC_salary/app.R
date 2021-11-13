#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

ttc <- read_xlsx("All Faculty and Staff Title and Salary Information - Post-TTC and Pre-TTC.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(full_time_equivalent > 0.01 & current_annual_contracted_salary > 1000)

divisions <- ttc %>% arrange(division) %>% distinct(division) %>% pull()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How does my salary compare"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput(
            "last_name_input",
            "Enter your last name",
            value = "Kliems"
          ),
          textInput(
            "first_name_input",
            "Enter your first name",
            value = "Harald"),
          selectInput(
            "division_input",
            "Your school/division",
            divisions,
            multiple = F,
            selected = "Sch of Med & Public Health"
          ),
          radioButtons(
            "comparison_select",
            "Compare across all campus or within school/division?",
            c("Across campus" = "campus",
              "Within school/division" = "school")
          ),
          actionButton("goButton", "Submit"),
          p(),
          p(a(img(src="img/UFAS_logo.png", width = '90%'), href = "http://ufas.wi.aft.org/"))
          
          ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("salaryPlot"),
           textOutput("positionSummary"),
           p(br(),"The plot shows the actual salaries of all other employees (light grey dots) that have the same job title as you. You can either compare salaries in the same title across campus or only within your school/division. The bar represents the median salary; the box shows the interquartile range (25th-75th percentile); and the whiskers represent 1.75 times the interquartile range. The chart does not include the TTC salary range (i.e. the min/max salary) for your job title."),
           p(br(),"If you have multiple appointments, only one will be shown at a time. Honorary/0% appointments are excluded."),
           p("Are you a member of", a("United Faculty & Academic Staff Local 223", href="http://ufas.wi.aft.org/join-union"), "yet? Without our union, we wouldn't have these data."),
           p("App development: Harald Kliems", a("@HaraldKliems", href="https://twitter.com/HaraldKliems"))
        )
    )
)

# Define server logic 
server <- function(input, output) {


    output$salaryPlot <- renderPlot({
      input$goButton
      my_last_name <- isolate(str_to_upper(input$last_name_input))
      my_first_name <- isolate(str_to_upper(input$first_name_input))
      my_division <- isolate(input$division_input)
      my_title <- ttc %>% filter(last_name == my_last_name & first_name == my_first_name & division == my_division) %>% pull(title)
      
      salaries <- reactive({
        salary_filtered <- ttc %>% filter(title == my_title)
        if(input$comparison_select == "campus")
          salary_filtered
        if(input$comparison_select == "school")
          salary_filtered <- salary_filtered %>% filter(division == my_division)
        
        salary_filtered
      }
        
      )
      salaries <- salaries() %>% filter(title == my_title)
      ggplot(salaries, aes(current_annual_contracted_salary, title)) +
        geom_boxplot(size = 1, outlier.shape = NA, alpha = .1) +
        geom_jitter(height = 0.1, alpha = .3) +
        scale_x_continuous(labels=scales::dollar_format()) +
        geom_point(data = ttc %>% filter(last_name == my_last_name & first_name == my_first_name & division == my_division), color = "red", size = 5) +
        geom_text(data = ttc %>% filter(last_name == my_last_name & first_name == my_first_name & division == my_division), label = "you", nudge_y = .13, color = "red", size = 5) +
        labs(title = "Your current salary, compared to all salaries in your job title.", 
             #subtitle = "The box plot shows the median (bar), interquartile range (box), 1.5 * the IQR (whiskers), and outliers (black dots)",
             x = "Annual salary (adjusted for FTE)"
             ) +
        theme_minimal() +
        theme(axis.title.y = element_blank())
        # generate bins based on input$bins from ui.R
        
    })
    output$positionSummary <- renderText({
      input$goButton
      ttc_filtered <- isolate(ttc %>% filter(last_name == str_to_upper(input$last_name_input) & first_name == str_to_upper(input$first_name_input) & division == input$division_input))
      my_title <- ttc_filtered %>% pull(title)
      my_salary <- ttc_filtered %>% pull(current_annual_contracted_salary)
      my_division <- ttc_filtered %>% pull(division)
      my_department <- ttc_filtered %>% pull(department)
      paste0("Your title is ", my_title, " in the ", my_division, "'s Department of ", my_department, ".\nYour annual salary (adjusted for FTE) is $", my_salary, ".")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
