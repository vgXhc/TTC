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
library(lubridate)

  ttc <- read_xlsx("Updated March 2022 All Faculty and Staff Title and Salary Information.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(full_time_equivalent > 0.01 & current_annual_contracted_salary > 1000) |> 
    mutate(date_of_hire = ymd(date_of_hire),
           length_tenure = ymd("2022-03-01") - date_of_hire, 
           length_tenure_fct = case_when(length_tenure < 365 ~ "less than 1 year",
                                         length_tenure >= 365 & length_tenure <= 1095 ~ "1-3 years",
                                         length_tenure > 1095 & length_tenure <= 1825 ~ "3-5 years",
                                         length_tenure > 1825 & length_tenure <= 3650 ~ "5-10 years",
                                         length_tenure > 3650 ~ "more than 10 years"),
           length_tenure_fct = forcats::fct(length_tenure_fct, levels = c("less than 1 year",
                                                                          "1-3 years",
                                                                          "3-5 years",
                                                                          "5-10 years",
                                                                          "more than 10 years")))

salary_ranges <- readRDS("www/salary_ranges_jan2024.RDS")

ttc <- ttc %>% 
  left_join(salary_ranges, by = "salary_grade")

divisions <- ttc %>% arrange(division) %>% distinct(division) %>% pull()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How does my salary compare"),
    
    fluidRow(
      column(10, offset = 1,
             p(
               em("This app will be retired by April 1, 2024. Please update your bookmarks to", a("https://ufas223.github.io/salaries/", href = "https://ufas223.github.io/salaries/"))
             ))
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput(
            "last_name_input",
            "Enter your last name",
            value = "Cramer"
          ),
          textInput(
            "first_name_input",
            "Enter your first name",
            value = "Robert"),
          selectInput(
            "division_input",
            "Your school/division",
            divisions,
            multiple = F,
            selected = "General Services"
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
           p(br(),"On top, the plot shows the actual salaries of all other employees (light grey dots) that have the same job title as you. The thick black line is the median; the box represents the range from the 25th to 75th percentile. You can either compare salaries in the same title across campus or only within your school/division.", 
             br(), 
             "In the lower part of the plot, you see the salary range for your job, represented by the arrow, again with your salary as a red dot. Not all job titles have a maximum salary."),
           p(br(),"If you have multiple appointments, only one will be shown at a time. Honorary/0% appointments are excluded."),
           p("Are you a member of", a("United Faculty & Academic Staff Local 223", href="http://ufas.wi.aft.org/join-union"), "yet? Without our union, we wouldn't have these data."),
           p("Salary data last updated: August 2022",
             br(),
             "Salary ranges last updated: January 24, 2024"),
           p("App development: Harald Kliems", a("@HaraldKliems", href="https://fosstodon.org/@haraldkliems"))
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
        geom_boxplot(size = 1, outlier.shape = NA, width = .5, alpha = .1) +
        geom_jitter(aes(color = length_tenure_fct), height = 0.1, alpha = 1, size = 1.5) +
        scale_color_viridis_d(name = "time since first hire") +
        geom_segment(aes(x = min_salary, xend = max_salary, y = 0.6, yend = 0.6), size = 2, arrow = arrow()) +
        geom_text(aes(label = paste0("Min salary \n", title), x = min_salary, y = 0.5), hjust = 0)+
        geom_text(aes(label = paste0("Max salary \n", title), x = max_salary, y = 0.5), hjust = 1)+
        scale_x_continuous(labels=scales::dollar_format()) +
        geom_point(data = ttc %>% filter(last_name == my_last_name & first_name == my_first_name & division == my_division), color = "red", size = 5) +
        geom_text(data = ttc %>% filter(last_name == my_last_name & first_name == my_first_name & division == my_division), label = "you", nudge_y = .13, color = "red", size = 5) +
        labs(title = "Your salary, compared to all salaries and the salary range in your job title", 
             #subtitle = "The box plot shows the median (bar), interquartile range (box), 1.5 * the IQR (whiskers), and outliers (black dots)",
             x = "Annual salary (adjusted for FTE)"
             ) +
        theme_minimal() +
        geom_point(aes(min_salary, y = .6), size = 3) +
        #geom_point(aes(max_salary, y = .6), size = 3) +
        geom_point(data = ttc %>% filter(last_name == my_last_name & first_name == my_first_name & division == my_division), aes(current_annual_contracted_salary, y = 0.6), color = "red", size = 3) +
        theme(axis.title.y = element_blank())

        
#        geom_point(aes(max_salary, title), color = "green", size = 3)
      
      # geom_segment(x = -2, y = 1,
      #              xend = 1, yend = -1,
      #              color = 2,
      #              arrow = arrow())
      #   
    })
    output$positionSummary <- renderText({
      input$goButton
      ttc_filtered <- isolate(ttc %>% filter(last_name == str_to_upper(input$last_name_input) & first_name == str_to_upper(input$first_name_input) & division == input$division_input))
      my_title <- ttc_filtered %>% pull(title)
      my_salary <- ttc_filtered %>% pull(current_annual_contracted_salary)
      my_division <- ttc_filtered %>% pull(division)
      my_department <- ttc_filtered %>% pull(department)
      my_title_min <- ttc_filtered %>% pull(min_salary)
      my_title_max <- ttc_filtered %>% pull(max_salary)
      
      my_title_max_formatted <- if_else(is.na(my_title_max), 
                                        "; there is no maximum salary for your title. ", 
                                        paste0("; the maximum salary for your title is ", 
                                               priceR::format_dollars(my_title_max),
                                               ". Your current salary is at ",
                                               round(my_salary/my_title_max, 1)*100,
                                               "% of your title's max salary."),)
      

      
      paste0("Your title is ",
             my_title, 
             " in the ", 
             my_division, 
             "'s Department of ", 
             my_department, 
             ".\nYour annual salary (adjusted for FTE) is ", 
             priceR::format_dollars(my_salary), ".", "The minimum salary of your title is ", 
             priceR::format_dollars(my_title_min),
             my_title_max_formatted)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
