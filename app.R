library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard)
library(DT)

data <- read_rds("gender_check.RDS") |> 
  filter(accuracy >90)|>
  select(gender, cihr_contribution, institution_paid, prc_name, year, term_years_months, accuracy) |> 
  arrange(year) |> 
  mutate(gender = factor(gender, levels = c("male", "female")),
         institution_paid = as.factor(institution_paid),
         prc_name = as.factor(prc_name),
         year = as.factor(year),
         term = as.factor(term_years_months),
         cihr_contribution = as.integer(gsub("\\$", "", gsub(",", "", cihr_contribution))))

html_string <- '
<div>
  THIS APPLICATION IS IN BETA
  <p>
  This application uses the publically available data on who and what was funded in the biannual
  <a href=https://webapps.cihr-irsc.gc.ca/decisions/p/main.html?lang=en#fq={!tag=programname2}programname2%3A%22Project%20Grant%22&sort=namesort%20asc&start=0&rows=20 target="_blank"> CIHR project grant competition</a>
  to show the gender split for funding.
  CIHR does list the female splits for their project grants on the analytics page for each competition 
  (e.g. <a href=https://cihr-irsc.gc.ca/e/53569.html target="_blank"> Spring 2023</a> and 
  <a href = https://cihr-irsc.gc.ca/e/53796.html target="_blank"> Fall 2023</a>) but do not provide a by review committee breakdown and its not all in one place.
  Unfortunately, CIHR does not yet include the self identified gender of the applicant in their public dataset. 
  So I have had to use machine learning to guess the gender. 
  I used the <a href=https://gender-api.com/ target="_blank"> Gender API service</a>, 
  and the <a href=https://platform.openai.com/docs/models/gpt-4-and-gpt-4-turbo target="_blank"> OpenAI GPT-4o Large Language Model</a>. 
  I have included applicants that were identified as the same gender by both models, 
  and that the Gender API model assigned a probability of 90% or over for its result. 
  Full details can be found on Github [LINK TO FOLLOW]. 
  </p>
  <p>
  PLEASE NOTE: AT PRESENT THE MODEL I USED CAN ONLY IDENTIFY TWO GENDERS (MALE AND FEMALE).
  </p>
  
  <p>TO FOLLOW:
  <ul>
    <li>Source code</li>
    <li>Figure showing funding by year</li>
    <li>Other Funding competitons</li>
  </ul>
</div>
'

ui <- ui <- dashboardPage(
  dashboardHeader(title = "CIHR_by_Gender"),
  dashboardSidebar(disable= T),
  dashboardBody(fluidRow(plotOutput("densityPlot", width = "97%", height = "300px")),
                br(),
                fluidRow(tabBox(tabPanel("Summary", 
                                         dataTableOutput("analysisTable"),
                                         dataTableOutput("diff")),
                                (tabPanel("Analysis", dataTableOutput("indepth")))),
                         (tabBox(tabPanel("Filter",
                                          selectInput("institution", "Select Institution:",
                                               choices = c("All", as.character(unique(data$institution_paid))),
                                               selected = "All"),
                              selectInput("prc_name", "Select PRC Name:",
                                               choices = c("All", as.character(unique(data$prc_name))),
                                               selected = "All"),
                              selectInput("year", "Select Year:",
                                          choices = c("All", as.character(unique(data$year))),
                                          selected = "All"),
                              selectInput("term", "Select Term:",
                                               choices = c("All", as.character(unique(data$term))),
                                               selected = "All"),
                              actionButton("reset", "Reset")),
                              tabPanel("Details",
                                       HTML(html_string))
                              )
                          )
                         )
                )
  )

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    # Reset select inputs to their default values
    updateSelectInput(session, "institution", selected = "All")
    updateSelectInput(session, "prc_name", selected = "All")
    updateSelectInput(session, "term", selected = "All")
    updateSelectInput(session, "year", selected = "All")
  })

  filtered_data <- reactive({
    filtered_data <- data 
    if (input$institution != "All") {
      filtered_data <- filtered_data %>% filter(institution_paid == input$institution)}
    if (input$prc_name != "All") {
      filtered_data <- filtered_data %>% filter(prc_name == input$prc_name)}
    if (input$term != "All") {
      filtered_data <- filtered_data %>% filter(term == input$term)}
    if (input$year != "All") {
      filtered_data <- filtered_data %>% filter(year == input$year)}
    filtered_data
  })
  
  output$densityPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = cihr_contribution, fill = gender)) +
      geom_density(alpha = 0.7) +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
      scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
      theme_classic() +
      labs(title = "Density Plot of CIHR Contribution by Gender",
           x = "CIHR Contribution",
           y = "Density",
           fill = "Gender")
  })
  
  output$analysisTable <- renderDataTable({
    filtered_data() |> 
      select(cihr_contribution, 
             Gender = gender) |> 
      group_by(Gender) |>
      mutate(num = n(),
             total = round(sum(cihr_contribution,na.rm = TRUE)/1000000,2),
             average = mean(cihr_contribution,na.rm = TRUE)/1000000,
             sd = sd(cihr_contribution,na.rm = TRUE)/1000000,
             average_sd = paste0(round(average,2), " (", round(sd,2),")"),
             Gender = str_to_sentence(Gender)) |> 
      ungroup() |> 
      distinct(Gender, 
               .keep_all = T) |> 
      select(Gender,
             'Number' = num,
             'Total (CAD$M)' = total, 
             'Average(SD) (CAD$M)' = average_sd) 
    
  },
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    info = FALSE,
    paging = FALSE)
  )
  
  output$diff <- renderDataTable({
    tryCatch({
      test <- t.test(filtered_data()$cihr_contribution ~factor(filtered_data()$gender, levels=c("male", "female")))
      md <- (test$estimate[1] - test$estimate[2])/1000000
      ci_l <-  test$conf.int[1]/1000000
      ci_u <-  test$conf.int[2]/1000000
      sum_difference = filtered_data() |>
        group_by(gender) |>
        summarise(sum(cihr_contribution))
      table <- tibble(
        "Difference Male-Female (CAD$M)" = round((sum_difference[[2]][[1]]-sum_difference[[2]][[2]])/1000000, 2),
        "Mean Difference Male-Female [95%CI] (CAD$M)" = paste0(round(md,2)," [",round(ci_l,2),",",round(ci_u,2),"]"))
      table
    }, error = function(e) {
      return(tibble(`Difference Male-Female (CAD$M)` = "Insufficient data", `Mean Difference Male-Female [95%CI] (CAD$M)` = "Insufficient data"))
    })
  },
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    info = FALSE,
    paging = FALSE)
  )
  
  output$indepth <- renderDataTable({
    tryCatch({
      filtered_data() |> 
        select(cihr_contribution, 
               Gender = gender,
               Institution = institution_paid,
               Commitee = prc_name, 
               Year = year) |> 
        group_by(Gender, Institution, Commitee, Year) |> 
        summarise('Number' = n(), 
                  'Total (CAD$M)' = sum(cihr_contribution,na.rm = TRUE)) |> 
        ungroup() |> 
        arrange(Institution, Commitee, Year) |> 
        mutate(Gender = str_to_sentence(Gender)) |> 
        pivot_wider(names_from = Gender, values_from = 'Total (CAD$M)')
    }, error = function(e) {
      return(tibble(`Insufficient data` = "Insufficient data for detailed analysis"))
    })
  })
}
shinyApp(ui = ui, server = server)
