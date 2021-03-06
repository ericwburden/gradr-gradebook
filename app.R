#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(pool)
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(DT)
library(purrr)
library(yaml)

source('db_module.R')
source('ui_module.R')

## UI Definition --------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
  ),
   
   # Application title
   titlePanel("Amy's Gradebook"),
   
   # Main application nav
   navlistPanel(
     id = 'main_nav',
     widths = c(2, 10),
     tabPanel(
       'Course Management',
       
       # tabPanel content
       fluidRow(
         
         # column for course controls
         column(
           width = 4,
           
           ## Course Controls -------------------------------------------------
           control_module('Courses', 'course'),
           
           ## Assignment Categories Controls ----------------------------------
           control_module('Assignment Categories', 'asscat')
         ),
         
         # column for student and assignment controls
         column(
           width = 4,
           
           ## Student Controls ------------------------------------------------
           control_module('Students', 'student'),
           
           ## Assignments Controls --------------------------------------------
           control_module('Assignments', 'assn')
         ),
         
         # column for grade controls
         column(
           width = 4,
           
           ## Grade Controls --------------------------------------------------
           control_module('Grades', 'grade')
         )
       )
     ),
     tabPanel(
       'Grade Explorer',
       DT::dataTableOutput('grade_display_table')
     )
   )
)

## +++++ ----------------------------------------------------------------------

## Server Function ------------------------------------------------------------
server <- function(input, output, session) {
  
  # create a connection pool
  # save the password that we can "hide" it as best as we can by collapsing it
 db_info <- yaml.load_file('config/db.yaml')
  
  pool <- dbPool(
    drv = dbDriver("PostgreSQL"),
    dbname = db_info$db,
    host = db_info$host, 
    port = db_info$port,
    user = db_info$user, 
    password = db_info$pw
  )
  
  rm(db_info) # removes the db info
  
  ## Render list groups -------------------------------------------------------
  ## Render Courses List
  output$course_list <- renderUI({
    radioGroupButtons(
      'select_course',
      choices = courses_named_list(pool),
      direction = 'vertical',
      justified = FALSE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  })
  
  ## Render Assignment Categories List
  output$asscat_list <- renderUI({
    ## Update when you add an assignment category
    input$add_asscat_ok
    
    if (!is.null(input$select_course)) {
      asscat_named_list <- course_asscats_named_list(pool, input$select_course)
      
      if (!is.null(asscat_named_list)) {
        radioGroupButtons(
          'select_asscat',
          choices = asscat_named_list,
          direction = 'vertical',
          justified = FALSE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )
      } else {
        h4('No assignment categories for this course.')
      }
    }
  })
  
  ## Render Assignment List
  output$assn_list <- renderUI({
    ## Update when you add an assignment
    input$add_assn_ok
    
    if (!is.null(input$select_course) & !is.null(input$select_asscat)) {
      assn_named_list <- course_assn_named_list(
        pool, 
        input$select_course, 
        input$select_asscat
      )
      
      if (!is.null(assn_named_list)) {
        radioGroupButtons(
          'select_assn',
          choices = assn_named_list,
          direction = 'vertical',
          justified = FALSE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )
      } else {
        h4('No assignments of this category for this course.')
      }
    }
  })
  
  ## Render Student List
  output$student_list <- renderUI({
    ## Update when you add a student
    input$add_student_ok
    
    if (!is.null(input$select_course)) {
      student_named_list <- course_students_named_list(
        pool, 
        input$select_course
      )
      
      if (!is.null(student_named_list)) {
        radioGroupButtons(
          'select_student',
          choices = student_named_list,
          direction = 'vertical',
          justified = FALSE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )
      } else {
        h4('No students for this course.')
      }
    }
  })
  
  ## Render Grade List
  output$grade_list <- renderUI({
    ## Update when you add a grade
    input$add_grade_ok
    
    if (
      !is.null(input$select_course) & 
      !is.null(input$select_assn) & 
      !is.null(input$select_student)
    ) {
      grade_named_list <- course_grades_named_list(
        pool, 
        input$select_course,
        input$select_assn,
        input$select_student
      )
      
      if (!is.null(grade_named_list)) {
        radioGroupButtons(
          'select_grade',
          choices = grade_named_list,
          direction = 'vertical',
          justified = FALSE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )
      } else {
        h4('No grades for this combination of course, assignment, and student.')
      }
    }
  })
  
  ## Add Button Listeners -----------------------------------------------------
  ## Add Course
  observeEvent(input$add_course, {
    showModal(add_course_modal)
  })
  
  observeEvent(input$add_course_ok, {
    new_course <- data.frame(
      'course_name' = as.character(input$add_course_name),
      'course_semester' = as.character(input$add_course_semester),
      'course_year' = as.integer(input$add_course_year),
      'course_section' = as.character(input$add_course_section),
      'course_description' = as.character(input$add_course_description),
      stringsAsFactors = F
    )
    
    write_to_db(new_course, pool, 'courses')
    
    removeModal()
  })
  
  ## Add Assignment Category
  observeEvent(input$add_asscat, {
    showModal(add_asscat_modal(pool))
  })

  observeEvent(input$add_asscat_ok, {
  
    new_asscat <- data.frame(
      'course_id' = as.integer(input$select_course),
      'asscat_name' = as.character(input$add_asscat_name),
      'asscat_weight' = as.integer(input$add_asscat_weight),
      stringsAsFactors = F
    )

    add_asscat_to_course(pool, new_asscat)

    removeModal()
  })
  
  ## Add Assignment
  observeEvent(input$add_assn, {
    showModal(add_assn_modal(pool, input))
  })
  
  observeEvent(input$add_assn_ok, {
    
    new_assn <- data.frame(
      'course_id' = as.integer(input$select_course),
      'asscat_id' = as.integer(input$select_asscat),
      'assn_name' = as.character(input$add_assn_name),
      'assn_due' = as.Date(input$add_assn_due),
      'assn_notes' = as.character(input$add_assn_notes),
      stringsAsFactors = F
    )
    
    add_assn_to_course(pool, new_assn)
    
    removeModal()
  })
  
  ## Add Student
  observeEvent(input$add_student, {
    showModal(add_student_modal(pool))
  })
  
  observeEvent(input$add_student_ok, {
    
    new_student <- data.frame(
      'course_id' = as.integer(input$select_course),
      'student_name' = as.character(input$add_student_name),
      stringsAsFactors = F
    )
    
    add_student_to_course(pool, new_student)
    
    removeModal()
  })
  
  ## Add Grade
  observeEvent(input$add_grade, {
    showModal(add_grade_modal(pool, input))
  })
  
  observeEvent(input$add_grade_ok, {
    
    new_grade <- data.frame(
      'course_id' = as.integer(input$select_course),
      'assn_id' = as.integer(input$select_assn),
      'student_id' = as.integer(input$select_student),
      'grade_score' = as.integer(input$add_grade_score),
      'grade_notes' = as.character(input$add_grade_notes),
      stringsAsFactors = F
    )
    
    add_grade(pool, new_grade)
    
    removeModal()
  })
  
  ## Render Data Tables -------------------------------------------------------
  output$grade_display_table <- DT::renderDataTable({
    grade_table_data <- grade_table(pool, input$select_course)
    
    total_weight <- grade_table_data %>%
      group_by(asscat_name, asscat_weight) %>%
      summarise() %>%
      ungroup() %>%
      select(asscat_weight) %>%
      sum()
    
    final_grades <- grade_table_data %>%
      mutate(mod_score = asscat_weight * grade_score) %>%
      group_by(student_name, asscat_name) %>%
      summarise(avg = mean(mod_score)) %>%
      group_by(student_name) %>%
      summarise(agg_score = sum(avg)) %>%
      mutate(final_score = round((agg_score/total_weight), 2)) %>%
      select(student_name, final_score)
      
  })
  
  ## Pool Cleanup -------------------------------------------------------------
  session$onSessionEnded(function() {
    poolClose(pool)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

