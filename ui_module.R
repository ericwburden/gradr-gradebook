# MODALS ----------------------------------------------------------------------
## add_course_modal -----------------------------------------------------------
add_course_modal <-  modalDialog(
  fluidRow(
    column(
      width = 6,
      textInput(
        'add_course_name',
        'Course Name'
      )
    ),
    column(
      width = 6,
      radioButtons(
        'add_course_semester',
        'Semester',
        choices = c('Fall', 'Spring', 'Summer')
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      textInput(
        'add_course_section',
        'Section'
      )
    ),
    column(
      width = 6,
      textInput(
        'add_course_year',
        'Year'
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      textAreaInput(
        'add_course_description',
        'Description'
      )
    )
  ),
  
  footer = tagList(
    modalButton("Cancel"),
    actionButton("add_course_ok", "OK")
  )
)

## add_asscat_modal -----------------------------------------------------------
add_asscat_modal <- function(pool) {
  modalDialog(
    fluidRow(
      column(
        width = 12,
        textInput(
          'add_asscat_name',
          'Assignment Category Name'
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        sliderInput(
          'add_asscat_weight',
          'Weight',
          min = 0, max = 100, value = 15, step = 5, round = T
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("add_asscat_ok", "OK")
    )
  )
}  

## add_assn_modal -------------------------------------------------------------
add_assn_modal <- function(pool, input) {

  modalDialog(
    fluidRow(
      column(
        width = 6,
        h3('Course'),
        p(input$select_course)
      ),
      column(
        width = 6,
        h3('Category'),
        p(input$select_asscat)
      )
    ),
    fluidRow(
      column(
        width = 6,
        textInput(
          'add_assn_name',
          'Assignment Name'
        )
      ),
      column(
        width = 6,
        dateInput(
          'add_assn_due',
          'Due Date'
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        textAreaInput(
          'add_assn_notes',
          'Assignment Notes'
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("add_assn_ok", "OK")
    )
  )
} 

## add_grade_modal -------------------------------------------------------------
add_grade_modal <- function(pool, input) {
  
  modalDialog(
    fluidRow(
      column(
        width = 4,
        h3('Course'),
        p(input$select_course)
      ),
      column(
        width = 4,
        h3('Assignment'),
        p(input$select_assn)
      ),
      column(
        width = 4,
        h3('Student'),
        p(input$select_student)
      )
    ),
    fluidRow(
      column(
        width = 12,
        sliderInput(
          'add_grade_score',
          'Grade',
          min = 0, max = 100, step = 1, value = 100
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        textAreaInput(
          'add_grade_notes',
          'Grade Notes'
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("add_grade_ok", "OK")
    )
  )
} 

## add_student_modal ----------------------------------------------------------
add_student_modal <- function(pool) {
  modalDialog(
    fluidRow(
      column(
        width = 12,
        textInput(
          'add_student_name',
          'Student Name'
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("add_student_ok", "OK")
    )
  )
} 

# CONTROLS --------------------------------------------------------------------
control_module <- function(title, type) {
  fluidRow(
    fluidRow(
      column(
        width = 12,
        h3(title)
      )
    ),
    column(
      width = 3,
      actionButton(
        paste0('add_', type), 
        '', 
        icon = icon('plus'), 
        class = 'btn btn-primary btn-lg', 
        style = 'margin: 5px 5px;'
      ),
      actionButton(
        paste0('mod_', type), 
        '', 
        icon = icon('pencil'), 
        class = 'btn btn-success btn-lg', 
        style = 'margin: 5px 5px;'
      ),
      actionButton(
        paste0('del_', type), 
        '', 
        icon = icon('trash'), 
        class = 'btn btn-danger btn-lg', 
        style = 'margin: 5px 5px;'
      )
    ),
    column(
      width = 9,
      uiOutput(paste0(type, '_list'))
    )
  )
}