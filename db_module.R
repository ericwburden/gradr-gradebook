## Utility Functions ----------------------------------------------------------
get_max_id <- function(pool, table) {
  max_id <- dbGetQuery(
    pool, 
    paste(
      "SELECT MAX(id) from",
      table
    )
  ) + 1
  
  max_id[[1]]
}

write_to_db <- function(data, pool, table) {
  
  text_cols <- sapply(data, is.character)
  data[, text_cols] <- paste0('\'', data[, text_cols], '\'')
  
  dbExecute(
    pool,
    paste(
      'insert into ',
      table,
      paste0(
        '(',
        paste(names(data), collapse = ', '),
        ')'
      ),
      'values',
      paste0(
        '(',
        do.call(paste, c(data, sep = ", ")),
        ')',
        collapse = ', '
      )
    )
  )
}

get_all_table <- function(pool, table) {
  dbGetQuery(
    pool,
    paste(
      'SELECT * from',
      table
    )
  )
}

get_filtered_table <- function(pool, table, filter_list) {
  filter_string <- purrr::map2_chr(
    filter_list$names, 
    filter_list$values, function(x,y) {
      paste(
        x,
        '=',
        {
          if (is.character(y)) {
            paste0('\'', y, '\'')
          } else {
            y
          }
        }
      )
    }
  ) %>%
    paste(collapse = ' and ')
  
  dbGetQuery(
    pool,
    paste(
      'select * from',
      table,
      'where',
      filter_string
    )
  )
}

## Named List Functions -------------------------------------------------------
courses_named_list <- function(pool) {
  courses <- get_all_table(pool, 'courses')
  if (nrow(courses) > 0) {
    course_choices <- setNames(
      courses$id, 
      paste0(
        courses$course_name, 
        ' (', 
        courses$course_semester, 
        ' ', 
        courses$course_year, 
        ', Section ', 
        courses$course_section, 
        ')'
      )
    )
  }
}

course_asscats_named_list <- function(pool, course_id) {
  asscats <- get_filtered_table(
    pool,
    'course_asscats',
    list(
      names = list('course_id'),
      values = list(course_id)
    )
  )
  
  
  if (nrow(asscats) > 0) {
    asscat_choices <- setNames(
      asscats$asscat_id,
      paste0(
        asscats$asscat_name,
        ' - ',
        asscats$asscat_weight,
        '%'
      )
    )
  }
}

course_assn_named_list <- function(pool, course_id, asscat_id) {
  assns <- get_filtered_table(
    pool,
    'course_assn',
    list(
      names = list('course_id', 'asscat_id'),
      values = list(course_id, asscat_id)
    )
  )
  
  
  if (nrow(assns) > 0) {
    assn_choices <- setNames(
      assns$assn_id,
      paste0(
        assns$assn_name,
        ' - ',
        format(assns$assn_due, '%m/%d/%Y')
      )
    )
  }
}

course_students_named_list <- function(pool, course_id) {
  students <- get_filtered_table(
    pool,
    'course_students',
    list(
      names = list('course_id'),
      values = list(course_id)
    )
  )
  
  
  if (nrow(students) > 0) {
    student_choices <- setNames(
      students$student_id,
      students$student_name
    )
  }
}

course_grades_named_list <- function(pool, course_id, assn_id, student_id) {
  grades <- get_filtered_table(
    pool,
    'grades',
    list(
      names = list('course_id', 'assn_id', 'student_id'),
      values = list(course_id, assn_id, student_id)
    )
  )
  
  
  if (nrow(grades) > 0) {
    grades$grade_score
  }
}

## Add Element Functions ------------------------------------------------------
add_asscat_to_course <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into course_asscats (course_id, asscat_id, asscat_name, asscat_weight)
      values (
        ', data$course_id, ',
        coalesce((select max(asscat_id) + 1 from course_asscats where course_id = ', data$course_id, '), 1),
        \'', data$asscat_name, '\',
        ', data$asscat_weight, '
      )'
    )
  )
}

add_assn_to_course <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into course_assn (course_id, asscat_id, assn_id, assn_name, assn_due, assn_notes)
      values (
        ', data$course_id, ',
        ', data$asscat_id, ',
        coalesce((select max(assn_id) + 1 from course_assn where course_id = ', data$course_id, '), 1),
        \'', data$assn_name, '\',
        \'', data$assn_due, '\',
        \'', data$assn_notes, '\'
      )'
    )
  )
}

add_student_to_course <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into course_students (course_id, student_id, student_name)
      values (
        ', data$course_id, ',
        coalesce((select max(student_id) + 1 from course_students where course_id = ', data$course_id, '), 1),
        \'', data$student_name, '\'
      )'
    )
  )
}

add_grade <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into grades (course_id, assn_id, student_id, grade_score, grade_notes)
      values (
        ', data$course_id, ',
        ', data$assn_id, ',
        ', data$student_id, ',
        \'', data$grade_score, '\',
        \'', data$grade_notes, '\'
      )'
    )
  )
}

## Specific Use Queries -------------------------------------------------------
grade_table <- function(pool, course_id) {
  dbGetQuery(
    pool, 
    paste(
      'select base.course_name,
  	    base.course_semester,
        base.course_year,
        asscat.asscat_name,
        asscat.asscat_weight,
        assn.assn_name,
        assn.assn_due,
        students.student_name,
        grades.grade_score
      from (
        select id,
        course_name,
        course_semester,
        course_year
        from courses
        where id = ', course_id,'
      ) base
      
      left join (
        select course_id,
        asscat_id,
        assn_id,
        assn_name,
        assn_due
        from course_assn
      ) assn
      on assn.course_id = id
      
      left join (
        select course_id,
        asscat_id,
        asscat_name,
        asscat_weight
        from course_asscats
      ) asscat
      on asscat.course_id = base.id
        and asscat.asscat_id = assn.asscat_id
      
      left join (
        select course_id,
        assn_id,
        student_id,
        grade_score
        from grades
      ) grades
      on grades.course_id = base.id
        and grades.assn_id = assn.assn_id
      
      left join (
        select course_id,
        student_id,
        student_name
        from course_students
      ) students
      on students.course_id = base.id
        and students.student_id = grades.student_id
      '
    )
  )
}