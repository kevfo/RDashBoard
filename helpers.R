library(DBI) ; library(RSQLite)
library(tidyverse) ; library(shiny)

# ONLY CALL THIS FUNCTION ONCE AND OUT OF 
# SHINY APPLICATION!
write_table <- function(df, table_name = 'sex and money', db_name = 'data.db') {
  con <- dbConnect(SQLite(), dbname = db_name)
  dbWriteTable(con, table_name, df)
  dbDisconnect(con)
}

fetch_df <- function(db_name = 'data.db', table_name = 'sex and money') {
  con <- dbConnect(SQLite(), db_name)
  df <- con %>% dbReadTable(table_name)
  dbDisconnect(con)
  return(df)
}

get_avg_income <- function() {
  df <- fetch_df()
  df$Salary %>% mean() %>% round(2) %>% return()
}

get_genders <- function(db_name = 'data.db', table_name = 'sex and money', sex = 'Male') {
  con <- dbConnect(SQLite(), dbname = db_name)
  result <- con %>% dbGetQuery(paste0('SELECT * FROM ', 
                                      paste0('"', table_name, '"'), 
                                      ' WHERE Sex == ',
                                      paste0('"', str_to_title(sex), '"'))) %>% 
    nrow()
  dbDisconnect(con)
  return(result)
}

upload_manual_data <- function(db_name = 'data.db', table_name = 'sex and money', sex, salary) {
  con <- dbConnect(SQLite(), 'data.db')
  dbSendStatement(con, 
                  paste0('INSERT INTO ', paste0('"', table_name, '"'),
                         ' VALUES ',
                         paste0('(', salary, ', "', sex, '"', ')')))
  dbDisconnect(con)
}

upload_data <- function(db_name = 'data.db', table_name = 'sex and money', df) {
  tryCatch({
    con <- dbConnect(SQLite(), db_name) ; to_insert <- c()
    for (i in 1:nrow(df)) {
      to_insert <- append(to_insert, 
                          paste0('(', df[i, ]$Salary, ', "', df[i, ]$Sex, '"', ')'))
    }
    to_insert %>% str_flatten(', ') %>% paste0('INSERT INTO ', paste0('"', table_name, '"'),
                                               ' VALUES ', .) %>% 
      dbSendStatement(con, .)
  },
  error = function(e) {
    paste0('An error occured: ', e)
  },
  finally = dbDisconnect(con))
}

process_statement <- function(db_name = 'data.db', table_name = 'sex and money', statement) {
  tryCatch({
      con <- dbConnect(SQLite(), db_name)
      dbSendStatement(con, statement)
    },
    error = function(e) {
      message(paste0('An error occured: ', e))
    },
    finally = dbDisconnect(con)
  )
}
