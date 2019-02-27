packages <- c('shiny','shinydashboard','shinythemes','shinyjs','highcharter','data.table','reshape2','ggplot2','stringr','plotly','tools','tidyr', 'scales',
              'readr','dplyr','plyr','rowr','DT','V8','tibble','gsubfn','lubridate','shinyWidgets','leaflet','openintro','stringr','RMariaDB','odbc','dbplyr','sp')
lapply(packages,library,character.only=TRUE)

con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  username = 'dev',
  password = 'dev', 
  host = '127.0.0.1',
  dbname = 'paction_db_qa',
  port = 3306
)

#Valid Awards
myquery <- "SELECT customer_id, award_short_description, CONCAT(UCASE(SUBSTRING(gender,1,1)),LOWER(SUBSTRING(gender,2))) AS gender, age, date(created_at) AS date, CASE WHEN hour(created_at) > 17 THEN 'Evening' ELSE (CASE WHEN hour(created_at) > 11 THEN 'Afternoon' ELSE (CASE WHEN hour(created_at) > 5 THEN 'Morning' ELSE 'Night' END) END) END as hour, COUNT(award_short_description) AS count1,
            UPPER(SUBSTRING(postal_code,1,1)) as province_code
            FROM (SELECT * FROM participaction_core_customeraward A
            INNER JOIN (SELECT _uid AS award_id2,award_type,award_code,sequence_number,start_timestamp,end_timestamp,days_valid,num_ballots,program_id,earn_multiple_allowed,award_class,minutes_required FROM participaction_core_award) B ON B.award_id2 = A.award_id
            INNER JOIN (SELECT master_id,id AS award_num,language_code,name AS award_name,short_description AS award_short_description,long_description AS award_long_description FROM participaction_core_award_translation WHERE language_code = 'en') C on C.master_id = A.award_id
            INNER JOIN (SELECT _uid AS customer_id2,postal_code,gender,age FROM participaction_core_customer) D on A.customer_id = D.customer_id2) X
            GROUP BY customer_id, award_short_description, gender, age, date, hour, province_code;"
            #LEFT JOIN (SELECT * FROM participaction_core_customerawardballot) D on D.customer_award_id = A._uid
            #LIMIT 1000;'
awards_data <- dbGetQuery(con,myquery)

provinces



#aaa <- set.DT(dbGetQuery(con, "SELECT * FROM participaction_core_award"))
#limit 3")