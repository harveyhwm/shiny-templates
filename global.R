#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LOAD PACKAGES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
packages <- c('shiny','shinydashboard','shinythemes','shinyjs','highcharter','data.table','reshape2','ggplot2','stringr','plotly','tools','tidyr', 'scales',
              'readr','dplyr','plyr','rowr','DT','V8','tibble','gsubfn','lubridate','shinyWidgets','leaflet','openintro','stringr','RMariaDB','odbc','dbplyr','sp')
lapply(packages,library,character.only=TRUE)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GTM
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jsCode <- paste0("shinyjs.init = function(){(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                 j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-W6CQG8K');}")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SQL DATABASE CONNECTION
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  username = 'dev',
  password = 'dev', 
  host = '127.0.0.1',
  dbname = 'paction_db_qa',
  port = 3306
)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  HELPER FUNCTIONS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titlec <- function(x){
  s <- strsplit(x, ' ')[[1]]
  paste(toupper(substring(s,1,1)),tolower(substring(s, 2)),sep='',collapse=' ')
}

default_rows <- function(x){
  if(is.null(x)){x <- 15}
  x
}

calcDiff <- function(x,y){
  if(is.infinite(x)||is.infinite(y)||is.nan(x)||is.nan(y)||is.na(x)||is.na(y)||y==0){y=0}
  ifelse(y==0,na_val,paste0(format(round(100*(x-y)/y,2),nsmall=2),"%"))  #paste0(round(100*(x-y)/y,2),"%")) #percent_format()((x-y)/y))
}

getDateRange <- function(r,p='None'){
  if(grepl('^(m|q|y)',r)){
    m <- ifelse(grepl('^m',r),month(Sys.Date()),ifelse(grepl('^q',r),(3*quarter(Sys.Date()))-2,'01'))
    o <- ifelse(grepl('^m',r),1,ifelse(grepl('^q',r),3,12))
    date1 <- as.Date(paste(year(Sys.Date()),m,'01',sep='-')) %m-% months(ifelse(grepl('p$',r),o,0))
    date2 <- date1 %m+% months(o)-1
    date3 <- date1 %m-% months(ifelse(grepl('^(N|P)',p),o,12))
    date4 <- date3 %m+% months(o)-1
  }
  else if(grepl('^w',r)){
    date1 <- as.Date(Sys.Date()+1-as.POSIXlt(Sys.Date())$wday-ifelse(grepl('p$',r),7,0))
    date2 <- date1+6
    date3 <- (date1-ifelse(grepl('^(N|P)',p),7,0)) %m-% years(ifelse(grepl('^(N|P)',p),0,1))
    date4 <- date3+6
  }
  else if(grepl('[0-9]{1}',r)){
    date1 <- Sys.Date() - as.numeric(r)
    date2 <- Sys.Date()-1
    date3 <- (date1 - ifelse(grepl('^(N|P)',p),as.numeric(r),0)) %m-% years(ifelse(grepl('^(N|P)',p),0,1))
    date4 <- date3+as.numeric(r)-1
  }
  c(date1,date2,date3,date4)
}

roundval <- function(x,n){
  print(x)
  if(x==na_val||is.na(x)||is.infinite(x)||is.nan(x)){x=0}
  round(x,n)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GLOBAL CONSTANTS & DEFINITIONS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
min_date <- as.Date('2018-11-01')
max_date <- Sys.Date()
na_val <- '--'

periods <- c('This Year'='y','Last Year'='yp','This Quarter'='q','Last Quarter'='qp','This Month'='m','Last Month'='mp','This Week'='w','Last Week'='wp',
             'Last 90 Days'='90','Last 30 Days'='30','Last 14 Days'='14','Last 7 Days'='7','Custom')

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  READ IN DATA
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mypath = '' #/Users/harveymanhood/Documents/clients/participaction/kookaburra/'
province_data <- fread(paste0(mypath,'data/provinces.csv'))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  QUERIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myquery_awards <- "SELECT customer_id, award_short_description, CONCAT(UCASE(SUBSTRING(gender,1,1)),LOWER(SUBSTRING(gender,2))) AS gender,
                  CASE WHEN age < 5 THEN '0-4' ELSE (CASE WHEN age < 18 THEN '5-17' ELSE (CASE WHEN age < 25 THEN '18-24' ELSE (CASE WHEN age < 35 THEN '25-34' ELSE (CASE WHEN age < 45 THEN '35-44' ELSE (CASE WHEN age < 55 THEN '45-54' ELSE (CASE WHEN age < 65 THEN '55-64' ELSE '65+' END) END) END) END) END) END) END AS age,
                  CONCAT('Q',QUARTER(created_at),' ',YEAR(created_at)) as quarter, CONCAT(SUBSTRING(MONTHNAME(created_at),1,3),' ',YEAR(created_at)) as month, DATE(created_at) AS date, CASE WHEN hour(created_at) > 17 THEN 'Evening' ELSE (CASE WHEN hour(created_at) > 11 THEN 'Afternoon' ELSE (CASE WHEN hour(created_at) > 5 THEN 'Morning' ELSE 'Night' END) END) END as hour, COUNT(award_short_description) AS count,
                  UPPER(SUBSTRING(postal_code,1,1)) as province_postcode
                  FROM (SELECT * FROM participaction_core_customeraward A
                  INNER JOIN (SELECT _uid AS award_id2,award_type,award_code,sequence_number,start_timestamp,end_timestamp,days_valid,num_ballots,program_id,earn_multiple_allowed,award_class,minutes_required FROM participaction_core_award) B ON B.award_id2 = A.award_id
                  INNER JOIN (SELECT master_id,id AS award_num,language_code,name AS award_name,short_description AS award_short_description,long_description AS award_long_description FROM participaction_core_award_translation WHERE language_code = 'en') C on C.master_id = A.award_id
                  INNER JOIN (SELECT _uid AS customer_id2,postal_code,gender,age FROM participaction_core_customer) D on A.customer_id = D.customer_id2) X
                  GROUP BY customer_id, award_short_description, gender, age, quarter, month, date, hour, province_postcode;"
                  #LIMIT 1000;'
awards_data <- dbGetQuery(con,myquery_awards)
awards_data <- merge(awards_data,province_data,by='province_postcode',all.x=T)

provinces <- unique(awards_data$Province)
unique_users <- len(unique(awards_data$customer_id))
total_awards <- sum(awards_data$count)

prepare_awards_new <- function(d,a,f,dates){
  d$date <- as.Date(d$date)
  print(dates[1])
  print(dates[2])
  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
  for(i in 1:(len(f)/2)){
    if(!grepl('^All',f[2*i])){
      d <- eval(parse(text=paste0('d[d$`',f[(2*i)-1],'` == "',f[2*i],'",]'))) #names(d),with=F]')))
    }
  }  
  s <- setDT(filter(d,d$award_short_description %in% a))
  s1 <- s[,c('award_short_description','count')][,lapply(.SD,sum),by=c('award_short_description')]
  s2 <- s[,c('customer_id','award_short_description')]
  s2$unique <- 1
  s2 <- s2[,c('award_short_description','unique')][,lapply(.SD,sum),by=c('award_short_description')]
  s <- merge(s1,s2,by='award_short_description',all.x=T)
  s$`% Total` <- paste0(format(round(100*s$count/total_awards,2),nsmall=2),"%")
  s$`% Uniques` <- paste0(format(round(100*s$unique/unique_users,2),nsmall=2),"%")
  setnames(s,old=c('award_short_description','count','unique'),new=c('Award Description','Total','Unique'))
  datatable(s,options=list(pageLength=default_rows(50)),escape=F,rownames=F)
}

prepare_awards_chart <- function(d){
  d <- setDT(d)
  d <- d[,c('date','count')][,lapply(.SD,sum),by = c('date')]
  highchart() %>% 
  hc_chart(type = 'spline') %>% #,color = '#FF0000') %>%
  hc_subtitle(enabled = F) %>%
  hc_legend(enabled = F) %>% #, align = "left", verticalAlign = "top") %>%
  hc_xAxis(categories = c(d$date), labels = list(rotation = 0, style = list(fontSize= '8px')), tickInterval = 2) %>% 
  hc_yAxis(opposite=T,labels=list(align = 'left')) %>%
  hc_plotOptions(
    dataLabels = list(enabled = F),
    marker = list(enabled = F)
  ) %>% 
  hc_series(
    list(
      name = 'Badges Earned',
      data = round(as.numeric(d$count),2),
      color = '#FF5588'
    )
  )
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LAYOUT
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
badges <- tabPanel('Badges',
  mainPanel(
    fluidRow(
     tags$h2('Summary of Badges Earned')
    ),
    fluidRow(
      tags$div(class='table_holder',
        tags$h3(class='h3-float h3-text h3-first','Streak Badges'),
        tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_1')),
        tags$h3(class='h3-float h3-text h3-first','Action Badges'),
        tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_2')),
        tags$h3(class='h3-float h3-text h3-first','Milestone Badges'),
        tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_3'))
      ),
      tags$div(class='chart_holder',
        tags$h3(class='h3-float h3-text h3-first','Badges Earned: All Badge Types'),
        tags$div(class='chart_top',highchartOutput('badges_chart_1'))
      )
    )
  )
)

rewards <- tabPanel('Rewards',
  mainPanel(
    fluidRow(
      tags$h2('Summary of Rewards Claimed')
    )
  )
)

activity_report <- tabPanel('Activity Report',
  mainPanel(
    fluidRow(
      tags$h2('Activity Level')
    )
  )
)

app_report <- tabPanel('App Behavior',
  mainPanel(
    fluidRow(
      tags$h2('App Behavior Report')
    )
  )
)

geo_report <- tabPanel('Geo Data',
  mainPanel(
    fluidRow(
      tags$h2('If we have time :)')
    )
  )
)