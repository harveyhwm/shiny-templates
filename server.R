server <- function(input,output,session){
  # BADGES TABLES #
  output$badges_table_1 <- DT::renderDataTable({prepare_awards(awards_data,c('Weekly Challenge Complete','2-Week Streak','4-Week Streak'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)}) #'date',input$date,
  output$badges_table_2 <- DT::renderDataTable({prepare_awards(awards_data,c('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  output$badges_table_3 <- DT::renderDataTable({prepare_awards(awards_data,c('50 Minutes Milestone','100 Minutes Milestone','150 Minute Milestone','200 Minute Milestone','250 Minute Milestone','300 Minutes Milestone','350 Minute Milestone','400 Minute Milestone','450 Minute Milestone','500 Minute Milestone','550 Minutes Milestone','600 Minute Milestone','650 Minute Milestone','700 Minute Milestone','750 Minute Milestone','800 Minute Milestone','850 Minute Milestone','900 Minute Milestone','950 Minute Milestone'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  output$badges_table_4 <- DT::renderDataTable({prepare_badge_count(awards_data)})

  # APP TABLES #
  output$app_table_1 <- DT::renderDataTable({prepare_app(app_data,NULL,dvals$data)})

  # CHARTS #
  output$badges_chart_1 <- renderHighchart({prepare_chart(awards_data)})
  output$app_chart_1 <- renderHighchart({prepare_chart(app_data)})
  
  #metrics1 <- c('Total','Unique Users','New Users','Earned','Earned To Date','Remaining','% Remaining')
  #metrics2 <- c('Earned','Earned To Date')
  #metrics3 <- c('Rewards Won','Total Ballots','Answer Questions','App Open','Level A','Level B','Level C','Level D','Onboarding Completed','Share Social','Take Ten','View Articles','View Videos','Weekly 1','Weekly 2','Weekly 4')

  # TABLES FROM OLD SCOPE #
  #output$summary_table  <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description'),c('Count'),dvals$data,input$cbx_cartesian),dvals$data,c(input$split1,input$split2,input$split3),metrics1,NULL,10000,input$format,input$compare)})
  #output$summary_table2 <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description'),c('Count'),dvals$data,input$cbx_cartesian),dvals$data,c(input$split1,input$split2,input$split3),metrics1,NULL,10000,input$format,input$compare)})
  #output$summary_table3 <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description','Customer ID'),c('Count'),dvals$data,F),dvals$data,c(input$split4,input$split5,input$split6,input$split7),metrics2,NULL,10000,F,input$compare)})
  #output$summary_table4 <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description','Customer ID'),c('Count'),dvals$data,F),dvals$data,c(input$split4,input$split5,input$split6,input$split7),metrics2,NULL,10000,F,input$compare)})
  #output$summary_table5 <- DT::renderDataTable({makeSummaryTableSorting(prepare_award_data(c('Award Code','Customer ID','Date'),c('Ballots'),c(as.Date('2018-11-01'),as.Date('2019-02-01')),T,input$cbx_date),dvals$data,award_cols$data,metrics3,NULL,10000)})
  #output$summary_table6 <- DT::renderDataTable({makeSummaryTableSorting(prepare_award_data(c('Award Code','Customer ID','Date'),c('Ballots'),c(as.Date('2018-11-01'),as.Date('2019-02-01')),T,input$cbx_date),dvals$data,award_cols$data,metrics3,NULL,10000)})
  
  dvals <- reactiveValues(data = NULL)
  
  observeEvent(input$period,{
    if(input$period!='Custom'){dvals$data <- getDateRange(input$period,input$compare)}
    else{dvals$data <- c(input$dateRange,input$dateRange2)}
    session$sendCustomMessage('updatePeriod',input$period)
  })
  observeEvent(input$dateRange,{
    dvals$data <- c(input$dateRange)
  })
  output$dateDisplay <- renderUI({  
    dateRangeInput('dateRange', label='Date Range:',start=getDateRange('90')[1],end=getDateRange('90')[2],format='M dd, yyyy')
  })
  observe({
    updateDateRangeInput(session,'dateRange',start = dvals$data[1],end = dvals$data[2])
  })
  
  #client_time<-reactive(as.numeric(input$client_time)/1000)
  #output$timeDisplay<-renderUI({selectInput('time','Time',c('None',input$client_time),selected='None')})
  #observeEvent(input$dates_qtr,{
  #  dvals$data <- getDateRange('q',input$compare)
  #})
}
