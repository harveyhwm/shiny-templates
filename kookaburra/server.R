server <- function(input,output,session){
  renderShinyTables <- function(s,r=1,total=NULL,ord=T){
    if(tableType == 'data.table'){DT::renderDataTable({makeDT(s,r,total,ord)})}
    else{shiny::renderTable(makeTable(s,total))}
  }
  progress <- function(x){session$sendCustomMessage('progress',x)}
  printPath <-function(x){session$sendCustomMessage('printPath',x)}
  
  # REACTIVE VALUES #
  dvals <- reactiveValues(data=c(min(weeks_extended),max(weeks_extended),max(weeks_extended)))
  dstate <- reactiveValues(data='week') 
  allflag <- reactiveValues(data=0,province=0,level=0,max_level=0)
  mytables <- reactiveValues(bt1=NULL,bt2=NULL,bt3=NULL,rt1=NULL,rt2=NULL)
  myselectedrows <- reactiveValues(bt1=c(),bt2=c(),bt3=c())
  selectstate1 <- reactive({!is.null(input$badges_1_rows_selected)})
  selectstate2 <- reactive({!is.null(input$badges_2_rows_selected)})
  selectstate3 <- reactive({!is.null(input$badges_3_rows_selected)})
  myselectedrows2 <- reactiveValues(rt1=c(),rt2=c())
  selectstate1r <- reactive({!is.null(input$reward_1_rows_selected)})
  selectstate2r <- reactive({!is.null(input$reward_2_rows_selected)})
  downloads_succinct <- reactiveValues(b0=NULL,b1=NULL,b2=NULL,b3=NULL,b4=NULL,r0=NULL,r1=NULL,r2=NULL,r3=NULL,r4=NULL,r5=NULL,r6=NULL,a0=NULL,a1=NULL,a2=NULL,a3=NULL,a4=NULL) 
  downloadBlocker <- reactiveValues(data=0)
  
  # MAIN OBSERVER #
  observeEvent({input$calculate
                input$initialize},{
    if(!grepl('^All',input$week)){
      allflag$data = 0
      dstate$data <- 'week'
      dvals$data <- c(input$week,weeks_extended[which(weeks_extended == min(weeks_extended[weeks_extended >= input$week])):min(which(weeks_extended == min(weeks_extended[weeks_extended >= input$week]))+1,len(weeks_extended))])
    }
    if(dstate$data != 'month'){updateSelectInput(session,'month',selected = 'All Months')}
    if(dstate$data != 'quarter'){updateSelectInput(session,'quarter',selected = 'All Quarters')}
    if(grepl('^All',input$week) && grepl('^All',input$month) && grepl('^All',input$quarter)){allflag$data = 1}
    allflag$province <- ifelse(grepl('^All',input$province),1,0)
    allflag$level <- ifelse(grepl('^All',input$level),1,0)
    allflag$max_level <- ifelse(grepl('^All',input$max_level),1,0)
    if(allflag$data == 1){
      p <- 'quarter'      
      d <- c('All Quarters')
      c_dates <- d
      range_len <- as.numeric(as.Date(max_date)-as.Date(min_date))
    }
    else{
      p <- dstate$data
      d <- dvals$data
      c_dates <- dvals$data[1:2]
      range_len <- as.numeric(as.Date(d[3])-as.Date(d[2]))
    }
    f <- c('gender',input$gender,'age',input$age,'day_part',input$day_part,'province',input$province,'answerValue',input$level,'max_level',input$max_level)
    
    # SQL DATA RETRIEVAL #
    progress(5)
    my_mega <- getSqlDataNew(my_mega_query,p,d[1],'date',input$max_level,input$level,input$province,F)
    progress(10)
    my_mega[,c(4:31,39:44)] <- lapply(my_mega[,c(4:31,39:44)],as.numeric)
    my_mega <- data.table(mutate_if(my_mega, is.numeric, ~replace(., is.na(.), 0)))
    progress(15)
    my_badges_full <- my_mega[my_mega$type == 'badges',c('award_short_description','delta_type','count','unique','ballots','role')]
    my_new_badged_users <- my_mega[my_mega$type == 'new users',c('unique')]
    my_userbadges <- my_mega[my_mega$type == 'badges per user',c('delta_type','count','unique')]
    my_charts <- my_mega[my_mega$type == 'charts',c('date','count','unique','ballots')]
    my_email <- my_mega[my_mega$type == 'email',c('Metric','Weekly Total','Monthly Total','Quarterly Total')]
    my_notif1 <- my_email
    my_notif2 <- my_email #temporary notifications tables
    my_notif1$Type <- 'Article'
    my_notif2$Type <- 'Reward'
    my_notif <- rbind(my_notif1,my_notif2)[,c('Type','Metric','Weekly Total','Monthly Total','Quarterly Total')]
    
    my_cheaters <- my_mega[my_mega$type == 'cheaters',c('Customer ID','ballots')]
    reward_wins <- my_mega[my_mega$type == 'winners',c('award_short_description','Weekly Total','Monthly Total','Quarterly Total','Weekly Users','Monthly Users','Quarterly Users')]
    reward_wunq <- my_mega[my_mega$type == 'unique winners',c('unique')]
    reward_wtot <- my_mega[my_mega$type == 'total winners',c('Weekly Total','Monthly Total','Quarterly Total','Weekly Users','Monthly Users','Quarterly Users')]
    reward_freq <- my_mega[my_mega$type == 'frequent winners',c('Customer ID','Weekly Users','Monthly Users','Quarterly Users')]
    
    my_badges <- my_badges_full[my_badges_full$role == 1,1:4]
    my_badges <- my_badges[,lapply(.SD,sum),by = c('award_short_description','delta_type')]
    
    t <- c(as.numeric(my_badges[my_badges$award_short_description == 'All Categories' & my_badges$delta_type == 'none',]$count),as.numeric(my_badges[my_badges$award_short_description == 'All Categories' & my_badges$delta_type == 'none',]$unique))
    t7 <- as.numeric(my_badges[my_badges$award_short_description == 'All Categories' & my_badges$delta_type == 'delta7',]$count)
    t28 <- as.numeric(my_badges[my_badges$award_short_description == 'All Categories' & my_badges$delta_type == 'delta28',]$count)
    my_badges_tables <- getBadgeTables(my_badges,t)
    my_badges_tables_print <- getBadgeTables(my_badges,t,print=T)
    my_ballots_tables <- getBallotsTables(my_badges_full,t)
    my_ballots_tables_print <- getBallotsTables(my_badges_full,t,print=T)
    t_ballot <- my_ballots_tables[which(my_ballots_tables$`Award Description` == 'All Categories'),c(3,5,7)]
    
    #activity user data
    my_onboarding <- my_mega[my_mega$type == 'activity answerValue',c('delta_type','answerValue','count','unique'),with=F]
    my_onboarding[is.na(my_onboarding)] <- 'Null'
    my_onboarding$answerValue <- gsub('9|(1.)','8',my_onboarding$answerValue)
    my_onboarding <- my_onboarding[,lapply(.SD,sum),by=c('delta_type','answerValue')]
    my_onboarding <- rbind(my_onboarding,cbind(my_onboarding[,c('delta_type','count','unique')][,lapply(.SD,sum),by=c('delta_type')][,1],'answerValue'='All Levels',my_onboarding[,c('delta_type','count','unique')][,lapply(.SD,sum),by=c('delta_type')][,2:3]))
    my_goal <- my_mega[my_mega$type == 'activity goal level',c('delta_type','role','count','unique'),with=F][,lapply(.SD,sum),by=c('delta_type','role')]
    my_weeklies <- my_mega[my_mega$type == 'activity weeklies',c('delta_type','Metric','count','unique'),with=F][,lapply(.SD,sum),by=c('delta_type','Metric')]
    
    #fil in gaps in weekly buckets if there are rangees with 0 minutes below the max active range
    weekly_buckets <- c('0-50','51-100','101-150','151-200','201-250','251-300','301-350','351-400','401-450','451-500','501-550','551-600','601-650','651-700','701-750','751-800','801-850','851-900','901-950','951+')
    maxbucket <- 0
    for(i in unique(my_weeklies[my_weeklies$Metric != 'All Times',]$Metric)){maxbucket <- max(maxbucket,which(weekly_buckets == i))}
    weekly_buckets <- weekly_buckets[1:maxbucket]
    for(i in weekly_buckets){my_weeklies <- rbind(my_weeklies,data.frame(delta_type='none',Metric=i,count=0,unique=0))}
    my_weeklies <- my_weeklies[,lapply(.SD,sum),by = c('delta_type','Metric')]
    
    #minutes and steps - pertinent slices
    my_active <- cbind(Metric='Active Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_active','unique_master_active'),with=F][,lapply(.SD,sum),by=c('delta_type')])
    my_move <- cbind(Metric='Move Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_move','unique_master_move')][,lapply(.SD,sum),by=c('delta_type')])
    my_device <- cbind(Metric='Device Active Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_device','unique_master_device')][,lapply(.SD,sum),by=c('delta_type')])
    
    my_active_fitbit <- cbind(Metric='Fitbit: Active Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_active_fitbit','unique_master_active_fitbit')][,lapply(.SD,sum),by=c('delta_type')])
    my_active_garmin <- cbind(Metric='Garmin: Active Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_active_garmin','unique_master_active_garmin')][,lapply(.SD,sum),by=c('delta_type')])
    #my_move_fitbit <- my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_move_fitbit','unique_master_move_fitbit')][,lapply(.SD,sum),by=c('delta_type')]
    #my_move_garmin <- my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_move_garmin','unique_master_move_garmin')][,lapply(.SD,sum),by=c('delta_type')]
    my_heartpoints <- cbind(Metric='Apple Watch: HeartPoints',my_mega[my_mega$type == 'minutes and steps',c('delta_type','heartPoints','unique_heartPoints')][,lapply(.SD,sum),by=c('delta_type')])
    my_exercise <- cbind(Metric='Google Fit: ExerciseMinutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','exerciseMinutes','unique_exerciseMinutes')][,lapply(.SD,sum),by=c('delta_type')])
    my_activeMins <- cbind(Metric='iPhone: Active Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','activeMinutes','unique_activeMinutes')][,lapply(.SD,sum),by=c('delta_type')])
    
    my_manual <- cbind(Metric='Manual Active Minutes',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_manual','unique_master_manual')][,lapply(.SD,sum),by=c('delta_type')])
    my_steps <- cbind(Metric='Total Steps',my_mega[my_mega$type == 'minutes and steps',c('delta_type','master_steps','unique_master_steps')][,lapply(.SD,sum),by=c('delta_type')])
    my_pct_active = cbind(Metric='% Active',my_active[,2:2],(my_active[,3:4]/my_move[,3:4]))
    my_pct_lpa = cbind(Metric='% LPA',my_active[,2:2],1-(my_active[,3:4]/my_move[,3:4]))
    my_workouts <- my_mega[my_mega$type == 'manual workouts',c('delta_type','activityType','master_manual','unique_master_manual')][,lapply(.SD,sum),by=c('delta_type','activityType')]
    
    #metric shelf
    activity_users <- sum(my_onboarding[my_onboarding$delta_type == 'none' & my_onboarding$answerValue == 'All Levels',c(unique)])
    mvpa <- my_active[my_active$delta_type == 'none',c(master_active)][1]
    mvpa_manual <- ifelse(sum(my_manual$master_manual) == 0,0,as.numeric(my_manual[my_manual$delta_type == 'none',c(master_manual)][1]))
    mvpa_device <- ifelse(sum(my_device$master_device) == 0,0,as.numeric(my_device[my_device$delta_type == 'none',c(master_device)][1]))
    mvpa7 <- my_active[my_active$delta_type == 'delta7',c(master_active)][1]
    mvpa28 <- my_active[my_active$delta_type == 'delta28',c(master_active)][1]
    mvpa84 <- my_active[my_active$delta_type == 'delta84',c(master_active)][1]
    mvpa_users <- my_active[my_active$delta_type == 'none',c(unique_master_active)][1]
    mvpa_users_manual <- ifelse(sum(my_manual$master_manual) == 0,0,as.numeric(my_manual[my_manual$delta_type == 'none',c(unique_master_manual)][1]))
    mvpa_users_device <- ifelse(sum(my_device$master_device) == 0,0,as.numeric(my_device[my_device$delta_type == 'none',c(unique_master_device)][1]))
    move <- my_move[my_move$delta_type == 'none',c(master_move)][1]
    steps <- my_steps[my_steps$delta_type == 'none',c(master_steps)][1]
    shelf <- c(mvpa,mvpa_manual,mvpa_device,mvpa7,mvpa28,mvpa84,mvpa_users,mvpa_users_manual,mvpa_users_device,move,steps,activity_users)
    
    colnames(my_active) <- colnames(my_move) <- colnames(my_device) <- colnames(my_manual) <- colnames(my_steps) <- colnames(my_pct_active) <- colnames(my_pct_lpa) <- colnames(my_active_fitbit) <- colnames(my_active_garmin) <- colnames(my_heartpoints) <- colnames(my_exercise) <- colnames(my_activeMins) <- c('Metric','delta_type','total','unique')
    colnames(my_workouts) <- c('delta_type','activityType','total','unique')
    
    #preparing data tables
    my_minute_totals <- getTotalMinutes(rbind(my_active,my_device,my_activeMins,my_heartpoints,my_exercise,my_active_fitbit,my_active_garmin,my_manual),shelf,range_len)
    my_minute_totals_print <- getTotalMinutes(rbind(my_active,my_device,my_manual),shelf,range_len,print=T)
    my_health_totals <- rbind(getTotalMinutes(rbind(my_active,my_move),shelf,range_len),getTotalMinutes(my_pct_active,shelf,range_len,pct=T),getTotalMinutes(my_pct_lpa,shelf,range_len,pct=T),getTotalMinutes(my_steps,shelf,range_len))
    my_health_totals_print <- rbind(getTotalMinutes(rbind(my_active,my_move),shelf,range_len,print=T),getTotalMinutes(my_pct_active,shelf,range_len,pct=T,print=T),getTotalMinutes(my_steps,shelf,range_len,print=T))
    my_workout_types <- getManualMinutes(my_workouts,shelf,range_len)
    
    progress(95)
    
    # BADGE TABLES #
    if(len(which(my_badges_tables$`Award Description` == 'Engagement')) == 0){range1 <- c(500)}
    else{range1 <- c(1:(which(my_badges_tables$`Award Description` == 'Engagement')-1))}
    output$badges_1 <- renderShinyTables(my_badges_tables[range1,],10)
    if(len(which(my_badges_tables$`Award Description` == 'Engagement') * which(my_badges_tables$`Award Description` == 'Activity')) == 0){range2 <- c(500)}
    else{range2 <- c((which(my_badges_tables$`Award Description` == 'Engagement')+1):(which(my_badges_tables$`Award Description` == 'Activity')-1))}
    output$badges_2 <- renderShinyTables(my_badges_tables[range2,],10)
    if(len(which(my_badges_tables$`Award Description` == 'Level') * which(my_badges_tables$`Award Description` == 'Activity')) == 0){range3 <- c(500)}
    else{range3 <- c((which(my_badges_tables$`Award Description` == 'Activity')+1):(which(my_badges_tables$`Award Description` == 'Level')-1))}
    output$badges_3 <- renderShinyTables(my_badges_tables[range3,],10)
    output$badges_4 <- renderShinyTables(getBadgesPerUser(my_userbadges,t),10)
    output$subtot_1 <- renderShinyTables(my_badges_tables[which(my_badges_tables$`Award Description` == 'Engagement'),],1,T)
    output$subtot_2 <- renderShinyTables(my_badges_tables[which(my_badges_tables$`Award Description` == 'Activity'),],1,T)
    output$subtot_3 <- renderShinyTables(my_badges_tables[which(my_badges_tables$`Award Description` == 'Level'),],1,T)
    
    # BADGE UPPER METRICS #
    output$badge_upper1 <- renderText(format(t[1],big.mark=','))
    output$badge_upper2 <- renderText(format(t[2],big.mark=','))
    output$badge_upper3 <- renderText(format(round(t[1]/t[2],2),nsmall=2))
    output$badge_upper4 <- renderText(format(my_new_badged_users,big.mark=','))
    output$badge_upper5 <- renderText(format(paste0(format(round(100*my_new_badged_users/t[2],2),nsmall=2),'%')))
    output$badge_upper6 <- renderText(replace_na(ifelse((t7<1 || d == c('All Quarters')),'--',ifelse(nchar(paste0(format(round(100*(t[1]-t7)/t7,2),nsmall=2),'%'))<4,'--',paste0(format(round(100*(t[1]-t7)/t7,2),nsmall=2),'%'))),'--'))
    #print(ifelse(nchar(paste0(format(round(100*(t[1]-t7)/t7,2),nsmall=2),'%'))==1,'--',paste0(format(round(100*(t[1]-t7)/t7,2),nsmall=2),'%')))
    output$badge_upper7 <- renderText(replace_na(ifelse((t28<1 || d == c('All Quarters')),'--',paste0(format(round(100*(t[1]-t28)/t28,2),nsmall=2),'%')),'--'))
    
    # BADGE DOWNLOADS #
    succinct_badges = makeDT(my_badges_tables_print,print=T)
    succinct_badges$`Badges Category` <- ifelse(grepl('Milestone|Multiplier|Level',succinct_badges$`Award Description`),'Milestone Badges',ifelse(grepl('Weekly|Streak|Activity',succinct_badges$`Award Description`),'Activity Badges','Engagement Badges'))
    succinct_badges <- cbind(succinct_badges[,ncol(succinct_badges)-1:ncol(succinct_badges)-1,with=F],succinct_badges[,1:(ncol(succinct_badges)-1),with=F])[order(`Badges Category`)]
    downloads_succinct$b1 <- myCatch(succinct_badges[grepl('Engagement',succinct_badges$`Badges Category`)][order(Total)],err=data.table(`No Data Available`=''))
    downloads_succinct$b1[nrow(downloads_succinct$b1),2] = 'ENGAGEMENT TOTAL'
    downloads_succinct$b2 <- myCatch(succinct_badges[grepl('Activity',succinct_badges$`Badges Category`)][order(Total)],err=data.table(`No Data Available`=''))
    downloads_succinct$b2[nrow(downloads_succinct$b2),2] = 'ACTIVITY TOTAL'
    downloads_succinct$b3 <- myCatch(succinct_badges[grepl('Milestone',succinct_badges$`Badges Category`)][order(Total)],err=data.table(`No Data Available`=''))
    downloads_succinct$b3[nrow(downloads_succinct$b3),2] = 'MILESTONE TOTAL'
    downloads_succinct$b4 <- myCatch(data.table(makeDT(getBadgesPerUser(my_userbadges,t,print=T),print=T)),err=data.table(`No Data Available`=''))
    
    # REWARD TABLES - BALLOTS #
    if(len(which(my_ballots_tables$`Award Description` == 'Engagement')) == 0){range4 <- c(500)}
    else{range4 <- c(1:(which(my_ballots_tables$`Award Description` == 'Engagement')-1))}
    output$reward_1 <- renderShinyTables(my_ballots_tables[range4,],10)
    if(len(which(my_ballots_tables$`Award Description` == 'Engagement') * which(my_ballots_tables$`Award Description` == 'Activity')) == 0){range5 <- c(500)}
    else{range5 <- c((which(my_ballots_tables$`Award Description` == 'Engagement')+1):(which(my_ballots_tables$`Award Description` == 'Activity')-1))}
    output$reward_2 <- renderShinyTables(my_ballots_tables[range5,],10)
    output$subtot_4 <- renderShinyTables(my_ballots_tables[which(my_ballots_tables$`Award Description` == 'Engagement'),],1,T)
    output$subtot_5 <- renderShinyTables(my_ballots_tables[which(my_ballots_tables$`Award Description` == 'Activity'),],1,T)
    
    # REWARD TABLES - WINNERS #    
    output$reward_3 <- renderShinyTables(getRewardsTables(reward_wins,t,c(1),winners=T),20)
    output$subtot_6 <- renderShinyTables(getRewardsTables(cbind(data.table('Total'='Total'),reward_wtot),t,c(1),winners=T),1,T)
    output$reward_4 <- renderShinyTables(getWinnersTables(reward_freq,total=T,type='frequent'),10)
    output$reward_5 <- renderShinyTables(getWinnersTables(my_cheaters,type='cheaters'),10)
    output$reward_6 <- renderShinyTables(getWinnersTables(my_email,type='email'),3,ord=F)
    output$reward_7 <- renderShinyTables(getWinnersTables(my_notif,type='notif'),3,ord=F)
    
    # REWARD UPPER METRICS #  
    progress(100)    
    output$reward_upper1 <- renderText(t_ballot[[1]][1])
    output$reward_upper2 <- renderText(t_ballot[[2]][1])
    output$reward_upper3 <- renderText(t_ballot[[3]][1])
    output$reward_upper4 <- renderText(format(reward_wunq,big.mark=','))
    output$reward_upper5 <- renderText(ifelse(t[2]<1,'--',paste0(format(round(100*reward_wunq/t[2],2),nsmall=2),'%')))
    output$reward_upper6 <- renderText(format(as.numeric(nrow(reward_freq)),big.mark=','))
    output$reward_upper7 <- renderText(format(as.numeric(nrow(my_cheaters)),big.mark=','))
    
    # REWARD DOWNLOADS #
    downloads_succinct$r1 <- myCatch(cbind(data.table('Ballot Category'=rep('Engagement Ballots',times=length(range4))),makeDT(my_ballots_tables_print[range4,],10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$r2 <- myCatch(cbind(data.table('Ballot Category'=rep('Activity Ballots',times=length(range5))),makeDT(my_ballots_tables_print[range5,],10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$r3 <- myCatch(data.table(makeDT(getRewardsTables(reward_wins,t,c(1),winners=T,print=T),10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$r4 <- myCatch(data.table(makeDT(getWinnersTables(reward_freq,total=T,type='frequent',print=T),10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$r5 <- myCatch(data.table(makeDT(getWinnersTables(my_cheaters,type='cheaters',print=T),10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$r6 <- myCatch(data.table(makeDT(getWinnersTables(my_email,type='email',print=T),10,print=T)),err=data.table(`No Data Available`=''))
    
    # ACTIVITY TABLES - USERS #
    onboarding_table <- getLevels(my_onboarding,shelf,input$level)
    onboarding_table_print <- getLevels(my_onboarding,shelf,input$level,print=T)
    maxlevel_table <- getLevels(my_goal,shelf,1,max_level=T)
    maxlevel_table_print <- getLevels(my_goal,shelf,1,print=T)
    output$activity_1 <- renderShinyTables(onboarding_table[grepl('[0-9]{1}|Null',onboarding_table$Level),],30)
    output$subtot_7  <- renderShinyTables(onboarding_table[onboarding_table$Level == 'All Levels',],1,T)
    output$activity_2 <- renderShinyTables(maxlevel_table[grepl('[0-9]{1}|Null',maxlevel_table$Level),],30)
    output$subtot_8  <- renderShinyTables(maxlevel_table[maxlevel_table$Level == 'All Goals',],1,T)
    output$activity_3 <- renderShinyTables(getWeeklies(my_weeklies[my_weeklies$Metric != 'All Times',],shelf),20)
    output$subtot_9  <- renderShinyTables(getWeeklies(my_weeklies[my_weeklies$Metric == 'All Times',],shelf),1,T)
    
    # ACTIVITY TABLES - MINUTES & STEPS #
    output$activity_4 <- renderShinyTables(my_minute_totals[my_minute_totals$Type != 'Active Minutes',],7)
    output$subtot_10  <- renderShinyTables(my_minute_totals[my_minute_totals$Type == 'Active Minutes',],1,T)
    output$activity_5 <- renderShinyTables(my_health_totals,5)
    output$activity_6 <- renderShinyTables(my_workout_types[my_workout_types$`Activity Type`!='All Activities',],25)
    output$subtot_11  <- renderShinyTables(my_workout_types[my_workout_types$`Activity Type`=='All Activities',],1,T)
    
    # ACTIVITY UPPER METRICS #
    output$activity_upper1 <- renderText(format(as.numeric(mvpa_users),big.mark=','))
    output$activity_upper1a<- renderText(format(as.numeric(activity_users),big.mark=','))
    device_upper <- HTML(ifelse(mvpa_device>=1e9,paste0(format(round(mvpa_device/1e9,2),nsmall=2),' Bn'),ifelse(mvpa_device>=1e8,paste0('<div class="metric_box activity_box_nine">',format(round(mvpa_device/1e6,1),nsmall=1),'<small> MM</small></div>'),ifelse(mvpa_device>=1e6,paste0('<div class="metric_box activity_box_nine">',format(round(mvpa_device/1e6,2),nsmall=2),'<small> MM</small></div>'),paste0('<div class="metric_box activity_box_nine">',format(mvpa_device,big.mark=','),'</div>')))))
    output$activity_upper2 <- renderText(device_upper) #renderText(format(as.numeric(mvpa_device),big.mark=','))
    manual_upper <- HTML(ifelse(mvpa_manual>=1e9,paste0(format(round(mvpa_manual/1e9,2),nsmall=2),' Bn'),ifelse(mvpa_manual>=1e8,paste0('<div class="metric_box activity_box_nine">',format(round(mvpa_manual/1e6,1),nsmall=1),'<small> MM</small></div>'),ifelse(mvpa_manual>=1e6,paste0('<div class="metric_box activity_box_nine">',format(round(mvpa_manual/1e6,2),nsmall=2),'<small> MM</small></div>'),paste0('<div class="metric_box activity_box_nine">',format(mvpa_manual,big.mark=','),'</div>')))))
    output$activity_upper3 <- renderText(manual_upper) #renderText(format(as.numeric(mvpa_manual),big.mark=','))
    mvpa_upper <- HTML(ifelse(mvpa>=1e9,paste0(format(round(mvpa/1e9,2),nsmall=2),' Bn'),ifelse(mvpa>=1e8,paste0('<div class="metric_box activity_box_nine">',format(round(mvpa/1e6,1),nsmall=1),'<small> MM</small></div>'),ifelse(mvpa>=1e6,paste0('<div class="metric_box activity_box_nine">',format(round(mvpa/1e6,2),nsmall=2),'<small> MM</small></div>'),paste0('<div class="metric_box activity_box_nine">',format(mvpa,big.mark=','),'</div>')))))
    output$activity_upper4 <- renderUI(mvpa_upper) #renderText(format(as.numeric(mvpa),big.mark=','))
    output$activity_upper5 <- renderText(format(round(mvpa/mvpa_users,2),nsmall=2))
    output$activity_upper6 <- renderText(gsub('(Inf|NA)%','--',format(paste0(format(round(100*(mvpa-mvpa7)/mvpa7,2),nsmall=2),'%'))))
    output$activity_upper6a <- renderText(gsub('(Inf|NA)%','--',format(paste0(format(round(100*(mvpa-mvpa28)/mvpa28,2),nsmall=2),'%'))))
    output$activity_upper7 <- renderText(gsub('(Inf|NA)%','--',format(paste0(format(round(100*(mvpa-mvpa84)/mvpa84,2),nsmall=2),'%'))))
    steps_upper <- HTML(ifelse(steps>=1e9,paste0('<div class="metric_box activity_box_nine metric_last">',format(round(steps/1e9,2),nsmall=2),'<small> Bn</small></div>'),ifelse(steps>=1e8,paste0('<div class="metric_box activity_box_nine metric_last">',format(round(steps/1e6,1),nsmall=1),'<small> MM</small></div>'),ifelse(steps>=1e6,paste0('<div class="metric_box activity_box_nine metric_last">',format(round(steps/1e6,2),nsmall=2),'<small> MM</small></div>'),paste0('<div class="metric_box activity_box_nine metric_last">',format(steps,big.mark=','),'</div>')))))
    output$activity_upper8 <- renderText(steps_upper)
    
    downloads_succinct$a1 <- myCatch(data.table(makeDT(onboarding_table_print[grepl('[0-9]{1}|Null',onboarding_table_print$Level),],10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$a2 <- myCatch(data.table(makeDT(maxlevel_table_print[grepl('[0-9]{1}|Null',maxlevel_table_print$Level),],10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$a3 <- myCatch(data.table(makeDT(getWeeklies(my_weeklies[my_weeklies$Metric != 'Total',],mvpa_users,print=T),10,print=T)),err=data.table(`No Data Available`=''))
    downloads_succinct$a4 <- rbind(my_minute_totals[my_minute_totals$Type != 'Active Minutes',],cbind(Type='Total',my_minute_totals[my_minute_totals$Type == 'Active Minutes',2:7]))
    downloads_succinct$a5 <- my_health_totals
    downloads_succinct$a6 <- rbind(my_workout_types[my_workout_types$`Activity Type` != 'All Activities',c(1,3,5,7,9,11,13)],cbind(`Activity Type_rank`='Total',my_workout_types[my_workout_types$`Activity Type` == 'All Activities',c(3,5,7,9,11,13)]))
    names(downloads_succinct$a6) <- gsub('_rank$','',names(downloads_succinct$a6))
    
    #hide filters based on current dsahboard state (disable for now)
    #filtertot <- allflag$data+allflag$province+allflag$level+allflag$max_level+ifelse(allflag$data==1,0,ifelse(p=='quarter',1,0))
    #session$sendCustomMessage('filterstates',filtertot)
    
    # CHARTS #  
    #output$badges_chart_1 <- renderHighchart({getMyChart(my_charts,'date','count')})
    #output$reward_chart_1 <- renderHighchart({getMyChart(my_charts,'date','ballots')})
    #rows <- c()
    #f2 <- c()
    #if(selectstate1()==T){rows <- c(rows,myselectedrows$bt1)}
    #if(selectstate2()==T){rows <- c(rows,myselectedrows$bt2)}
    #if(selectstate3()==T){rows <- c(rows,myselectedrows$bt3)}
    #if(len(rows) > 0){f2 <- c(f,'award_short_description',paste(rows,collapse="','"))}
    #badges_chart <- getSqlData(myquery_badges_chart,p,d,f)
    #rows2 <- c()
    #f2 <- c()
    #if(selectstate1r()==T){rows2 <- c(rows2,myselectedrows2$rt1)}
    #if(selectstate2r()==T){rows2 <- c(rows2,myselectedrows2$rt2)}
    #if(len(rows2) > 0){f2 <- c(f,'award_short_description',paste(rows2,collapse="','"))}
    #reward_chart <- getSqlData(myquery_reward_chart,p,d,f)
    #output$reward_chart_1 <- renderHighchart({getRewardChart(reward_chart)})
                })
  
  #output$moreControls <- renderUI({
  #  rawBlocker <- ifelse(grepl('^All',input$week),'blocked','ok')
  #  tags$div(class='cbx-depth',id=rawBlocker,radioButtons('fileDepth','',c('Raw'='raw','Summary'='summary'),selected='summary'))
  #})
  
  output$downloadData <- downloadHandler(
    filename = function(){
      ts <- gsub('G.*|-|:| ','',Sys.time())
      if(input$fileDepth == 'raw'){
        paste(ifelse(input$fileChoice=='badges','badges_raw',ifelse(input$fileChoice=='rewards','rewards_raw','activity_raw')),'_',ts,'.csv',sep='')
      }
      else if(input$fileDepth == 'summary'){
        if(input$fileType == 'xlsx'){
          paste('paction_summary_',ts,'.xlsx','',sep='')
        }
        else{
          paste('paction_summary_',ts,'.zip','',sep='')
        }
      }
    },
    content = function(file){
      if(input$fileDepth == 'raw'){
        fileName <- ifelse(input$fileChoice=='badges','badges_raw',ifelse(input$fileChoice=='rewards','rewards_raw','activity_raw'))
        p <- ifelse(allflag$data==1,'quarter',dstate$data)
        d <- ifelse(allflag$data==1,c('All Quarters'),dvals$data)
        sql <- getSqlDataNew(my_mega_query,p,d[1],'date',input$max_level,input$level,input$province,T,fileName)
        #write.csv(eval(parse(text=paste0('sql'))),gzfile(file),row.names=F)
        #files <- paste0(fileName,'.csv')
        write.csv(eval(parse(text=paste0('sql'))),file,row.names=F)
        #zip(file,files)
      }
      else if(input$fileDepth == 'summary'){
        mydata1 <- myCatch(rbind(downloads_succinct$b1,downloads_succinct$b2,downloads_succinct$b3),err=data.table(`No Data Available`=''))
        mydata2 <- myCatch(downloads_succinct$b4,err=data.table(`No Data Available`=''))
        mydata3 <- myCatch(rbind(downloads_succinct$r1,downloads_succinct$r2),err=data.table(`No Data Available`=''))
        mydata4 <- downloads_succinct$r3
        mydata5 <- downloads_succinct$r6
        mydata6 <- downloads_succinct$r5
        mydata7 <- downloads_succinct$r4
        mydata8 <- downloads_succinct$a1
        mydata9 <- downloads_succinct$a2
        mydata10<- downloads_succinct$a3
        mydata11<- downloads_succinct$a4
        mydata12<- downloads_succinct$a5
        mydata13<- downloads_succinct$a6
        if(input$fileType == 'xlsx'){
          sheets <- list(
            'Badge Types Earned'=stripNaCols(mydata1),
            'Badges Per User'=stripNaCols(mydata2),
            'Ballots by Type'=stripNaCols(mydata3),                    
            'Reward Winners'=stripNaCols(mydata4),
            'Email Interaction'=stripNaCols(mydata5),
            'Frequent Winners'=stripNaCols(mydata6),
            'Potential Cheaters'=stripNaCols(mydata7),
            'User Onboarding Levels'=stripNaCols(mydata8),
            'Current Weekly Goal'=stripNaCols(mydata9),
            'Weekly Totals'=stripNaCols(mydata10),
            'Total Activity Data'=stripNaCols(mydata11),
            'Activity Health Data'=stripNaCols(mydata12),
            'Manual Entry Minutes'=stripNaCols(mydata13)
          )
          writexl::write_xlsx(sheets,file)
        }
        else if(input$fileType == 'csv'){
          setwd('/tmp')
          printPath(getwd())
          files <- ''
          fnames <- c(
            'badge_types',
            'badges_per_user',
            'ballots_by_type',                  
            'reward_winners',
            'email_interaction',
            'frequent_winners',
            'potential_cheaters',
            'user_onboarding_levels',
            'current_weekly_goal',
            'weekly_totals',
            'activity_data',
            'health_data',
            'manual_minutes'
          )
          for (i in 1:length(fnames)){
            write.csv(eval(parse(text=paste0('stripNaCols(mydata',i,')'))),paste0(fnames[i],'.csv'),row.names=F)
            files <- c(paste0(fnames[i],'.csv'),files)
          }
          zip(file,files)
        }
      }
    }
  )
  
  observeEvent(input$week,{
    if(!grepl('^All',input$week)){
      allflag$data = 0
      dstate$data <- 'week'
      dvals$data <- c(input$week,weeks_extended[which(weeks_extended == min(weeks_extended[weeks_extended >= input$week])):min(which(weeks_extended == min(weeks_extended[weeks_extended >= input$week]))+1,len(weeks_extended))])
    }
    if(dstate$data != 'month'){updateSelectInput(session,'month',selected = 'All Months')}
    if(dstate$data != 'quarter'){updateSelectInput(session,'quarter',selected = 'All Quarters')}
    if(grepl('^All',input$week) && grepl('^All',input$month) && grepl('^All',input$quarter)){
      allflag$data = 1
      updateTextInput(session,'period',value = 'All')
    }
    else if(!grepl('^All',input$week)){updateTextInput(session,'period',value = paste0('Week: ',input$week))}
  })
  observeEvent(input$month,{
    if(!grepl('^All',input$month)){
      allflag$data = 0
      dstate$data <- 'month'
      dvals$data <- c(input$month,months_extended[which(months_extended == min(months_extended[months_extended >= input$month])):min(which(months_extended == min(months_extended[months_extended >= input$month]))+1,len(months_extended))])
    }
    if(dstate$data != 'week'){updateSelectInput(session,'week',selected = 'All Weeks')}
    if(dstate$data != 'quarter'){updateSelectInput(session,'quarter',selected = 'All Quarters')}
    if(grepl('^All',input$week) && grepl('^All',input$month) && grepl('^All',input$quarter)){
      allflag$data = 1
      updateTextInput(session,'period',value = 'All')
    }
    else if(!grepl('^All',input$month)){updateTextInput(session,'period',value = paste0('Month: ',input$month))}
  })
  observeEvent(input$quarter,{
    if(!grepl('^All',input$quarter)){
      allflag$data = 0
      dstate$data <- 'quarter'
      dvals$data <- c(input$quarter,quarters_extended[which(quarters_extended == min(quarters_extended[quarters_extended >= input$quarter])):min(which(quarters_extended == min(quarters_extended[quarters_extended >= input$quarter]))+1,len(quarters_extended))])
    }
    if(dstate$data != 'week'){updateSelectInput(session,'week',selected = 'All Weeks')}
    if(dstate$data != 'month'){updateSelectInput(session,'month',selected = 'All Months')}
    if(grepl('^All',input$week) && grepl('^All',input$month) && grepl('^All',input$quarter)){
      allflag$data = 1
      updateTextInput(session,'period',value = 'All')
    }
    else if(!grepl('^All',input$quarter)){updateTextInput(session,'period',value = paste0('Quarter: ',input$quarter))}
  })
  observeEvent(input$reset_filters,{
    updateSelectInput(session,'week',selected = 'All Weeks')
    updateSelectInput(session,'month',selected = 'All Months')
    updateSelectInput(session,'quarter',selected = 'All Quarters')
    updateSelectInput(session,'age',selected = 'All Ages')
    updateSelectInput(session,'gender',selected = 'All Genders')
    updateSelectInput(session,'day_part',selected = 'All Times')
    updateSelectInput(session,'province',selected = 'All Provinces')
    updateSelectInput(session,'level',selected = 'All Levels')
    updateSelectInput(session,'max_level',selected = 'All Levels')
    if(input$initialize == 'All Levels'){updateSelectInput(session,'initialize',selected = '0')}
    else{updateSelectInput(session,'initialize',selected = 'All Levels')}
    allflag$data = 1
  })
  
  #TABLE-SELECT CHART INTRACTIONS
  #observeEvent(input$badges_1_rows_selected,{
  #  myselectedrows$bt1 <- mytables$bt1$`Award Description`[input$badges_1_rows_selected]
  #})
  #observeEvent(input$badges_2_rows_selected,{
  #  myselectedrows$bt2 <- mytables$bt2$`Award Description`[input$badges_2_rows_selected]
  #})
  #observeEvent(input$badges_3_rows_selected,{
  #  myselectedrows$bt3 <- mytables$bt3$`Award Description`[input$badges_3_rows_selected]
  #})
  #observeEvent(input$reward_1_rows_selected,{
  #  myselectedrows2$rt1 <- mytables$rt1$`Award Description`[input$reward_1_rows_selected]
  #})
  #observeEvent(input$reward_2_rows_selected,{
  #  myselectedrows2$rt2 <- mytables$rt2$`Award Description`[input$reward_2_rows_selected]
  #})
  
  #client_time<-reactive(as.numeric(input$client_time)/1000)
  #output$timeDisplay<-renderUI({selectInput('time','Time',c('None',input$client_time),selected='None')})
  #observeEvent(input$dates_qtr,{dvals$data <- getDateRange('q',input$compare)})
}