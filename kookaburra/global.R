#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LOAD PACKAGES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#setwd('/Users/harveymanhood/Documents/- github/projects/ParticipACTION-Dashboard')
packages <- c('shiny','shinydashboard','shinythemes','shinyjs','highcharter','data.table','reshape2','ggplot2','stringr','plotly','tools','tidyr','scales','anytime','rjson',
              'readr','plyr','rowr','DT','V8','tibble','gsubfn','lubridate','shinyWidgets','leaflet','openintro','stringr','RMariaDB','odbc','dbplyr','sp','bigQueryR','dplyr')
lapply(packages,library,character.only=TRUE)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  MODULES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
source('sql.R')
source('lib.R')

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SQL DATABASE CONNECTION & BIGQUERY AUTH
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
credentials <- fromJSON(file='data/credentials_db_prod.json')
connectType <- 'live' # choose local, sock or live. live is for when we deploy, others are for the local machine
sqlConnectionHandler <- function(q,t='live',c='query'){
  if(t == 'local'){p <- c('dev','dev','127.0.0.1','paction_db_qa',3306,NULL)}
  else if(t == 'sock'){p <- c('root','root','127.0.0.1','paction_db_qa2',8889,'/Applications/MAMP/tmp/mysql/mysql.sock')}
  else{p <- c(credentials$user,credentials$password,credentials$host,credentials$database,credentials$port,NULL)}
  con <- dbConnect(drv=RMariaDB::MariaDB(),username=p[1],password=p[2],host=p[3],dbname=p[4],port=p[5],socket=p[6])
  if(c=='execute'){res <- dbExecute(con,q)}
  else{
    s <- dbSendQuery(con,q)
    res <- dbFetch(s)
    dbClearResult(s)
  }
  dbDisconnect(con)
  res
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GLOBAL CONSTANTS & DEFINITIONS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tableType <- 'data.table'
na_val <- '--'
periods <- c('This Year'='y','Last Year'='yp','This Quarter'='q','Last Quarter'='qp','This Month'='m','Last Month'='mp','This Week'='w','Last Week'='wp','Last 90 Days'='90','Last 30 Days'='30','Last 14 Days'='14','Last 7 Days'='7','Custom')

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  RUN SQL QUERIES & PREPARE DATA
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
startup_data <- sqlConnectionHandler(getLoadingVals,connectType)
total_users <- as.numeric(startup_data[startup_data$type=='total_users',1])
max_level <- startup_data[startup_data$type=='max_level',1]
provinces <- sort(startup_data[startup_data$type=='province',1])
weeks <- sort(startup_data[startup_data$type=='week',1])
months <- sort(startup_data[startup_data$type=='month',1])
quarters <- sort(startup_data[startup_data$type=='quarter',1])
email_len <- as.numeric(startup_data[startup_data$type=='email_len',1])
notif_len <- as.numeric(startup_data[startup_data$type=='notif_len',1])
active_levels <- c('1','2','3','4','5','6','7','8')
max_date <- sort(startup_data[startup_data$type=='max_date',1])
min_date <- '2019-02-04'
weeks_extended <- unique(c(max(min_date,weeks[1]),weeks[2:len(weeks)],max_date))
months_extended <- unique(c(max(min_date,months[1]),months[2:len(months)],max_date))
quarters_extended <- unique(c(max(min_date,quarters[1]),quarters[2:len(quarters)],max_date))

engagementRows <- c('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!')
activityRows <- c('Weekly','2-Week Streak','4-Week Streak')
levelRows <- c('50 Minute Milestone','100 Minute Milestone','150 Minute Milestone','200 Minute Milestone','250 Minute Milestone','300 Minute Milestone','2x Multiplier')

getBadgeTables <- function(s,t,to_order=NULL,fill_rows=NULL,print=F){
  s1 <- s[s$award_short_description != 'All Categories' & s$delta_type == 'none',c(1,3:4)]
  s2 <- s[s$award_short_description != 'All Categories' & s$delta_type == 'delta7',c(1,3)]
  s3 <- s[s$award_short_description != 'All Categories' & s$delta_type == 'delta28',c(1,3)]
  s <- merge(merge(s1,s2,by='award_short_description',all.x=T),s3,by='award_short_description',all.x=T)
  renameCols(s,c('count.x','unique','count.y','count'),c('total','unique','total7','total28'))
  #s[is.na(s)] <- 0
  if(!is.null(fill_rows)){s <- rowCompleter(s[s$award_short_description %in% fill_rows,],fill_rows)}
  s$`Badges/ User` <- mapply(divideZero,as.numeric(s$total),as.numeric(s$unique))
  s$`Distribution` <- 100*s$total/t[1]
  s$`Penetration` <- 100*s$unique/t[2]
  s$`7-Day Growth` <- (s$total-s$total7)/s$total7
  s$`28-Day Growth` <- (s$total-s$total28)/s$total28
  renameCols(s,c('award_short_description','total','unique'),c('Award Description','Total','Unique'))
  s <- subset(s,select=-c(total7,total28))
  if(print==F){
    s <- addSortColumns(s,1)
    s$`Badges/ User_rank` <- format(round(as.numeric(s$`Badges/ User_rank`),2),nsmall=2)
    s$`Badges/ User_rank` <- lapply(s$`Badges/ User_rank`,function(x) zeroToBlank(x))
    s$`Distribution_rank` <- makePercent(s$`Distribution_rank`,1)
    s$`Penetration_rank` <- makePercent(s$`Penetration_rank`,1)
    s$`7-Day Growth_rank` <- makePercent(100*s$`7-Day Growth_rank`,1)
    s$`28-Day Growth_rank` <- makePercent(100*s$`28-Day Growth_rank`,1)
    s$Total_rank <- format(as.numeric(s$Total_rank),big.mark=',')
    s$Unique_rank <- format(as.numeric(s$Unique_rank),big.mark=',')
    for(i in names(s)){eval(parse(text=paste0("s$`",i,"` <- gsub('NA(|N)(|%)$','--',s$`",i,"`,ignore.case=T)")))}
    s[order(factor(s$`Award Description`,levels = c(engagementRows,'Engagement',activityRows,'Activity',levelRows,'Level'))),]
  }
  else{
    s
  }
}

getBadgesPerUser <- function(s,t,fill_rows=NULL,print=F){
  s1 <- s[s$delta_type == 'none',c(2:3)]
  s2 <- s[s$delta_type == 'delta7',c(2:3)]
  s3 <- s[s$delta_type == 'delta28',c(2:3)]
  s <- merge(merge(s1,s2,by='count',all.x=T),s3,by='count',all.x=T)
  renameCols(s,c('count','unique.x','unique.y','unique'),c('count','total','total7','total28'))
  if(!is.null(fill_rows)){s <- rowCompleter(s,fill_rows)}
  s$`Distribution` <- 100*s$total/t[2]
  s$`7-Day Growth` <- 100*(s$total-s$total7)/s$total7
  s$`28-Day Growth` <- 100*(s$total-s$total28)/s$total28
  renameCols(s,c('count','total'),c('Badge Count','Total'))
  s <- subset(s,select=-c(total7,total28))
  if(print==F){
    s <- addSortColumns(s,0)
    s$Total_rank <- format(as.numeric(s$Total_rank),big.mark=',')
    s$`Distribution_rank` <- makePercent(s$`Distribution_rank`,1)
    s$`7-Day Growth_rank` <- makePercent(s$`7-Day Growth_rank`,1)
    s$`28-Day Growth_rank` <- makePercent(s$`28-Day Growth_rank`,1)
    for(i in names(s)){eval(parse(text=paste0("s$`",i,"` <- gsub('(NA(|N)|Inf)%','--',s$`",i,"`,ignore.case=T)")))}
    s
  }
  else{
    s
  }
}

getRewardsTables <- function(s,t,to_order=NULL,fill_rows=NULL,winners=NULL,print=F){
  renameCols(s,c('award_short_description'),c('Award Description'))
  if(!is.null(fill_rows)){s <- rowCompleter(s[s$`Award Description` %in% fill_rows,],fill_rows)}
  if(print==F){
    s <- addSortColumns(s,1)
    if(is.null(winners)){
      tryCatch({
        s$`% Users (Weekly)_rank` <- makePercent(s$`% Users (Weekly)_rank`,1)
        s$`% Users (Monthly)_rank` <- makePercent(s$`% Users (Monthly)_rank`,1)
        s$`% Users (Quarterly)_rank` <-makePercent(s$`% Users (Quarterly)_rank`,1)
      },error = function(err){})
    }
    tryCatch({
      if(s$`Weekly Total_rank`){s$`Weekly Total_rank` <- format(as.numeric(s$`Weekly Total_rank`),big.mark=',')}
      if(s$`Monthly Total_rank`){s$`Monthly Total_rank` <- format(as.numeric(s$`Monthly Total_rank`),big.mark=',')}
      if(s$`Quarterly Total_rank`){s$`Quarterly Total_rank` <- format(as.numeric(s$`Quarterly Total_rank`),big.mark=',')}
      if(s$`Weekly Users_rank`){s$`Weekly Users_rank` <- format(as.numeric(s$`Weekly Users_rank`),big.mark=',')}
      if(s$`Monthly Users_rank`){s$`Monthly Users_rank` <- format(as.numeric(s$`Monthly Users_rank`),big.mark=',')}
      if(s$`Quarterly Users_rank`){s$`Quarterly Users_rank` <- format(as.numeric(s$`Quarterly Users_rank`),big.mark=',')}
    },error = function(err){})
  }
  for(i in names(s)){eval(parse(text=paste0("s$`",i,"` <- gsub('NA(|N)%','--',s$`",i,"`,ignore.case=T)")))}
  setorder(s[,.r:=order(to_order)],.r)[,.r:=NULL]
}

getBallotsTables <- function(s,t,to_order=NULL,fill_rows=NULL,winners=NULL,print=F){
  s1 <- s[s$delta_type == 'none' & s$role == 1,c(1,3:5)]
  s2 <- s[s$delta_type == 'none' & s$role == 2,c(1,3:5)]
  s3 <- s[s$delta_type == 'none' & s$role == 3,c(1,3:5)]
  s <- merge(merge(s1,s2,by='award_short_description'),s3,by='award_short_description')
  s <- s[order(factor(s$award_short_description,levels = c(engagementRows,'Engagement',activityRows,'Activity'))),]
  s$`% Users (Weekly)` <- 0
  s$`% Users (Monthly)` <- 0
  s$`% Users (Quarterly)` <- 0 
  tryCatch({
    s$`% Users (Weekly)` <- s$unique.x/s$unique.x[which(s$award_short_description == 'All Categories')]
    s$`% Users (Monthly)` <- s$unique.y/s$unique.y[which(s$award_short_description == 'All Categories')]
    s$`% Users (Quarterly)` <- s$unique/s$unique[which(s$award_short_description == 'All Categories')]
  },error = function(err){})
  renameCols(s,c('award_short_description','ballots.x','ballots.y','ballots'),c('Award Description','Weekly Ballots','Monthly Ballots','Quarterly Ballots'))
  s <- s[,c('Award Description','Weekly Ballots','Monthly Ballots','Quarterly Ballots','% Users (Weekly)','% Users (Monthly)','% Users (Quarterly)')]
  if(!is.null(fill_rows)){s <- rowCompleter(s[s$`Award Description` %in% fill_rows,],fill_rows)}
  if(print==F){
    s <- addSortColumns(s,1)
    if(is.null(winners)){
      tryCatch({
        s$`% Users (Weekly)_rank` <- makePercent(100*s$`% Users (Weekly)_rank`,1)
        s$`% Users (Monthly)_rank` <- makePercent(100*s$`% Users (Monthly)_rank`,1)
        s$`% Users (Quarterly)_rank` <-makePercent(100*s$`% Users (Quarterly)_rank`,1)
      },error = function(err){})
    }
    tryCatch({
      if(s$`Weekly Ballots_rank`){s$`Weekly Ballots_rank` <- format(as.numeric(s$`Weekly Ballots_rank`),big.mark=',')}
      if(s$`Monthly Ballots_rank`){s$`Monthly Ballots_rank` <- format(as.numeric(s$`Monthly Ballots_rank`),big.mark=',')}
      if(s$`Quarterly Ballots_rank`){s$`Quarterly Ballots_rank` <- format(as.numeric(s$`Quarterly Ballots_rank`),big.mark=',')}
      if(s$`Weekly Total_rank`){s$`Weekly Total_rank` <- format(as.numeric(s$`Weekly Total_rank`),big.mark=',')}
      if(s$`Monthly Total_rank`){s$`Monthly Total_rank` <- format(as.numeric(s$`Monthly Total_rank`),big.mark=',')}
      if(s$`Quarterly Total_rank`){s$`Quarterly Total_rank` <- format(as.numeric(s$`Quarterly Total_rank`),big.mark=',')}
      if(s$`Weekly Users_rank`){s$`Weekly Users_rank` <- format(as.numeric(s$`Weekly Users_rank`),big.mark=',')}
      if(s$`Monthly Users_rank`){s$`Monthly Users_rank` <- format(as.numeric(s$`Monthly Users_rank`),big.mark=',')}
      if(s$`Quarterly Users_rank`){s$`Quarterly Users_rank` <- format(as.numeric(s$`Quarterly Users_rank`),big.mark=',')}
    },error = function(err){})
    for(i in names(s)){eval(parse(text=paste0("s$`",i,"` <- gsub('NA(|N)%','--',s$`",i,"`,ignore.case=T)")))}
  }
  s
}

getWinnersTables <- function(s,total=F,fill_rows=NULL,type='none',print=F){
  if(!is.null(fill_rows)){s <- rowCompleter(s,fill_rows)}
  renameCols(s,c('Weekly Users','Monthly Users','Quarterly Users','ballots','Email Opened'),c('Weekly','Monthly','Quarterly','Number of Ballots','Email Opened (Unique)'))
  if(total==T){s$Total <- rowSums(s[,2:ncol(s)])}
  if(type=='email'){
    s <- s[order(factor(s$Metric,levels = c('Email Sent','Email Opened'))),]
    o_r <- data.table('Metric' = c('Open Rate (Unique)'),'Weekly Total'=makePercent(100*as.numeric(s[2,2])/as.numeric(s[1,2]),1),'Monthly Total'=makePercent(100*as.numeric(s[2,3])/as.numeric(s[1,3]),1),'Quarterly Total'=makePercent(100*as.numeric(s[2,4])/as.numeric(s[1,4]),1))
    s[,2:ncol(s)] <- lapply(s[,2:ncol(s)], function(x) format(x,big.mark=','))
    s <- rbind(s,o_r)
  }
  else if(type=='cheaters'){
    s <- s[order(-`Number of Ballots`),]
  }
  else if(type=='frequent'){
    s <- s[order(-Total),]
  }
  if(print==F){
    s <- addSortColumns(s,1)
    tryCatch({
      if(s$`Number of Ballots_rank`){s$`Number of Ballots_rank` <- format(as.numeric(s$`Number of Ballots_rank`),big.mark=',')}
    },error = function(err){})
  }
  for(i in names(s)){eval(parse(text=paste0("s$`",i,"` <- gsub('NA(|N)%','--',s$`",i,"`,ignore.case=T)")))}
  s
}

getMyChart <- function(s,p,m){
  if(!'count' %in% names(s)){s$count <- 1} #eval(parse(text=paste0('s$',m,' <- as.numeric(s$',m,')')))
  s <- s[,c(p,m),with=F][,lapply(.SD,sum),by = c(p)]
  highchart() %>% 
    hc_chart(type = 'spline') %>%  #,color = '#FF0000') %>%
    hc_subtitle(enabled = F) %>%
    hc_legend(enabled = F) %>% #, align = "left", verticalAlign = "top") %>%
    hc_xAxis(categories = c(s$date), labels = list(rotation = 0, style = list(fontSize= '8px')), startOnTick = F, tickInterval = round(nrow(s)/4,0)) %>% 
    hc_yAxis(opposite=T,labels=list(align = 'left')) %>%
    hc_plotOptions(
      dataLabels = list(enabled = F),
      marker = list(enabled = F)
    ) %>% 
    hc_series(
      list(
        name = 'Badges Earned',
        data = eval(parse(text=paste0("as.numeric(s$",m,")"))),
        color = '#DFA07F' #'#FF948B'
      )
    )
}

getLevels <- function(s,t,r,max_level=F,print=F){
  colnames(s)[2] <- 'Level'
  s$`% Users` <- as.numeric(s$unique)/t[12]
  s1 <- s[s$delta_type =='none',c('Level','unique','% Users')]
  s2 <- s[s$delta_type =='delta7',c('Level','unique')]
  s3 <- s[s$delta_type =='delta28',c('Level','unique')]
  s4 <- s[s$delta_type =='delta84',c('Level','unique')]
  s <- merge(merge(merge(s1,s2,by='Level',all.x=T),s3,by='Level',all.x=T),s4,by='Level',all.x=T)
  colnames(s) <- c('Level','Unique Users','% Users','7-Day Growth','28-Day Growth','12-Week Growth')
  for(j in 1:ncol(s))
    s[[j]][is.na(s[[j]])] = 0
  s$`7-Day Growth` <- s$`7-Day Growth` <- deltaMaker(s$`Unique Users`,s$`7-Day Growth`)
  s$`28-Day Growth` <- s$`28-Day Growth` <- deltaMaker(s$`Unique Users`,s$`28-Day Growth`)
  s$`12-Week Growth` <- s$`12-Week Growth` <- deltaMaker(s$`Unique Users`,s$`12-Week Growth`)
  if(print==F){
    s <- addSortColumns(s,1)
    tryCatch({
      if('Unique Users' %in% colnames(s)){s$`Unique Users_rank` <- formx_numeric(s$`Unique Users_rank`)}
      if('% Users' %in% colnames(s)){s$`% Users_rank` <- makePercent(100*as.numeric(s$`% Users_rank`),1)}
      if('7-Day Growth' %in% colnames(s)){s$`7-Day Growth_rank` <- makePercent(100*as.numeric(s$`7-Day Growth_rank`),1)}
      if('28-Day Growth' %in% colnames(s)){s$`28-Day Growth_rank` <- makePercent(100*as.numeric(s$`28-Day Growth_rank`),1)}
      if('12-Week Growth' %in% colnames(s)){s$`12-Week Growth_rank` <- makePercent(100*as.numeric(s$`12-Week Growth_rank`),1)}
    },error = function(err){})
    s <- s[order(factor(s$Level,levels = c(0:1000,'Null','All Levels'))),]
    if(max_level==T){s$Level_rank <- unlist(lapply(s$Level_rank,function(x) paste0(x,' min')))}
  }
  else if(print==T){
    s <- s[order(factor(s$Level,levels = c(0:1000,'Null','All Levels'))),]
    if(max_level==T){s$Level <- unlist(lapply(s$Level,function(x) paste0(x,' min')))}
  }
  s
}

getWeeklies <- function(s,t,print=F){
  colnames(s)[2] <- 'Category'
  s$`% Users` <- as.numeric(s$unique)/t[12]
  s1 <- s[s$delta_type =='none',c('Category','count','unique','% Users')]
  s2 <- s[s$delta_type =='delta7',c('Category','count')]
  s3 <- s[s$delta_type =='delta28',c('Category','count')]
  s4 <- s[s$delta_type =='delta84',c('Category','count')]
  s <- merge(merge(merge(s1,s2,by='Category',all.x=T),s3,by='Category',all.x=T),s4,by='Category',all.x=T)
  colnames(s) <- c('Category','Total','Unique Users','% Users','7-Day Growth','28-Day Growth','12-Week Growth')
  for(j in 1:ncol(s))
    s[[j]][is.na(s[[j]])] = 0
  s$`7-Day Growth` <- s$`7-Day Growth` <- deltaMaker(s$`Total`,s$`7-Day Growth`)
  s$`28-Day Growth` <- s$`28-Day Growth` <- deltaMaker(s$`Total`,s$`28-Day Growth`)
  s$`12-Week Growth` <- s$`12-Week Growth` <- deltaMaker(s$`Total`,s$`12-Week Growth`)
  if(print==F){
    s <- addSortColumns(s,1)
    tryCatch({
      if('Total' %in% colnames(s)){s$`Total_rank` <- formx_numeric(s$`Total_rank`)}
      if('Unique Users' %in% colnames(s)){s$`Unique Users_rank` <- formx_numeric(s$`Unique Users_rank`)}
      if('% Users' %in% colnames(s)){s$`% Users_rank` <- makePercent(100*as.numeric(s$`% Users_rank`),1)}
      if('7-Day Growth' %in% colnames(s)){s$`7-Day Growth_rank` <- makePercent(100*as.numeric(s$`7-Day Growth_rank`),1)}
      if('28-Day Growth' %in% colnames(s)){s$`28-Day Growth_rank` <- makePercent(100*as.numeric(s$`28-Day Growth_rank`),1)}
      if('12-Week Growth' %in% colnames(s)){s$`12-Week Growth_rank` <- makePercent(100*as.numeric(s$`12-Week Growth_rank`),1)}
    },error = function(err){})
    s[] <- lapply(s, as.character)
    s$Category_rank[s$Category_rank == '0-50'] <- '1-50'
    s <- s[order(factor(s$Category,levels = c('0-50','1-50','51-100','101-150','151-200','201-250','251-300','301-350','351-400','401-450','451-500','501-550','551-600','601-650','651-700','701-750','751-800','801-850','851-900','901-950','951+'))),]
    s$Category_rank <- paste0(s$Category_rank,' min')
  }
  else{
    s$Category[s$Category == '0-50'] <- '1-50'
    s <- s[order(factor(s$Category,levels = c('0-50','1-50','51-100','101-150','151-200','201-250','251-300','301-350','351-400','401-450','451-500','501-550','551-600','601-650','651-700','701-750','751-800','801-850','851-900','901-950','951+'))),]
    s$Category <- paste0(s$Category,' min')    
  }
  s
}

getTotalMinutes <- function(s,t,r,print=F,pct=F){
  s$`% Users` <- as.numeric(s$unique)/as.numeric(t[7])
  s1 <- s[s$delta_type =='none',c('Metric','total','unique','% Users')]
  s2 <- s[s$delta_type =='delta7',c('Metric','total')]
  s3 <- s[s$delta_type =='delta28',c('Metric','total')]
  s4 <- s[s$delta_type =='delta84',c('Metric','total')]
  s <- join(join(join(s1,s2,by='Metric'),s3,by='Metric'),s4,by='Metric') #all.x=T
  colnames(s) <- c('Type','Total','Unique Users','% Users','7-Day Growth','28-Day Growth','12-Week Growth')
  for(j in 1:ncol(s))
    s[[j]][is.na(s[[j]])] = 0
  s$`Daily Average` <- s$`Total`/r
  s$`Weekly Average` <- 7*s$`Total`/r
  s$`7-Day Growth` <- (s$`Total`-s$`7-Day Growth`)/s$`7-Day Growth`
  s$`28-Day Growth` <- (s$`Total`-s$`28-Day Growth`)/s$`28-Day Growth`
  s$`12-Week Growth` <- (s$`Total`-s$`12-Week Growth`)/s$`12-Week Growth`
  setcolorder(s,c('Type','Total','Daily Average','Weekly Average','Unique Users','% Users','7-Day Growth','28-Day Growth','12-Week Growth'))
  s <- s[,c('Type','Total','Daily Average','Weekly Average','7-Day Growth','28-Day Growth','12-Week Growth')] #suppress user cols
  tryCatch({
    if('Total' %in% colnames(s)){
      if(pct==F){s$`Total` <- formx_numeric(s$`Total`)}
      else{s$`Total` <- makePercent(100*s$`Total`,2)}
    }
    if('Unique Users' %in% colnames(s)){s$`Unique Users` <- formx_numeric(s$`Unique Users`)}
    if('% Users' %in% colnames(s)){s$`% Users` <- makePercent(100*s$`% Users`,1)}
    if('7-Day Growth' %in% colnames(s)){s$`7-Day Growth` <- makePercent(100*s$`7-Day Growth`,1)}
    if('28-Day Growth' %in% colnames(s)){s$`28-Day Growth` <- makePercent(100*s$`28-Day Growth`,1)}
    if('12-Week Growth' %in% colnames(s)){s$`12-Week Growth` <- makePercent(100*s$`12-Week Growth`,1)}
  },error = function(err){})
  if(pct==F){
    s$`Daily Average` <- format(round(s$`Daily Average`,1),big.mark=',')
    s$`Weekly Average` <- format(round(s$`Weekly Average`,1),big.mark=',')
  }
  else{
    s$`Daily Average` <- '--'
    s$`Weekly Average` <- '--'
  }
  for(i in names(s)){eval(parse(text=paste0("s$`",i,"` <- gsub('(NA(|N)|Inf)%','--',s$`",i,"`,ignore.case=T)")))}
  s
}

getManualMinutes <- function(s,t,r){
  s$`% Users` <- s$unique/t[1]
  s$activityType <- unlist(lapply(s$activityType,function(x) gsub('^Yog.*','Yoga & Pilates',titlec(gsub('_',' ',gsub('paction.','',x))))))  #Yoga/pilates
  s <- s[,lapply(.SD,sum),by = c('activityType','delta_type')]
  s1 <- s[s$delta_type =='none',c('activityType','total','unique','% Users')]
  s2 <- s[s$delta_type =='delta7',c('activityType','total')]
  s3 <- s[s$delta_type =='delta28',c('activityType','total')]
  s4 <- s[s$delta_type =='delta84',c('activityType','total')]
  s <- merge(merge(merge(s1,s2,by='activityType',all.x=T),s3,by='activityType',all.x=T),s4,by='activityType',all.x=T)
  colnames(s) <- c('Activity Type','Minutes','Unique Users','% Users','7-Day Growth','28-Day Growth','12-Week Growth')
  s$`Daily Average` <- s$`Minutes`/r
  s$`Weekly Average` <- 7*s$`Minutes`/r
  s$`7-Day Growth` <- (s$`Minutes`-s$`7-Day Growth`)/s$`7-Day Growth`
  s$`28-Day Growth` <- (s$`Minutes`-s$`28-Day Growth`)/s$`28-Day Growth`
  s$`12-Week Growth` <- (s$`Minutes`-s$`12-Week Growth`)/s$`12-Week Growth`
  setcolorder(s,c('Activity Type','Minutes','Daily Average','Weekly Average','Unique Users','% Users','7-Day Growth','28-Day Growth','12-Week Growth'))
  s <- s[,c('Activity Type','Minutes','Daily Average','Weekly Average','7-Day Growth','28-Day Growth','12-Week Growth')] #suppress users
  s <- addSortColumns(s,1)
  tryCatch({
    if('Minutes' %in% colnames(s)){s$`Minutes_rank` <- formx_numeric(s$`Minutes_rank`)}
    if('Unique Users' %in% colnames(s)){s$`Unique Users_rank` <- formx_numeric(s$`Unique Users_rank`)}
    if('% Users' %in% colnames(s)){s$`% Users_rank` <- makePercent(100*s$`% Users_rank`,1)}
    if('7-Day Growth' %in% colnames(s)){s$`7-Day Growth_rank` <- makePercent(100*s$`7-Day Growth_rank`,1)}
    if('28-Day Growth' %in% colnames(s)){s$`28-Day Growth_rank` <- makePercent(100*s$`28-Day Growth_rank`,1)}
    if('12-Week Growth' %in% colnames(s)){s$`12-Week Growth_rank` <- makePercent(100*s$`12-Week Growth_rank`,1)}
  },error = function(err){})
  s$`Daily Average_rank` <- format(round(s$`Daily Average_rank`,1),big.mark=',')
  s$`Weekly Average_rank` <- format(round(s$`Weekly Average_rank`,1),big.mark=',')
  #for(j in 1:ncol(s))
  #  s[[j]][is.na(s[[j]])] = '--'
  #is.na(s)<-sapply(s, is.infinite)
  s[is.na(s)]<-'--'
  s <- s[order(factor(s$`Activity Type`,levels = c(s$`Activity Type`[s$`Activity Type` != 'Other'],'Other'))),]
}

makeShinyTables <- function(t){
  if(tableType == 'data.table'){DT::dataTableOutput(t)}
  else{tableOutput(t)}
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LAYOUT
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
badges <- tabPanel('Badges',
                   mainPanel(
                     fluidRow(
                       tags$h2('Summary of Badges Earned')
                     ),
                     fluidRow(class='top_metric_names right_pad',
                              tags$div(class='metric_name_box','BADGES CLAIMED'),
                              tags$div(class='metric_name_box','7-DAY DELTA'),
                              tags$div(class='metric_name_box','28-DAY DELTA'),
                              tags$div(class='metric_name_box split_box'),
                              tags$div(class='metric_name_box','UNIQUE USERS'),
                              tags$div(class='metric_name_box','BADGES / USER'),
                              tags$div(class='metric_name_box','NEW USERS'),
                              tags$div(class='metric_name_box metric_last','% NEW USERS')
                     ),
                     fluidRow(class='top_metrics right_pad',
                              tags$div(class='metric_box',textOutput('badge_upper1')),
                              tags$div(class='metric_box',textOutput('badge_upper6')),
                              tags$div(class='metric_box',textOutput('badge_upper7')),
                              tags$div(class='metric_box split_box'),
                              tags$div(class='metric_box',textOutput('badge_upper2')),
                              tags$div(class='metric_box',textOutput('badge_upper3')),
                              tags$div(class='metric_box',textOutput('badge_upper4')),
                              tags$div(class='metric_box metric_last',textOutput('badge_upper5'))
                     ),
                     fluidRow(class=paste0('table_holder sortcols_active ',tableType),
                              #tags$h3(class='h3-float h3-text h3-first','Badges Earned: All Badge Types'),
                              #tags$div(class='chart_top',highchartOutput('badges_chart_1')),
                              tags$h3(class='h3-float h3-text h3-first','Engagement Badges'),
                              tags$div(class='badges-table sort table-expanded',makeShinyTables('badges_1')),
                              tags$div(class='badges-table sort table-total',makeShinyTables('subtot_1')),
                              tags$h3(class='h3-float h3-text h3-first','Activity Badges'),
                              tags$div(class='badges-table sort table-expanded',makeShinyTables('badges_2')),
                              tags$div(class='badges-table sort table-total',makeShinyTables('subtot_2')),
                              tags$h3(class='h3-float h3-text h3-first','Milestone Badges'),
                              tags$div(class='badges-table sort table-expanded',makeShinyTables('badges_3')),
                              tags$div(class='badges-table sort table-total',makeShinyTables('subtot_3')),
                              tags$h3(class='h3-float h3-text h3-first','Number of Badges per User'),
                              tags$div(class='badges-table sort table-expanded pagination-true',makeShinyTables('badges_4'))
                     )
                     #fluidRow(class='chart_holder',
                     #  tags$h3(class='h3-float h3-text h3-first','Badges Earned: All Badge Types'),
                     #  tags$div(class='chart_top',highchartOutput('badges_chart_1'))
                     #)
                   )
)

rewards <- tabPanel('Rewards',
                    mainPanel(
                      fluidRow(
                        tags$h2('Summary of Rewards Claimed')
                      ),
                      fluidRow(class='top_metric_names right_pad',
                               tags$div(class='metric_name_box','WEEKLY BALLOTS'),
                               tags$div(class='metric_name_box','MONTHLY BALLOTS'),
                               tags$div(class='metric_name_box','QUARTERLY BALLOTS'),
                               tags$div(class='metric_name_box split_box'),
                               tags$div(class='metric_name_box','UNIQUE WINNERS'),
                               tags$div(class='metric_name_box','% USERS WINNING'),
                               tags$div(class='metric_name_box','FREQUENT WINNERS'),
                               tags$div(class='metric_name_box metric_last','CHEATERS')   
                      ),
                      fluidRow(class='top_metrics right_pad',
                               tags$div(class='metric_box',textOutput('reward_upper1')),
                               tags$div(class='metric_box',textOutput('reward_upper2')),
                               tags$div(class='metric_box',textOutput('reward_upper3')),
                               tags$div(class='metric_box split_box'),
                               tags$div(class='metric_box',textOutput('reward_upper4')),
                               tags$div(class='metric_box',textOutput('reward_upper5')),
                               tags$div(class='metric_box',textOutput('reward_upper6')),
                               tags$div(class='metric_box metric_last',textOutput('reward_upper7'))
                      ),
                      fluidRow(class=paste0('table_holder sortcols_active ',tableType),
                               #tags$h3(class='h3-float h3-text h3-first','Ballots Earned'),
                               #tags$div(class='chart_top',highchartOutput('reward_chart_1')),
                               tags$h3(class='h3-float h3-text h3-first','Engagement Ballots'),
                               tags$div(class='badges-table sort ballots table-expanded',makeShinyTables('reward_1')),
                               tags$div(class='badges-table sort ballots table-total',makeShinyTables('subtot_4')),
                               tags$h3(class='h3-float h3-text h3-first','Activity Ballots'),
                               tags$div(class='badges-table sort ballots table-expanded',makeShinyTables('reward_2')),
                               tags$div(class='badges-table sort ballots table-total',makeShinyTables('subtot_5')),
                               tags$h3(class='h3-float h3-text h3-first','Reward Winners'),
                               tags$div(class='badges-table sort ballots table-expanded',makeShinyTables('reward_3')),
                               tags$div(class='badges-table sort ballots table-total',makeShinyTables('subtot_6')),
                               if(email_len > 0){tags$h3(class='h3-float h3-text h3-first','Email Interaction')},
                               if(email_len > 0){tags$div(class='badges-table sort ballots table-expanded',makeShinyTables('reward_6'))},
                               if(notif_len > 0){tags$h3(class='h3-float h3-text h3-first','Notification Interaction')},
                               if(notif_len > 0){tags$div(class='badges-table sort ballots table-expanded',makeShinyTables('reward_7'))},
                               tags$h3(class='h3-float h3-text h3-first','Frequent Winners (> 2 Rewards Won)'),
                               tags$div(class='badges-table sort ballots table-expanded pagination-true',makeShinyTables('reward_4')),
                               tags$h3(class='h3-float h3-text h3-first','Potential Cheaters (> 10000 Ballots)'),
                               tags$div(class='badges-table sort cheaters table-expanded pagination-true',makeShinyTables('reward_5'))
                      )
                      #fluidRow(class='chart_holder',
                      #  tags$h3(class='h3-float h3-text h3-first','Ballots Earned'),
                      #  tags$div(class='chart_top',highchartOutput('reward_chart_1'))
                      #)
                    )
)

activity <- tabPanel('Activity',
                     mainPanel(
                       fluidRow(
                         tags$h2('Summary of Activity & Workouts')
                       ),
                       fluidRow(class='top_metric_names right_pad',
                                tags$div(class='metric_name_box activity_box_nine','TOTAL MVPA MIN'),
                                tags$div(class='metric_name_box activity_box_nine','7-DAY GROWTH'),
                                tags$div(class='metric_name_box activity_box_nine','28-DAY GROWTH'),
                                tags$div(class='metric_name_box activity_box_nine','12-WK GROWTH'),
                                tags$div(class='metric_name_box activity_box_nine','DEVICE TOTAL'),
                                tags$div(class='metric_name_box activity_box_nine','MANUAL TOTAL'),
                                tags$div(class='metric_name_box split_box'),
                                tags$div(class='metric_name_box activity_box_nine','TOTAL USERS'),
                                tags$div(class='metric_name_box activity_box_nine','MVPA USERS'),
                                tags$div(class='metric_name_box activity_box_nine','MVPA MINS / USER'),
                                tags$div(class='metric_name_box activity_box_nine metric_last','TOTAL STEPS')
                       ),
                       fluidRow(class='top_metrics right_pad',
                                uiOutput('activity_upper4'),
                                tags$div(class='metric_box activity_box_nine',textOutput('activity_upper6')),
                                tags$div(class='metric_box activity_box_nine',textOutput('activity_upper6a')),
                                tags$div(class='metric_box activity_box_nine',textOutput('activity_upper7')),
                                uiOutput('activity_upper2'),
                                uiOutput('activity_upper3'),
                                tags$div(class='metric_box split_box'),
                                tags$div(class='metric_box activity_box_nine',textOutput('activity_upper1a')),
                                tags$div(class='metric_box activity_box_nine',textOutput('activity_upper1')),
                                tags$div(class='metric_box activity_box_nine',textOutput('activity_upper5')),
                                uiOutput('activity_upper8')
                       ),
                       fluidRow(class=paste0('table_holder sortcols_active activity_holder ',tableType),
                                tags$h3(class='h3-float h3-text h3-first','User Onboarding Levels'),
                                tags$div(class='badges-table sort table-expanded',makeShinyTables('activity_1')),
                                tags$div(class='badges-table sort table-total',makeShinyTables('subtot_7')),
                                tags$h3(class='h3-float h3-text h3-first','Current Weekly Goal'),
                                tags$div(class='badges-table sort table-expanded',makeShinyTables('activity_2')),
                                tags$div(class='badges-table sort table-total',makeShinyTables('subtot_8')),
                                tags$h3(class='h3-float h3-text h3-first','Weekly Totals'),
                                tags$div(class='badges-table sort table-expanded',makeShinyTables('activity_3')),
                                tags$div(class='badges-table sort table-total',makeShinyTables('subtot_9')),
                                
                                # manual vs device
                                tags$h3(class='h3-float h3-text h3-first','Total Activity Data (Active Minutes)'),
                                tags$div(class='badges-table table-expanded',makeShinyTables('activity_4')),
                                tags$div(class='badges-table table-total',makeShinyTables('subtot_10')),
                                
                                tags$h3(class='h3-float h3-text h3-first','Activity Health Data'),
                                tags$div(class='badges-table table-expanded',makeShinyTables('activity_5')),
                                
                                # manual workout types
                                tags$h3(class='h3-float h3-text h3-first','Manual Entry Minutes'),
                                tags$div(class='badges-table sort table-expanded',makeShinyTables('activity_6')),
                                tags$div(class='badges-table sort table-total',makeShinyTables('subtot_11'))
                       )
                     )
)

app_report <- tabPanel('App Behavior',
                     mainPanel(
                       fluidRow(
                         tags$h2('App Behavior Report')
                       ),
                       fluidRow(
                         tags$div(class='table_holder',
                                  tags$h3(class='h3-float h3-text h3-first','First-Time App Opens')
                         ),
                         tags$div(class='chart_holder',
                                  tags$h3(class='h3-float h3-text h3-first','App Metrics')
                         )
                       )
                     )
)

geo_report <- tabPanel('Geo Data',
                     mainPanel(
                       fluidRow(
                         tags$h2('If we have any time :)')
                       )
                     )
)