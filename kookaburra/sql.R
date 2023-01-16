#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SQL DB QUERIES FOR PACTION
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# MAIN CATCH-ALL SQL QUERY
my_mega_query <- function(p,d,c,max_level,answerValue,province,download=F,filename=NULL){
  if(p=='quarter'){p2 <- c('quarter','quarter','quarter')}
  else if(p=='month'){p2 <- c('month','month','quarter')}
  else if(p=='week'){p2 <- c('week','month','quarter')}
  if(d == 'All Quarters'){d1 <- c('All Quarters','All Quarters','All Quarters')}
  else if(p=='quarter'){d1 <- c(as.Date(d),as.Date(d),as.Date(d))}
  else if(p=='month'){d1 <- c(as.Date(d),as.Date(d),as.Date(max(quarters[quarters < d])))}
  else if(p=='week'){d1 <- c(as.Date(d),as.Date(max(months[months < d])),as.Date(max(quarters[quarters < d])))}
  
  #email params
  if(d == 'All Quarters'){d2 <- c('','','')}
  else if(p=='quarter'){d2 <- c(paste0(p2[1]," = '",as.Date(d),"'"),paste0(p2[2]," = '",as.Date(d),"'"),paste0(p2[3]," = '",as.Date(d),"'"))}
  else if(p=='month'){d2 <- c(paste0(p2[1]," = '",as.Date(d),"'"),paste0(p2[2]," = '",as.Date(d),"'"),paste0(p2[3]," = '",as.Date(max(quarters[quarters < d])),"'"))}
  else if(p=='week'){d2 <- c(paste0(p2[1]," = '",as.Date(d),"'"),paste0(p2[2]," = '",as.Date(max(months[months < d])),"'"),paste0(p2[3]," = '",as.Date(max(quarters[quarters < d])),"'"))}
  
  if(max_level != 'All Levels'){m <- paste0(" AND max_level = '",max_level,"'")}
  else{m <- ''}
  if(answerValue != 'All Levels'){a <- paste0(" AND answerValue = '",answerValue,"'")}
  else{a <- ''}
  if(province != 'All Provinces'){v <- paste0(" AND province = '",province,"'")}
  else{v <- ''}
  
  #chart params
  if(d != 'All Quarters'){chart_dates <- paste0("AND period >= '",d[1],"' AND period <= '",d[2],"' ")}
  else{chart_dates <- ""}
  
  #reward winners params
  if(d != 'All Quarters'){win_dates <- paste0(" AND ",p," = '",d1[1],"'")}
  else{win_dates <- ''}
  f <- c('')
  if(max_level != 'All Levels'){f <- c(f,'max_level',max_level)}
  if(answerValue != 'All Levels'){f <- c(f,'answerValue',answerValue)}
  if(province != 'All Provinces'){f <- c(f,'province',province)}
  f <- applyFiltersSQL(f[min(2,len(f)):len(f)])
  
  if(download==F){
    paste0("SELECT 'badges' AS type,award_short_description,deltaType AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,`unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    ballots,
    '1' AS `role`,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS Metric,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinybadges_main WHERE ",p2[1],"_period = '",d1[1],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[1],"' AND province = '",province,"'
    UNION ALL
    SELECT 'badges' AS type,award_short_description,deltaType AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,`unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    ballots,
    '2' AS `role`,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS Metric,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinybadges_main WHERE ",p2[2],"_period = '",d1[2],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[2],"' AND province = '",province,"'
    UNION ALL
    SELECT 'badges' AS type,award_short_description,deltaType AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,`unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    ballots,
    '3' AS `role`,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS Metric,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinybadges_main WHERE ",p2[3],"_period = '",d1[3],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[3],"' AND province = '",province,"'
    UNION ALL
    SELECT
    'new users' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    (SELECT MAX(`new`) FROM dashboard_core_shinybadges_main WHERE ",p2[1],"_period = '",d1[1],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[1],"' AND province = '",province,"' AND deltaType =  'none' AND award_short_description = 'All Categories') AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS Metric,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    UNION ALL
    SELECT
    'badges per user' AS type,
    '' AS award_short_description,
    deltaType AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    badge_count AS count,user_count AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS Metric,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinybadges_counts WHERE max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND period = '",d1[1],"' AND periodType = '",p2[1],"' AND province = '",province,"'
    UNION ALL
    SELECT
    'cheaters' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    customer_id AS `Customer ID`,
    '' AS Metric,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinybadges_cheaters WHERE period = '",d1[1],"' AND periodType = '",p2[1],"'",ifelse(answerValue=='All Levels','',paste0(" AND answerValue = '",answerValue,"'")),ifelse(max_level=='All Levels','',paste0(" AND max_level = '",max_level,"'")),ifelse(province=='All Provinces','',paste0(" AND province = '",province,"'")),"
    UNION ALL
    SELECT
    'email' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    action AS `Metric`,`Weekly Total`,`Monthly Total`,`Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date",
    gsub(' WHERE )',')',gsub('WHERE  ','',gsub('WHERE  AND','WHERE',paste0(" FROM (SELECT 'Email Sent' AS action,COUNT(DISTINCT user_id) AS `Weekly Total` FROM dashboard_core_shinyemail_main WHERE ",d2[1],m,a,v,") A LEFT JOIN (SELECT 'Email Sent' AS action2,COUNT(DISTINCT user_id) AS `Monthly Total` FROM dashboard_core_shinyemail_main WHERE ",d2[2],m,a,v,") B ON B.action2 = A.action  LEFT JOIN (SELECT 'Email Sent' AS action3,COUNT(DISTINCT user_id) AS `Quarterly Total` FROM dashboard_core_shinyemail_main WHERE ",d2[3],m,a,v,") C ON C.action3 = A.action ")))),
    "UNION ALL
    SELECT
    'email' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    action AS `Metric`,`Weekly Total`,`Monthly Total`,`Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date",
    gsub('WHERE  ','',gsub('WHERE  AND','WHERE',paste0(" FROM (SELECT 'Email Opened' AS action,COUNT(DISTINCT user_id) AS `Weekly Total` FROM dashboard_core_shinyemail_main WHERE ",d2[1],m,a,v," AND action LIKE '%open%') A LEFT JOIN (SELECT 'Email Opened' AS action2,COUNT(DISTINCT user_id) AS `Monthly Total` FROM dashboard_core_shinyemail_main WHERE ",d2[2],m,a,v," AND action LIKE '%open%') B ON B.action2 = A.action  LEFT JOIN (SELECT 'Email Opened' AS action3,COUNT(DISTINCT user_id) AS `Quarterly Total` FROM dashboard_core_shinyemail_main WHERE ",d2[3],m,a,v," AND action LIKE '%open%') C ON C.action3 = A.action "))),
    "UNION ALL SELECT
    'charts' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,`unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    date FROM
    (SELECT SUM(count) AS count,SUM(`unique`) AS `unique`,SUM(ballots) AS ballots, period AS date FROM
    dashboard_core_shinybadges_charts WHERE max_level = '",max_level,"' AND answerValue = '",answerValue,"' ",chart_dates,"AND periodType = '",c,"' AND province = '",province,"' AND award_short_description = 'All Badges' GROUP BY period,award_short_description) A
    UNION ALL
    SELECT
    'winners' AS type,
    award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    weekly_total AS `Weekly Total`,monthly_total AS `Monthly Total`,quarterly_total AS `Quarterly Total`,weekly_unique AS `Weekly Users`,monthly_unique AS `Monthly Users`,quarterly_unique AS `Quarterly Users`,
    '' AS date ",
    gsub('shinyrewards AND','shinyrewards WHERE',paste0("FROM (SELECT award_short_description, (SUM(CASE WHEN name LIKE '%Weekly%' THEN 1 ELSE 0 END)) AS weekly_total, (SUM(CASE WHEN name LIKE '%Monthly%' THEN 1 ELSE 0 END)) AS monthly_total, (SUM(CASE WHEN name LIKE '%Quarterly%' THEN 1 ELSE 0 END)) AS quarterly_total FROM dashboard_core_shinyrewards",win_dates,f," GROUP BY award_short_description) A LEFT JOIN (SELECT award_short_description AS aid, IFNULL(COUNT(DISTINCT customer_id),0) AS weekly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Weekly%'",win_dates,f," GROUP BY award_short_description) B ON A.award_short_description = B.aid LEFT JOIN (SELECT award_short_description AS aid2, IFNULL(COUNT(DISTINCT customer_id),0) AS monthly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Monthly%'",win_dates,f," GROUP BY award_short_description) C ON A.award_short_description = C.aid2 LEFT JOIN (SELECT award_short_description AS aid3, IFNULL(COUNT(DISTINCT customer_id),0) AS quarterly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Quarterly%'",win_dates,f," GROUP BY award_short_description) D ON A.award_short_description = D.aid3")),
    " UNION ALL
    SELECT
    'total winners' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    weekly_total AS `Weekly Total`,monthly_total AS `Monthly Total`,quarterly_total AS `Quarterly Total`,weekly_unique AS `Weekly Users`,monthly_unique AS `Monthly Users`,quarterly_unique AS `Quarterly Users`,
    '' AS date ",
    gsub('shinyrewards AND','shinyrewards WHERE',paste0("FROM (SELECT 'Total' as total, (SUM(CASE WHEN name LIKE '%Weekly%' THEN 1 ELSE 0 END)) AS weekly_total, (SUM(CASE WHEN name LIKE '%Monthly%' THEN 1 ELSE 0 END)) AS monthly_total,(SUM(CASE WHEN name LIKE '%Quarterly%' THEN 1 ELSE 0 END)) AS quarterly_total FROM dashboard_core_shinyrewards",win_dates,f," GROUP BY total) A LEFT JOIN (SELECT 'Total' AS aid, IFNULL(COUNT(DISTINCT customer_id),0) AS weekly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Weekly%'",win_dates,f,") B ON A.total = B.aid LEFT JOIN (SELECT 'Total' AS aid2, IFNULL(COUNT(DISTINCT customer_id),0) AS monthly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Monthly%'",win_dates,f,") C ON A.total = C.aid2 LEFT JOIN (SELECT 'Total' AS aid3, IFNULL(COUNT(DISTINCT customer_id),0) AS quarterly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Quarterly%'",win_dates,f,") D ON A.total = D.aid3")),
    " UNION ALL
    SELECT
    'unique winners' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM ",
    gsub('shinyrewards AND','shinyrewards WHERE',paste0("(SELECT IFNULL(COUNT(DISTINCT customer_id),0) AS `unique` FROM dashboard_core_shinyrewards",win_dates,f,") A")),
    " UNION ALL
    SELECT
    'frequent winners' AS type,
    '' AS award_short_description,
    '' AS delta_type,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS  answerValue,
    '' AS  max_level,
    '' AS  province,
    '' AS  activityType,
    customer_id AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    Weekly AS `Weekly Users`,
    Monthly AS `Monthly Users`,
    Quarterly AS `Quarterly Users`,
    '' AS date
    FROM (SELECT customer_id,
    CASE WHEN name LIKE '%Weekly%' THEN SUM(A.count) ELSE 0 END AS Weekly,
    CASE WHEN name LIKE '%Monthly%' THEN SUM(A.count) ELSE 0 END AS Monthly,
    CASE WHEN name LIKE '%Quarterly%' THEN SUM(A.count) ELSE 0 END AS Quarterly
    FROM (select DISTINCT customer_id as customer_id, count(award_short_description) as count,name From dashboard_core_shinyrewards 
    WHERE award_short_description LIKE '%Card%'",win_dates,f," GROUP BY customer_id,name) A WHERE A.count > 2 GROUP BY customer_id,name) A GROUP BY customer_id,Weekly,Monthly,Quarterly
    UNION ALL SELECT 'activity answerValue' AS type,
    '' AS award_short_description,
    deltaType,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,
    `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    answerValue AS role,  #answerValue AS role,
    answerValue,
    '' AS max_level,
    '' AS  province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinyactivity_main WHERE period = '",d,"' AND periodType = '",p,"'",ifelse(max_level=='All Levels','',paste0(" AND max_level = '",max_level,"'")),ifelse(province=='All Provinces','',paste0(" AND province = '",province,"'"))," AND goalMinutes = 'All Goals' AND activeTime = 'All Times' AND workoutType = 'All Activities' AND max_level != 'All Levels' AND province != 'All Provinces' AND answerValue != 'All Levels'
    UNION ALL
    SELECT 'activity goal level' AS type,
    '' AS award_short_description,
    deltaType,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,
    `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    goalMinutes AS role,
    '' AS answerValue,
    '' AS max_level,
    '' AS province,
    '' AS  activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinyactivity_main WHERE period = '",d,"' AND periodType = '",p,"'",ifelse(answerValue=='All Levels','',paste0(" AND answerValue = '",answerValue,"'")),ifelse(max_level=='All Levels','',paste0(" AND max_level = '",max_level,"'")),ifelse(province=='All Provinces','',paste0(" AND province = '",province,"'"))," AND activeTime = 'All Times' AND workoutType = 'All Activities' AND answerValue != 'All Levels' AND max_level != 'All Levels' AND province != 'All Provinces'
    UNION ALL
    SELECT 'activity weeklies' AS type,
    '' AS award_short_description,
    deltaType,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    '' AS master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    count,
    `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    '' AS unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS answerValue,
    '' AS max_level,
    '' AS province,
    '' AS activityType,
    '' AS `Customer ID`,
    activeTime AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date  
    FROM dashboard_core_shinyactivity_main WHERE period = '",d,"' AND periodType = '",p,"'",ifelse(answerValue=='All Levels','',paste0(" AND answerValue = '",answerValue,"'")),ifelse(max_level=='All Levels','',paste0(" AND max_level = '",max_level,"'")),ifelse(province=='All Provinces','',paste0(" AND province = '",province,"'"))," AND goalMinutes = 'All Goals' AND workoutType = 'All Activities' AND answerValue != 'All Levels' AND max_level != 'All Levels' AND province != 'All Provinces'
    UNION ALL
    SELECT 'minutes and steps' AS type,
    '' AS award_short_description,
    deltaType,
    master_active,
    master_move,
    master_device,
    master_active_fitbit,
    master_active_garmin,
    master_move_fitbit,
    master_move_garmin,
    master_manual,
    heartPoints,
    exerciseMinutes,
    activeMinutes,
    master_steps,
    '' AS total,
    count,
    '' AS `unique`,
    unique_master_active,
    unique_master_move,
    unique_master_device,
    unique_master_active_fitbit,
    unique_master_active_garmin,
    unique_master_move_fitbit,
    unique_master_move_garmin,
    unique_master_manual,
    unique_heartPoints,
    unique_exerciseMinutes,
    unique_activeMinutes,
    unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS answerValue,
    '' AS max_level,
    '' AS province,
    '' AS activityType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date  
    FROM dashboard_core_shinyactivity_main WHERE period = '",d,"' AND periodType = '",p,"'",ifelse(answerValue=='All Levels','',paste0(" AND answerValue = '",answerValue,"'")),ifelse(max_level=='All Levels','',paste0(" AND max_level = '",max_level,"'")),ifelse(province=='All Provinces','',paste0(" AND province = '",province,"'"))," AND goalMinutes = 'All Goals' AND activeTime = 'All Times' AND workoutType = 'All Activities' AND answerValue != 'All Levels' AND max_level != 'All Levels' AND province != 'All Provinces'
    UNION ALL
    SELECT 'manual workouts' AS type,
    '' AS award_short_description,
    deltaType,
    '' AS master_active,
    '' AS master_move,
    '' AS master_device,
    '' AS master_active_fitbit,
    '' AS master_active_garmin,
    '' AS master_move_fitbit,
    '' AS master_move_garmin,
    master_manual,
    '' AS heartPoints,
    '' AS exerciseMinutes,
    '' AS activeMinutes,
    '' AS master_steps,
    '' AS total,
    '' AS count,
    '' AS `unique`,
    '' AS unique_master_active,
    '' AS unique_master_move,
    '' AS unique_master_device,
    '' AS unique_master_active_fitbit,
    '' AS unique_master_active_garmin,
    '' AS unique_master_move_fitbit,
    '' AS unique_master_move_garmin,
    unique_master_manual,
    '' AS unique_heartPoints,
    '' AS unique_exerciseMinutes,
    '' AS unique_activeMinutes,
    '' AS unique_master_steps,
    '' AS ballots,
    '' AS role,
    '' AS answerValue,
    '' AS max_level,
    '' AS province,
    workoutType,
    '' AS `Customer ID`,
    '' AS `Metric`,
    '' AS `Weekly Total`,
    '' AS `Monthly Total`,
    '' AS `Quarterly Total`,
    '' AS `Weekly Users`,
    '' AS `Monthly Users`,
    '' AS `Quarterly Users`,
    '' AS date
    FROM dashboard_core_shinyactivity_main WHERE period = '",d,"' AND periodType = '",p,"'",ifelse(answerValue=='All Levels','',paste0(" AND answerValue = '",answerValue,"'")),ifelse(max_level=='All Levels','',paste0(" AND max_level = '",max_level,"'")),ifelse(province=='All Provinces','',paste0(" AND province = '",province,"'"))," AND goalMinutes = 'All Goals' AND activeTime = 'All Times' AND answerValue != 'All Levels' AND max_level != 'All Levels' AND province != 'All Provinces';")
  }
  #FROM dashboard_core_shinyactivity_main WHERE period = '",d,"' AND periodType = '",p,"' AND answerValue = '",answerValue,"' AND max_level = '",max_level,"' AND province = '",province,"' AND goalMinutes = 'All Goals' AND activeTime = 'All Times';")
  else{
    get_badges_raw <- gsub('shinybadges AND','shinybadges WHERE',paste0("SELECT
      customer_id AS `Customer ID`,
      clearbridge_appuser_id AS `Clearbridge ID`,
      customer_award_id AS `Custoimer Award ID`,
      award_short_description AS Description,
      age AS Age,
      gender AS Gender,
      answerValue AS `User Onboarding Level`,
      max_level AS `Current Weekly Goal`,
      `date` AS Date,
      `week` AS Week,
      `month` AS Month,
      `quarter` AS Quarter,
      award_category AS `Category`,
      num_ballots AS `Ballots Earned`,
      province AS Province,
      `time` AS `UTC Timestamp`
      FROM dashboard_core_shinybadges",win_dates,f))
    get_rewards_raw <- gsub('shinyrewards AND','shinyrewards WHERE',paste0("SELECT
      customer_id AS `Customer ID`,
      clearbridge_appuser_id AS `Clearbridge ID`,
      customer_award_id AS `Customer Award ID`,
      award_id AS `Award ID`,
      `name` AS Name,
      award_short_description AS Description,
      `date` AS Date,
      `week` AS Week,
      `month` AS Month,
      `quarter` AS Quarter,
      province AS Province,
      gender AS Gender,
      age AS Age,
      answerValue AS `User Onboarding Level`,
      max_level AS `Current Weekly Goal`
      FROM dashboard_core_shinyrewards",win_dates,f))
    get_activity_raw <- gsub('shinyactivity AND','shinyactivity WHERE',paste0("SELECT
      userId,syncSource,province,age,gender,answerValue,max_level,week,month,quarter,week7,month7,quarter7,week28,month28,quarter28,week84,month84,quarter84,activeTime,goalMinutes,workoutType,exerciseMinutes,heartPoints,activeMinutes,lightPhysicalActivity,moveMinutes,steps,manualActiveMinutes,fitbit_steps,fitbit_minutesVeryActive,fitbit_minutesFairlyActive,fitbit_minutesLightlyActive,garmin_steps,garmin_activeTimeInSeconds,garmin_moderateIntensityDurationInSeconds,garmin_vigorousIntensityDurationInSeconds,master_active,master_move,master_device,master_active_fitbit,master_active_garmin,master_move_fitbit,master_move_garmin,master_manual,master_steps
      FROM dashboard_core_shinyactivity",win_dates,f))
    eval(parse(text=paste0('get_',filename)))
  }
}

#STARTUP ARRAYS OF DATA
getLoadingVals <- "SELECT MAX(CAST(max_level AS UNSIGNED)) AS value,'max_level' AS type FROM dashboard_core_shinybadges
UNION SELECT DISTINCT province AS value, 'province' AS type FROM dashboard_core_shinybadges WHERE province != 'Other'
UNION SELECT DISTINCT week AS value, 'week' AS type FROM dashboard_core_shinybadges
UNION SELECT DISTINCT month AS value, 'month' AS type FROM dashboard_core_shinybadges
UNION SELECT DISTINCT quarter AS value, 'quarter' AS type FROM dashboard_core_shinybadges
UNION SELECT DISTINCT answerValue AS value, 'answerValue' as type FROM dashboard_core_shinybadges WHERE answerValue IN ('1','2','3','4','5','6','7','8')
UNION SELECT COUNT(*) AS count, 'total_users' as type FROM dashboard_core_shinycustomer 
UNION SELECT DATE(MAX(startDate)) AS value, 'max_date' as type FROM WORKOUTSAMPLE
UNION SELECT COUNT(*) AS email_len,'email_len' AS type from dashboard_core_shinyemail_main
UNION SELECT COUNT(*) AS notif_len,'notif_len' AS type from dashboard_core_shinyemail_main;"

#RAW DOWNLOAD PULLS (TEASE WITH CAUTION!)
get_badges_raw <- "SELECT customer_id AS `Customer ID`,
                  clearbridge_appuser_id AS `Clearbridge ID`,
                  customer_award_id AS `Custoimer Award ID`,
                  award_short_description AS `Description`,
                  age AS `Age`,
                  gender AS `Gender`,
                  answerValue AS `User Onboarding Level`,
                  max_level AS `Current Weekly Goal`,
                  `date` AS `Date`,
                  `week` AS `Week`,
                  `month` AS `Month`,
                  `quarter` AS `Quarter`,
                  award_category AS `Category`,
                  num_ballots AS `Ballots Earned`,
                  province AS `Province`,
                  `time AS `UTC Timestamp`
                  FROM dashboard_core_shinybadges WHERE week = '' AND ;"
get_rewards_raw <- "SELECT customer_award_id,clearbridge_appuser_id,customer_id,award_id,`name`,award_short_description,`date`,`week`,`month`,`quarter`,province,gender,age,answerValue,max_level FROM dashboard_core_shinyrewards;"
get_activity_customers_raw <- "SELECT * FROM dashboard_core_shinyactivity;"
get_activity_minutes_raw <- "SELECT * FROM dashboard_core_shinyminutes;"
get_activity_health_raw <- "SELECT * FROM dashboard_core_shinyhealth;"

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LEGACY CODE (NOT USED IN CURRENT PRODUCTION)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# TABLE MAKING
myquery_execute <- c(
  #-----------REWARD CYCLES TABLE------------#
  c("DROP TABLE IF EXISTS dashboard_core_shinyrewardcycles;"),
  c("CREATE TABLE dashboard_core_shinyrewardcycles SELECT _uid,cycle_sequence,starting_date,ending_date,SUBDATE(DATE(starting_date), WEEKDAY(starting_date)) AS startDate2,SUBDATE(DATE(ending_date),
    WEEKDAY(ending_date)) AS endingDate2, reward_cycle_id,master_id,name FROM participaction_core_rewardcycleinstance LEFT JOIN (SELECT master_id, name FROM participaction_core_rewardcycle_translation
    WHERE language_code = 'en') A ON participaction_core_rewardcycleinstance.reward_cycle_id = A.master_id;"),
  
  #--------------BALLOTS TABLE---------------#  
  c("DROP TABLE IF EXISTS dashboard_core_shinyballots;"),
  c("CREATE TABLE dashboard_core_shinyballots SELECT customer_award_id,reward_cycle_instance_id, SUM(value) as ballots FROM(SELECT * FROM (SELECT * FROM participaction_core_customerawardballot WHERE created_at >= '2019-02-08 05:00:00') A
    INNER JOIN (SELECT _uid AS rci, reward_cycle_id FROM participaction_core_rewardcycleinstance WHERE reward_cycle_id = 'c4ffd80003d811e9ac140242ac130004') B
    ON A.reward_cycle_instance_id = B.rci) C GROUP BY customer_award_id,reward_cycle_instance_id;"),
  
  #-------------CUSTOMERS TABLE--------------#
  c("DROP TABLE IF EXISTS dashboard_core_shinycustomer;"),
  c("CREATE TABLE dashboard_core_shinycustomer SELECT _uid AS customer_id, clearbridge_appuser_id,
    CASE WHEN age < 5 THEN '0-4'
    WHEN age >= 5 AND age < 18 THEN '5-17'
    WHEN age >= 18 AND age < 25 THEN '18-24'
    WHEN age >= 25 AND age < 35 THEN '25-34'
    WHEN age >= 35 AND age < 45 THEN '35-44'
    WHEN age >= 45 AND age < 55 THEN '45-54'
    WHEN age >= 55 AND age < 65 THEN '55-64'
    ELSE '65+' END AS age,
    CASE WHEN LOWER(SUBSTRING(gender,1,1)) = 'F' THEN 'Female' ELSE (CASE WHEN LOWER(SUBSTRING(gender,1,1)) IN ('M','H') THEN 'Male' ELSE 'Other' END) END AS gender,user_selected_level AS answerValue,
    CASE WHEN SUBSTRING(postal_code,1,1) = 'A' THEN 'Newfoundland & Labrador'
    WHEN SUBSTRING(postal_code,1,1) = 'B' THEN 'Nova Scotia'
    WHEN SUBSTRING(postal_code,1,1) = 'C' THEN 'Prince Edward Island'
    WHEN SUBSTRING(postal_code,1,1) = 'E' THEN 'New Brunswick'
    WHEN SUBSTRING(postal_code,1,1) IN ('G','H','J') THEN 'Québec'
    WHEN SUBSTRING(postal_code,1,1) IN ('K','L','M','N','P') THEN 'Ontario'
    WHEN SUBSTRING(postal_code,1,1) = 'R' THEN 'Manitoba'
    WHEN SUBSTRING(postal_code,1,1) = 'S' THEN 'Saskatchewan'
    WHEN SUBSTRING(postal_code,1,1) = 'T' THEN 'Alberta'
    WHEN SUBSTRING(postal_code,1,1) = 'V' THEN 'British Columbia'
    WHEN SUBSTRING(postal_code,1,1) = 'X' THEN 'NW Territories & Nunavut'
    WHEN SUBSTRING(postal_code,1,1) = 'Y' THEN 'Yukon Territory'
    ELSE 'Other' END AS province FROM participaction_core_customer;"),
  
  #-----------ACTIVE CUSTOMERS TABLE---------#
  c("DROP TABLE IF EXISTS dashboard_core_shinyactivity;"),
  c("CREATE TABLE dashboard_core_shinyactivity SELECT userId,province,age,gender,answerValue,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < weekDate AND name = 'Weekly Draw') AS week,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < weekDate AND name = 'Monthly Draw') AS month,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < weekDate AND name = 'Quarterly Draw') AS quarter,
    CASE WHEN activeMinutes < 200 THEN CONCAT(FLOOR(activeMinutes/50)*50,'-',FLOOR((50+activeMinutes)/50)*50) ELSE '200+' END AS activeTime,
    activeMinutes,workoutMinutes FROM ACTIVEMINUTES
    LEFT JOIN (SELECT DISTINCT clearbridge_appuser_id,province,age,gender,answerValue FROM dashboard_core_shinycustomer) A ON ACTIVEMINUTES.userId = A.clearbridge_appuser_id
    LEFT OUTER JOIN (SELECT userId AS uid, SUBDATE(DATE(startDate), WEEKDAY(startDate)) as week, SUM(TIMESTAMPDIFF(minute,startDate,endDate)) as workoutMinutes FROM WORKOUTSAMPLE GROUP BY userId, week) B ON
    ACTIVEMINUTES.userId = B.uid AND DATE(ACTIVEMINUTES.weekDate) = B.week;"),
  
  #---------------BADGES TABLE---------------#  
  c("DROP TABLE IF EXISTS dashboard_core_shinybadges;"),
  c("CREATE TABLE dashboard_core_shinybadges SELECT * FROM (SELECT A.status, A.customer_id, D.clearbridge_appuser_id,caid,
    REPLACE(REPLACE(REPLACE(C.award_short_description,'Minutes Mile','Minute Mile'),'Earned when leveling up','2x Multiplier'),'Weekly Challenge Complete','Weekly') AS award_short_description,age,
    CASE WHEN LOWER(SUBSTRING(D.gender,1,1)) = 'F' THEN 'Female' ELSE (CASE WHEN LOWER(SUBSTRING(D.gender,1,1)) IN ('M','H') THEN 'Male' ELSE 'Other' END) END AS gender, Y.answerValue,
    DATE(Z.earned_at) as date,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < Z.earned_at AND name = 'Weekly Draw') AS week,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < Z.earned_at AND name = 'Monthly Draw') AS month,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < Z.earned_at AND name = 'Quarterly Draw') AS quarter,
    DATE(DATE_ADD(Z.earned_at, INTERVAL 7 DAY)) as date7,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(Z.earned_at, INTERVAL 7 DAY) AND name = 'Weekly Draw') AS week7,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(Z.earned_at, INTERVAL 7 DAY) AND name = 'Monthly Draw') AS month7,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(Z.earned_at, INTERVAL 7 DAY) AND name = 'Quarterly Draw') AS quarter7,
    DATE(DATE_ADD(Z.earned_at, INTERVAL 28 DAY)) as date28,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(Z.earned_at, INTERVAL 28 DAY) AND name = 'Weekly Draw') AS week28,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(Z.earned_at, INTERVAL 28 DAY) AND name = 'Monthly Draw') AS month28,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(Z.earned_at, INTERVAL 28 DAY) AND name = 'Quarterly Draw') AS quarter28,
    CASE WHEN HOUR(Z.earned_at) >= 11 AND HOUR(Z.earned_at) < 17 THEN 'Morning'
    WHEN HOUR(Z.earned_at) >= 17 AND HOUR(Z.earned_at) < 23 THEN 'Afternoon'
    WHEN HOUR(Z.earned_at) >= 5 AND HOUR(Z.earned_at) < 11 THEN 'Night'
    ELSE 'Evening' END AS day_part,province,
    CASE WHEN award_short_description IN ('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!')
    THEN 'Engagement'
    WHEN award_short_description IN ('Weekly Challenge Complete','2-Week Streak','4-Week Streak') THEN 'Activity'
    WHEN (award_short_description LIKE '%Milestone%' OR award_short_description LIKE '%leveling%') THEN 'Level'
    ELSE 'Other' END AS award_category,num_ballots2 AS num_ballots
    FROM participaction_core_customeraward A
    INNER JOIN (SELECT _uid AS award_id2,award_type,award_code,sequence_number,start_timestamp,end_timestamp,days_valid,
    num_ballots,program_id,earn_multiple_allowed,award_class,minutes_required FROM participaction_core_award) B
    ON B.award_id2 = A.award_id INNER JOIN (SELECT master_id,id AS award_num,language_code,name AS award_name,short_description AS award_short_description,long_description AS award_long_description FROM
    participaction_core_award_translation WHERE language_code = 'en') C ON C.master_id = A.award_id
    INNER JOIN (SELECT customer_id AS customer_id2,clearbridge_appuser_id,province,gender,age FROM dashboard_core_shinycustomer) D ON A.customer_id = D.customer_id2
    LEFT JOIN (SELECT userId, answerValue FROM QA WHERE type = 'onboarding' AND questionKey = 'activityLevel') Y ON D.clearbridge_appuser_id = Y.userId
    INNER JOIN (SELECT customer_award_id AS caid,created_at AS earned_at FROM participaction_core_customerawardhistory WHERE new_status = 'earned') Z ON Z.caid = A._uid
    LEFT JOIN (SELECT customer_award_id,SUM(ballots) AS num_ballots2 FROM dashboard_core_shinyballots GROUP BY customer_award_id) X ON Z.caid = X.customer_award_id) E
    WHERE E.status IN ('earned','expired') AND E.date >= '2019-02-08 05:00:00';"),
  
  #-----------INDEXING BADGES TABLE-----------#  
  c("CREATE INDEX INDEX1 ON dashboard_core_shinybadges(customer_id, clearbridge_appuser_id, award_short_description, gender, age, answerValue, quarter, month, week, date, day_part, province);"),
  c("CREATE INDEX INDEX2 ON dashboard_core_shinybadges(award_short_description,date,customer_id);"),
  c("CREATE INDEX INDEX3 ON dashboard_core_shinybadges(award_short_description);"),
  c("CREATE INDEX INDEX4 ON dashboard_core_shinybadges(customer_id,date);"),
  c("CREATE INDEX INDEX5 ON dashboard_core_shinybadges(customer_id);"),
  c("CREATE INDEX INDEX6 ON dashboard_core_shinybadges(date);"),
  c("CREATE INDEX INDEX7 ON dashboard_core_shinybadges(month);"),
  c("CREATE INDEX INDEX8 ON dashboard_core_shinybadges(week);"),
  c("ALTER TABLE dashboard_core_shinybadges ADD COLUMN max_level INT AFTER answerValue;"),
  c("UPDATE dashboard_core_shinybadges
    LEFT JOIN (SELECT customer_id, MAX(CONVERT(SUBSTRING(award_short_description,1,LOCATE(' ',award_short_description)-1),UNSIGNED INTEGER)) AS max_level FROM dashboard_core_shinybadges WHERE award_short_description LIKE '%Milestone%' GROUP BY customer_id) B ON dashboard_core_shinybadges.customer_id = B.customer_id
    SET dashboard_core_shinybadges.max_level = B.max_level;"),
  c("DROP VIEW IF EXISTS dashboard_core_shinybadges_ENGAGEMENT;"),
  c("DROP VIEW IF EXISTS dashboard_core_shinybadges_ACTIVITY;"),
  c("DROP VIEW IF EXISTS dashboard_core_shinybadges_LEVEL;"),
  c("CREATE VIEW dashboard_core_shinybadges_ENGAGEMENT AS (SELECT * FROM dashboard_core_shinybadges WHERE award_short_description IN ('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!'));"),
  c("CREATE VIEW dashboard_core_shinybadges_ACTIVITY AS (SELECT * FROM dashboard_core_shinybadges WHERE award_short_description IN ('Weekly','2-Week Streak','4-Week Streak'));"),
  c("CREATE VIEW dashboard_core_shinybadges_LEVEL AS (SELECT * FROM dashboard_core_shinybadges WHERE award_short_description LIKE '%Milestone%' OR award_short_description = '2x Multiplier');"),
  
  #---------------REWARDS TABLE---------------#
  c("DROP TABLE IF EXISTS dashboard_core_shinyrewards;"),
  c("CREATE TABLE dashboard_core_shinyrewards SELECT _uid AS customer_award_id,clearbridge_appuser_id,customer_id,award_id,name,award_short_description,
    DATE(created_at) as date,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < created_at AND name = 'Weekly Draw') AS week,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < created_at AND name = 'Monthly Draw') AS month,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < created_at AND name = 'Quarterly Draw') AS quarter,
    DATE(DATE_ADD(created_at, INTERVAL 7 DAY)) as date7,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(created_at, INTERVAL 7 DAY) AND name = 'Weekly Draw') AS week7,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(created_at, INTERVAL 7 DAY) AND name = 'Monthly Draw') AS month7,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(created_at, INTERVAL 7 DAY) AND name = 'Quarterly Draw') AS quarter7,
    DATE(DATE_ADD(created_at, INTERVAL 28 DAY)) as date28,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(created_at, INTERVAL 28 DAY) AND name = 'Weekly Draw') AS week28,
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(created_at, INTERVAL 28 DAY) AND name = 'Monthly Draw') AS month28,       
    (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < DATE_ADD(created_at, INTERVAL 28 DAY) AND name = 'Quarterly Draw') AS quarter28,
    CASE WHEN HOUR(created_at) >= 11 AND HOUR(created_at) < 17 THEN 'Morning'
    WHEN HOUR(created_at) >= 17 AND HOUR(created_at) < 23 THEN 'Afternoon'
    WHEN HOUR(created_at) >= 5 AND HOUR(created_at) < 11 THEN 'Night'
    ELSE 'Evening' END AS day_part,province,gender,age,answerValue
    FROM (SELECT * FROM participaction_core_customeraward WHERE created_at >= '2019-02-04 05:00:00' AND status = 'Assigned') A
    INNER JOIN (SELECT master_id,name,short_description AS award_short_description FROM participaction_core_award_translation WHERE language_code = 'en') C ON C.master_id = A.award_id
    INNER JOIN (SELECT customer_id AS customer_id2,clearbridge_appuser_id,province,gender,age FROM dashboard_core_shinycustomer) D ON A.customer_id = D.customer_id2
    LEFT JOIN (SELECT userId, answerValue FROM QA WHERE type = 'onboarding' AND questionKey = 'activityLevel') Y ON D.clearbridge_appuser_id = Y.userId;")
  )

# ----- SELECT QUERIES - LEGACY ----- #
myquery_badges_upper <- function(p,d,f){
  paste0("SELECT (SELECT COUNT(customer_id) FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,") AS total,
         (SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,") AS `unique`,
         (SELECT COUNT(customer_id) FROM dashboard_core_shinybadges WHERE ",p,"7 >= '",as.character(d[1]),"' AND ",p,"7 <= '",as.character(d[2]),"'",f,") AS total7,
         (SELECT COUNT(customer_id) FROM dashboard_core_shinybadges WHERE ",p,"28 >= '",as.character(d[1]),"' AND ",p,"28 <= '",as.character(d[2]),"'",f,") AS total28,
         (SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,
         " AND customer_id NOT IN (SELECT DISTINCT customer_id FROM dashboard_core_shinybadges WHERE ",p," < '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,")) AS new;")
}

myquery_badges <- function(p,d,f){
  paste0("SELECT A.award_short_description,total,`unique`,total7,total28 FROM (SELECT award_short_description, COUNT(customer_id) AS total, COUNT(DISTINCT customer_id) AS `unique` FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f," GROUP BY award_short_description) A
         LEFT JOIN (SELECT award_short_description, COUNT(customer_id) AS total7 FROM dashboard_core_shinybadges WHERE ",p,"7 >= '",as.character(d[1]),"' AND ",p,"7 <= '",as.character(d[2]),"'",f," GROUP BY award_short_description) B
         ON A.award_short_description = B.award_short_description
         LEFT JOIN (SELECT award_short_description, COUNT(customer_id) AS total28 FROM dashboard_core_shinybadges WHERE ",p,"28 >= '",as.character(d[1]),"' AND ",p,"28 <= '",as.character(d[2]),"'",f," GROUP BY award_short_description) C
         ON A.award_short_description = C.award_short_description;")
}

myquery_badges_total <- function(p,d,f){
  paste0("SELECT A.award_category,total,`unique`,total7,total28 FROM (SELECT award_category, COUNT(customer_id) AS total, COUNT(DISTINCT customer_id) AS `unique` FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f," AND award_category != 'Other' GROUP BY award_category) A
         LEFT JOIN (SELECT award_category, COUNT(customer_id) AS total7 FROM dashboard_core_shinybadges WHERE ",p,"7 >= '",as.character(d[1]),"' AND ",p,"7 <= '",as.character(d[2]),"'",f," GROUP BY award_category) B ON A.award_category = B.award_category
         LEFT JOIN (SELECT award_category, COUNT(customer_id) AS total28 FROM dashboard_core_shinybadges WHERE ",p,"28 >= '",as.character(d[1]),"' AND ",p,"28 <= '",as.character(d[2]),"'",f," GROUP BY award_category) C
         ON A.award_category = C.award_category;")
}

myquery_badges_user <- function(p,d,f){
  paste0("SELECT A1.num_badges AS `Badge Count (Unique)`, A1.total, B1.total7, C1.total28 FROM
         (SELECT a.times AS num_badges, count(1) AS total FROM ( SELECT customer_id, count(DISTINCT award_short_description) AS times FROM dashboard_core_shinybadges
         WHERE ",p," >='",as.character(d[1]),"'AND ",p," <='", as.character(d[2]),"'",f,"
         GROUP BY customer_id) a GROUP BY times) A1
         LEFT JOIN (SELECT b.times AS num_badges, count(1) AS total7 FROM ( SELECT customer_id, count(DISTINCT award_short_description) AS times FROM dashboard_core_shinybadges 
         WHERE ",p,"7 >='",as.character(d[1]),"'AND ",p,"7 <='", as.character(d[2]),"'",f,"
         GROUP BY customer_id) b GROUP BY times) B1
         ON A1.num_badges = B1.num_badges
         LEFT JOIN (SELECT c.times AS num_badges, count(1) AS total28 FROM ( SELECT customer_id, count(DISTINCT award_short_description) AS times FROM dashboard_core_shinybadges 
         WHERE ",p,"28 >='",as.character(d[1]),"' AND ",p,"28 <= '", as.character(d[2]),"'",f,"
         GROUP BY customer_id) c GROUP BY times) C1
         ON A1.num_badges = C1.num_badges;")
}

myquery_badges_chart <- function(p,d,f){
  paste0("SELECT date,COUNT(award_short_description) as badges FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f," GROUP BY date;")
}

myquery_rewards_upper <- function(p,d,f){
  paste0("SELECT (SELECT COUNT(customer_id) FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,") AS total,
         (SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,") AS `unique`,
         (SELECT COUNT(customer_id) FROM dashboard_core_shinybadges WHERE ",p,"7 >= '",as.character(d[1]),"' AND ",p,"7 <= '",as.character(d[2]),"'",f,") AS total7,
         (SELECT COUNT(customer_id) FROM dashboard_core_shinybadges WHERE ",p,"28 >= '",as.character(d[1]),"' AND ",p,"28 <= '",as.character(d[2]),"'",f,") AS total28,
         (SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,
         " AND customer_id NOT IN (SELECT DISTINCT customer_id FROM dashboard_core_shinybadges WHERE ",p," < '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f,")) AS new;")
}

myquery_reward <- function(p,d,f){
  if(p=='quarter'){p <- c('quarter','quarter','quarter')}
  else if(p=='month'){p <- c('month','month','quarter')}
  else if(p=='week'){p <- c('week','month','quarter')}
  #print(p)
  paste0("SELECT award_short_description AS `Award Description`,weekly_ballots AS `Weekly Ballots`,monthly_ballots AS `Monthly Ballots`,quarterly_ballots AS `Quarterly Ballots`,weekly_users AS `% Users (Weekly)`,monthly_users AS `% Users (Monthly)`,quarterly_users AS `% Users (Quarterly)` FROM
         (SELECT award_short_description, SUM(num_ballots) AS weekly_ballots, 100*COUNT(DISTINCT customer_id)/(SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p[1]," >= '",as.character(d[1]),"' AND ",p[1]," <= '",as.character(d[2]),"') AS weekly_users FROM dashboard_core_shinybadges WHERE ",p[1]," >= '",as.character(d[1]),"' AND ",p[1]," <= '",as.character(d[2]),"' AND num_ballots > 0 ",f," GROUP BY award_short_description) A
         LEFT JOIN (SELECT award_short_description AS asd, SUM(num_ballots) AS monthly_ballots, 100*COUNT(DISTINCT customer_id)/(SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p[2]," >= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[2]," <= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,")) AS monthly_users FROM dashboard_core_shinybadges WHERE ",p[2]," >= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[2]," <= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,") AND num_ballots > 0 ",f," GROUP BY award_short_description) B ON B.asd = A.award_short_description
         LEFT JOIN (SELECT award_short_description AS asd2, SUM(num_ballots) AS quarterly_ballots, 100*COUNT(DISTINCT customer_id)/(SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p[3]," >= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[3]," <= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,")) AS quarterly_users FROM dashboard_core_shinybadges WHERE ",p[3]," >= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[3]," <= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,") AND num_ballots > 0 ",f," GROUP BY award_short_description) C ON C.asd2 = A.award_short_description;")
}

myquery_reward_total <- function(p,d,f){
  if(p=='quarter'){p <- c('quarter','quarter','quarter')}
  else if(p=='month'){p <- c('month','month','quarter')}
  else if(p=='week'){p <- c('week','month','quarter')}
  #print(p)
  paste0("SELECT award_category,weekly_ballots AS `Weekly Ballots`,monthly_ballots AS `Monthly Ballots`,quarterly_ballots AS `Quarterly Ballots`,weekly_users AS `% Users (Weekly)`,monthly_users AS `% Users (Monthly)`,quarterly_users AS `% Users (Quarterly)` FROM
         (SELECT award_category, SUM(num_ballots) AS weekly_ballots, 100*COUNT(DISTINCT customer_id)/(SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p[1]," >= '",as.character(d[1]),"' AND ",p[1]," <= '",as.character(d[2]),"') AS weekly_users FROM dashboard_core_shinybadges WHERE ",p[1]," >= '",as.character(d[1]),"' AND ",p[1]," <= '",as.character(d[2]),"' AND num_ballots > 0 ",f," GROUP BY award_category) A
         LEFT JOIN (SELECT award_category AS asd, SUM(num_ballots) AS monthly_ballots, 100*COUNT(DISTINCT customer_id)/(SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p[2]," >= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[2]," <= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,")) AS monthly_users FROM dashboard_core_shinybadges WHERE ",p[2]," >= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[2]," <= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,") AND num_ballots > 0 ",f," GROUP BY award_category) B ON B.asd = A.award_category
         LEFT JOIN (SELECT award_category AS asd2, SUM(num_ballots) AS quarterly_ballots, 100*COUNT(DISTINCT customer_id)/(SELECT COUNT(DISTINCT customer_id) FROM dashboard_core_shinybadges WHERE ",p[3]," >= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[3]," <= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,")) AS quarterly_users FROM dashboard_core_shinybadges WHERE ",p[3]," >= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[1]),"'",f,") AND ",p[3]," <= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE ",p[1]," = '",as.character(d[2]),"'",f,") AND num_ballots > 0 ",f," GROUP BY award_category) C ON C.asd2 = A.award_category;")
}

myquery_reward_freq <- function(p,d,f){
  paste0("SELECT  customer_id AS `Customer ID`,SUM(Weekly) AS Weekly,SUM(Monthly) AS Monthly,SUM(Quarterly) AS Quarterly,SUM(Weekly+Monthly+Quarterly) AS Total FROM (SELECT customer_id,
         CASE WHEN name LIKE '%Weekly%' THEN A.count ELSE 0 END AS Weekly,
         CASE WHEN name LIKE '%Monthly%' THEN A.count ELSE 0 END AS Monthly,
         CASE WHEN name LIKE '%Quarterly%' THEN A.count ELSE 0 END AS Quarterly
         FROM (select DISTINCT customer_id as customer_id, count(award_short_description) as count,name From dashboard_core_shinyrewards 
         WHERE award_short_description LIKE '%Card%' group by customer_id,name) A WHERE A.count > 2) A GROUP BY customer_id ORDER BY Total DESC;")
}

myquery_reward_cheat <- function(p,d,f){
  paste0("SELECT * FROM (SELECT customer_id AS `Customer ID`,SUM(num_ballots) AS `Number of Ballots` FROM dashboard_core_shinybadges GROUP BY customer_id ORDER BY `Number of Ballots` DESC) A WHERE `Number of Ballots` >= 10000;")
}

myquery_pixel_email <- function(p,d,f){
  if(p=='quarter'){p <- c('quarter','quarter','quarter')}
  else if(p=='month'){p <- c('month','month','quarter')}
  else if(p=='week'){p <- c('week','month','quarter')}
  paste0("SELECT REPLACE(REPLACE(action,'open','Email Opened'),'sen','Email Sen') AS Metric,SUM(CASE WHEN ",p[1]," >= '",as.character(d[1]),"' AND ",p[1]," <= '",as.character(d[2]),"'",f," THEN 1 ELSE 0 END) AS `Weekly Total`,
         SUM(CASE WHEN ",p[2]," >= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE week = '",as.character(d[1]),"'",f,") AND ",p[2]," <= (SELECT MIN(",p[2],") FROM dashboard_core_shinybadges WHERE week = '",as.character(d[2]),"'",f,") THEN 1 ELSE 0 END) AS `Monthly Total`,
         SUM(CASE WHEN ",p[3]," >= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE week = '",as.character(d[1]),"'",f,") AND ",p[3]," <= (SELECT MIN(",p[3],") FROM dashboard_core_shinybadges WHERE week = '",as.character(d[2]),"'",f,") THEN 1 ELSE 0 END) AS `Quarterly Total` FROM
         (SELECT user_id,province,age,gender,answerValue,content_id,reward_id,award_id,action,
         CASE WHEN HOUR(ts) >= 11 AND HOUR(ts) < 17 THEN 'Morning' WHEN HOUR(ts) >= 17 AND HOUR(ts) < 23 THEN 'Afternoon' WHEN HOUR(ts) >= 5 AND HOUR(ts) < 11 THEN 'Night' ELSE 'Evening' END AS day_part,
         (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < ts AND name = 'Weekly Draw') AS week,
         (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < ts AND name = 'Monthly Draw') AS month,       
         (SELECT MAX(DATE(starting_date)) FROM dashboard_core_shinyrewardcycles WHERE starting_date < ts AND name = 'Quarterly Draw') AS quarter FROM dashboard_core_pixel_email
         INNER JOIN (SELECT * FROM dashboard_core_shinycustomer) A ON A.clearbridge_appuser_id = dashboard_core_pixel_email.user_id) B GROUP BY action ORDER BY action DESC;")
}

myquery_reward_chart <- function(p,d,f){
  paste0("SELECT date,SUM(num_ballots) as ballots FROM dashboard_core_shinybadges WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'",f," GROUP BY date;")
}

#getDateTable <- function(p=NULL,d=NULL,f=NULL){
#  paste0("SELECT week,month,quarter FROM dashboard_core_shinybadges GROUP BY week,month,quarter;")
#}

#getMaxLevel <- "SELECT MAX(CAST(max_level AS UNSIGNED)) FROM dashboard_core_shinybadges;"
#getProvinces <- "SELECT DISTINCT province FROM dashboard_core_shinybadges WHERE province != 'Other' ORDER BY province;"
#getWeeks <- "SELECT DISTINCT week FROM dashboard_core_shinybadges ORDER BY week;"
#getMonths <- "SELECT DISTINCT month FROM dashboard_core_shinybadges ORDER BY month;"
#getQuarters <- "SELECT DISTINCT quarter FROM dashboard_core_shinybadges ORDER BY quarter;"
#getActiveLevels <- "SELECT DISTINCT answerValue AS level FROM dashboard_core_shinybadges WHERE answerValue != '9' ORDER BY answerValue;"

#WHERE date > '2019-02-08 05:00:00';
# ------- NEW QUERIES FOR SPEED ------- #

my_badges_query <- function(p,d,max_level,answerValue,province){
  paste0("SELECT award_short_description,deltaType AS delta_type,count,`unique`,ballots FROM dashboard_core_shinybadges_main WHERE ",p,"_period = '",d,"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p,"' AND province = '",province,"';")
}

my_badges_query_full <- function(p,d,max_level,answerValue,province){
  if(p=='quarter'){p2 <- c('quarter','quarter','quarter')}
  else if(p=='month'){p2 <- c('month','month','quarter')}
  else if(p=='week'){p2 <- c('week','month','quarter')}
  if(d == 'All Quarters'){d <- c('All Quarters','All Quarters','All Quarters')}
  else if(p=='quarter'){d <- c(as.Date(d),as.Date(d),as.Date(d))}
  else if(p=='month'){d <- c(as.Date(d),as.Date(d),as.Date(dateTable[which(dateTable$month == d)[1],]$quarter))}
  else if(p=='week'){d <- c(as.Date(d),as.Date(dateTable[which(dateTable$week == d)[1],]$month),as.Date(dateTable[which(dateTable$week == d)[1],]$quarter))}
  paste0("SELECT award_short_description,deltaType AS delta_type,count,`unique`,ballots,'1' AS `role` FROM dashboard_core_shinybadges_main WHERE ",p2[1],"_period = '",d[1],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[1],"' AND province = '",province,
         "' UNION SELECT award_short_description,deltaType AS delta_type,count,`unique`,ballots,'2' AS `role` FROM dashboard_core_shinybadges_main WHERE ",p2[2],"_period = '",d[2],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[2],"' AND province = '",province,
         "' UNION SELECT award_short_description,deltaType AS delta_type,count,`unique`,ballots,'3' AS `role` FROM dashboard_core_shinybadges_main WHERE ",p2[3],"_period = '",d[3],"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p2[3],"' AND province = '",province,"';")
}

my_newusers_query <- function(p,d,max_level,answerValue,province){
  paste0("SELECT IFNULL((SELECT `unique` FROM dashboard_core_shinybadges_newusers WHERE period = '",d,"' AND max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND periodType = '",p,"' AND province = '",province,"'),0) AS `unique`;")
}

my_userbadges_query <- function(p,d,max_level,answerValue,province){
  paste0("SELECT count,num_users AS `unique`,deltaType AS delta_type FROM dashboard_core_shinybadges_users WHERE max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND period = '",d,"' AND periodType = '",p,"' AND province = '",province,"';")
}

my_cheaters_query <- function(p,d,max_level,answerValue,province){
  paste0("SELECT customer_id AS `Customer ID`,ballots AS `Number of Ballots` FROM dashboard_core_shinybadges_customers WHERE max_level = '",max_level,"' AND answerValue = '",answerValue,"' AND period = '",d,"' AND periodType = '",p,"' AND province = '",province,"' AND deltaType = 'none' AND ballots >= 10000;")
}

my_email_query <- function(p,d,max_level,answerValue,province){
  if(p=='quarter'){p2 <- c('quarter','quarter','quarter')}
  else if(p=='month'){p2 <- c('month','month','quarter')}
  else if(p=='week'){p2 <- c('week','month','quarter')}
  if(d == 'All Quarters'){d <- c('','','')}
  else if(p=='quarter'){d <- c(paste0(p2[1]," = '",as.Date(d),"'"),paste0(p2[2]," = '",as.Date(d),"'"),paste0(p2[3]," = '",as.Date(d),"'"))}
  else if(p=='month'){d <- c(paste0(p2[1]," = '",as.Date(d),"'"),paste0(p2[2]," = '",as.Date(d),"'"),paste0(p2[3]," = '",as.Date(dateTable[which(dateTable$month == d)[1],]$quarter),"'"))}
  else if(p=='week'){d <- c(paste0(p2[1]," = '",as.Date(d),"'"),paste0(p2[2]," = '",as.Date(dateTable[which(dateTable$week == d)[1],]$month),"'"),paste0(p2[3]," = '",as.Date(dateTable[which(dateTable$week == d)[1],]$quarter),"'"))}
  if(max_level != 'All Levels'){m <- paste0(" AND max_level = '",max_level,"'")}
  else{m <- ''}
  if(answerValue != 'All Levels'){a <- paste0(" AND answerValue = '",answerValue,"'")}
  else{a <- ''}
  if(province != 'All Provinces'){v <- paste0(" AND province = '",province,"'")}
  else{v <- ''}
  gsub('WHERE  ','',gsub('WHERE  AND','WHERE',paste0("SELECT REPLACE(REPLACE(action,'open','Email Opened'),'sen','Email Sen') AS `Metric`,`Weekly Total`,`Monthly Total`,`Quarterly Total` FROM
                                                     (SELECT action,COUNT(*) AS `Weekly Total` FROM dashboard_core_pixel_email_derived WHERE ",d[1],m,a,v," GROUP BY action) A
                                                     LEFT JOIN
                                                     (SELECT action AS action2,COUNT(*) AS `Monthly Total` FROM dashboard_core_pixel_email_derived WHERE ",d[2],m,a,v," GROUP BY action) B ON B.action2 = A.action
                                                     LEFT JOIN
                                                     (SELECT action AS action3,COUNT(*) AS `Quarterly Total` FROM dashboard_core_pixel_email_derived WHERE ",d[3],m,a,v," GROUP BY action) C ON C.action3 = A.action;")))
}

myquery_reward_winners <- function(p,d,f){
  if(d[1] != 'All Quarters'){p <- paste0(" AND ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'")}
  else{p <- ''}
  gsub('WHERE  AND','WHERE',gsub('WHERE  GROUP','GROUP',paste0("SELECT award_short_description AS `Reward Description`,weekly_total AS `Weekly Total`,monthly_total AS `Monthly Total`,quarterly_total AS `Quarterly Total`,IFNULL(weekly_unique,0) AS `Weekly Users`,IFNULL(monthly_unique,0) AS `Monthly Users`, IFNULL(quarterly_unique,0) as `Quarterly Users` FROM (SELECT award_short_description, 
                                                               (SUM(CASE WHEN name LIKE '%Weekly%' THEN 1 ELSE 0 END)) AS weekly_total,
                                                               (SUM(CASE WHEN name LIKE '%Monthly%' THEN 1 ELSE 0 END)) AS monthly_total,
                                                               (SUM(CASE WHEN name LIKE '%Quarterly%' THEN 1 ELSE 0 END)) AS quarterly_total FROM dashboard_core_shinyrewards WHERE ",p,f," GROUP BY award_short_description) A
                                                               LEFT JOIN (SELECT award_short_description AS aid, COUNT(DISTINCT customer_id) AS weekly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Weekly%'",p,f," GROUP BY award_short_description) B ON A.award_short_description = B.aid
                                                               LEFT JOIN (SELECT award_short_description AS aid2, COUNT(DISTINCT customer_id) AS monthly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Monthly%'",p,f," GROUP BY award_short_description) C ON A.award_short_description = C.aid2
                                                               LEFT JOIN (SELECT award_short_description AS aid3, COUNT(DISTINCT customer_id) AS quarterly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Quarterly%'",p,f," GROUP BY award_short_description) D ON A.award_short_description = D.aid3;")))
}

myquery_reward_winners_total <- function(p,d,f){
  if(d[1] != 'All Quarters'){p <- paste0(" AND ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'")}
  else{p <- ''}
  gsub('WHERE  AND','WHERE',gsub('WHERE  GROUP','GROUP',paste0("SELECT 'Total' as total,weekly_total AS `Weekly Total`,monthly_total AS `Monthly Total`,quarterly_total AS `Quarterly Total`,IFNULL(weekly_unique,0) AS `Weekly Users`,IFNULL(monthly_unique,0) AS `Monthly Users`, IFNULL(quarterly_unique,0) as `Quarterly Users` FROM (SELECT 'Total' as total, 
                                                               (SUM(CASE WHEN name LIKE '%Weekly%' THEN 1 ELSE 0 END)) AS weekly_total,
                                                               (SUM(CASE WHEN name LIKE '%Monthly%' THEN 1 ELSE 0 END)) AS monthly_total,
                                                               (SUM(CASE WHEN name LIKE '%Quarterly%' THEN 1 ELSE 0 END)) AS quarterly_total FROM dashboard_core_shinyrewards WHERE ",p,f," GROUP BY total) A
                                                               LEFT JOIN (SELECT 'Total' AS aid, COUNT(DISTINCT customer_id) AS weekly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Weekly%'",p,f,") B ON A.total = B.aid
                                                               LEFT JOIN (SELECT 'Total' AS aid2, COUNT(DISTINCT customer_id) AS monthly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Monthly%'",p,f,") C ON A.total = C.aid2
                                                               LEFT JOIN (SELECT 'Total' AS aid3, COUNT(DISTINCT customer_id) AS quarterly_unique FROM dashboard_core_shinyrewards WHERE name LIKE '%Quarterly%'",p,f,") D ON A.total = D.aid3;")))
  
}

myquery_reward_winners_unique <- function(p,d,f){
  if(as.character(d[1]) == 'All Quarters'){dates <- ""}
  else{dates <-  paste0(" WHERE ",p," >= '",as.character(d[1]),"' AND ",p," <= '",as.character(d[2]),"'")}
  gsub('shinyrewards AND','shinyrewards WHERE',paste0("SELECT COUNT(DISTINCT customer_id) AS `unique` FROM dashboard_core_shinyrewards",dates,f,";"))
}

my_charts_query <- function(p,d,max_level,answerValue,province){
  if(d[1] != 'All Quarters'){dates <- paste0("AND period <= '",d[2],"' AND period >= '",d[1],"' ")}
  else{dates <- ""}
  paste0("SELECT period AS `",p,"`,SUM(count) AS count,SUM(`unique`) AS `unique`,SUM(ballots) AS ballots FROM dashboard_core_shinybadges_charts WHERE max_level = '",max_level,"' AND answerValue = '",answerValue,"' ",dates,"AND periodType = '",p,"' AND province = '",province,"' AND award_short_description = 'All Badges' GROUP BY period;") #,province,answerValue,max_level,award_short_description;")
}