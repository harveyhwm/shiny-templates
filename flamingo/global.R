#LOAD ALL PACKAGES IN ONE FUNCTION
packages <- c('shiny','shinydashboard','shinythemes','shinyjs','highcharter','data.table','reshape2','ggplot2','stringr','plotly','scales','readr','dplyr','plyr','DT','V8') #'rowr'
lapply(packages,library,character.only=TRUE)

#TITLECASE FUNCTION
titlec <- function(x){
  s <- strsplit(x, ' ')[[1]]
  paste(toupper(substring(s,1,1)),tolower(substring(s, 2)),sep='',collapse=' ')
}

#SET DATES
min_date <- as.Date('2018-04-28')
max_date <- Sys.Date()
dateRangeFull <- seq(min_date,max_date,by='days')

#MAP USERNAMES TO ASSOCIATED ACCESS RIGHTS
username_map <- function(x){
  if(grepl('user_lcv',x)){return('LCV')}
  else if(grepl('user_pp',x)){return('Planned Parenthood')}
  else if(grepl('user_pm',x)){return('Progress Michigan')}
  else if(grepl('user_seesaw|wrobinson',x)){return('all_groups')}
}
default_user <- 'user_seesaw'
interest_groups <- c('LCV','Progress Michigan','Planned Parenthood')
h <- c('Summary View','Creative Detail','Post Reactions','Sentiment','Social Listening','Glossary')
channels <- c('Facebook','Twitter','Google')
active_topics <- c('Education','Betsy DeVos','Healthcare','Legalization of Marijuana','Unemployment','Infrastructure','Environment','Civil Rights & Equality','Abortion & Birth Control','Seniors Issues','Second Amendment Rights','Gun Crime')
active_topics_lcv <- c('Environment')
active_topics_pp <- c('Abortion & Birth Control')
active_topics_pm <- c('Education','Betsy DeVos','Healthcare','Legalization of Marijuana','Unemployment','Infrastructure','Civil Rights & Equality','Seniors Issues','Second Amendment Rights','Gun Crime')
chart_dims <- c('Gender','Ethnicity','Matrix Position','Psychographic Segment','Message','Date','Age','Coastal','Protestant','Urban','Income','Married')
chart_metrics <- c('Spend','Engagements','Shares','Impressions','Engagement Rate','CPM','Cost / Engagement','Cost / Share')

#READ IN DATA
mypath = '/Users/harveym/Downloads/- shiny dashboards/seesaw-dashboard/flamingo/'
facebook_new <- fread(paste0(mypath,'data/facebook.csv'))
reach_day <- fread(paste0(mypath,'data/reach.csv'))
twitter_new <- fread(paste0(mypath,'data/twitter.csv'))
google <- fread(paste0(mypath,'data/google.csv'))
sentiment <- fread(paste0(mypath,'data/sentiment/daily_sentiment.csv'))
sentiment_lookup <- fread(paste0(mypath,'data/sentiment/daily_sentiment_lookup.csv'))
messages <- fread(paste0(mypath,'data/messages.csv'))
tone <- fread(paste0(mypath,'data/tone.csv'))
theme <- fread(paste0(mypath,'data/theme.csv'))
news <- fread(paste0(mypath,'data/daily_news.csv'))
hashtags <- fread(paste0(mypath,'data/daily_hashtags.csv'))
hashtags_lookup <- fread(paste0(mypath,'data/daily_hashtags_lookup.csv'))
competitor_display <- fread(paste0(mypath,'data/competitor_display.csv'))
clusters <- fread(paste0(mypath,'data/global_cluster_centroid.csv'),data.table=F)
cluster_key <- fread(paste0(mypath,'data/cluster_key.csv'))
cluster_list <- fread(paste0(mypath,'data/cluster_quadrant_list.csv'))
survey <- fread(paste0(mypath,'data/survey.csv'))
survey_test <- fread(paste0(mypath,'data/survey_test.csv'))
billing <- fread(paste0(mypath,'data/billing_groups.csv'))
billing_lookup <- fread(paste0(mypath,'data/billing_lookup.csv'))
invoices <- fread(paste0(mypath,'data/invoices.csv'))

#READ IN NEW DATA VERSIONS
facebook_latest <- fread('/Users/harveymanhood/Documents/- tools/- python/- facebook ads api/facebook_new.csv')
twitter_latest <- fread('/Users/harveymanhood/Documents/- tools/- python/- twitter api/twitter_new.csv')
google_latest <- fread('/Users/harveymanhood/Documents/- tools/- python/- adwords api/google_new.csv')
ga_latest <- fread('/Users/harveymanhood/Documents/- tools/- python/- google analytics api/analytics_new.csv')

news <- news[order(news$Date,decreasing=TRUE),]
hashtags <- merge(hashtags,hashtags_lookup,all.x=TRUE)
news$Category <- gsub(' Election',' Race',news$Category)
categories_news <- sort(unique(news$Category))
categories_social <- sort(unique(hashtags$Category))

competitor_display$TextDate <- format(as.Date(competitor_display$`Date Created`), format='%B %d, %Y')
categorydisplay <- unique(competitor_display$Name)

formats <- data.frame(c('30','15','6','image','carousel','survey'),c('Video: 30 seconds','Video: 15 seconds','Video: 6 seconds','Image','Carousel','Sentiment Survey'))
colnames(formats) <- c('old','new')


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  CLUSTER DATA PREPARATION
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
clusters$x <- round(clusters$catalistmodel_vch_index, 2)
clusters$y <- round(clusters$catalistmodel_voteprop2018, 2)
clusters$z <- clusters$count
clusters$age <- round(clusters$age, 2)
clusters <- dplyr::rename(
  clusters,
  Age = age,
  Income = income_num,
  `Percent Caucasian` = is_caucasian,
  `Percent Female` = is_female,
  `Percent Urban` = is_urban,
  `Gun Ownership` = catalistmodel_gun_owner,
  `Progressive Ideology` = catalistmodel_ideology,
  `Educated` = catalistmodel_education,
  `Religious` = catalistmodel_religious_attendance_bin
) %>%
  arrange(global_clust_number)

cluster_dims <- c('Age','Income','Percent Caucasian','Percent Female','Percent Urban')
cluster_cats <- c('Segment','Educated','Progressive Ideology','Religious','Gun Ownership')
quadrant <- c('Unspecified (Phase 1)',unique(cluster_list$Quadrant))

segment_names <- c(
  'Super Secular',
  'Traditional Democrats',
  'Paycheck to Paycheck Progressives',
  'Libertarian Left',
  'New Suburbans',
  'Nostalgic Traditionalists',
  'Merit and Markets',
  'Identity Conservatives',
  'Fox Loyalists'
)

clusters$`Psychographic Segment` <- segment_names[max.col(clusters[,grepl('seg_',names(clusters))])]
clusters$Cluster <- as.character(clusters$global_clust_number)
clusters$Combined <- paste(clusters$votepropensity2017g_buckets,clusters$sy_vch_index_buckets,sep='') 
clusters$Gender <- cut(as.numeric(clusters$`Percent Female`), breaks = c(-Inf, 0.5, Inf),labels = c('Male','Female'))
clusters$Ethnicity <- cut(as.numeric(clusters$`Percent Caucasian`), breaks = c(-Inf, 0.5, Inf),labels = c('Caucasian','Non-Caucasian'))
clusters$Bi_Coastal <- cut(as.numeric(clusters$is_coastal), breaks = c(-Inf, 0.5, Inf),labels = c('Coastal','Non_Costal'))
clusters$Bi_Protestant <- cut(as.numeric(clusters$is_protestant), breaks = c(-Inf, 0.5, Inf),labels = c('Protestant','Non_Protestant'))
clusters$Bi_Urban <- cut(as.numeric(clusters$`Percent Urban`), breaks = c(-Inf, 0.6, Inf),labels = c('Rural','Urban'))
clusters$IncomeLvl <- cut(as.numeric(clusters$Income), breaks = c(0, 30, 69,Inf),labels = c('Low Income','Middle Class','Upper Class'))
clusters$AgeLvl <- cut(as.numeric(clusters$Age), breaks = c(0, 36, 57,Inf),labels = c('Young','Middle Aged','Older'))
clusters$Bi_Married <- cut(as.numeric(clusters$married), breaks = c(-Inf, 0.6, Inf),labels = c('Not Married','Married'))
clusters$Segment <- segment_names[max.col(clusters[,grepl('seg_',names(clusters))])]
clusters$Cluster <- as.character(clusters$global_clust_number)
clusters$Combined <- paste(clusters$votepropensity2017g_buckets,clusters$sy_vch_index_buckets,sep='') 
catalist_clusters <- merge(clusters,cluster_key,by='Combined')

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  FACEBOOK
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
facebook_colnames <- c('account_id','ad_id','Ad name','adset_id','Ad set name','campaign_id','Campaign name','Date','date_stop','Impressions','inline_link_clicks','inline_post_engagement','Reach','relevance_score','Amount spent','unique_clicks','Comments','actions_like','actions_photo_view','Shares','Link Clicks','Website Clicks','Post engagements','actions_page_engagement','actions_post_reaction','Video Views','actions_video_avg_percent_watched_actions','video_avg_percent_watched_actions_28d_click','actions_video_avg_time_watched_actions','video_avg_time_watched_actions_28d_click','Video Completions','video_p100_watched_actions_28d_click','actions_video_p25_watched_actions','video_p25_watched_actions_28d_click','actions_video_p50_watched_actions','video_p50_watched_actions_28d_click','actions_video_p75_watched_actions','video_p75_watched_actions_28d_click','actions_video_p95_watched_actions','video_p95_watched_actions_28d_click','Like','Love','Haha','Wow','Sad','Angry','configured_status','creative_id','name','instagram_actor_id','page_id','link_name2','video_id','creative_image_hash','creative_message','External destination URL','creative_title','creative_link_description','creative_cta_type')
setnames(facebook_new, old=colnames(facebook_new), new=facebook_colnames)
facebook_summary_colnames <- c('Date','Campaign name','Ad set name','Ad name','External destination URL','Amount spent','Reach','Impressions','Post engagements','Comments','Shares','Video Views','Video Completions','Link Clicks','Website Clicks','Like','Love','Haha','Wow','Sad','Angry')
facebook_summary <- facebook_new[,facebook_summary_colnames,with=FALSE]

#MUNGING NEW VERSION
facebook_latest_colnames <- c('account_id','ad_id','Ad name','adset_id','Ad set name','campaign_id','Campaign name','Date','date_stop','Impressions','inline_link_clicks','inline_post_engagement','Reach','relevance_score','Amount spent','unique_clicks','Comments','actions_like','actions_photo_view','Shares','Link Clicks','Website Clicks','Post engagements','actions_page_engagement','actions_post_reaction','Video Views','Website Views','actions_video_avg_percent_watched_actions','video_avg_percent_watched_actions_28d_click','actions_video_avg_time_watched_actions','video_avg_time_watched_actions_28d_click','Video Completions','video_p100_watched_actions_28d_click','actions_video_p25_watched_actions','video_p25_watched_actions_28d_click','actions_video_p50_watched_actions','video_p50_watched_actions_28d_click','actions_video_p75_watched_actions','video_p75_watched_actions_28d_click','actions_video_p95_watched_actions','video_p95_watched_actions_28d_click','Like','Love','Haha','Wow','Sad','Angry','creative_id','Ad Bid Amount','bid_info','configured_status','created_time','effective_status','creative_name','instagram_actor_id','page_id','link_name2','video_id','creative_image_hash','creative_message','External destination URL','creative_title','creative_link_description','creative_cta_type','adset_billing_event','adset_budget_remaining','adset_configured_status','campaign_buying_type','campaign_configured_status','campaign_effective_status','campaign_objective','Campaign Spend Cap','campaign_start_time','campaign_stop_time','campaign_updated_time')
setnames(facebook_latest, old=colnames(facebook_latest), new=facebook_latest_colnames)
facebook_summary_latest_colnames <- c('Date','Campaign name','Ad set name','Ad name','External destination URL','Amount spent','Reach','Impressions','Post engagements','Comments','Shares','Video Views','Video Completions','Link Clicks','Website Clicks','Website Views','Like','Love','Haha','Wow','Sad','Angry','Campaign Spend Cap')
facebook_summary <- facebook_latest[,facebook_summary_latest_colnames,with=FALSE]

facebook_summary$Coalition <- ifelse(grepl('LCV',facebook_summary$`Campaign name`),'LCV',ifelse(grepl('Planned|Protect',facebook_summary$`Campaign name`),'Planned Parenthood',ifelse(grepl('Progress',facebook_summary$`Campaign name`),'Progress Michigan','OTHER')))
facebook_summary$Channel <- 'Facebook'
facebook_summary$Date <- as.Date(facebook_summary$Date,'%Y-%m-%d')
facebook_summary$`True Engagements` <- rowSums(facebook_summary[,c('Like','Love','Haha','Wow','Sad','Angry','Shares')],na.rm=TRUE)
facebook_summary <- facebook_summary[,c('Date','Channel','Coalition','Campaign name','Ad set name','Ad name','External destination URL','Amount spent','Reach','Impressions','Post engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Link Clicks','Website Clicks','Website Views','Like','Love','Haha','Wow','Sad','Angry','Campaign Spend Cap')]

#PHASE 1 - OUR CAMPAIGNS & NAMING STRUCTURE UP UNTIL 31 MAY
facebook_summary_phase1a <- facebook_summary[(grepl('V2',facebook_summary$`Ad name`) & Date < as.Date('2018-06-01','%Y-%m-%d')),]
facebook_summary_phase1b <- facebook_summary[(!grepl('V2',facebook_summary$`Ad name`) & Date < as.Date('2018-06-01','%Y-%m-%d')),]
facebook_summary_phase1a$`Ad name` <- paste0(gsub('_V2','',facebook_summary_phase1a$`Ad name`),'_V2')
facebook_summary_phase1 <- bind_rows(facebook_summary_phase1a,facebook_summary_phase1b)
facebook_summary_phase1 <- facebook_summary_phase1[,c('Theme','Message','Tone','Format','Version') := tstrsplit(`Ad name`, '_')]
facebook_summary_phase1 <- facebook_summary_phase1[,c('Political Ad Id') := titlec(str_sub(str_extract(facebook_summary_phase1$`Campaign name`,'\\| [A-Za-z0-9\\-_]+$'),3,-1))]
facebook_summary_phase1$Cluster <- ''

#PHASE 2 - OUR CAMPAIGNS & NAMING STRUCTURE FROM AUGUST, WITH CLUSTER DATA
facebook_summary_phase2 <- facebook_summary[Date >= as.Date('2018-06-01','%Y-%m-%d'),]
facebook_summary_phase2$`Ad name` <- gsub(' - v2','',facebook_summary_phase2$`Ad name`)
facebook_summary_phase2[,c('Theme','Message','Tone','Format','Version','Political Ad Id')] <- '-'
facebook_summary_phase2$Message <- unlist(lapply(facebook_summary_phase2$`Ad name`,function(x) paste(strsplit(x, '_')[[1]][2],strsplit(x, '_')[[1]][3])))
facebook_summary_phase2$Theme <- facebook_summary_phase2$Message
facebook_summary_phase2$Format <- unlist(lapply(facebook_summary_phase2$`Ad name`,function(x) strsplit(x, '_')[[1]][4]))
facebook_summary_phase2$Cluster <- facebook_summary_phase2$`Ad set name` #duplication here for futureproofing and clarity
facebook_summary_phase2$Message <- ifelse(grepl('survey',facebook_summary_phase2$`Ad name`),'Sentiment Survey',facebook_summary_phase2$Message)
facebook_summary_phase2$Format <- ifelse(grepl('survey',facebook_summary_phase2$`Ad name`),'survey',facebook_summary_phase2$Format)

#COMBINE SUBTABLES
facebook_summary <- bind_rows(facebook_summary_phase1,facebook_summary_phase2)

#MESSAGE LOOKUP
facebook_summary$Message <- messages$MessageNew[match(unlist(facebook_summary$Message), messages$Message)]
facebook_summary$Tone <- tone$ToneNew[match(unlist(facebook_summary$Tone), tone$Tone)]
facebook_summary$Theme <- theme$ThemeNew[match(unlist(facebook_summary$Theme), theme$Theme)]
#facebook_summary$Format <- gsub(' - Copy','',facebook_summary$Format)
facebook_summary$Format <- formats$new[match(unlist(gsub(' - Copy','',facebook_summary$Format)), formats$old)]
facebook_summary$`Campaign Spend Cap` <- as.numeric(facebook_summary$`Campaign Spend Cap`)

#SUBSET WITH NO REACTION DATA
facebook_summary2 <- facebook_summary[,c(1:16,23:29)] #remove reactions from this version

#REACH CALCULATIONS
reach_day$Start <- as.Date(reach_day$Start,'%Y-%m-%d')
reach_day$End <- as.Date(reach_day$End,'%Y-%m-%d')

#MERGE FACEBOOK TABLE WITH REACTIONS TO GET FULL REACTIONS TABLE
reactions_table1 <- facebook_summary
reactions_table1[,c('Like','Love','Haha','Wow','Sad','Angry')][is.na(reactions_table1[,c('Like','Love','Haha','Wow','Sad','Angry')])] <- 0

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  TWITTER   'replies','retweets','video_3s100pct_views','video_views_100','url_clicks'
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
twitter_colnames <- c('tweet_approval_status','tweet_created_at','tweet_deleted','tweet_entity_status','tweet_id','adgroup_id','status_id','tweet_updated_at','account_id','adgroup_advertiser_domain','adgroup_advertiser_user_id','adgroup_automatically_select_bid','adgroup_bid_amount_local_micro','adgroup_bid_type','adgroup_bid_unit','campaign_id','adgroup_categories','adgroup_charge_by','adgroup_created_at','adgroup_creative_source','adgroup_currency','adgroup_deleted','adgroup_end_time','adgroup_entity_status','adgroup_include_sentiment','adgroup_name','adgroup_objective','adgroup_optimization','adgroup_placements','adgroup_primary_web_event_tag','adgroup_product_type','adgroup_start_time','adgroup_target_cpa_local_micro','adgroup_total_budget_amount_local_micro','adgroup_tracking_tags','adgroup_updated_at','card_uri','status_text','status_link','card_type','card_content_duration_seconds','card_created_at','card_deleted','card_id','card_image','card_image_display_height','card_image_display_width','card_name','card_preview_url','card_title','card_updated_at','card_video_content_id','card_video_height','card_video_hls_url','card_video_owner_id','card_video_poster_height','card_video_poster_url','card_video_poster_width','card_video_url','card_video_width','card_website_dest_url','card_website_display_url','card_website_shortened_url','card_website_title','card_website_url','website_url','campaign_created_at','campaign_currency','campaign_daily_budget_amount_local_micro','campaign_deleted','campaign_duration_in_days','campaign_end_time','campaign_entity_status','campaign_frequency_cap','campaign_funding_instrument_id','campaign_name','campaign_reasons_not_servable','campaign_servable','campaign_standard_delivery','campaign_start_time','campaign_total_budget_amount_local_micro','campaign_updated_at','account_approval_status','account_business_id','account_business_name','account_created_at','account_deleted','account_industry_type','account_name','account_salt','account_timezone','account_timezone_switch_at','account_updated_at','app_clicks','auto_created_conversion_add_to_cart','auto_created_conversion_add_to_wish_list','auto_created_conversion_added_payment_info','auto_created_conversion_checkout_initiated','auto_created_conversion_complete_registration','auto_created_conversion_content_view','auto_created_conversion_download','auto_created_conversion_page_view','auto_created_conversion_purchase','auto_created_conversion_search','auto_created_conversion_sign_up_initiated','billed_charge_local_micro','billed_engagements','card_engagements','carousel_swipes','clicks','conversion_custom','conversion_downloads','conversion_purchases','conversion_sign_ups','conversion_site_visits','engagements','follows','impressions','likes','media_engagements','media_views','poll_card_vote','qualified_impressions','replies','retweets','tweets_send','unfollows','url_clicks','video_3s100pct_views','video_content_starts','video_cta_clicks','video_mrc_views','video_total_views','video_views_100','video_views_25','video_views_50','video_views_75','date')

twitter_new <- twitter_latest                 

twitter_colnames <- c('tweet_approval_status','tweet_created_at','tweet_deleted','tweet_entity_status','tweet_id','adgroup_id','status_id','tweet_updated_at','card_uri','status_text','status_link','card_type','card_content_duration_seconds','card_created_at','card_deleted','card_id','card_image','card_name','card_preview_url','card_title','card_updated_at','card_video_content_id','card_video_height','card_video_hls_url','card_video_owner_id','card_video_poster_height','card_video_poster_url','card_video_poster_width','card_video_url','card_video_width','card_website_dest_url','card_website_display_url','card_website_shortened_url','card_website_title','card_website_url','website_url','adgroup_advertiser_domain','adgroup_advertiser_user_id','adgroup_automatically_select_bid','adgroup_bid_amount_local_micro','adgroup_bid_type','adgroup_bid_unit','campaign_id','adgroup_categories','adgroup_charge_by','adgroup_created_at','adgroup_creative_source','adgroup_currency','adgroup_deleted','adgroup_end_time','adgroup_entity_status','adgroup_include_sentiment','adgroup_name','adgroup_objective','adgroup_optimization','adgroup_placements','adgroup_primary_web_event_tag','adgroup_product_type','adgroup_start_time','adgroup_target_cpa_local_micro','adgroup_total_budget_amount_local_micro','adgroup_tracking_tags','adgroup_updated_at','account_id','campaign_created_at','campaign_currency','campaign_daily_budget_amount_local_micro','campaign_deleted','campaign_duration_in_days','campaign_end_time','campaign_entity_status','campaign_frequency_cap','campaign_funding_instrument_id','campaign_name','campaign_reasons_not_servable','campaign_servable','campaign_standard_delivery','campaign_start_time','campaign_total_budget_amount_local_micro','campaign_updated_at','account_approval_status','account_business_id','account_business_name','account_created_at','account_deleted','account_industry_type','account_name','account_salt','account_timezone','account_timezone_switch_at','account_updated_at','app_clicks','auto_created_conversion_add_to_cart','auto_created_conversion_add_to_wish_list','auto_created_conversion_added_payment_info','auto_created_conversion_checkout_initiated','auto_created_conversion_complete_registration','auto_created_conversion_content_view','auto_created_conversion_download','auto_created_conversion_page_view','auto_created_conversion_purchase','auto_created_conversion_search','auto_created_conversion_sign_up_initiated','billed_charge_local_micro','billed_engagements','card_engagements','carousel_swipes','clicks','conversion_custom','conversion_downloads','conversion_purchases','conversion_sign_ups','conversion_site_visits','engagements','follows','impressions','likes','media_engagements','media_views','poll_card_vote','qualified_impressions','replies','retweets','tweets_send','unfollows','url_clicks','video_3s100pct_views','video_content_starts','video_cta_clicks','video_mrc_views','video_total_views','video_views_100','video_views_25','video_views_50','video_views_75','date')
twitter_new$Coalition <- ifelse(grepl('LCV',twitter_new$account_name),'LCV',ifelse(grepl('Planned',twitter_new$account_name),'Planned Parenthood',ifelse(grepl('Progress',twitter_new$account_name),'Progress Michigan','OTHER')))
twitter_new[is.na(twitter_new)] <- 0
twitter_new$Channel <- 'Twitter'
twitter_new$Reach <- 0
twitter_new$Spend <- as.numeric(twitter_new$billed_charge_local_micro)/1000000 #convert twitter micro-dollars back to dollars and cents
twitter_new[,c('Theme','Message','Tone','Format','Version','Political Ad Id')] <- '-'
twitter_new$Engagements <- as.numeric(twitter_new$engagements) + as.numeric(twitter_new$video_3s100pct_views) #this is video views
twitter_new$`True Engagements` <- rowSums(twitter_new[,c('likes','retweets')],na.rm=TRUE)
twitter_new$impressions <- as.numeric(twitter_new$impressions)
twitter_new$replies <- as.numeric(twitter_new$replies)
twitter_new$retweets <- as.numeric(twitter_new$retweets)
twitter_new$video_3s100pct_views <- as.numeric(twitter_new$video_3s100pct_views)
twitter_new$video_views_100 <- as.numeric(twitter_new$video_views_100)
twitter_new$video_content_starts <- as.numeric(twitter_new$video_content_starts)
twitter_new$video_total_views <- as.numeric(twitter_new$video_total_views)
twitter_new$url_clicks <- as.numeric(twitter_new$url_clicks)
twitter_new$date <- as.Date(twitter_new$date,'%Y-%m-%d')
twitter_new[,c('Love','Haha','Wow','Sad','Angry','Website Views')] <- 0
twitter_summary <- twitter_new[,c('date','Channel','Coalition','campaign_name','adgroup_name','status_text','card_website_dest_url','Spend','Reach','impressions','Engagements','True Engagements','replies','retweets','video_3s100pct_views','video_views_100','clicks','url_clicks','Website Views','likes','Love','Haha','Wow','Sad','Angry','campaign_total_budget_amount_local_micro','Theme','Message','Tone','Format','Version','Political Ad Id')]
twitter_summary$Cluster <- ''
twitter_summary$`campaign_total_budget_amount_local_micro` <- as.numeric(twitter_summary$`campaign_total_budget_amount_local_micro`)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GOOGLE   '-','-','-','Video played to 100%','Clicks'
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
google <- google_latest[google_latest$Impressions > 0,]
google$Coalition <- ifelse(grepl('LCV',google$Campaign),'LCV',ifelse(grepl('PP|Planned|Protect',google$Campaign),'Planned Parenthood',ifelse(grepl('PM|Progress',google$Campaign),'Progress Michigan','OTHER')))
google$Channel <- 'Google'
google$Day <- as.Date(google$Day,'%Y-%m-%d')
google$Reach <- 0
google[,c('Theme','Message','Tone','Format','Version','Political')] <- ''
google[,c('Like','Love','Haha','Wow','Sad','Angry','Link Clicks')] <- 0
google$Comments <- 0.0
google$Shares <- 0
google$`Website Clicks` <- 0
google$`Website Views` <- 0
google$`Video Views` <- google$Impressions
google$Clicks <- as.numeric(google$Clicks)
google$Cost <- as.numeric(google$Cost)
google$`True Engagements` <- rowSums(google[,c('Clicks')],na.rm=TRUE)
google_summary <- google[,c('Day','Channel','Coalition','Campaign','Ad group','Ad','Ad final URL','Cost','Reach','Impressions','Clicks','True Engagements','Comments','Shares','Video Views','Video played to 100%','Link Clicks','Website Clicks','Website Views','Like','Love','Haha','Wow','Sad','Angry','Budget','Theme','Message','Tone','Format','Version','Political')]
google_summary$Cluster <- ''
google_summary$Budget <- as.numeric(google_summary$Budget)
google_summary$Impressions <- as.numeric(google_summary$Impressions)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  COMBINE CHANNELS
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
standard_names <- c('Date','Channel','Coalition','Campaign','Ad Group','Ad Name','Destination','Spend','Reach','Impressions','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Link Clicks','Website Clicks','Website Views','Like','Love','Haha','Wow','Sad','Angry','Budget','Theme','Message','Tone','Format','Version','Political','Cluster')
setnames(facebook_summary, old=colnames(facebook_summary), new=standard_names)
setnames(twitter_summary, old=colnames(twitter_summary), new=standard_names)
setnames(google_summary, old=colnames(google_summary), new=standard_names)
summary_table <- bind_rows(facebook_summary,twitter_summary,google_summary)
for (i in names(summary_table)){summary_table[is.na(get(i)),(i):=0]}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  BRING IN CLUSTER DATA
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_table_cluster <- setDT(merge(x = catalist_clusters, y = summary_table, by = 'Cluster', all.y = TRUE))
summary_table_cluster$`Matrix Position` <- cluster_list$Quadrant[match(unlist(summary_table_cluster$`Matrix Position`), cluster_list$`Matrix Position`)]
summary_table_cluster$`Matrix Position`[is.na(summary_table_cluster$`Matrix Position`)] <- 'Unspecified (Phase 1)'
summary_table_cluster <- merge(summary_table_cluster,billing,on='Campaign',all.x=TRUE)
yaxistest <- aggregate(cbind(Spend,Engagements, Impressions) ~ Date + Gender + Ethnicity+Bi_Urban+Bi_Coastal+Bi_Protestant+`Matrix Position`+`Psychographic Segment`+Message,data=summary_table_cluster, sum, na.rm = TRUE)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  BRING IN GOOGLE ANALYTICS DATA
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ga_latest_colnames <- c('Date','Destination','Deep_Views','Duration','Find A Polling Station|Find A Polling Station','Form Submit|Stand Up, Fight Back!','Home_Views','Navigation|Donate','Navigation|Home','Navigation|News',"Navigation|Schuette's Agenda",'Navigation|Take Action',"Navigation|Whitmer's Record",'Navigation|Why We Stand With Whitmer','Navigation|click','Scroll Depth|100','Scroll Depth|25','Scroll Depth|50','Scroll Depth|75','View Content|Find Polling Place','View Content|Radical Agenda','View Content|Whitmer Record','View Content|Why Stand Whitmer','ga:bounces','ga:sessions','ga:users')
setnames(ga_latest, old=colnames(ga_latest), new=ga_latest_colnames)
ga_latest$Date <- as.Date(as.character(ga_latest$Date),'%Y%m%d')
summary_table_cluster$Destination <- gsub('https','http',summary_table_cluster$Destination)
ga_latest$Destination <- gsub('https','http',ga_latest$Destination)
summary_table_cluster$Destination <- gsub('\\/\\?','?',summary_table_cluster$Destination)
ga_latest$Destination <- gsub('\\/\\?','?',ga_latest$Destination)
summary_table_cluster_analytics <- merge(summary_table_cluster,ga_latest,by=c('Date','Destination'),all.x=TRUE)
summary_table_cluster_analytics_names1 <- c('Date','Destination','Channel','Campaign','Cluster','Combined','global_clust_number','cluster_name','votepropensity2017g_buckets.x','sy_vch_index_buckets.x','Age','Religious','Gun Ownership','Educated','Progressive Ideology','catalistmodel_child','married','non_caucasian','Income','is_protestant','Percent Female','Percent Caucasian','catalistmodel_income','Percent Urban','is_coastal','Republican','Persuadable','Democrat','Low Vote Likelihood','Medium Vote Likelihood','High Vote Likelihood','Vote Likelihood Score (Catalist)','Likelihood to Vote Democrat (Catalist)','seg_1','seg_2','seg_3','seg_4','seg_5','seg_6','seg_7','seg_8','seg_9','count','Damage_to_the_Great_Lakes_because_of_pipeline_leaks_could_hurt_Michigans_economy','The_Line_5_pipeline_has_a_serious_impact_on_clean_water','The_Line_5_pipeline_poses_a_direct_risk_to_health_of_Michigan_communities','Alternatives_to_the_Line_5_pipeline_have_their_own_problems_to_consider','Michigan_should_do_more_to_protect_fresh_water_bodies_from_oil_spills_and_other_pollution','The_economic_boost_that_pipelines_create_for__Michigan__is_worth_some_risks_of_potential_pipeline_leaks','The_risks_of_oil_pipeline_leakages_is_too_low_a_probability_to_worry_about_very_much','a_national_effort_to_cut_carbon_dioxide_emissions_by_setting_state_limits_is_a_priority_to_implement','Carbon_taxes_and_CO2_emissions_caps_pose_too_much_risk_to_the_economy','Michigan_should_do_more_to_help_children_and_families_affected_by_lead_poisoning_in_Flint','Investments_in_remedial_education_and_enrichment_programs_are_needed_to_support_children_affected_by_lead_poisoning_in_Flint','Women_have_a_right_to_choose_to_have_an_abortion','Women_should_be_able_to_access_abortions_through_health_insurance_in_the_case_of_rape_or_incest','Abortions_should_not_be_covered_by_health_insurance_in_any_circumstance','Planned_Parenthood_should_not_receive_public_funding','Undocumented_women_under_the_age_of_18_should_be_able_to_access_abortions_legally','Employers_should_have_the_right_to_choose_whether_to_cover_birth_control_as_part_of_their_health_insurance_policies','Medicaid_recipients_should_be_required_to_meet_minimum_work_requirements_before_receiving_benefits','NAFTA_should_be_abolished_or_dramatically_reformed','I_support_stricter_gun_reform_legislation','Michigan_is_facing_an_opioid_crisis','Health_Care_is_unaffordable_in_Michigan_these_days','Auto_insurance_rates_are_too_high_in_Michigan','The_costs_of_a_college_education__have_become_unaffordable_in_Michigan','Closing_skill_gaps_for_the_workforce_in_Michigan_is_a_priority','Michigan_should_raise_the_state_minimum_wage','Politics_in_Michigan_are_corrupt','Legalizing_and_taxing_marijuana_is_a_good_option_for_increasing_state_revenues_and_funding','Legalizing_marijuana_is_a_bad_idea','The_current_policies_of_elected_officials_in_Michigan_are_detrimental_to_the_environment','Ensuring_women_receive_equal_pay_for_their_work_is_a_political_priority','15_per_hour_is_a_fair_and_appropriate_minimum_wage_for_Michigan_workers','PreKindergarten_education_should_be_available_to_all_Michigan_families','Licensing_and_training_teachers_to_use_firearms_is_an_important_option_to_consider_for_mitigating_school_shootings','Police_should_be_able_to_confiscate_firearms_and_ammunition_from_a_person_whom_a_judge_deems_a_legitimate_threat','Charter_schools_are_leaving_Michigan_children_behind','Same_sex_couples_should_be_allowed_to_be_legally_married_in_Michigan','Married_samesex_couples_in_Michigan_should_be_allowed_to_adopt_children','Students_should_be_allowed_to_use_the_restroom_and_locker_room_that_matches_their_gender_identity_whether_or_not_that_is_the_same_as_their_sex_at_birth','Farms_should_have_flexibility_in_the_minimum_wage_they_pay_to_migrant_or_seasonal_workers','Adoption_agencies_should_be_able_to_deny_couples_an_adoption_for_religious_reasons','A_study_by_Michigan_State_U_puts_the_cost_of_cleaning_a_spill_from_the_Line_5_pipeline_at_6_billion','ruptured_or_leakedoil_spilled_could_cover_more_than_17000_square_miles')
summary_table_cluster_analytics_names2 <- c('Enbridge_Line_5_has_spilled_at_least_11M_gallons_in_past_50_years','In_2010_when_the_Line_6B_spilled_into_the_Kalamazoo_River_the_company_didnt_notice_the_pipeline_had_ruptured_until_being_alerted_by_an_outside_caller_17_hours_later','where_children_suffered_brain_damage_due_to_lead_poisoning_from_contaminated_water_sources_dropped_from_418_percent_to_107_percent','Children_in_Flint_exposed_to_contaminated_water_are_at_risk_for_a_number_of_problems_including_lower_IQ_scores_developmental_delays_and_behavioral_issues_Even_after_lead_exposure_stops_the_effects_can_last_for_years_or_even_be_permanent','A_2016_Study_estimated_275_people_in_Michigan_die_each_year_due_to_air_pollution','Michigan_has_high_rates_of_asthma_even_higher_in_Detroit_where_asthma_hospitalization_rate_is_triple_the_state_average','AfricanAmerican_children_in_metro_Detroit_suffered_2402_asthma_attacks_and_missed_1751_school_days_due_to_asthma_per_year','40_of_Michigan_women_live_in_counties_with_no_abortion_clinics','Breast_cancer_is_the_second_leading_cause_of_cancer_death_Each_year_1400_to_1500_Michigan_women_die_of_breast_cancer','1_in_5_women_in_America_has_relied_on_Planned_Parenthood','Bill_Schuette_Republican','Patrick_Colbeck_Republican','Brian_Calley_Republican','Gretchen_Whitmer_Democrat','Abdul_ElSayed_Democrat','Shri_Thanedar_Democrat','Top Issue 1','Top Issue 2','Top Issue 3','x','y','z','Psychographic Segment','Gender','Ethnicity','Coastal','Protestant','Urbanity','Income Level','Age Level','Marital Status','Segment','Matrix Position','votepropensity2017g_buckets.y','sy_vch_index_buckets.y','Budget Allocation','Dollars based on Budget','Coalition','Ad Group','Ad Name','Spend','Reach','Impressions','All Social Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Link Clicks','Website Clicks','Website Views','Like','Love','Haha','Wow','Sad','Angry','Budget','Theme','Message','Tone','Format','Version','Political','Billing Index','Billing Category','Billing Description','Deep_Views','Duration','Find A Polling Station|Find A Polling Station','Form Submit|Stand Up, Fight Back!','Home_Views','Navigation|Donate','Navigation|Home','Navigation|News',"Navigation|Schuette's Agenda",'Navigation|Take Action',"Navigation|Whitmer's Record",'Navigation|Why We Stand With Whitmer','Navigation|click','Scroll Depth|100','Scroll Depth|25','Scroll Depth|50','Scroll Depth|75','View Content|Find Polling Place','View Content|Radical Agenda','View Content|Whitmer Record','View Content|Why Stand Whitmer','ga:bounces','ga:sessions','ga:users')
summary_table_cluster_analytics_names <- c(summary_table_cluster_analytics_names1,summary_table_cluster_analytics_names2)
setnames(summary_table_cluster_analytics, old=colnames(summary_table_cluster_analytics), new=summary_table_cluster_analytics_names)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  MAKE EXCEL RAW DATA SHEET (NOT FOR PRODUCTION DASHBOARD)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
makeExcelRawData <- function(dt){
  check <- dt[(dt$Impressions>0),] # & (as.Date(dt$Date)>'2018-08-28'),]
  for(col in names(check)) set(check, i=which(grepl('^$|^NA$',check[[col]])),j=col,value='-')
  for (i in names(check)){check[is.na(get(i)),(i):=0]}
  check2 <- check[,c(3,4,5,26,27,28,29,30,31,32,33,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,100,103,104,105,110,111,112,113,114,115,116,117,109,119,1,124,125,126,2,146,147,148,149,152,153,154,127,128,129,130,132,133,134,135,136,137,138,139,140,141,142,143,144,131,155,156,157,158,159,160,161,162,163,164,165,166,167,169,170,171,172,173,174,175,176,177,178)]
  write.csv(check2,'new_data.csv')
}
makeExcelRawData(summary_table_cluster_analytics)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SUMMARY TABLE
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_table1_lcv <- summary_table_cluster[summary_table_cluster$Coalition=='LCV',c('Date','Coalition','Channel','Matrix Position','Spend','Reach','Impressions','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Website Clicks'),with=FALSE][,lapply(.SD,sum),by=c('Date','Coalition','Channel','Matrix Position')]
summary_table1_pp <- summary_table_cluster[summary_table_cluster$Coalition=='Planned Parenthood',c('Date','Coalition','Channel','Matrix Position','Spend','Reach','Impressions','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Website Clicks'),with=FALSE][,lapply(.SD,sum),by=c('Date','Coalition','Channel','Matrix Position')]
summary_table1_pm <- summary_table_cluster[summary_table_cluster$Coalition=='Progress Michigan',c('Date','Coalition','Channel','Matrix Position','Spend','Reach','Impressions','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Website Clicks'),with=FALSE][,lapply(.SD,sum),by=c('Date','Coalition','Channel','Matrix Position')]
summary_table1 <- bind_rows(summary_table1_lcv,summary_table1_pp,summary_table1_pm)
newdf <- expand.grid(dateRangeFull,interest_groups,channels,quadrant)
newdf[,c('Spend','Reach','Impressions','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Website Clicks')] <- 0
setnames(newdf, old=colnames(newdf), new=colnames(summary_table1))
summary_table1 <- rbind(summary_table1,newdf)[,lapply(.SD,sum),by=c('Date','Coalition','Channel','Matrix Position')][order(Date,Coalition,Channel,`Matrix Position`)]
summary_table1 <- summary_table1[!is.na(Spend), CumSpend := cumsum(Spend), by = c('Coalition','Channel','Matrix Position')]
summary_table1 <- summary_table1[!is.na(Impressions), CumImpressions := cumsum(Impressions), by = c('Coalition','Channel','Matrix Position')]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SENTIMENT
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sentiment <- merge(sentiment,sentiment_lookup,all.x=TRUE) #match theme names with our booleans
sentiment$`Positive Mentions` <- round(sentiment$`Total Mentions`*sentiment$`Positive Sentiment`/100)
sentiment$`Neutral Mentions`  <- round(sentiment$`Total Mentions`*sentiment$`Neutral Sentiment`/100)
sentiment$`Negative Mentions` <- round(sentiment$`Total Mentions`*sentiment$`Negative Sentiment`/100)
sentiment$Date <- as.Date(sentiment$Date)
sentiment$TextDate <- format(as.Date(sentiment$Date), format='%B %d, %Y')

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SOCIAL LISTENING
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#NEWS
b <- gsub('.*www.','',news$Link,perl=TRUE)
test <- urltools::url_parse(b)$domain
news$Link <- paste0("<a href='",news$Link,"' target='_blank'>",test,'</a>')
news$Date <- as.Date(news$Date)
news$TextDate <- format(as.Date(news$Date), format='%B %d, %Y')

#MENTIONS TABLE
mentions <- sentiment[,c('Date','TopicRollup','Total Mentions'),with=FALSE][,lapply(.SD,sum),by=c('Date','TopicRollup')][order(Date,TopicRollup)]
newsfeed_array <- "['General','Gubernatorial','LCV','Planned Parenthood']"
social_array <- "['LCV','Planned Parenthood','Progress Michigan','Michigan Gubernatorial Race']"

#SOCIAL LISTENING - COMPETITIVE DISPLAY TABLES
dat_ <- subset(competitor_display, Category=='RightWing')
dat_$Content <- paste0('<img src="display/',dat_$Image,'" height="60"></img>')
dat1 <- select(dat_, 'Date Created', TextDate, Name, Content, Topic)
dat_2 <- subset(competitor_display, Category == "Republican")
dat_2$Content <- paste0('<img src="display/',dat_2$Image,'" height="60"></img>')
dat2 <- select(dat_2, 'Date Created', TextDate, Name, Content, Topic)
dat_3 <- subset(competitor_display, Category == "Democratic")
dat_3$Content <- paste0('<img src="display/',dat_3$Image,'" height="60"></img>')
dat3 <- select(dat_3, 'Date Created', TextDate, Name, Content, Topic)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SURVEY DATA
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
survey_new <- survey
survey_new$Counter <- 1
names(survey_new)[7] <- 'Response'
treated_clusters <- unique(summary_table_cluster[!grepl('Survey',summary_table_cluster$Campaign) & (summary_table_cluster$Cluster != ''),]$Cluster)
survey_new <- setDT(merge(x = survey_new[!grepl('participant',survey_new$Origin),], y = unique(summary_table_cluster[,c('Cluster','Matrix Position')]), by.x = 'Origin', by.y = 'Cluster',all.x=TRUE))
clusters$`global_clust_number` <- as.character(clusters$`global_clust_number`)
survey_new <- merge(x = survey_new, y = unique(clusters[,c('global_clust_number','count','catalistmodel_voteprop2018','catalistmodel_vch_index')]),by.x='Origin',by.y='global_clust_number',all.x=TRUE)
survey_new$`Matrix Position` <- ifelse(survey_new$Origin=='0','Control',survey_new$`Matrix Position`)
survey_new$Treated <- ifelse((survey_new$Origin %in% treated_clusters)==TRUE,'Treated Clusters',ifelse(survey_new$Origin=='0','Control','Non-Treated Clusters'))
survey_new$Success <- ifelse(survey_new$Response=='Gretchen Whitmer',1,ifelse(survey_new$Response=='Bill Schuette',0,0.5))
survey_new$Date <- lapply(survey_new$`Date and time`,function(x) as.Date(strsplit(x,' ')[[1]][1],'%Y-%m-%d'))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GLOSSARY
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glossaryTitle <- '<h3 class=glossary-title>GLOSSARY OF TERMS</h3>'
glossaryImpressions <- '<h4>Impressions</h4><p class="p-def">The total number of times that ads were viewed. This includes repeat views for the same user.</p>'
glossaryReach <- '<h4>Reach</h4><p class="p-def">The estimated number of people who saw the ads at least once.  Repeat views by the same person are not counted.</p>'
glossaryEngagements <- '<h4>Engagements</h4><p class="p-def"><b>Facebook:</b> The combined sum of Likes,Comments,Shares and 3-second video Views on all ads.</p>'
glossaryEngagements2 <- '<p class="p-def-small"><b>Twitter:</b> Any click-based interactions on a tweet (Retweets, Replies, Follows, Favorites, Links, Cards, Hashtags, Embedded Media, Username, Photo & Tweet Expansion) + 3-second video views</p>'
glossaryEngagements3 <- '<p class="p-def-small"><b>Google:</b> Combined clicks on ads.</p>'
glossaryEngagementRate <- '<h4>Engagement Rate</h4><p class="p-def">The sum of all engagement types, divided by the sum of Impressions, for the ads in question.</p>'
glossaryCPM <- '<h4>CPM</h4><p class="p-def">The cost of 1,000 impressions being served to users.  This is a standard industry metric and is always measured in groups of 1000 impressions.</p>'
glossaryMentions <- '<h4>Mentions</h4><p class="p-def">A post or comment made on a social media site, new site or blog concerning the topic.  The majority of this data covers Twitter, though a small amount is blog and forum data.  A mention can either be Positive, Negative or Neutral depending on its content.</p>'
glossarySentiment <- '<h4>Sentiment</h4><p class="p-def">The overall feeling of the topic on the web based on the Positive and Negative mentions.  The higher the percentage of Positive Mentions, the more positive the sentiment and vice versa.</p>'
glossaryPosSentiment <- '<h4>Positive Sentiment</h4><p class="p-def">Total number of Positive Mentions/(Total Number of Positive Mentions + Total Number of Negative Mentions)</p>'
glossaryNegSentiment <- '<h4>Negative Sentiment</h4><p class="p-def">Total number of Negative Mentions/(Total Number of Positive Mentions + Total Number of Negative Mentions)</p>'

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GTM
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jsCode <- paste0("shinyjs.init = function(){(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
  j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-NB9F7B7');}")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  DASHBOARD STRUCTURE
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
header <- fluidRow(
  tags$h1("MICHIGAN ISSUE CAMPAIGN"),
  tags$img(src = 'michigan.png', class = 'map'),
  #tags$div(class = 'sidebar_toggle',actionButton('do','Toggle Sidebar')),
  tags$a(class = 'logout_link', 'Logout',href = 'https://insights.galepartners.com/flamingo/__logout__/'),
  tags$h5('Powered by', class = 'power'),
  tags$img(src = 'alchemy_white.svg', class = 'alchemy'),
  tags$img(src = 'alchemy.svg', class = 'alchemy2')
)

summary_view <- tabPanel(
  h[1],
  mainPanel(
    header,
    fluidRow(
      tags$h2('Campaign Summary View'),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3(class='h3-float h3-first','Channel Performance Table'),
      tags$h3(class='h3-float',' - '),
      tags$h3(class='h3-float','Billing Summary'),
      tags$h3(class='h3-float',' - '),
      tags$h3(class='h3-float','Record of Invoices'),
      tags$h3(class='h3-float',' - '),
      tags$h3(class='h3-float','Pacing'),
      tags$div(class='table-mask'),
      tags$div(class='media-table',DT::dataTableOutput('summary_table'))
    ),
    fluidRow(
      tags$h3(class='h3-float h3-first','Billing Summary'),
      #tags$div(class='table-mask'),
      tags$div(class='billing-table',DT::dataTableOutput('billing_table'))
    ),
    fluidRow(
      tags$h3(class='h3-float h3-first','Record of Invoices'),
      #tags$div(class='table-mask'),
      tags$div(class='invoices-table',DT::dataTableOutput('invoices_table'))
    ),
    fluidRow(class='chart-row',
             tags$h3(class='h3-chart h3-float h3-first','Campaign Spend'),
             tags$div(class='cbx cumsum',checkboxInput('cumcheck','Show As Cumulative',FALSE)),
             tags$div(class='cbx split',radioButtons('chartsplit','',c('Total' = 'total','By Channel' = 'channel','By Coalition' = 'coalition'),selected = 'total')),
             tags$div(class='cbx ctype',radioButtons('chartType','',c('Line' = 'spline','Bar' = 'bar','Column' = 'column'),selected = 'column')),
             highchartOutput('spend_chart')
    ),
    fluidRow(class='chart-row',
             tags$h3(class='h3-chart2 h3-float h3-first','Campaign Impressions'),
             tags$div(class='cbx cumsum2',checkboxInput('cumcheck2','Show As Cumulative',FALSE)),
             tags$div(class='cbx split2',radioButtons('chartsplit2','',c('Total' = 'total','By Channel' = 'channel','By Coalition' = 'coalition'),selected = 'total')),
             tags$div(class='cbx ctype2',radioButtons('chartType2','',c('Line' = 'spline','Bar' = 'bar','Column' = 'column'),selected = 'spline')),
             highchartOutput('impression_chart')
    ))
)

creative_detail <- tabPanel(
  h[2],
  mainPanel(
    header,
    fluidRow(
      tags$h2('Creative Detail Reports'),tags$div(class='below_title')
    ),
    fluidRow(
      tags$div(class = 'h3-reaction-wrapper cluster-graph-wrapper', 
               tags$h3(class='h3_fb h3_cluster','Creative Performance by Cluster Attributes'),
               tags$div(class='filterbox filter-xaxis',selectInput('xaxis', "Select Split:", c(chart_dims), selected = 'Matrix Position')),
               tags$div(class='filterbox filter-yaxis',selectInput('yaxis', "Select Metric:", c(chart_metrics),selected = 'Engagement Rate')),
               tags$div(class='filterbox filter-chart-message',uiOutput('chart_messages'))
      ),
      highchartOutput('clusterGraph')
    ),
    fluidRow(
      tags$div(class='h3-reaction-wrapper table-wrapper',
               tags$h3(class='h3_fb','Facebook Creative Performance: Message'),
               tags$div(class='cbx percent',checkboxInput('percentCheckMsg','Show Percentages',FALSE))
      ),
      tags$div(class='creative-table sml creative-table1',DT::dataTableOutput('creative_table1')),
      tags$div(class='creative-table sml creative-table-total creative-table1a',DT::dataTableOutput('creative_table1a'))
    ),
    fluidRow(class='row-nobreak',
      tags$div(class='h3-reaction-wrapper',
              tags$h3(class='h3_fb h3_mid','Facebook Creative Performance: Tone'),
              tags$div(class='cbx percent',checkboxInput('percentCheckTone','Show Percentages',FALSE))
      ),
      tags$div(class='creative-table sml creative-table2',DT::dataTableOutput('creative_table2')),
      tags$div(class='creative-table sml creative-table-total creative-table2a',DT::dataTableOutput('creative_table2a'))
    ),
    fluidRow(class='row-nobreak',
      tags$div(class='h3-reaction-wrapper',
              tags$h3(class='h3_fb h3_mid','Facebook Creative Performance: Format'),
              tags$div(class='cbx percent',checkboxInput('percentCheckFormat','Show Percentages',FALSE))
      ),
      tags$div(class='creative-table sml creative-table3',DT::dataTableOutput('creative_table3')),
      tags$div(class='creative-table sml creative-table-total creative-table3a',DT::dataTableOutput('creative_table3a'))
    ))
)

creatives_list <- tabPanel(
  'Creatives List',
  mainPanel(
    header,
    fluidRow(
      tags$h2('Creative Image View'),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3(class='h3-float h3-first','Creative Performance'),
      tags$h3(class='h3-float',' - '),
      tags$h3(class='h3-float','Post Reactions'),
      tags$h3(class='h3-float',' - '),
      tags$h3(class='h3-float','Creative Analysis'),
      tags$div(class='filterbox filter-creative-table-split', selectInput('creative_table_split', 'Select Split:', c('None',chart_dims), selected = 'None')),
      tags$div(class='filterbox filter-creative-table-metric',selectInput('creative_table_metric','Select Metric:',c('Default View'='default_view',chart_metrics),selected = 'Default View')),
      tags$label(class='search-label-float','Search:'),
      tags$div(class='creative-image-table',DT::dataTableOutput('creative_image_table'))
    )
  )
)

post_reactions <- tabPanel(
  h[3],
  mainPanel(
    header,
    fluidRow(
      tags$h2('Facebook Post Reaction Summary'),
      tags$div(class='below_title')
    ),
    fluidRow(
      fluidRow(
        tags$div(class='icon-grid',
           tags$div(class='reaction-bumper',
              tags$div(class='bumper1',tags$h3(class='h3_fb_react','Facebook Post Reactions')),
              tags$div(class='bumper2',tags$div(class='cbx percent',checkboxInput('percentCheck','Show Percentages',FALSE))),
              tags$div(class='react-icons',
                tags$img(class='react-icons-img', src='reactions2.png')
              )
           )
        )
      ),
      fluidRow(
        tags$div(class='reactions-table',DT::dataTableOutput('reactions_table')),
        tags$div(class='reactions-table reactions-table-a',DT::dataTableOutput('reactions_tablea'))
      )
   ))
)

sentiment_tab <- tabPanel(
  h[4],
  mainPanel(
    header,
    fluidRow(
      tags$h2('Review of Topic Activity & Sentiment'),
      tags$div(class='below_title'),
      tags$div(class='filterbox filter-sentiment-range',radioButtons('sentiment_range','SELECT RANGE:',c('1-Day','7-Day','28-Day'),selected='7-Day'))
    ),
    tabsetPanel(type = "tabs",
      tabPanel('CANDIDATES',
        tags$div(class='profile-mini-row',
          fluidRow(class='candidate-name-box whitmer',
             tags$h2(class='candidate-name whitmer','Gretchen ',tags$b('WHITMER')),
             tags$img(src = 'whitmer.jpg', class = 'candidate-photo whitmer'),
             tags$div(class='candidate-param-holder',
                tags$div(class='candidate-param',tags$h5('Sentiment Score '),tags$b(textOutput('whitmer_sentiment'))),
                tags$div(class='candidate-param',tags$h5('Change '),tags$b(textOutput('whitmer_change'))),
                tags$div(class='candidate-param',tags$h5('Total Mentions '),tags$b(textOutput('whitmer_mentions'))),
                tags$div(class='candidate-param',tags$h5('Positive Mentions '),tags$b(textOutput('whitmer_positives'))),
                tags$div(class='candidate-param',tags$h5('Negative Mentions '),tags$b(textOutput('whitmer_negatives')))
             )
          ),
          fluidRow(class='candidate-name-box schuette',
             tags$h2(class='candidate-name schuette','Bill ',tags$b('SCHUETTE')),
             tags$img(src = 'schuette.jpg', class = 'candidate-photo schuette'),
             tags$div(class='candidate-param-holder',
                tags$div(class='candidate-param',tags$h5('Sentiment Score '),tags$b(textOutput('schuette_sentiment'))),
                tags$div(class='candidate-param',tags$h5('Change '),tags$b(textOutput('schuette_change'))),
                tags$div(class='candidate-param',tags$h5('Total Mentions '),tags$b(textOutput('schuette_mentions'))),
                tags$div(class='candidate-param',tags$h5('Positive Mentions '),tags$b(textOutput('schuette_positives'))),
                tags$div(class='candidate-param',tags$h5('Negative Mentions '),tags$b(textOutput('schuette_negatives')))
             )
          )
        ),
        tags$div(class='chart-mini-row',
          fluidRow(tags$h3(class='mention-chart h3-sentiment',textOutput('coefficient1')),
            highchartOutput('sentiment_chart1',height = "270px")
          ),
          fluidRow(tags$h3(class='mention-chart h3-sentiment',textOutput('coefficient2')),
            highchartOutput('sentiment_chart2',height = "270px")
          )
        ),
        tags$div(class='chart-mini-row',
          fluidRow(tags$h3(class='mention-chart','Mentions'),
            highchartOutput('mention_chart1',height = "270px")
          ),
          fluidRow(tags$h3(class='mention-chart','Mentions'),
            highchartOutput('mention_chart2',height = "270px")
          )           
        ),
        tags$div(class='chart-mini-row-wide',
          fluidRow(class='chart-mini-row-1',tags$p(class = 'sysomos-caption sysomos-caption-2',''))
        ),
        tags$div(class='chart-mini-row',
          fluidRow(tags$h3(class='mention-chart','Weekly Change in Negative Sentiment'),
            highchartOutput('sentiment_deltachart1',height = "270px")
          ),
          fluidRow(tags$h3(class='mention-chart','Weekly Change in Negative Sentiment'),
            highchartOutput('sentiment_deltachart2',height = "270px")
          )
        ),
        tags$div(class='chart-mini-row',    
          fluidRow(class='last-row',tags$h3(class='mention-chart extra','Partiality vs Neutrality (%)'),
            highchartOutput('sentiment_stackedbar1',height = "270px")
          ),
          fluidRow(class='last-row',tags$h3(class='mention-chart extra','Partiality vs Neutrality (%)'),
            highchartOutput('sentiment_stackedbar2',height = "270px")
          )
        )),
    tabPanel('THEMES',
      tags$div(class='chart-mini-row',
        fluidRow(tags$h3(class='mention-chart h3-sentiment',textOutput('coefficient3')),
          highchartOutput('sentiment_chart3',height = "270px")
        ),
        fluidRow(tags$h3(class='mention-chart','Mentions'),
          highchartOutput('mention_chart3',height = "270px")
        )
      ),
      tags$div(class='chart-mini-row',    
        fluidRow(class='last-row',tags$h3(class='mention-chart extra','Weekly Change in Negative Sentiment'),
          highchartOutput('sentiment_deltachart3',height = "270px")
        ),
        fluidRow(class='last-row',tags$h3(class='mention-chart extra','Partiality vs Neutrality (%)'),
          highchartOutput('sentiment_stackedbar3',height = "270px")
        )
      ))
   ))
)

analysis_tab <- tabPanel('Analysis',mainPanel(
  header,
  fluidRow(tags$h2('Deep-Dive Performance Analysis'),tags$div(class='below_title')),
  fluidRow(class='chart-row',
    tags$h3(class='h3-chart','Cluster-Based Media Performance'),
    highchartOutput('scatter_plot',height = '450px')
  )
))

survey_tab <- tabPanel('Survey',mainPanel(
  header,
  fluidRow(tags$h2('Survey Results'),tags$div(class='below_title')),
  tags$div(class='chart-mini-row survey-row',
    fluidRow(class='chart-row',
      tags$h3(class='h3-chart','% Democrat Survey Responses by Quadrant'),
      highchartOutput('survey_chart1')
    ),
    fluidRow(class='chart-row',
      tags$h3(class='h3-chart','% Democrat Shift From Base Sentiment'),
      highchartOutput('survey_chart2')
    )
  ),
  fluidRow(class='chart-row survey-row',
     tags$h3(class='h3-chart','Cluster-Based Change In Sentiment'),
     tags$div(class='cbx survey',radioButtons('surveyGroup','',c('All'='all','Treated Clusters'='treated','Non-Treated Clusters'='non-treated'),selected='all')),
     #sliderInput('shift_slider','Range:',min = 1,max = 1000,value = c(200,600)),
     tags$h6(class='chart-label label-rep','Republican'),
     tags$h6(class='chart-label label-per','Persuadable'),
     tags$h6(class='chart-label label-dem','Democrat'),
     highchartOutput('scatter_plot_survey',height = '450px')
  )
  #fluidRow(class='table-row',
  #  tags$h3(class='h3-table','Who Has Changed Their Sentiment?'),
  #  DT::dataTableOutput('survey_table')
  #)
))

social_listening <- tabPanel(
  h[5],
  mainPanel(
    header,
    fluidRow(tags$h2('News & Social Listening'),tags$div(class='below_title')),
    tabsetPanel(type = "tabs",
        tabPanel('Newsfeed',fluidRow(tags$h3('Publications'),fluidRow(DT::dataTableOutput("newstable")))),
        tabPanel('Twitter',fluidRow(tags$h3("Trending Hashtags")),fluidRow(highchartOutput("hashhigh",height = "700px"))),
        tabPanel('Competitor Display',
           fluidRow(class='display-wrapper1 wrapper-hide',tags$h3('Right Wing Influencer Content'),tags$div(class='display-tables',DT::dataTableOutput('display1'))),
           fluidRow(class='display-wrapper2 wrapper-hide',tags$h3('Republican Content'),tags$div(class='display-tables',DT::dataTableOutput('display2'))),
           fluidRow(class='display-wrapper3 wrapper-hide',tags$h3('Democratic Content'),tags$div(class='display-tables',DT::dataTableOutput('display3')))
        )
    )))
colorChoices <- c('rgb(124, 181, 236)','rgb(224, 157, 157)','rgb(124, 236, 181)','rgb(255, 245, 165)','#7cb5ec','#535364','#ff7788')

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  CHARTS
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_chart <- function(dims,metrics,checks,ctype,dates,interest,channel,split,username,quad,btn,metrics2){
  btn
  s <- summary_table1[(summary_table1$Date >= dates[1]) & (summary_table1$Date <= dates[2]),]
  if(username_map(username) == 'all_groups'){
    if(interest !='all_groups'){s <- s[s$Coalition==interest,]}
  }
  else{s <- s[s$Coalition==username_map(username),]}
  if(channel != 'all_channels'){s <- s[s$Channel==channel,]}
  if(quad != 'all_quadrants'){s <- s[s$`Matrix Position`==quad,]}
  s1 <- s[,c('Date',metrics2),with=FALSE][,lapply(.SD,sum),by=c('Date')]
  splits <- channels
  ctitle2 = 'by Channel'
  if(split=='coalition'){
    splits <- interest_groups
    ctitle2 = 'by Coalition'
    s$Channel <- s$Coalition
  }
  s2 <- s[,c('Date','Channel',metrics2),with=FALSE][,lapply(.SD,sum),by=c('Date','Channel')]
  switchvar <- list(name = paste0('Total ',metrics2[1]),data = as.numeric(unlist(s1[,2])))
  stackvar <- ifelse(grepl('line',ctype),FALSE,'normal')
  ctitle <-  paste0('Daily ',metrics2[1])
  if(checks[1]==TRUE){
    switchvar <- list(name = paste0('Total ',metrics2[1]),data = round(as.numeric(unlist(s1[,3])),2))
    s1 <- cbind(s1[,1],s1[,3],s1[,2])
    s2 <- cbind(s2[,1:2],s2[,4],s2[,3])
    ctitle <- paste0('Cumulative ',metrics2[1],' to Date')
  }
  switchvar1 <- list(name = splits[1],data = as.numeric(unlist(s2[s2$Channel==splits[1],3])),color = colorChoices[5])
  switchvar2 <- list(name = splits[2],data = as.numeric(unlist(s2[s2$Channel==splits[2],3])),color = colorChoices[6])
  switchvar3 <- list(name = splits[3],data = as.numeric(unlist(s2[s2$Channel==splits[3],3])),color = colorChoices[7])
  summary_chart1 <-(
    highchart() %>% 
      hc_chart(type = ctype) %>% 
      hc_title(text = ctitle) %>% 
      hc_subtitle(text = 'Sources: FB Ad Manager, Twitter Ad Mananger, Google Ad Manager') %>%
      hc_xAxis(categories = c(s1$Date),tickInterval = 2) %>%
      hc_plotOptions(line = list( #color = '#ff0000',
        dataLabels = list(enabled = FALSE),
        enableMouseTracking = TRUE),
        column=list(color = '#FF7788'),
        bar=list(color = '#FF7788')
      ) %>% 
      hc_series(switchvar)
  )
  summary_chart2 <-(
    highchart() %>% 
      hc_chart(type = ctype) %>% 
      hc_title(text = paste(ctitle,ctitle2)) %>% 
      hc_subtitle(text = 'Sources: FB Ad Manager, Twitter Ad Mananger, Google Ad Manager') %>% 
      #hc_legend(layout='vertical',align='left',verticalAlign='top',floating=true,x=40,itemMarginBottom=12) %>%
      hc_xAxis(categories = sort(unique(s2$Date)),tickInterval = 2) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = FALSE),
        enableMouseTracking = TRUE
      ),
      series = list(stacking=stackvar)
      ) %>% 
      hc_series(switchvar1,switchvar2,switchvar3)
  )
  aa <- summary_chart1
  if(split != 'total'){aa <- summary_chart2}
  aa
}

getCoeffs <- function(dt,topic,type,range){
  days <- ifelse(range=='1-Day',1,ifelse(range=='7-Day',7,28))
  s <- dt[(dt$Date>=max(dt$Date)-days+1) & (dt$TopicRollup==topic),]
  s2 <- dt[(dt$Date<max(dt$Date)-days+1) & (dt$Date>=max(dt$Date)-(2*days)+1) & (dt$TopicRollup==topic),]
  if(type == 'sentiment'){
    #if(sum(s$`Positive Mentions`)==0){
    #coeff <- percent_format()(as.numeric(sum(s$`Neutral Mentions`))/(as.numeric(sum(s$`Total Mentions`))))
    #}
    #else{
    coeff <- percent_format()(as.numeric(sum(s$`Positive Mentions`))/(as.numeric(sum(s$`Positive Mentions`))+as.numeric(sum(s$`Negative Mentions`))))
    #}
  }
  else if(type == 'mentions'){
    coeff <- as.numeric(sum(s$`Total Mentions`))
  }
  else if(type == 'neg_mentions'){
    coeff <- as.numeric(sum(s$`Negative Mentions`))
  }
  else if(type == 'pos_mentions'){
    coeff <- as.numeric(sum(s$`Positive Mentions`))
  }
  else if(type == 'change'){
    coeff1 <- as.numeric(sum(s$`Positive Mentions`))/(as.numeric(sum(s$`Positive Mentions`))+as.numeric(sum(s$`Negative Mentions`)))
    coeff2 <- as.numeric(sum(s2$`Positive Mentions`))/(as.numeric(sum(s2$`Positive Mentions`))+as.numeric(sum(s2$`Negative Mentions`)))
    coeff <- percent_format()((coeff1-coeff2)/coeff2)
  }
  coeff
}

plotSentiment <- function(dt,topic,dates,type,username,range){
  if(username_map(username) == 'all_groups'){topic <- topic[1]}
  else if(username_map(username) == 'LCV'){topic <- topic[2]}
  else if(username_map(username) == 'Planned Parenthood'){topic <- topic[3]}
  else if(username_map(username) == 'Progress Michigan'){topic <- topic[4]}
  s <- setDT(dt)[dt$TopicRollup==topic,c('Date','Neutral Sentiment','Negative Sentiment','Positive Sentiment','Total Mentions','Positive Mentions','Neutral Mentions','Negative Mentions'),with=FALSE][,lapply(.SD,sum),by=c('Date')][order(Date)]
  s$week <- format(s$Date, format="%Y-%U")
  #s <- s[s$Date<=as.Date('2018-08-16','%Y-%m-%d'),] #temp
  s1 <- s[,c('week','Positive Mentions','Negative Mentions','Neutral Mentions','Total Mentions')][,lapply(.SD,sum), by = c("week")][order(week)]
  s1$Pos <- round(as.numeric((s1$'Positive Mentions'/(s1$'Positive Mentions' + s1$'Negative Mentions'))),2)
  s1$Neg <- (round(as.numeric((s1$'Negative Mentions'/(s1$'Positive Mentions' + s1$'Negative Mentions'))),2))*100
  s1 <- mutate(s1, pct.chg = (s1$Neg - lag(s1$Neg))/lag(s1$Neg))
  s1$pct.chg <- s1$pct.chg * 100
  s2 <- s[!duplicated(s$week),]
  s1 <- cbind(s2$Date,s1[,week:=NULL])
  setnames(s1, old = 'V1', new = "Week")
  s1$Week <- format(as.Date(s1$Week), format="%b %d")
  s1$index <- c(1:nrow(s1))
  model <- lm(s1$Neg ~ s1$index, data = s1) #coefficients(model)
  s1$y <- coefficients(model)[2] * s1$index + coefficients(model)[1]
  if(type=='trend'){
    sentiment_chart <-(
      highchart() %>% 
        hc_chart(type = 'line') %>%
        hc_subtitle(enabled = FALSE) %>%
        hc_legend(enabled = TRUE, align = "left", verticalAlign = "top") %>%
        hc_xAxis(categories = c(s1$Week), labels = list(rotation = 0, style = list(fontSize= '8px')), tickInterval = 2) %>% 
        hc_yAxis(labels = list(format = "{value}%"),min = 0, max = 100) %>%
        hc_plotOptions(line = list(
          dataLabels = list(enabled = FALSE),
          marker = list(enabled = FALSE)
        )) %>% 
        hc_series(
          list(
            name = "Negative Sentiment",
            data = round(as.numeric(s1$Neg),2)
          ),
          list(
            name = "Trended line",
            data = round(as.numeric(s1$y),5),
            dashStyle = "shortdash",
            enableMouseTracking = FALSE
          )
       )
    )
  }
  else if(type=='barchart'){
    sentiment_chart <-(
      highchart() %>% 
        hc_chart(type = 'column') %>% 
        hc_subtitle(enabled = FALSE) %>%
        hc_legend(enabled = TRUE, align = "left", verticalAlign = "top") %>%
        hc_xAxis(categories = c(s1$Week), labels = list(rotation = 0, style = list(fontSize= '8px')),tickInterval = 2) %>% 
        hc_yAxis(labels = list(format = "{value}%"))%>%
        hc_plotOptions(series = list(stacking = "percent"),column = list(
          dataLabels = list(enabled = FALSE),
          enableMouseTracking = TRUE
        )) %>% 
        hc_series(
          list(
            name = "Negative Mentions",
            data = as.numeric(s1$'Negative Mentions')
          ),
          list(
            name = "Positive Mentions",
            data = as.numeric(s1$'Positive Mentions')
          ),
          list(
            name = "Neutral Mentions",
            data = as.numeric(s1$'Neutral Mentions')
          )
        )
    )
  }
  else if(type=='delta'){
    sentiment_chart <-(
      highchart() %>% 
        hc_chart(type = 'column') %>%
        hc_subtitle(enabled = FALSE) %>%
        hc_legend(enabled = TRUE, align = "left", verticalAlign = "top") %>%
        hc_xAxis(categories = c(s1$Week), labels = list(rotation = 0, style = list(fontSize= '8px'))) %>% 
        hc_yAxis(labels = list(format = "{value}%"),min = -100, max = 100) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE,format = '{y}%'),
          enableMouseTracking = TRUE
        )) %>% 
        hc_series(
          list(
            name = "Negative Sentiment Delta vs. Previous Week",
            data = round(s1$pct.chg,2))
        )
    )
  }
  else if(type=='coeff'){
    sentiment_chart <- percent_format()(coefficients(model)[2]/100)
  }
  sentiment_chart
}

mention_charts <- function(dt,theme,dates,username,range){
  topicSummary <- setDT(dt)[,c('Date','TopicRollup','Total Mentions'),with=FALSE][,lapply(.SD,sum),by=c('Date','TopicRollup')]
  gretchenMax <- max(topicSummary[topicSummary$TopicRollup=='Gretchen Whitmer',]$`Total Mentions`)
  schuetteMax <- max(topicSummary[topicSummary$TopicRollup=='Bill Schuette',]$`Total Mentions`)
  candidateMax <- max(gretchenMax,schuetteMax)
  if(username_map(username) == 'all_groups'){theme <- theme[1]}
  else if(username_map(username) == 'LCV'){theme <- theme[2]}
  else if(username_map(username) == 'Planned Parenthood'){theme <- theme[3]}
  else if(username_map(username) == 'Progress Michigan'){theme <- theme[4]}
  s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
  s <- s[s$TopicRollup==theme,]
  s$Date <- format(as.Date(s$Date),format='%b %d')
  highchart() %>% 
    hc_chart(type = 'spline',color = '#FF0000') %>%
    hc_subtitle(enabled = FALSE) %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = c(s$Date),tickInterval = 7) %>% 
    hc_yAxis(tickAmount = 4,max = candidateMax) %>% 
    hc_plotOptions(spline = list(
      dataLabels = list(enabled = FALSE),
      marker = list(enabled = FALSE),
      enableMouseTracking = TRUE)
    ) %>% 
    hc_series(
      list(
        name = "Mentions",
        data = as.numeric(sub("%",'',s$`Total Mentions`))
      )
    )
}

plotQuadrant <- function(dt,dates,metrics,dims,xaxis,yaxis,quad,msg){
  dt <- setDT(dt) 
  dt <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
  dt <- dt[,c(metrics,dims),with=FALSE][,lapply(.SD,sum),by=c(dims)]
  dt <- transform(dt, `Engagement Rate` = Engagements / Impressions)
  dt$CPM <- 1000*dt$Spend/dt$Impressions
  if(msg !='all_messages'){
    dt <- dt[dt$Message==msg,]
  }
  dt <- dt[,c(xaxis,metrics),with=FALSE][,lapply(.SD,sum),by=c(xaxis)]
  dt <- transform(dt, `Engagement Rate` = Engagements/Impressions)
  dt <- transform(dt, CPM = 1000*Spend/Impressions)
  dt <- dt[,c(xaxis,yaxis),with=FALSE][,lapply(.SD,sum),by=c(xaxis)]
  if (xaxis == "Date"){
    dt_mini <- data.frame(as.Date((unlist(dt[,1])),origin='1970-01-01'),c(as.numeric(unlist(dt[,2]))))
  }
  else{
    dt_mini <- data.frame(as.character(unlist(dt[,1])),c(as.numeric(unlist(dt[,2]))))}
  colnames(dt_mini) <- c('dim','metric')
  dt_mini <- dt_mini[order(dt_mini$dim),]
  dims <- dt_mini$dim
  if(nrow(dt_mini) == 1){dims <- list(dims)}
  title_msg = ifelse(msg !='all_messages',paste0('"',msg,'"'),'')
  if (xaxis == "Date") {
    hc <- highchart() %>% 
      hc_title(text = paste(title_msg,'Creative Performance by',xaxis) ) %>%
      hc_subtitle(text = 'Source: FB Ad Manager') %>%
      hc_chart(type = "line") %>% 
      hc_xAxis(categories = dims) %>%
      hc_add_series(data = dt_mini$metric, name = yaxis) %>%
      hc_legend(align = "left", verticalAlign = "top",
                layout = "horizontal", x = 0, y = 10) }
  else {
    hc <- highchart() %>% 
      hc_title(text = paste(title_msg,'Creative Performance by',xaxis) ) %>%
      hc_subtitle(text = 'Source: FB Ad Manager') %>%
      hc_chart(type = "column") %>% 
      hc_xAxis(categories = dims) %>%
      hc_add_series(data = dt_mini$metric, name = yaxis) %>%
      hc_legend(align = "left", verticalAlign = "top",
                layout = "horizontal", x = 0, y = 10) 
  }
  hc
}

plotSurvey <- function(s,dim,split=FALSE,type='total'){
  s$ch_weight <- s$count*s$catalistmodel_vch_index
  s$prop_weight <- s$count*s$catalistmodel_voteprop2018
  s <- s[,c(dim,'Success','Counter','catalistmodel_vch_index','catalistmodel_voteprop2018','count','ch_weight','prop_weight'),with=FALSE][,lapply(.SD,sum),by=c(dim)]
  s$catalistmodel_voteprop2018 <- s$prop_weight/s$count
  s$catalistmodel_vch_index <- s$ch_weight/s$count
  s$`Success Rate` <- 100*s$Success/s$Counter
  s$change_pp <- s$`Success Rate` - s$`catalistmodel_vch_index`
  s$change_pc <- 100*(s$`Success Rate`-s$`catalistmodel_vch_index`)/s$`catalistmodel_vch_index`
  metric = s$`Success Rate`
  min = 0
  if(type == 'delta1'){
    metric = s$change_pp
    min = -100
  }
  else if(type == 'delta2'){
    metric = s$change_pc
    min = -100
  }
  hc <- highchart() %>% 
    #hc_subtitle(text = '.....') %>%
    hc_legend(enabled = FALSE) %>%
    hc_chart(type = 'bar',height = '400') %>% 
    hc_xAxis(categories = s$`Matrix Position`) %>%
    hc_yAxis(labels = list(format = '{value}%'),min=min,max=100) %>%
    hc_add_series(name = '% Responding Democrat', data = metric)
  hc
}

filterDropdowns <- function(dt,dates){
  dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
}

#-------------------------------#
#---------Scatter Plot----------#
#-------------------------------#
summary_table_cluster_nosurvey <- summary_table_cluster[!grepl('Survey',summary_table_cluster$Campaign),]
testplot <- select(summary_table_cluster_nosurvey, Cluster,Engagements,Impressions,Spend)
testplot1 <- aggregate(cbind(Impressions, Engagements,Spend)~Cluster,data=summary_table_cluster_nosurvey, sum, na.rm = TRUE)
testplot1$`Engagement Rate` <- round(testplot1$Engagements/testplot1$Impressions,4)
testplot1$CPM <- 1000*testplot1$Spend/testplot1$Impressions
testplot1 <- arrange(testplot1, desc(`Engagement Rate`))
testplot2 <- merge(clusters, testplot1, by = "Cluster")
testplot2$x <- round(testplot2$catalistmodel_vch_index, 2)
testplot2$y <- round(testplot2$catalistmodel_voteprop2018, 2)
colorlist <-c("#d3e1d5","#9ebfa4","#638769","#517657","#466b4c","#315437","#1b331f","#0f2b14","#061a09","#000000")
plotScatter <- function(dt){
  testplot2$z <- round(testplot2$`Engagement Rate`, 4)
  testplot2 <- arrange(testplot2, desc(z))
  plotlist <- split(testplot2, rep(1:10, length.out = nrow(testplot2), each = ceiling(nrow(testplot2)/10)))
  list2env(plotlist ,.GlobalEnv)
  hc <- highchart(height = "100%") %>%
    hc_chart(type = "bubble",
      zoomType = "xy") %>%
    hc_tooltip(useHTML = TRUE,
      formatter = JS("function() { 
        return '<small>' +
        'Cluster - ' + this.point.global_clust_number + 
        '</small><table>' +
        '<tr><td>Engagement Rate: </td>' +
        '<td style = \"text-align: right\"><b>' +
        ((this.point.z)*100).toFixed(2) + '%</b></td></tr>' +
        '<tr><td>Vote Likelihood: </td>' +
        '<td style = \"text-align: right\"><b>' +
        this.y + '%</b></td></tr>' +
        '<tr><td>Expected Partisanship: </td>' +
        '<td style = \"text-align: right\"><b>' +
        this.x + '%</b></td></tr>' +
        '</table>'}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_title(text = "Engagement Rate by Cluster Expected Partisanship vs. Vote Likelihood",
      style = list(fontSize = '15px')) %>%
    hc_xAxis(gridLineWidth = 1,
             title = list(text = "Expected Partisanship"),
             min = 0,
             max = 100,
             events = list(
               afterSetExtremes = JS("function(event) {
                 console.log(event);
                 if(typeof event.userMin == 'undefined' && typeof event.userMax == 'undefined'){
                 Shiny.onInputChange('hc_reset_zoom', 1);   
                 } else {
                 Shiny.onInputChange('hc_reset_zoom', 0);
                 Shiny.onInputChange('hc_xmax', event.userMax);
                 Shiny.onInputChange('hc_xmin', event.userMin);}}")
               ),
             plotLines = list(
               list(
                 color = "black",
                 dashStyle = 'dot',
                 width = 2,
                 value = 50,
                 zIndex = 3
               ))) %>%
    hc_yAxis(title = list(text = "Vote Likelihood"),
             min = 0,
             max = 100,
             events = list(
               afterSetExtremes = JS("function(event) {
                                     Shiny.onInputChange('hc_ymax', event.userMax);
                                     Shiny.onInputChange('hc_ymin', event.userMin);}")
               ),
             plotLines = list(
               list(
                 color = "black",
                 dashStyle = "dot",
                 width = 2,
                 value = 50,
                 zIndex = 3
               )
             )) %>%
    
    hc_add_series(data = `1`, color = colorlist[10]) %>%
    hc_add_series(data = `2`, color = colorlist[9]) %>%
    hc_add_series(data = `3`, color = colorlist[8]) %>%
    hc_add_series(data = `4`, color = colorlist[7]) %>%
    hc_add_series(data = `5`, color = colorlist[6]) %>%
    hc_add_series(data = `6`, color = colorlist[5]) %>%
    hc_add_series(data = `7`, color = colorlist[4]) %>%
    hc_add_series(data = `8`, color = colorlist[3]) %>%
    hc_add_series(data = `9`, color = colorlist[2]) %>%
    hc_add_series(data = `10`, color = colorlist[1]) %>%
    
    hc_plotOptions(series = list(events = list(click = JS("function(event) {Shiny.onInputChange('hc_cluster_number', event.point.global_clust_number);}")),
                                 minSize = '1%', maxSize = '5%'))
  hc
}

plotScatterSurvey <- function(s,choice){
  s <- s[s$Treated!='Control',]
  if(choice=='treated'){s <- s[s$Treated=='Treated Clusters',]}
  else if(choice=='non-treated'){s <- s[s$Treated=='Non-Treated Clusters',]}
  s$ch_weight <- s$count*s$catalistmodel_vch_index
  s$prop_weight <- s$count*s$catalistmodel_voteprop2018
  s <- s[,c('Origin','Success','Counter','catalistmodel_vch_index','catalistmodel_voteprop2018','count','ch_weight','prop_weight'),with=FALSE][,lapply(.SD,sum),by=c('Origin')]
  s$catalistmodel_voteprop2018 <- s$prop_weight/s$count
  s$catalistmodel_vch_index <- s$ch_weight/s$count
  s$`Success Rate` <- 100*s$Success/s$Counter
  s$change_pp <- s$`Success Rate` - s$`catalistmodel_vch_index`
  s$change_pc <- 100*(s$`Success Rate`-s$`catalistmodel_vch_index`)/s$`catalistmodel_vch_index`
  s$x <- round(s$catalistmodel_vch_index, 2)
  s$y <- round(s$catalistmodel_voteprop2018, 2)
  s$z <- round(sqrt(s$count),4)
  s <- arrange(s, desc(change_pp))
  plotlist <- split(s, rep(1:10, length.out = nrow(s), each = ceiling(nrow(s)/10)))
  colorlist <-c('#fdff9a','#f8e78a','#f1cd7c','#ebb36c','#e5985c','#de7d4d','#d8613b','#b60000','#910000','6b0000')
  list2env(plotlist ,.GlobalEnv)
  hc <- highchart(height = '100%') %>%
    hc_chart(type = 'bubble',
             zoomType = "xy") %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS("function() { 
                              return '<small>' +
                              'Cluster - ' + this.point.Origin + 
                              '</small><table>' +
                              '<tr><td>Sentiment Shift: </td>' +
                              '<td style = \"text-align: right\"><b>' +
                              (this.point.change_pp).toFixed(2) + ' Pct Pts</b></td></tr>' +
                              '<tr><td>Vote Likelihood: </td>' +
                              '<td style = \"text-align: right\"><b>' +
                              this.y + '%</b></td></tr>' +
                              '<tr><td>Expected Partisanship: </td>' +
                              '<td style = \"text-align: right\"><b>' +
                              this.x + '%</b></td></tr>' +
                              '</table>'}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(gridLineWidth = 1,
             title = list(text = "Expected Partisanship"),
             min = 0,
             max = 100,
             events = list(
               afterSetExtremes = JS("function(event) {
                                     console.log(event);
                                     if(typeof event.userMin == 'undefined' && typeof event.userMax == 'undefined'){
                                     Shiny.onInputChange('hc_reset_zoom', 1);   
                                     } else {
                                     Shiny.onInputChange('hc_reset_zoom', 0);
                                     Shiny.onInputChange('hc_xmax', event.userMax);
                                     Shiny.onInputChange('hc_xmin', event.userMin);}}")
               ),
             plotLines = list(
               list(
                 color = "black",
                 dashStyle = 'dot',
                 width = 2,
                 value = 50,
                 zIndex = 3
               ))) %>%
    hc_yAxis(title = list(text = "Vote Likelihood"),
             min = 0,
             max = 100,
             events = list(
               afterSetExtremes = JS("function(event) {
                                     Shiny.onInputChange('hc_ymax', event.userMax);
                                     Shiny.onInputChange('hc_ymin', event.userMin);}")
               ),
             plotLines = list(
               list(
                 color = "black",
                 dashStyle = "dot",
                 width = 2,
                 value = 50,
                 zIndex = 3
               )
             )) %>%
    
    hc_add_series(data = `1`, color = colorlist[10]) %>%
    hc_add_series(data = `2`, color = colorlist[9]) %>%
    hc_add_series(data = `3`, color = colorlist[8]) %>%
    hc_add_series(data = `4`, color = colorlist[7]) %>%
    hc_add_series(data = `5`, color = colorlist[6]) %>%
    hc_add_series(data = `6`, color = colorlist[5]) %>%
    hc_add_series(data = `7`, color = colorlist[4]) %>%
    hc_add_series(data = `8`, color = colorlist[3]) %>%
    hc_add_series(data = `9`, color = colorlist[2]) %>%
    hc_add_series(data = `10`, color = colorlist[1]) %>%
    
    hc_plotOptions(series = list(events = list(click = JS("function(event) {Shiny.onInputChange('hc_cluster_number', event.point.global_clust_number);}")),
                                 minSize = '2%', maxSize = '10%'),color = 'transparent')
  hc
  }



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  TABLES
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
makeSummaryTable <- function(dt,fixed,dims,metrics,dates,channel,interest,theme,checks,username,quad){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    s <- s[,Date:=NULL]
    #calculating accurate daily reach from FB for any date range (segmentable only by coalition - if we go more granular the aggregates will be N/A)
    s$Reach <- '--'
    first_reach_day <- min(reach_day[reach_day$End>=dates[1],]$End)
    last_reach_day <- max(reach_day[reach_day$Start<=dates[2],]$Start)
    first_reach_day_lcv <- min(reach_day[reach_day$Campaign=='LCV' & reach_day$End>=dates[1],]$End)
    last_reach_day_lcv <- max(reach_day[reach_day$Campaign=='LCV' & reach_day$Start<=dates[2],]$Start)
    first_reach_day_pp <- min(reach_day[reach_day$Campaign=='Planned Parenthood' & reach_day$End>=dates[1],]$End)
    last_reach_day_pp  <- max(reach_day[reach_day$Campaign=='Planned Parenthood' & reach_day$Start<=dates[2],]$Start)
    first_reach_day_pm <- min(reach_day[reach_day$Campaign=='Progress Michigan' & reach_day$End>=dates[1],]$End)
    last_reach_day_pm <- max(reach_day[reach_day$Campaign=='Progress Michigan' & reach_day$Start<=dates[2],]$Start)
    totalReach <- max(reach_day[reach_day$Start>=first_reach_day & reach_day$End<=last_reach_day,]$Reach,0)
    lcvReach <- max(reach_day[reach_day$Campaign=='LCV' & reach_day$Start>=first_reach_day_lcv & reach_day$End<=last_reach_day_lcv,]$Reach,0)
    ppReach <- max(reach_day[reach_day$Campaign=='Planned Parenthood' & reach_day$Start>=first_reach_day_pp & reach_day$End<=last_reach_day_pp,]$Reach,0)
    pmReach <- max(reach_day[reach_day$Campaign=='Progress Michigan' & reach_day$Start>=first_reach_day_pm & reach_day$End<=last_reach_day_pm,]$Reach,0)
    if(username_map(username) == 'all_groups'){
      if(interest !='all_groups'){s <- s[s$Coalition==interest,]}}
      else{s <- s[s$Coalition==username_map(username),]
    }
    if(channel !='all_channels'){s <- s[s$Channel==channel,]}
    if(quad != 'all_quadrants'){s <- s[s$`Matrix Position`==quad,]}
    s$`Matrix Position` <- NULL
    dims <- c('Coalition','Channel')
    metrics <- c('Spend','Impressions','Reach','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions')
    newdf <- expand.grid(interest_groups,channels)
    newdf[,c('Spend','Reach','Impressions','Engagements','True Engagements','Comments','Shares','Video Views','Video Completions','Website Clicks','CumSpend','CumImpressions')] <- 0
    setnames(newdf, old=colnames(newdf), new=colnames(s))
    s <- rbind(s,newdf)
    summary_table_1a <- s[(s$Coalition=='LCV') & (s$Channel=='Facebook'),][,lapply(.SD,sum),by=dims]
    summary_table_1b <- s[(s$Coalition=='LCV') & (s$Channel=='Twitter'),][,lapply(.SD,sum),by=dims]
    summary_table_1c <- s[(s$Coalition=='LCV') & (s$Channel=='Google'),][,lapply(.SD,sum),by=dims]
    summary_table_1s <- s[s$Coalition=='LCV',][,Channel:=NULL][,lapply(.SD,sum),by=c('Coalition')]
    summary_table_1s$Channel <- 'Subtotal'
    summary_table_2a <- s[(s$Coalition=='Planned Parenthood') & (s$Channel=='Facebook'),][,lapply(.SD,sum),by=dims]
    summary_table_2b <- s[(s$Coalition=='Planned Parenthood') & (s$Channel=='Twitter'),][,lapply(.SD,sum),by=dims]
    summary_table_2c <- s[(s$Coalition=='Planned Parenthood') & (s$Channel=='Google'),][,lapply(.SD,sum),by=dims]
    summary_table_2s <- s[s$Coalition=='Planned Parenthood',][,Channel:=NULL][,lapply(.SD,sum),by=c('Coalition')]
    summary_table_2s$Channel <- 'Subtotal'
    summary_table_3a <- s[(s$Coalition=='Progress Michigan') & (s$Channel=='Facebook'),][,lapply(.SD,sum),by=dims]
    summary_table_3b <- s[(s$Coalition=='Progress Michigan') & (s$Channel=='Twitter'),][,lapply(.SD,sum),by=dims]
    summary_table_3c <- s[(s$Coalition=='Progress Michigan') & (s$Channel=='Google'),][,lapply(.SD,sum),by=dims]
    summary_table_3s <- s[s$Coalition=='Progress Michigan',][,Channel:=NULL][,lapply(.SD,sum),by=c('Coalition')]
    summary_table_3s$Channel <- 'Subtotal'
    s$const <- '1'
    summary_table_t <- s[,Channel:=NULL]
    summary_table_t <- s[,Coalition:=NULL]
    s <- s[,lapply(.SD,sum),by=const]
    summary_table_t <- s[,const:=NULL]
    summary_table_t$Channel <- ''
    summary_table_t$Coalition <- 'TOTAL'
    summary_table_t$Reach <- format(ifelse((grepl('LCV',interest ) || username_map(username)== 'LCV'),lcvReach,ifelse(interest=='Planned Parenthood' || username_map(username)== 'Planned Parenthood',ppReach,ifelse(interest=='Progress Michigan'|| username_map(username)== 'Progress Michigan',pmReach,totalReach))), big.mark=",")
    summary_table_1a$Reach <- format(ifelse((grepl('LCV|all',interest) && (s$Spend > 0)),lcvReach,0), big.mark=",")
    summary_table_1s$Reach <- summary_table_1a$Reach
    summary_table_2a$Reach <- format(ifelse((grepl('Planned|all',interest) && (s$Spend > 0)),ppReach,0), big.mark=",")
    summary_table_2s$Reach <- summary_table_2a$Reach
    summary_table_3a$Reach <- format(ifelse((grepl('Progress|all',interest) && (s$Spend > 0)),pmReach,0), big.mark=",")
    summary_table_3s$Reach <- summary_table_3a$Reach
    
    s <- rbind(summary_table_1a,summary_table_1b,summary_table_1c,summary_table_1s,summary_table_2a,summary_table_2b,summary_table_2c,summary_table_2s,summary_table_3a,summary_table_3b,summary_table_3c,summary_table_3s,summary_table_t)
    if (interest == 'LCV' || username_map(username) == 'LCV'){
      s <- rbind(summary_table_1a,summary_table_1b,summary_table_1c,summary_table_t)
      if(checks[1] == FALSE || username_map(username) == 'LCV'){s$Coalition <- 'HIDE'}
    }
    else if (interest == 'Planned Parenthood' || username_map(username) == 'Planned Parenthood'){
      s <- rbind(summary_table_2a,summary_table_2b,summary_table_2c,summary_table_t)
      if(checks[1] == FALSE || username_map(username) == 'Planned Parenthood'){s$Coalition <- 'HIDE'}
    }
    else if (interest == 'Progress Michigan' || username_map(username) == 'Progress Michigan'){
      s <- rbind(summary_table_3a,summary_table_3b,summary_table_3c,summary_table_t)
      if(checks[1] == FALSE || username_map(username) == 'Progress Michigan'){s$Coalition <- 'HIDE'}
    }
    s <- s[,c(dims,metrics),with=FALSE]
    s <- s[,Engagements:=NULL]
    colnames(s)[6] <- 'Engagements'
    s$`Eng Rate` <- ifelse(s$Impressions<=0,0,s$Engagements/s$Impressions)
    s$`Eng Rate` <- percent_format()(s$`Eng Rate`)
    s$`Comment %` <- ifelse(s$Impressions<=0,0,percent_format()(s$Comments/s$Impressions))
    s$`Share %` <- ifelse(s$Impressions<=0,0,percent_format()(s$Shares/s$Impressions))
    s$`V Comp %` <- ifelse(s$Impressions<=0,0,percent_format()(s$`Video Completions`/s$Impressions))
    s$CPM <- ifelse(s$Impressions<=0,'--',paste0('$',format(round(1000*s$`Spend`/s$Impressions,2), nsmall = 2, big.mark=',')))
    s$CPS <- ifelse(s$Shares<=0,'--',paste0('$',format(round(s$`Spend`/s$Shares,2), nsmall = 2, big.mark=',')))
    s$Spend <- paste0('$',format(round(s$Spend,2), nsmall = 2, big.mark=','))
    s$Impressions <- format(s$Impressions, big.mark=',')
    s$Engagements <- format(s$Engagements, big.mark=',')
    if(grepl('Twitter|Google',channel) ){s$Reach <- '--'}
    if(quad != 'all_quadrants')
    {
      s$Reach <- '--'
    }
    s <- s[,c(dims,c('Spend','Impressions','Reach','Engagements','Eng Rate','V Comp %','Share %','CPS','CPM')),with=FALSE]
    s
  },options=list(pageLength=15),escape=FALSE)  
}

makeBillingTable <- function(s,s2,dates){
    s2[is.na(s2)] <- 0
    s2 <- s2[,c('Flight Index','Amount Invoiced','Survey Budget','Onboarding Cost'),with=FALSE][,lapply(.SD,sum),by=c('Flight Index')]
    s3 <- merge(s,s2,by='Flight Index',all.x=TRUE)
    s3$`Flight Period` <- paste0(as.Date(s3$`Planned End`,'%Y-%m-%d')-as.Date(s3$`Planned Start`,'%Y-%m-%d'),' days')
    
    s4 <- summary_table_cluster[,c('Billing Index','Date'),with=FALSE][,lapply(.SD,min),by=c('Billing Index')]
    s5 <- summary_table_cluster[,c('Billing Index','Date'),with=FALSE][,lapply(.SD,max),by=c('Billing Index')]
    
    s3 <- merge(s3,s4,by.x='Flight Index',by.y='Billing Index')
    s3 <- merge(s3,s5,by.x='Flight Index',by.y='Billing Index')
    names(s3)[-1:-(len(names(s3))-2)] <- c('Effective Start','Effective End')
    
    s3 <- cbind(s3[,1:5],s3[,9:11],s3[,6:8])
    s3$`Working Budget` <- s3$`Amount Invoiced` - s3$`Survey Budget` - s3$`Onboarding Cost`
    
    s6 <- summary_table_cluster[!grepl('Survey',summary_table_cluster$Campaign),c('Spend','Channel','Billing Index'),with=FALSE][,lapply(.SD,sum),by=c('Channel','Billing Index')]
    s6a <- s6[,c('Spend','Billing Index'),with=FALSE][,lapply(.SD,sum),by=c('Billing Index')]
    s6b <- s6[s6$Channel=='Facebook',c('Spend','Billing Index'),with=FALSE][,lapply(.SD,sum),by=c('Billing Index')]
    s6c <- s6[s6$Channel=='Twitter',c('Spend','Billing Index'),with=FALSE][,lapply(.SD,sum),by=c('Billing Index')]
    s6d <- s6[s6$Channel=='Google',c('Spend','Billing Index'),with=FALSE][,lapply(.SD,sum),by=c('Billing Index')]
    
    s3 <- merge(s3,s6a,by.x='Flight Index',by.y='Billing Index',all.x=TRUE)
    s3 <- merge(s3,s6b,by.x='Flight Index',by.y='Billing Index',all.x=TRUE)
    s3 <- merge(s3,s6c,by.x='Flight Index',by.y='Billing Index',all.x=TRUE)
    s3 <- merge(s3,s6d,by.x='Flight Index',by.y='Billing Index',all.x=TRUE)
    names(s3)[-1:-(len(names(s3))-4)] <- c('Spend','Spend (Facebook)','Spend (Twitter)','Spend (Google)')

    s3[,c('Spend','Spend (Facebook)','Spend (Twitter)','Spend (Google)')][is.na(s3[,c('Spend','Spend (Facebook)','Spend (Twitter)','Spend (Google)')])] <- 0
    #reactions_table1[,c('Like','Love','Haha','Wow','Sad','Angry')][is.na(reactions_table1[,c('Like','Love','Haha','Wow','Sad','Angry')])]
    
    s3$`Budget Remaining` <- s3$`Working Budget` - s3$Spend
    
    paste0('$',format(round(s$`Spend`/s$Shares,2), nsmall = 2, big.mark=','))
    
    s3$`Amount Invoiced` <- paste0('$',format(round(abs(s3$`Amount Invoiced`)),2,nsmall = 2,big.mark=','))
    s3$`Survey Budget` <- paste0('$',format(round(abs(s3$`Survey Budget`)),2,nsmall = 2,big.mark=','))
    s3$`Onboarding Cost` <- paste0('$',format(round(abs(s3$`Onboarding Cost`)),2,nsmall = 2,big.mark=','))
    s3$`Working Budget` <- paste0('$',format(round(abs(s3$`Working Budget`)),2,nsmall = 2,big.mark=','))
    s3$`Spend` <- paste0('$',format(round(abs(s3$`Spend`)),2,nsmall = 2,big.mark=','))
    s3$`Spend (Facebook)` <- paste0('$',format(round(abs(s3$`Spend (Facebook)`)),2,nsmall = 2,big.mark=','))
    s3$`Spend (Twitter)` <- paste0('$',format(round(abs(s3$`Spend (Twitter)`)),2,nsmall = 2,big.mark=','))
    s3$`Spend (Google)` <- paste0('$',format(round(abs(s3$`Spend (Google)`)),2,nsmall = 2,big.mark=','))
    s3$`Budget Remaining` <- paste0(ifelse(s3$`Budget Remaining`<0,'(',''),'$',format(round(abs(s3$`Budget Remaining`)),2,nsmall = 2,big.mark=','),ifelse(s3$`Budget Remaining`<0,')',''))
    s3[(s3$`Planned End` >= dates[1]) & (s3$`Planned Start` <= dates[2]),-1]
}

makeInvoicesTable <- function(s,dates){
  DT::datatable({
  s$`Amount Invoiced` <- paste0(ifelse(s$`Amount Invoiced`<0,'(',''),'$',format(round(abs(s$`Amount Invoiced`)),2,nsmall = 2,big.mark=','),ifelse(s$`Amount Invoiced`<0,')',''))
  s[(s$`Invoice Date` >= dates[1]) & (s$`Invoice Date` <= dates[2]),2:(ncol(s)-2)]
  },options=list(pageLength=20),escape=FALSE)  
}

makeCreativeTableSmall <- function(dt,fixed,dims,metrics,dates,channel,interest,quad,theme,msg,checks,total,username){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),][order(dt$Coalition)]
    if(username_map(username) == 'all_groups'){if(interest !='all_groups'){s <- s[s$Coalition==interest,]}}
    else{s <- s[s$Coalition==username_map(username),]}
    if(interest !='all_groups'){s <- s[s$Coalition==interest,]}
    if(quad != 'all_quadrants'){s <- s[s$`Matrix Position`==quad,]}
    if(msg !='all_messages'){s <- s[s$Message==msg,]}
    s <- s[s$Channel=='Facebook',]
    if(checks[2] == FALSE){s$Channel <- 'HIDE'}
    if(checks[3] == FALSE  || username_map(username) != 'all_groups'){s$Coalition <- 'HIDE'}
    if(total == TRUE){
      s[,c('Channel','Coalition','Tone','Format','Message')] <- ''
      if(checks[2] == TRUE){s$Channel='TOTAL'}
      else if(checks[3] == TRUE && username_map(username) == 'all_groups'){s$Coalition ='TOTAL'}
      else{s[,c('Tone','Format','Message')]='TOTAL'}
    }
    s <- s[,c(dims,metrics),with=FALSE][,lapply(.SD,sum),by=dims]
    s$`Eng Rate` <- ifelse(s$Impressions<=0,0,s$Engagements/s$Impressions)
    s$CPM <- 1000*s$`Spend`/s$Impressions
    s$Spend2 <- s$Spend
    s$Impressions2 <- s$Impressions
    s$Engagements2 <- s$Engagements
    s$`Eng Rate2` <- s$`Eng Rate`
    s$CPM2 <- s$CPM
    s$`Eng Rate2` <- percent_format()(s$`Eng Rate2`)
    s$CPM2 <- paste0('$',format(round(s$CPM2,2), big.mark=',')) #dollar_format()(s$CPM)
    if(checks[1]==TRUE){
      s$Spend2 <- percent_format()(s$Spend2/sum(s$Spend2))
      s$Impressions2 <- percent_format()(s$Impressions2/sum(s$Impressions2))
      s$Engagements2 <- percent_format()(s$Engagements2/sum(s$Engagements2))
    }
    else{
      s$Spend2 <- paste0('$',format(round(s$Spend2,2), nsmall=2, big.mark=','))
      s$Impressions2 <- format(s$Impressions2, big.mark=',') #dollar_format()(s$`Spend`)
      s$Engagements2 <- format(s$Engagements2, big.mark=',')
    }
    s
  },options=list(pageLength=25),escape=FALSE)  
}

makeCreativeTable <- function(dt,fixed,dims,metrics,dates,channel,interest,theme,checks,username){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if(username_map(username) == 'all_groups'){
      if(interest !='all_groups'){s <- s[s$Coalition==interest,]}
    }
    else{s <- s[s$Coalition==username_map(username),]}
    if(channel !='all_channels'){s <- s[s$Channel==channel,]}
    s <- s[,c(dims,metrics),with=FALSE][,lapply(.SD,sum),by=dims]
    s$`Eng Rate` <- ifelse(s$Impressions<=0,0,s$Engagements/s$Impressions)
    s$`Eng Rate` <- percent_format()(s$`Eng Rate`)
    s$CPM <- 1000*s$`Spend`/s$Impressions
    s$CPM <- paste0('$',format(round(s$CPM,2), big.mark=',')) #dollar_format()(s$CPM)
    s$Spend <- paste0('$',format(s$Spend, big.mark=','))
    s$Impressions <- format(s$Impressions, big.mark=',')
    s$Engagements <- format(s$Engagements, big.mark=',')
    s
  },options=list(pageLength=25),escape=FALSE)  
}

makeCreativeImageTable <- function(s,dates,interest,channel,username,theme,tone,quadrant,segment,split,metric){ #fixed,dims,metrics,dates,channel,interest,theme,checks,username){
  DT::datatable({
    if(metric == 'Engagement Rate'){metric <- c(metric,'Impressions','Engagements',1,'Percent')}
    if(metric == 'Spend'){metric <- c(metric,1,'Dollar')}
    if(metric == 'Impressions'){metric <- c(metric,1,'Integer')}
    if(metric == 'CPM'){metric <- c(metric,'Impressions','Spend',1000,'Dollar')}
    if(metric == 'Cost / Engagement'){metric <- c(metric,'Engagements','Spend',1,'Dollar')}
    if(metric == 'Cost / Share'){metric <- c(metric,'Shares','Spend',1,'Dollar')}
    
    if(channel !='all_channels'){s <- s[s$Channel==channel,]}
    if(theme !='all_themes'){s <- s[s$Theme==theme,]}
    if(tone !='all_tones'){s <- s[s$Tone==tone,]}
    if(quadrant !='all_quadrants'){s <- s[s$`Matrix Position`==quadrant,]}
    if(segment !='all_segments'){s <- s[s$`Psychographic Segment`==segment,]}
    s$`Ad Name` <- gsub('_V2| - Copy| - v2|Ad name: |_logo|\\.jpg; .*','',s$`Ad Name`)
    s$`Ad Name` <- gsub('""','"',s$`Ad Name`)
    
    s2 <- s[,c('Date','Ad Name'),with=FALSE][,lapply(.SD,min),by=c('Ad Name')]
    s3 <- s[,c('Date','Ad Name'),with=FALSE][,lapply(.SD,max),by=c('Ad Name')]
    mDate <- max(s$Date)
    
    s <- s[(s$Date >= dates[1]) & (s$Date <= dates[2]),c('Coalition','Channel','Ad Name','Theme','Tone','Format','AgeLvl','Gender','Ethnicity','Bi_Coastal','Bi_Protestant','Bi_Urban','IncomeLvl','Bi_Married','Segment','Cluster','Spend','Impressions','Reach','True Engagements','Video Views','Video Completions','Shares'),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Ad Name','Theme','Tone','Format','AgeLvl','Gender','Ethnicity','Bi_Coastal','Bi_Protestant','Bi_Urban','IncomeLvl','Bi_Married','Segment','Cluster')]
    if(username_map(username) == 'all_groups'){
      if(interest !='all_groups'){s <- s[s$Coalition==interest,]}
    }
    else{s <- s[s$Coalition==username_map(username),]}
    
    s$Creative <- mapply(function(x,y) ifelse(grepl('Facebook',y),paste0('<img src="creative/',tolower(x),'.png" height="45" width="58"></img>'),ifelse(grepl('http',x),paste0(unlist(strsplit(x, 'http'))[1],' <a href="http',unlist(strsplit(x, 'http'))[2],'">http',unlist(strsplit(x, 'http'))[2],'</a>'),x)),s$`Ad Name`,s$Channel)
    
    s <- merge(s,s2,by='Ad Name',all.x=TRUE)
    s <- merge(s,s3,by='Ad Name',all.x=TRUE)
    names(s)[-1:-(len(names(s))-2)] <- c('Start','End')
    
    s$End <- unlist(lapply(s$End, function(x) ifelse(x==as.Date(mDate),'Ongoing',as.character(as.Date(x,'%Y-%m-%d')))))
    setnames(s, old=c('AgeLvl','Gender','Ethnicity','Bi_Coastal','Bi_Protestant','Bi_Urban','IncomeLvl','Bi_Married','True Engagements'), new=c('Age','Gender','Ethnicity','Coastal','Protestant','Urban','Income','Married','Engagements'))
    
    if(metric != 'default_view'){
        s_new <- s[,c('Coalition','Channel','Creative','Theme','Tone','Format','Start','End','Impressions'),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format','Start','End')]
        s_new <- s_new[,c('Coalition','Channel','Creative','Theme','Tone','Format','Start','End')]
        if(split != 'None'){
          if(length(metric) == 3){
            s_new2 <- s[!is.na(split),c('Coalition','Channel','Creative','Theme','Tone','Format',split,metric[1]),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format',split)]
            cols1 <- ncol(s_new2)
            s_new2 <- setDT(dcast(s_new2, Coalition + Channel + Creative + Theme + Tone + Format ~ ..., value.var = metric[1], fun.aggregate = sum))
            cols2 <- ncol(s_new2) - (cols1-1)
            col_names <- colnames(s_new2)
            if(metric[3]=='Dollar'){
              s_new2[,7:=unlist(lapply(Young,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))]
              s_new2[,8:=unlist(lapply(`Middle Aged`,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))]
              s_new2[,9:=unlist(lapply(Older,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))]
              s_new2[,10:=unlist(lapply(`NA`,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))]
            }
            else if(metric[3]=='Percent'){
              s_new2$A <- unlist(lapply(s_new2$A,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
              s_new2$B <- unlist(lapply(s_new2$B,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
              s_new2$C <- unlist(lapply(s_new2$C,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
              s_new2$D <- unlist(lapply(s_new2$D,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
            }
            s_new2 <- cbind(s_new2[,1:6],s_new2[,7:ncol(s_new2)])
            setnames(s_new2,old=colnames(s_new2)[(length(colnames(s_new2))-cols2):length(colnames(s_new2))],new=col_names[(length(col_names)-cols2):length(col_names)])
          }
          else{ #handler for calculated metrics
            s_new2 <- s[!is.na(split),c('Coalition','Channel','Creative','Theme','Tone','Format',split,metric[2]),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format',split)]
            cols1 <- ncol(s_new2)
            s_new2 <- dcast(s_new2, Coalition + Channel + Creative + Theme + Tone + Format ~ ..., value.var = metric[2], fun.aggregate = sum)
            cols2 <- ncol(s_new2) - (cols1-1)
            col_names <- colnames(s_new2)
            s_new3 <- s[!is.na(split),c('Coalition','Channel','Creative','Theme','Tone','Format',split,metric[3]),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format',split)]
            s_new3 <- dcast(s_new3, Coalition + Channel + Creative + Theme + Tone + Format ~ ..., value.var = metric[3], fun.aggregate = sum)
            s_new2 <- setDT(merge(s_new2,s_new3,by=c('Coalition','Channel','Creative','Theme','Tone','Format'),all.y=TRUE))
            s_new2$A <- as.numeric(metric[4])*s_new2[,11]/s_new2[,7]
            s_new2$B <- as.numeric(metric[4])*s_new2[,12]/s_new2[,8]
            s_new2$C <- as.numeric(metric[4])*s_new2[,13]/s_new2[,9]
            s_new2$D <- as.numeric(metric[4])*s_new2[,14]/s_new2[,10]
            if(metric[5]=='Dollar'){
              s_new2$A <- unlist(lapply(s_new2$A,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))
              s_new2$B <- unlist(lapply(s_new2$B,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))})) #ifelse(grepl('N',percent_format()(s_new2$B)),'-',paste0('$',format(round(s_new2$B,2), nsmall = 2, big.mark=',')))
              s_new2$C <- unlist(lapply(s_new2$C,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))
              s_new2[,18:=unlist(lapply(D,function(x) if(grepl('N',percent_format()(x))){'-'}else{paste0('$',format(round(x,2), nsmall = 2, big.mark=','))}))]
            }
            else if(metric[5]=='Percent'){
              s_new2$A <- unlist(lapply(s_new2$A,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
              s_new2$B <- unlist(lapply(s_new2$B,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
              s_new2$C <- unlist(lapply(s_new2$C,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
              s_new2$D <- unlist(lapply(s_new2$D,function(x) if(grepl('N',percent_format()(x))){'-'}else{percent_format()(x)}))
            }
            s_new2 <- cbind(s_new2[,1:6],s_new2[,15:ncol(s_new2)])
            setnames(s_new2,old=colnames(s_new2)[(length(colnames(s_new2))-cols2):length(colnames(s_new2))],new=col_names[(length(col_names)-cols2):length(col_names)])
          }
        }
        else{
          s_new2 <- s[!is.na(split),c('Coalition','Channel','Creative','Theme','Tone','Format',metric[1]),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format')]        
        }
        s <- merge(s_new,s_new2,by=c('Coalition','Channel','Creative','Theme','Tone','Format'),all.y=TRUE)
        #for(i in split)
        #s <- merge(cbind(s[,2:3],s[,13],s[,4:6],s[,14:15]),s[,c('Coalition','Channel','Creative','Theme','Tone','Format','AgeLvl'),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format','AgeLvl')],by=c('Coalition','Channel','Creative','Theme','Tone','Format'),all.x=TRUE)
    }
    else{
      s <- s[,c('Coalition','Channel','Creative','Theme','Tone','Format','Start','End','Impressions','Reach','Engagements','Video Views','Video Completions','Shares'),with=FALSE][,lapply(.SD,sum),by=c('Coalition','Channel','Creative','Theme','Tone','Format','Start','End')]
      #s <- s[,c(dims,metrics),with=FALSE][,lapply(.SD,sum),by=dims]
      #s$Thumbnail <- paste0('<img src="creative/',tolower(s$`Ad Name`),'.png" height="200" width="235"></img>')
      s$`Share Rate` <- percent_format()(ifelse(s$Impressions<=0,0,s$Shares/s$Impressions))
      s$`V Comp Rate` <- percent_format()(ifelse(s$`Video Views`<=0,0,s$`Video Completions`/s$`Video Views`))
      s$`Eng Rate` <- percent_format()(ifelse(s$Impressions<=0,0,s$`Engagements`/s$Impressions))
      #s$CPM <- 1000*s$`Spend`/s$Impressions
      #s$CPM <- paste0('$',format(round(s$CPM,2), big.mark=',')) #dollar_format()(s$CPM)
      #s$Spend <- paste0('$',format(s$Spend, big.mark=','))
      s$Impressions <- format(s$Impressions, big.mark=',')
      s$Reach <- format(s$Reach, big.mark=',')
      #s$Engagements <- format(s$Engagements, big.mark=',')
      s <- s[order(Channel)] #:11  cbind(s[,2:3],s[,13],s[,4:6],s[,14:15],s[,7:8],s[,16:18])
    }
    tryCatch({setnames(s, old=c('NA'), new=c('Broad/Control'))},error=function(cond){})
    s[,'NA':=NULL]
  },options=list(pageLength=1000),escape=FALSE)  
}

makeReactionTable <- function(dt,fixed,dims,metrics,dates,channel,interest,theme,tone,msg,checks,total,username){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if(username_map(username) == 'all_groups'){
      if(interest !='all_groups'){s <- s[s$Coalition==interest,]}
    }
    else{s <- s[s$Coalition==username_map(username),]}
    if(theme !='all_themes'){s <- s[s$Theme==theme,]}
    if(tone !='all_tones'){s <- s[s$Tone==tone,]}
    if(msg !='all_messages'){s <- s[s$Message==msg,]}
    if(checks[2] == FALSE || username_map(username) != 'all_groups'){s$Coalition <- 'HIDE'}
    if(checks[3] == FALSE){s$Theme <- 'HIDE'}   
    if(checks[4] == FALSE){s$Tone <- 'HIDE'}
    if(checks[5] == FALSE){s$Message <- 'HIDE'}
    if(total == TRUE){
      s$Channel='TOTAL'
      s[,c('Coalition','Tone','Theme','Message')] <- ''
    }
    s <- s[,c(dims,metrics),with=FALSE][,lapply(.SD,sum),by=dims]
    s$Total <- rowSums(s[,metrics,with=FALSE])
    if(checks[1]==TRUE){
      s$Like2 <- percent_format()(s$Like/s$Total)
      s$Love2 <- percent_format()(s$Love/s$Total)
      s$Haha2 <- percent_format()(s$Haha/s$Total)
      s$Wow2 <- percent_format()(s$Wow/s$Total)
      s$Sad2 <- percent_format()(s$Sad/s$Total)
      s$Angry2 <- percent_format()(s$Angry/s$Total)
    }
    else{
      s$Like2 <- format(s$Like, big.mark=',')
      s$Love2 <- format(s$Love, big.mark=',')
      s$Haha2 <- format(s$Haha, big.mark=',')
      s$Wow2 <- format(s$Wow, big.mark=',')
      s$Sad2 <- format(s$Sad, big.mark=',')
      s$Angry2 <- format(s$Angry, big.mark=',')
    }
    s$Total2 <- format(s$Total, big.mark=',')
    s
  },options=list(pageLength=50),escape=FALSE)  
}

makeNewsTable <- function(dt,dims,dates,cats,username){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if(username_map(username) != 'all_groups'){
      s <- rbind(s[grepl('Michigan ',s$Category),],s[grepl(username_map(username),s$Category),])
    }
    for (c in cats){if (c != 'all_categories'){s <- s[s$Category == c,]}}
    s <- s[order(-s$Date,s$Category)][,c(dims),with=FALSE]
    if('Positive Mentions' %in% colnames(s)){
      s$`Positive Sentiment` <- format(round(s$`Positive Mentions` / (s$`Positive Mentions` + s$`Negative Mentions`),2),nsmall=2)
      s$`Positive Sentiment (with Neutrals)` <- format(round((s$`Positive Mentions` + (s$`Neutral Mentions`/2)) / (s$`Total Mentions`),2),nsmall=2)
    }
    s 
  },escape = FALSE)  
}

makeSurveyTable <- function(dt,metrics){
  DT::datatable({
    s <- dt
    s2 <- s[,2:11]
    s2 <- s2[,(names(s2)) := lapply(.SD, function(x) percent_format()(x))]
    cbind(s[,1],s2) 
  },escape = FALSE)  
}

makeHighChartHashtags <- function(dt,dates,cats){
  s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
  if (cats != 'all_categories') {s <- s[s$Category == cats,]}
  s <- s[,c('Date', 'Category', 'HashTag','Mentions')][, j=list(Mentions = sum(Mentions)), by = c('HashTag')][order(s$Mentions, decreasing = TRUE),][1:20,]
  hc2 <- highchart() %>%
    hc_title(text = 'Trending Hashtags') %>% 
    hc_subtitle(text = 'Source: Sysomos') %>% 
    hc_chart(type = 'bar') %>% 
    hc_xAxis(categories = c(s$HashTag)) %>% 
    hc_add_series(name = 'Mentions', data = (s$Mentions))
  hc2
}

makeDisplay <- function(dt,username){
  DT::datatable({
    s <- dt
    if(username_map(username) != 'all_groups'){
      s <- rbind(s[s$Topic=='All Groups',],s[s$Topic==username_map(username),])
    }
    s[,Topic:=NULL]
  },selection = 'none', options=list(pageLength = 25), escape = FALSE)
}