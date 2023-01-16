ui <- fluidPage(
  titlePanel('Michigan: Powered by Alchemy'),
  useShinyjs(),
  extendShinyjs(text = jsCode),
  theme = 'style.css',
  tags$div(class='loading-overlay',
     tags$div(class='loading-overlay-alchemy',
        tags$img(src = 'alchemy.svg', class = 'alchemy-overlay-image'),
        tags$p(class='loading-title','Powering Your Experience....'),
        tags$div(class='loading-progress')
     )
  ),
  column(3,
    tags$div(class='filterbox',
      tags$div(class='filterbox filter-dates',dateRangeInput('dateRange', label = 'Select Date Range:',start = min_date, end = max_date, format = "MM dd, yyyy")), #max(summary_table1$Date)),
      tags$div(class='filterbox filter-dates filter-dates-news',dateRangeInput('dateRangeNews', label = 'Select Date Range:',start = min_date, end = max(max_date,max(news$Date)), format = "MM dd, yyyy")),
      tags$div(class='cbx cbx-sidebar coalition',checkboxInput('coalitionCheck','Show Column',TRUE)),
      tags$div(class='filterbox filter-interests',selectInput('interest','Coalition:',c('All Coalitions'='all_groups',interest_groups))),
      tags$div(class='cbx cbx-sidebar channel',checkboxInput('channelCheck','Show Column',TRUE)),
      tags$div(class='filterbox filter-channels',selectInput("channel", "Performance Channel:",c("All Channels"="all_channels",channels))),
      tags$div(class='cbx cbx-sidebar tone',checkboxInput('toneCheck','Show Column',TRUE)),
      tags$div(class='filterbox filter-tone',selectInput('tone','Tone:',c('All Tones' = 'all_tones', unique(reactions_table1$Tone)))),
      tags$div(class='cbx cbx-sidebar theme',checkboxInput('themeCheck','Show Column',TRUE)),
      tags$div(class='filterbox filter-theme',selectInput('theme', 'Theme:', c('All Themes' = 'all_themes', unique(reactions_table1$Theme)))),
      tags$div(class='cbx cbx-sidebar message',checkboxInput('messageCheck','Show Column',TRUE)),
      tags$div(class='filterbox filter-message',selectInput('message', 'Message:', c('All Messages' = 'all_messages', unique(reactions_table1$Message)))),
      #tags$div(class='cbx cbx-sidebar cluster',checkboxInput('clusterCheck','Show Column',FALSE)),
      tags$div(class='filterbox filter-quadrant', selectInput('quadrant', 'Matrix Position:',c("All Matrix Positions" = "all_quadrants", quadrant))),
      tags$div(class='filterbox filter-segment', selectInput('segment', 'Psychographic Segment:',c("All Segments" = "all_segments", unique(clusters$`Psychographic Segment`)))),
      tags$div(class='filterbox filter-topics',selectInput('topic', "Topic:",active_topics,selected='Education')),
      tags$div(class='filterbox filter-topics',selectInput('topicLCV', 'Topic:',active_topics_lcv,selected='Environment')),
      tags$div(class='filterbox filter-topics',selectInput('topicPP', 'Topic:',active_topics_pp,selected='Abortion & Birth Control')),
      tags$div(class='filterbox filter-topics',selectInput('topicPM', 'Topic:',active_topics_pm,selected='Education')),
      tags$div(class='filterbox filter-twitter',selectInput('categoryTwitter', 'Twitter Category:', c('All Categories' = 'all_categories', categories_social))),
      tags$div(class='filterbox filter-news',selectInput('categoryNews', 'News Category:', c('All Categories' = 'all_categories', categories_news))),
      tags$div(class='filterbox filter-news',selectInput('categoryNewsLCV','News Category:', c('All Categories' = 'all_categories', categories_news[!grepl('Progress|Planned',categories_news)]))),
      tags$div(class='filterbox filter-news',selectInput('categoryNewsPP', 'News Category:', c('All Categories' = 'all_categories', categories_news[!grepl('Progress|LCV',categories_news)]))),
      tags$div(class='filterbox filter-news',selectInput('categoryNewsPM', 'News Category:', c('All Categories' = 'all_categories', categories_news[!grepl('Planned|LCV',categories_news)]))),
      tags$div(class='filterbox filter-display',selectInput('categoryDisplay', 'Select Category', list(
        'All Categories' = 'all_categories',
        'Right Wing Influencer' = c('Influencer'),
        'Republican Content' = c('Pro-Schuette','Anti-Whitmer','Calley','Calley_Whitmer','Colbeck','Schuette'),
        'Democratic Content' = c('Gretchen','Shady','El-Sayed','Thanedar','Whitmer')
      ))))
  ),
  column(9,
     navbarPage("",position = "fixed-top",
        summary_view,
        creative_detail,
        creatives_list,
        post_reactions,
        #analysis_tab,
        survey_tab,
        sentiment_tab
        #social_listening
     )
  )
)