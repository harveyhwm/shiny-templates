ui <- fluidPage(
  useShinyjs(),
  #extendShinyjs(text = jsCode),
  theme = 'style.css',
  tags$head(
    tags$script(src='js.js'),
    tags$link(href='https://fonts.googleapis.com/css?family=Rubik',rel='stylesheet')
  ),
  
  #HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
  #HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
  #tags$script('$(function(){var time_now = new Date()
  #$("input#client_time").val(time_now.getTime())
  #$("input#client_time_zone_offset").val(time_now.getTimezoneOffset())});'),
  
  tags$img(class='left-logo',src='logo_alchemy_white.svg'),
  tags$div(id='gradient-block'),
  tags$div(id='right-block',
      tags$img(class='right-logo logo-1',src='logo_paction_text_white.png'),
      tags$img(class='right-logo logo-2',src='logo_paction_notext_white.png')
  ),
  tags$div(class='logout-div',
    tags$a(class='logout-button',href='__logout__','Logout')
  ),
  tags$div(id='transparent_mask'),
  tags$div(class='loading-overlay',
    tags$div(class='loading-overlay-alchemy',
      tags$img(src='logo_paction_notext_1.svg',class='paction-overlay-image'),
      tags$img(src='logo_paction_notext_2.svg',class='paction-overlay-image overlay-image2'),
      tags$img(src='logo_paction_notext_3.svg',class='paction-overlay-image overlay-image3'),
      #tags$img(src='logo_paction.svg',class='paction-overlay-text'),
      #tags$p(class='loading-title','Powering Your Experience....'),
      tags$div(class='loading-progress'),
      tags$div(class='loading-text',tags$h3(class='loading-text-left','Loading .'),tags$h3(class='loading-text-right','5%'))
    )
  ),
  column(3,
    tags$div(class='periodbuttons',
      tags$label(class='label-cycles','Cycle Type:'),
      tags$a(class='link-period link-quarter link-inactive','Quarter'),
      tags$a(class='link-period link-month link-inactive','Month'),
      tags$a(class='link-period link-week','Week')
    ),
    tags$div(class='filterset',
      tags$div(class='filterbox filter-split filter-group1 filter-hide',textInput('period','Period:',paste0('Week: ',max(weeks)))),
      tags$div(class='filterbox filter-split filter-group1 filter-week',selectInput('week','Week Start:',c('All Weeks',as.character(weeks)),selected = as.character(max(weeks[!weeks %in% max(weeks)])))),
      tags$div(class='filterbox filter-split filter-group1 filter-month filter-hide',selectInput('month','Month Start:',c('All Months',as.character(months)),selected = 'All Months')),
      tags$div(class='filterbox filter-split filter-group1 filter-quarter filter-hide',selectInput('quarter','Quarter Start:',c('All Quarters',as.character(quarters)),selected = 'All Quarters')),
      tags$div(class='filterbox filter-split filter-group1 filter-hide',selectInput('age','Age:',c('All Ages','5-17','18-24','25-34','35-44','45-54','55-64','65+'),selected='All Ages')),
      tags$div(class='filterbox filter-split filter-group1 filter-hide',selectInput('gender','Gender:',c('All Genders','Male','Female','Other'),selected = 'All Genders')),
      tags$div(class='filterbox filter-split filter-group1 filter-hide',selectInput('day_part','Day Part:',c('All Times','Morning','Afternoon','Evening','Night'),selected = 'All Times')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('province','Province:',c('All Provinces',provinces),selected = 'All Provinces')),
      tags$div(class='filterbox filter-split filter-group1 filter-onboarding',selectInput('level','Onboard Level:',c('All Levels',active_levels),selected = 'All Levels')),
      tags$div(class='filterbox filter-split filter-group1 filter-maxlevel',selectInput('max_level','Level Reached:',c('All Levels',seq(50,max_level,50)),selected = 'All Levels')),
      tags$div(class='filterbox filter-split filter-group1 filter-hide',selectInput('initialize','Level Reached:',c('All Levels','0',seq(50,max_level,50)),selected = 'All Levels'))
    ),
    tags$div(class='filterbuttons',
      tags$div(class='filter-calculate',actionButton('calculate','Recalculate')),
      tags$div(class='filter-reset',actionButton('reset_filters','Clear All Filters')),
      tags$div(class='dl-button',downloadButton('downloadData','Download')),
      tags$div(class='radio-holder',
        tags$div(class='cbx-type',radioButtons('fileType','',c('csv'='csv','xlsx'='xlsx'),selected='xlsx')),
        tags$div(class='cbx-depth',radioButtons('fileDepth','',c('Raw'='raw','Summary'='summary'),selected='summary'))
      ),
      tags$div(class='cbx-choice',
        conditionalPanel(
          condition='input.fileDepth == "raw"', # & input.fileType == "csv"',
          radioButtons('fileChoice','',c('Badges'='badges','Rewards'='rewards','Activity'='activity'),selected='badges')
        )
      )
    )
  ),
  column(9,navbarPage('',position='fixed-top',badges,rewards,activity))
)