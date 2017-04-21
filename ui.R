shinyUI(function(request) {
  
  # define plot height variable
  plot.height <- 250
  
  fluidPage(
    
    # CSS to temporarily suppress 'argument is not a character vector' 
    # error in diversity and mobility tables
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    # Navbar HTML
    includeHTML("www/navbar.html"),
  
    # Navbar padding
    div(style = "padding-top: 50px", titlePanel("Workforce Demographics")),
  
    sidebarLayout(
      
      sidebarPanel(width = 3, 
      
        selectInput(inputId    = "wddSelEmpTyp"
                    , label    = "Choose employment filter:"
                    , choices  = c("All Employment Types"
                                  , "Ongoing"
                                  , "Non-Ongoing"
                                  , "Casual"
                                  , "External") # explicitly ordered
                    , selected = "All Employment Types"),
        
        radioButtons(inputId    = "wddSelView"
                     , label    = "Choose result type:"
                     , choices  = c("Headcount"
                                    , "Percentage")
                     , selected = "Headcount"),
        
        radioButtons(inputId   = "wddSelOrg"
                     , label   = "Choose pre-fill option:"
                     , choices = c("ATO"
                                   , "Group"
                                   , "BSL"
                                   , "Branch"
                                   , "Team/Org.Unit"
                                   , "Cost Centre"
                                   , "Classification"
                                   , "Job Family"
                                   , "Site"
                                   , "Manager"
                                   , "Gender"
                                   , "NESB"
                                   , "Disability"
                                   , "Indigenous") # explicitly ordered
                     , selected = "ATO"),
      
        conditionalPanel(condition = "input.wddSelOrg == 'Group'", 
          selectInput(inputId   = "wddSelGrp"
                      , label   = "Choose Group:"
                      , choices = sort(unique(df$Subplan)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'BSL'",
          selectInput(inputId   = "wddSelBSL"
                      , label   = "Choose BSL:"
                      , choices = sort(unique(df$BSL)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Branch'",
          selectInput(inputId   = "wddSelBranch"
                      , label   = "Choose Branch:"
                      , choices = sort(unique(df$Org_Unit_Branch)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Team/Org.Unit'",
          selectInput(inputId   = "wddSelTeam"
                      , label   = "Choose Team/Org.Unit:"
                      , choices = sort(unique(df$Org_Unit_Team)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Cost Centre'",
          selectInput(inputId   = "wddSelCstCntr"
                      , label   = "Choose Cost Centre:"
                      , choices = sort(unique(df$Cost_Centre_Code)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Classification'",
        selectInput(inputId   =  "wddSelClassn"
                    , label   = "Choose Classification:"
                    , choices = as.character(sort(unique(df$Actual_Classification))))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Job Family'",
          selectInput(inputId   = "wddSelJob"
                      , label   = "Choose Job Family:"
                      , choices = sort(unique(df$Job_Family)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Site'",
          selectInput(inputId   = "wddSelSite"
                      , label   = "Choose Site:"
                      , choices = sort(unique(df$Position_Location)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Manager'",
          selectInput(inputId   = "wddSelMgr"
                      , label   = "Choose Manager Indicator:"
                      , choices = sort(unique(df$Manager_Indicator)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Gender'",
          selectInput(inputId   = "wddSelGndr"
                      , label   = "Choose Gender:"
                      , choices = sort(unique(df$Gender)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'NESB'",
          selectInput(inputId   = "wddSelNESB"
                      , label   = "Choose NESB Indicator:"
                      , choices = sort(unique(df$NESB_Sum)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Disability'",
          selectInput(inputId   = "wddSelDsbl"
                      , label   = "Choose Disability Indicator:"
                      , choices = sort(unique(df$Disability_HC)))),
        
        conditionalPanel(condition = "input.wddSelOrg == 'Indigenous'",
          selectInput(inputId   = "wddSelIndg"
                      , label   = "Choose Indigenous Indicator:"
                      , choices = sort(unique(df$Indigenous_HC)))),
      
        actionButton(inputId = "buildDashboard"
                     , label = "Refresh Dashboard"
                     , class = "btn-primary"),
        br(),
        br(),
        
        conditionalPanel(condition = "input.buildDashboard > 0",
          bookmarkButton(label = "Bookmark")
          , br()
          , br()),
        
        uiOutput(outputId = "dt"),
        br(),
        
        p("This application's data excludes Statutory Office Holders ie. Commissioners."),
        p("For privacy purposes SES3 data has been aggregated into the SES2 classification.")
    
      ),
  
    mainPanel(
      
      conditionalPanel(condition = "output.mnPnl == 0",
        p(style = "color: red;", "Your current selection does not have employees")),
      
      conditionalPanel(condition = "output.mnPnl > 0", 
        fluidRow(
          column(width = 6
                 , h4(strong("Age and Tenure"))
                 , tabsetPanel(id = "ageTnrTab"
                              , tabPanel(title = "Age"
                                        , plotlyOutput(outputId = "agePlot"
                                                       , height = plot.height))
                              , tabPanel(title = "ATO Tnr"
                                         , plotlyOutput(outputId = "atoPlot"
                                                        , height = plot.height))
                              , tabPanel(title = "Age by Tnr"
                                         , plotlyOutput(outputId = "ageTnrPlot"
                                                        , height = plot.height))
                            , selected = "Age")),

          column(width = 6
                 , h4(strong("Classification and Job Family"))
                 , tabsetPanel(id = "clsnJobTab"
                               , tabPanel(title = "Classification"
                                          , plotlyOutput(outputId = "classnPlot"
                                                         , height = plot.height))
                               , tabPanel(title = uiOutput("jfGrpTitle")
                                          , plotlyOutput(outputId = "jfPlot"
                                                         , height = plot.height))
                               , selected = "Classification"))),
        
        br(),
  
        fluidRow(
          column(width = 6
                 , h4(strong("Diversity"))
                 , tableOutput("divTable")
                 , em(p(style = "font-size:12px", "Note: for privacy purposes, a '*' in the table     
                   indicates a metric that does not display due to selected population being < 100"))),
      
          column(width = 6
                 , h4(strong("Mobility"))
                 , tableOutput("mobTable"))),
  
        fluidRow(
          column(width = 6
                 , h4(strong("Learning & Development"))
                 , tabsetPanel(id = "learningTab"
                               , tabPanel(title = "MDP"
                                          , br()
                                          , tableOutput("mdpTbl"))
                               , tabPanel(title = "Events Rate"
                                          , br()
                                          , tableOutput("ldTbl"))
                               , tabPanel(title = "External Cost"
                                          , br()
                                          , tableOutput("costTbl"))
                               , selected = "MDP")),
        
          column(width = 6
                 , h4(strong(uiOutput("locnGrpTitle")))
                 , plotlyOutput(outputId = "locnPlot"
                                , height = plot.height))))
    
      )
    )
  )
})