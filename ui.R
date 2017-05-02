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
      
        selectInput(inputId    = "selEmpTyp"
                    , label    = "Choose employment filter:"
                    , choices  = c("All Employment Types"
                                  , "Ongoing"
                                  , "Non-Ongoing"
                                  , "Casual"
                                  , "External") # explicitly ordered
                    , selected = "All Employment Types"),
        
        radioButtons(inputId    = "selView"
                     , label    = "Choose result type:"
                     , choices  = c("Headcount"
                                    , "Percentage")
                     , selected = "Headcount"),
        
        radioButtons(inputId   = "selOrg"
                     , label   = "Choose pre-fill option:"
                     , choices = c("ATO"
                                   , "Group"
                                   , "BSL"
                                   , "Branch"
                                   , "Team/Org.Unit"
                                   , "Cost Centre"
                                   , "Classification"
                                   , "Classification (Grouped)"
                                   , "Job Family"
                                   , "Site"
                                   , "Manager"
                                   , "Gender"
                                   , "NESB"
                                   , "Disability"
                                   , "Indigenous") # explicitly ordered
                     , selected = "ATO"),
      
        conditionalPanel(condition = "input.selOrg == 'Group'", 
          selectInput(inputId   = "selGrp"
                      , label   = "Choose Group:"
                      , choices = sort(unique(df$Subplan)))),
        
        conditionalPanel(condition = "input.selOrg == 'BSL'",
          selectInput(inputId   = "selBSL"
                      , label   = "Choose BSL:"
                      , choices = sort(unique(df$BSL)))),
        
        conditionalPanel(condition = "input.selOrg == 'Branch'",
          selectInput(inputId   = "selBranch"
                      , label   = "Choose Branch:"
                      , choices = sort(unique(df$Org_Unit_Branch)))),
        
        conditionalPanel(condition = "input.selOrg == 'Team/Org.Unit'",
          selectInput(inputId   = "selTeam"
                      , label   = "Choose Team/Org.Unit:"
                      , choices = sort(unique(df$Org_Unit_Team)))),
        
        conditionalPanel(condition = "input.selOrg == 'Cost Centre'",
          selectInput(inputId   = "selCstCntr"
                      , label   = "Choose Cost Centre:"
                      , choices = sort(unique(df$Cost_Centre_Code)))),
        
        conditionalPanel(condition = "input.selOrg == 'Classification'",
          selectInput(inputId   =  "selClssn"
                    , label     = "Choose Classification:"
                    , choices   = as.character(sort(unique(df$Actual_Classification))))),
        
        conditionalPanel(condition = "input.selOrg == 'Classification (Grouped)'",
          selectInput(inputId   = "selClssnGrp"
                      , label   = "Choose Classification Group:"
                      , choices = as.character(sort(unique(df$clssnCat))))),
        
        conditionalPanel(condition = "input.selOrg == 'Job Family'",
          selectInput(inputId   = "selJob"
                      , label   = "Choose Job Family:"
                      , choices = sort(unique(df$Job_Family)))),
        
        conditionalPanel(condition = "input.selOrg == 'Site'",
          selectInput(inputId   = "selSite"
                      , label   = "Choose Site:"
                      , choices = sort(unique(df$Position_Location)))),
        
        conditionalPanel(condition = "input.selOrg == 'Manager'",
          selectInput(inputId   = "selMgr"
                      , label   = "Choose Manager Indicator:"
                      , choices = c("Yes", "No"))),
        
        conditionalPanel(condition = "input.selOrg == 'Gender'",
          selectInput(inputId   = "selGndr"
                      , label   = "Choose Gender:"
                      , choices = sort(unique(df$Gender)))),
        
        conditionalPanel(condition = "input.selOrg == 'NESB'",
          selectInput(inputId   = "selNESB"
                      , label   = "Choose NESB Indicator:"
                      , choices = c("Yes", "No"))),
        
        conditionalPanel(condition = "input.selOrg == 'Disability'",
          selectInput(inputId   = "selDsbl"
                      , label   = "Choose Disability Indicator:"
                      , choices = c("Yes", "No"))),
        
        conditionalPanel(condition = "input.selOrg == 'Indigenous'",
          selectInput(inputId   = "selIndg"
                      , label   = "Choose Indigenous Indicator:"
                      , choices = c("Yes", "No"))),
      
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
                 , h4(strong(uiOutput("clssnTitle")))
                 , tabsetPanel(id = "clssnJobTab"
                               , tabPanel(title = "Classification"
                                          , plotlyOutput(outputId = "clssnPlot"
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