shinyUI(function(request) {
  
  # define plot height variable
  plot.height <- 250
  
  fluidPage(
    
  useShinyjs(),
    
  # CSS style for validation message
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;}"))),
  
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
                 , choices  = list("Headcount"
                                  , "Percentage")
                 , selected = "Headcount"),
    
    radioButtons("wddSelOrg"
                 , label = "Choose pre-fill option:"
                 , choices = list("ATO"
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
                                  , "Indigenous")
                 , selected = "ATO"),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Group'"
      , selectInput("wddSelGrp"
                    , label = "Choose Group:"
                    , choices = sort(unique(df$Subplan)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'BSL'"
      , selectInput("wddSelBSL"
                    , label = "Choose BSL:"
                    , choices = sort(unique(df$BSL)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Branch'"
      , selectInput("wddSelBranch"
                    , label = "Choose Branch:"
                    , choices = sort(unique(df$Org_Unit_Branch)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Team/Org.Unit'"
      , selectInput("wddSelTeam"
                    , label = "Choose Team/Org.Unit:"
                    , choices = sort(unique(df$Org_Unit_Team)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Cost Centre'"
      , selectInput("wddSelCstCntr"
                    , label = "Choose Cost Centre:"
                    , choices = sort(unique(df$Cost_Centre_Code)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Classification'"
      , selectInput("wddSelClassn"
                    , label = "Choose Classification:"
                    , choices = as.character(sort(unique(df$Actual_Classification))))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Job Family'"
      , selectInput("wddSelJob"
                    , label = "Choose Job Family:"
                    , choices = sort(unique(df$Job_Family)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Site'"
      , selectInput("wddSelSite"
                    , label = "Choose Site:"
                    , choices = sort(unique(df$Position_Location)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Manager'"
      , selectInput("wddSelMgr"
                    , label = "Choose Manager Indicator:"
                    , choices = sort(unique(df$Manager_Indicator)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Gender'"
      , selectInput("wddSelGndr"
                    , label = "Choose Gender:"
                    , choices = sort(unique(df$Gender)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'NESB'"
      , selectInput("wddSelNESB"
                    , label = "Choose NESB Indicator:"
                    , choices = sort(unique(df$NESB_Sum)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Disability'"
      , selectInput("wddSelDsbl"
                    , label = "Choose Disability Indicator:"
                    , choices = sort(unique(df$Disability_HC)))),
    
    conditionalPanel(
      condition = "input.wddSelOrg == 'Indigenous'"
      , selectInput("wddSelIndg"
                    , label = "Choose Indigenous Indicator:"
                    , choices = sort(unique(df$Indigenous_HC)))),
    
    actionButton("buildDashboard", label = "Refresh Dashboard", class = "btn-primary"),
    br(),
    br(),
    
    bookmarkButton(label = "Bookmark"),
    br(),
    br(),
    
    uiOutput(outputId = "dt"),
    br(),
    
    p("This application's data excludes Statutory Office Holders ie. Commissioners."),
    p("For privacy purposes SES3 data has been aggregated into the SES2 classification.")
  ),
  
  # Build dashboard layout
  mainPanel(
    
    uiOutput("msg"),
    
    fluidRow(

      column(width = 6
        , h4(strong("Age and Tenure"))
        , tabsetPanel(id = "ageTnrTab"
                      , tabPanel("Age",        plotlyOutput("agePlot", height = plot.height))
                      , tabPanel("ATO Tnr",    plotlyOutput("atoPlot", height = plot.height))
                      , tabPanel("Age by Tnr", plotlyOutput("ageTnrPlot", height = plot.height))
                      , selected = "Age")
        ),

      column(width = 6
        , h4(strong("Classification and Job Family"))
        , tabsetPanel(id = "clsnJobTab"
                      , tabPanel("Classification", plotlyOutput("classnPlot", height = plot.height))
                      , tabPanel("Job Family", plotlyOutput("jfPlot", height = plot.height))
                      , selected = "Classification")
        )
    ),
    
    br(),

    fluidRow(

      column(width = 6
        , h4(strong("Diversity"))
        # , div(style = "font-size: 70%; padding-top: 70px", 
        , tableOutput("divTable")
        , em(p(style = "font-size:12px", "Note: for privacy purposes, a '*' in the table indicates a 
               metric that does not display due to selected population being < 100"))
        ),
    
      column(width = 6
        , h4(strong("Mobility"))
        , tableOutput("mobTable")
        )

    ),

    fluidRow(

      column(width = 6
             , h4(strong("Learning & Development"))
             , tabsetPanel(id = "learningTab"
                           , tabPanel("MDP", br(), tableOutput("mdpTbl"))
                           , tabPanel("Events Rate", br(), tableOutput("ldTbl"))
                           , tabPanel("External Cost", br(), tableOutput("costTbl"))
                           , selected = "MDP")
        ),
      
      column(width = 6
        , h4(strong(uiOutput("locnGrpTitle")))
  
        , plotlyOutput("locnPlot", height = plot.height)
        )
    )
    
      )
    )
  )
})