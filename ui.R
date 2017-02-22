# define plot height variable
plot.height <- 250

# Create dashboard header
header <- dashboardHeader(
  
  dropdownMenu(type = "notifications"
               , notificationItem(text = read.table(paste0(dataPath, "workforceDmgs/dateComment.txt"))
               , icon("exclamation-circle"))), 
  title = "Workforce Demographics",
  titleWidth = 300
)


# Widgets for dahsboard sidebar
sidebar <- dashboardSidebar(
  
  selectInput(
    "wddSelEmpTyp"
    , label = "Choose employment filter:"
    , choices = c("All Employment Types"
                  , "Ongoing"
                  , "Non-Ongoing"
                  , "Casual"
                  , "External") #explicitly ordered
    , selected = "All Employment Types"),
  
  radioButtons(
    "wddSelView"
    , label = "Choose result type:"
    , choices = list("Headcount"
                     , "Percentage")
    , selected = "Headcount"),
  
  radioButtons(
    "wddSelOrg"
    , label = "Choose pre-fill option:"
    , choices = list("ATO"
                     , "Group"
                     , "BSL"
                     , "Branch"
                     , "Team/Org.Unit"
                     , "Cost Centre"
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
                  , choices = sort(unique(wddDmgs$Subplan)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'BSL'"
    , selectInput("wddSelBSL"
                  , label = "Choose BSL:"
                  , choices = sort(unique(wddDmgs$BSL)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Branch'"
    , selectInput("wddSelBranch"
                  , label = "Choose Branch:"
                  , choices = sort(unique(wddDmgs$Org_Unit_Branch)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Team/Org.Unit'"
    , selectInput("wddSelTeam"
                  , label = "Choose Team/Org.Unit:"
                  , choices = sort(unique(wddDmgs$Org_Unit_Team)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Cost Centre'"
    , selectInput("wddSelCstCntr"
                  , label = "Choose Cost Centre:"
                  , choices = sort(unique(wddDmgs$Cost_Centre_Code)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Classification'"
    , selectInput("wddSelClassn"
                  , label = "Choose Classification:"
                  , choices = sort(unique(wddDmgs$Actual_Classification)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Job Family'"
    , selectInput("wddSelJob"
                  , label = "Choose Job Family:"
                  , choices = sort(unique(wddDmgs$Job_Family)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Site'"
    , selectInput("wddSelSite"
                  , label = "Choose Site:"
                  , choices = sort(unique(wddDmgs$Position_Location)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Manager'"
    , selectInput("wddSelMgr"
                  , label = "Choose Manager Indicator:"
                  , choices = sort(unique(wddDmgs$Manager_Indicator)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Gender'"
    , selectInput("wddSelGndr"
                  , label = "Choose Gender:"
                  , choices = sort(unique(wddDmgs$Gender)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'NESB'"
    , selectInput("wddSelNESB"
                  , label = "Choose NESB Indicator:"
                  , choices = sort(unique(wddDmgs$NESB_Sum)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Disability'"
    , selectInput("wddSelDsbl"
                  , label = "Choose Disability Indicator:"
                  , choices = sort(unique(wddDmgs$Disability_HC)))),
  
  conditionalPanel(
    condition = "input.wddSelOrg == 'Indigenous'"
    , selectInput("wddSelIndg"
                  , label = "Choose Indigenous Indicator:"
                  , choices = sort(unique(wddDmgs$Indigenous_HC)))),
  
  br(),
  div(style="padding-left: 12px", actionButton("buildDashboard", label = "Refresh Dashboard", class = "btn-primary")),
  br(), 
  p(style="padding-left: 12px; padding-right: 12px", "Note that for privacy purposes SES3 data has been aggregated into the SES2 classification.")
)

# Build dashboard layout
body <- dashboardBody(
  
  # style messages provided to the user
  tags$head(
    tags$style(
      HTML(".shiny-output-error-validation {color: red;}"))),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  fluidRow(
  
    tabBox(
      "ageTnrTab"
      , title = "Age and Tenure"
      , side = "right"
      , tabPanel("Age by Tnr", plotlyOutput("ageTnrPlot", height = plot.height))
      , tabPanel("ATO Tnr",    plotlyOutput("atoPlot", height = plot.height))
      , tabPanel("Age",        plotlyOutput("agePlot", height = plot.height))
      , selected = "Age"),
    
    box(
      title = "Classification"
      , solidHeader = TRUE
      , collapsible = TRUE
      , status = "info"
      , plotlyOutput("classnPlot", height = plot.height))
  
  ), 
  
  fluidRow(
  
    tabBox( 
      "diversityTab"
      , title = "Diversity"
      , side = "right"
      , tabPanel("Indigenous", plotlyOutput("indgPlot", height = plot.height))
      , tabPanel("Disability", plotlyOutput("dsblPlot", height = plot.height))
      , tabPanel("NESB", plotlyOutput("nesbPlot", height = plot.height))
      , tabPanel("Gender", plotlyOutput("gndrPlot", height = plot.height))
      , selected = "Gender"
      , width = 4),
    
    tabBox(
      "learningTab"
      , title = "Learning & Development"
      , side = "right"
      , tabPanel("External Cost", tableOutput("costTbl"))
      , tabPanel("Events Rate", plotlyOutput("ldPlot", height = plot.height + 8))
      , tabPanel("MDP", plotlyOutput("mdpPlot", height = plot.height + 8))
      , selected = "MDP"
      , width = 4),
    
    tabBox(
      "mobilityTab"
      , title = "Mobility"
      , side = "right"
      , tabPanel("") #kludge to match tabBox heights
      , tabPanel("Order of Merit", plotlyOutput("oomPlot", height = plot.height + 8))
      , tabPanel("Mobility Register", plotlyOutput("mobPlot", height = plot.height + 8))
      , selected = "Mobility Register"
      , width = 4)
  
  ), 
  
  fluidRow(
    
    tabBox(
      "functionsTab"
      , title = "Workforce"
      , side = "right"
      , tabPanel(uiOutput("jfGrpTitle"), plotlyOutput("jfPlot", height = plot.height))
      , tabPanel("Function", plotlyOutput("funcPlot", height = plot.height))
      , tabPanel("Comms Persona", plotlyOutput("commsPlot", height = plot.height))
      , selected = uiOutput("jfGrpTitle")),
    
    box(
      title = uiOutput("locnGrpTitle")
      , solidHeader = TRUE
      , collapsible = TRUE
      , status = "danger"
      , plotlyOutput("locnPlot", height = plot.height))
  
  )
  
)

# Render dashboard
dashboardPage(header
              , sidebar
              , body
              , skin = "purple")
