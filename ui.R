# define plot height variable
plot.height <- 245

# Create dashboard header
header <- dashboardHeader(
  
  dropdownMenu(type = "notifications"
               , notificationItem(text = read.table("data/dateComment.txt")
               , icon("exclamation-circle"))), 
  title = "Workforce Demographics",
  titleWidth = 300
)


# Widgets for dahsboard sidebar
sidebar <- dashboardSidebar(
  selectInput(
    "wddSelEmpTyp"
    , label = "Choose employment filter:"
    , choices = c("All Employment Types", "Ongoing", "Non-Ongoing", "Casual", "External") #explicitly ordered
    , selected = "All Employment Types"), 
  selectInput(
    "wddSelMgr"
    , label = "Manager Indicator:"
    , choices = c("All", sort(unique(wddDmgs$Manager_Indicator)))
    , selected = "All"),
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
                     , "Site")
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
                  , choices = sort(unique(wddDmgs$Position_Location))))
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
  
  
  tabBox(
    "ageTnrTab"
    , title = "Age and Tenure"
    , side = "right"
    , tabPanel("Age by Tnr", plotlyOutput("ageTnrPlot", height = plot.height-2))
    , tabPanel("ATO Tnr",    plotlyOutput("atoPlot", height = plot.height-2))
    , tabPanel("Age",        plotlyOutput("agePlot", height = plot.height-2))
    , selected = "Age"),
  
  box(
    title = "Classification"
    , solidHeader = TRUE
    , collapsible = TRUE
    , status = "info"
    , plotlyOutput("classnPlot", height = plot.height)),
  
  tabBox( 
    "diversityTab"
    , title = "Diversity"
    , side = "right"
    , tabPanel("Indigenous", plotlyOutput("indgPlot", height = plot.height-2))
    , tabPanel("Disability", plotlyOutput("dsblPlot", height = plot.height-2))
    , tabPanel("NESB",       plotlyOutput("nesbPlot", height = plot.height-2))
    , tabPanel("Gender",     plotlyOutput("gndrPlot", height = plot.height-2))
    , selected = "Gender"
    , width = 4),
  
  tabBox(
    "learningTab"
    , title = "Learning & Development"
    , side = "right"
    , tabPanel("External Cost",  tableOutput("costTbl"))
    , tabPanel("Events Rate", plotlyOutput("ldPlot", height = plot.height-2))
    , tabPanel("MDP",          plotlyOutput("mdpPlot", height = plot.height-2))
    , selected = "MDP"
    , width = 4),
  
  tabBox(
    "mobilityTab"
    , title = "Mobility"
    , side = "right"
    , tabPanel("") #kludge to match tabBox heights
    , tabPanel("Order of Merit",    plotlyOutput("oomPlot", height = plot.height-2))
    , tabPanel("Mobility Register", plotlyOutput("mobPlot", height = plot.height-2))
    , selected = "Mobility Register"
    , width = 4),
  
  box(
    title = uiOutput("jfGrpTitle")
    , solidHeader = TRUE
    , collapsible = TRUE
    , status = "success"
    , plotlyOutput("jfPlot", height = plot.height)),
  
  box(
    title = uiOutput("locnGrpTitle")
    , solidHeader = TRUE
    , collapsible = TRUE
    , status = "danger"
    , plotlyOutput("locnPlot", height = plot.height))
  
  
)

# Render dashboard
dashboardPage(header
              , sidebar
              , body
              , skin = "purple")
