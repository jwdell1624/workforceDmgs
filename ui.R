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
                  , choices = sort(unique(wddDmgs$Position_Location)))), 
  selectInput(
    "wddSelEmpTyp"
    , label = "Choose employment filter:"
    , choices = c("All Employment Types", "Ongoing", "Non-Ongoing", "Casual", "External")
    , selected = "All Employment Types")
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
  
  
#   tabBox( 
#     "ageTab"
#     , title = "Age"
#     , side = "right"
#     , tabPanel("By ATO Tnr", plotlyOutput("ageTnrPlot", height = plot.height-2))
#     , tabPanel("All",        plotlyOutput("agePlot2", height = plot.height-2))
#     , tabPanel("5yrs",       plotlyOutput("agePlot", height = plot.height-2))
#     , selected = "5yrs"),

  box(
    title = "Age"
    , solidHeader = TRUE
    , collapsible = TRUE
    , status = "primary"
    , plotlyOutput("agePlot", height = plot.height)),
  
  box(
    title = "Classification"
    , solidHeader = TRUE
    , collapsible = TRUE
    , status = "info"
    , plotlyOutput("classnPlot", height = plot.height)),
  
  box(
    title = "ATO Tenure"
    , solidHeader = TRUE
    , collapsible = TRUE
    , status = "warning"
    , plotlyOutput("atoPlot", height = plot.height)),
  
  tabBox( 
    "diversityTab"
    , title = "Diversity"
    , side = "right"
    , tabPanel("Indigenous", plotlyOutput("indgPlot", height = plot.height))
    , tabPanel("Disability", plotlyOutput("dsblPlot", height = plot.height))
    , tabPanel("NESB",       plotlyOutput("nesbPlot", height = plot.height))
    , tabPanel("Gender",     plotlyOutput("gndrPlot", height = plot.height))
    , selected = "Gender"),
  
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
