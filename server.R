function(input, output, session) {
  
# Dynamic UI ####

# dynamic ui for Location/Group plot
output$locnGrpTitle <- renderUI({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    if (input$wddSelOrg != "Site"){
      ttl <- "Position Location" 
    } else { 
      ttl <- "Group by BSL"
    }
    # print object
    ttl
    
  })
  
})

# dynamic ui for Job Family/Group plot
output$jfGrpTitle <- renderUI({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    if (input$wddSelOrg != "Job Family"){
      ttl <- "Job Family" 
    } else { 
      ttl <- "Group by BSL"
    }
    # print object
    ttl
  
  })
  
})

####################################################################################################

# Data ####

# subset all data by employment type option
wddDataset <- reactive({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    if (input$wddSelEmpTyp == "All Employment Types"){
      wddDmgsSbst <- wddDmgs
    } else {
      wddDmgsSbst <- subset(wddDmgs, wddDmgs$Perm_Temp == input$wddSelEmpTyp)
    }
      # print dataframe
      wddDmgsSbst
    
  })
  
})

#__________________________________________________________________________________________________#

# then subset by lens option
wddDataset2 <- reactive({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    if (input$wddSelOrg == "ATO"){
      df <- wddDataset()
    } else if (input$wddSelOrg == "Group"){
      df <- subset(wddDataset(), wddDataset()$Subplan == input$wddSelGrp)
    } else if (input$wddSelOrg == "BSL"){
      df <- subset(wddDataset(), wddDataset()$BSL == input$wddSelBSL)
    } else if (input$wddSelOrg == "Branch"){
      df <- subset(wddDataset(), wddDataset()$Org_Unit_Branch == input$wddSelBranch)
    } else if (input$wddSelOrg == "Team/Org.Unit"){
      df <- subset(wddDataset(), wddDataset()$Org_Unit_Team == input$wddSelTeam)
    } else if (input$wddSelOrg == "Cost Centre"){
      df <- subset(wddDataset(), wddDataset()$Cost_Centre_Code == input$wddSelCstCntr)
    } else if (input$wddSelOrg == "Classification"){
      df <- subset(wddDataset(), wddDataset()$Actual_Classification == input$wddSelClassn)
    } else if (input$wddSelOrg == "Job Family"){
      df <- subset(wddDataset(), wddDataset()$Job_Family == input$wddSelJob)
    } else if (input$wddSelOrg == "Site"){
      df <- subset(wddDataset(), wddDataset()$Position_Location == input$wddSelSite)
    } else if (input$wddSelOrg == "Manager"){
      df <- subset(wddDataset(), wddDataset()$Manager_Indicator == input$wddSelMgr)
    } else if (input$wddSelOrg == "Gender"){
      df <- subset(wddDataset(), wddDataset()$Gender == input$wddSelGndr)
    } else if (input$wddSelOrg == "NESB"){
      df <- subset(wddDataset(), wddDataset()$NESB_Sum == input$wddSelNESB)
    } else if (input$wddSelOrg == "Disability"){
      df <- subset(wddDataset(), wddDataset()$Disability_HC == input$wddSelDsbl)
    } else if (input$wddSelOrg == "Indigenous"){
      df <- subset(wddDataset(), wddDataset()$Indigenous_HC == input$wddSelIndg)
    }
      # print dataframe
      df
  })
  
})

#__________________________________________________________________________________________________#

# Group/bsl data
wddOrgPrfl <- reactive({
  
  wddDataset2() %>% 
    select(Subplan
           , BSL) %>% 
    group_by(Subplan
             , BSL) %>% 
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  orgPrfl
  
})

#__________________________________________________________________________________________________#

# Average Age
mean.age <- reactive({
  
  mean(wddDataset2()$Age_Integer)
  
})

# Median Age
med.age <- reactive({
  
  median(wddDataset2()$Age_Integer)
  
})

# Age/tenure data 
wddAgeTnrPrfl <- reactive({
  
  wddDataset2() %>% 
    select(Age_Range_5yr
           , ATO_Tenure_Range) %>% 
    group_by(Age_Range_5yr
             , ATO_Tenure_Range) %>% 
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  ageTnrPrfl  
  
})

# Age data - 5yrs
wddAgePrfl <- reactive({
  
  wddDataset2() %>% 
    select(Age_Range_5yr) %>% 
    group_by(Age_Range_5yr) %>%
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  agePrfl
  
})

# Age data - Single Yrs
wddAgePrfl2 <- reactive({
  
  wddDataset2() %>% 
    select(Age_Integer) %>%
    group_by(Age_Integer) %>%
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)
           , Mean = mean.age()
           , Median = med.age()) %>% 
    filter(Age_Integer > 10 & Age_Integer < 85) ->
  agePrfl2
  
})

#__________________________________________________________________________________________________#

# Classification data
wddClassnPrfl <- reactive({
  
  wddDataset2() %>% 
    select(Actual_Classification) %>% 
    group_by(Actual_Classification) %>%
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  classnPrfl    
  
})

#__________________________________________________________________________________________________#

# ATO Tenure data
wddTnrPrfl <- reactive({
  
  wddDataset2() %>% 
    select(ATO_Tenure_Range) %>% 
    group_by(ATO_Tenure_Range) %>%
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  tnrPrfl    
  
})

#__________________________________________________________________________________________________#

# Job Family data
wddJobPrfl <- reactive({
  
  wddDataset2() %>% 
    select(Job_Family) %>% 
    group_by(Job_Family) %>%
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  jfPrfl
  
})

#__________________________________________________________________________________________________#

# Position Location data
wddLocnPrfl <- reactive({
  
  wddDataset2() %>% 
    select(Position_Location) %>% 
    group_by(Position_Location) %>%
    summarise(HC = n()) %>% 
    ungroup() %>% 
    mutate(Percent = round(HC/sum(HC)*100,2)) ->
  locnPrfl
  
})

#__________________________________________________________________________________________________#

# Diversity data ####

# gender plot data
gndrPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$Gender))

})

# nesb data 
nesbPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$NESB_Sum))

})

# disability data
dsblPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$Disability_HC))

})

# indigenous data
indgPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$Indigenous_HC))

})

#__________________________________________________________________________________________________#

# Learning and development data ####

# MDP plot data
mdpPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$MDP_Status))
  
})

# F2F, eLRN and External training data
ldPrfl <- reactive({

   wddDataset2() %>% 
    select(F2F_Count
           , eLRN_Count
           , External_Count) %>% 
    summarise(HC = n()
              , F2F = round(sum(F2F_Count)/HC,2)
              , eLEARN = round(sum(eLRN_Count)/HC,2)
              , EXTERNAL = round(sum(External_Count)/HC,2)) %>% 
    gather() %>% 
    slice(2:4) ->
  ldPrfl
 
})

# external training cost data
costPrfl <- reactive({
  
  wddDataset2() %>% 
    select(External_Cost) %>% 
    summarise(sumCost = sum(External_Cost)
              , HC = n()
              , avgCost = round(sumCost/HC,2)
              , maxCost = max(External_Cost)) %>% 
    select(`Total Cost` = sumCost
          , Headcount   = HC
          , `Average Cost` = avgCost
          , `Maximum Cost` = maxCost) %>% 
    gather() ->
  costPrfl
  
  # rename cols for table output
  colnames(costPrfl) <- c("Measure", "Dollars ($)")
  
  # format costs for
  costPrfl$`Dollars ($)` <- formatC(costPrfl$`Dollars ($)`, format = "d", big.mark = ",")
  
  # print dataframe
  costPrfl
  
})

#__________________________________________________________________________________________________#

# Mobility Data ####

# Mobility Register data 
mobPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$Mobility_Indicator))
  
})

# OOM data
oomPrfl <- reactive({
  
  as.data.frame(table(wddDataset2()$OOM_Indicator))
  
})

####################################################################################################

# Error Messages to client ####

# error message to user where no data exists in selection
data.msg <- reactive({
  
  msg <- validate(need(nrow(wddDataset2()) != 0
                       , "Your current selection does not have any employees"))
  # print message
  msg
  
})

# error message to user where less than 100 records 
prvcy.msg <- reactive({
  
  msg <- validate(need(nrow(wddDataset2()) >= 100
                       , "For privacy purposes, this metric only displays for populations of 100 or more"))
  # print message
  msg
  
})

####################################################################################################

# Plots ####

# Age plot 5yr ####
output$agePlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x <- list(title = "")
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount"){
      
      # plotly build
      p <- plot_ly(data = wddAgePrfl()
                   , x = Age_Range_5yr
                   , y = HC
                   , type = "bar") %>% 
        layout(xaxis = x
               , yaxis = y
               , margin = m) %>% 
        config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage"){
      
      # plotly layout
      p <- plot_ly(data = wddAgePrfl()
                   , x = Age_Range_5yr
                   , y = Percent
                   , type = "bar") %>% 
        layout(xaxis = x
               , yaxis = y
               , margin = m) %>% 
        config(displayModeBar = F)
      
      # print plotly build
      p
      
    }
  
  })
  
})

#__________________________________________________________________________________________________#

# Age plot Single Yr ####
output$agePlot2 <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x <- list(title = "")
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount"){
      
      # plotly build
      p <- plot_ly(data = wddAgePrfl2()
                   , x = Age_Integer
                   , y = HC
                   , type = "bar"
                   , name = "HC") %>%
           add_trace(x = c(mean.age(), mean.age())
                     , y = c(min(HC), max(HC))
                     , mode = "lines"
                     , name = "Mean") %>% 
           add_trace(x = c(med.age(), med.age())
                     , y = c(min(HC), max(HC))
                    , mode = "lines"
                    , name = "Median") %>% 
           layout(xaxis = x
                   , yaxis = y
                   , margin = m
                   , showlegend = F) %>% 
           config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage"){
      
      # plotly layout
      p <- plot_ly(data = wddAgePrfl2()
                   , x = Age_Integer
                   , y = Percent
                   , type = "bar"
                   , name = "%") %>%
           add_trace(x = c(mean.age(), mean.age())
                     , y = c(min(Percent), max(Percent))
                     , mode = "lines"
                     , name = "Mean") %>% 
          add_trace(x = c(med.age(), med.age())
                    , y = c(min(Percent), max(Percent))
                    , mode = "lines"
                    , name = "Median") %>% 
           layout(xaxis = x
                  , yaxis = y
                  , margin = m
                  , showlegend = F) %>% 
           config(displayModeBar = F)
      
      # print plotly build
      p
      
    }
  
  })
    
})

#__________________________________________________________________________________________________#

# Age by Tenure plot ####
output$ageTnrPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x.order = list("< 20","20 - 24","25 - 29","30 - 34","35 - 39", "40 - 44"
                , "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", ">= 70")
    x <- list(title = "", categoryorder = "array", categoryarray = x.order)
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount"){
      
      # plotly layout
      p <- plot_ly(data = wddAgeTnrPrfl()
                   , x = Age_Range_5yr
                   , y = HC
                   , group = ATO_Tenure_Range
                   , type = "bar") %>% 
           layout(barmode = "stack"
                  , xaxis = x
                  , yaxis = y
                  , margin = m
                  , showlegend = F) %>% 
           config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage"){
      
      # plotly layout
      p <- plot_ly(data = wddAgeTnrPrfl()
                   , x = Age_Range_5yr
                   , y = Percent
                   , group = ATO_Tenure_Range
                   , type = "bar") %>% 
            layout(barmode = "stack"
                   , xaxis = x
                   , yaxis = y
                   , margin = m
                   , showlegend = F) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    }
    
  })
  
}) 

#__________________________________________________________________________________________________#

# Classification profile plot ####
output$classnPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x <- list(title = "")
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount"){
      
      # plotly layout
      p <- plot_ly(data = wddClassnPrfl()
                   , x = Actual_Classification
                   , y = HC
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage"){
      
      # plotly layout
      p <- plot_ly(data = wddClassnPrfl()
                   , x = Actual_Classification
                   , y = Percent
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    }
    
  })
  
})

#__________________________________________________________________________________________________#

# ATO Tenure profile plot ####
output$atoPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x <- list(title = "")
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount"){
      
      # plotly layout
      p <- plot_ly(data = wddTnrPrfl()
                   , x = ATO_Tenure_Range
                   , y = HC
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage"){
      
      # plotly layout
      p <- plot_ly(data = wddTnrPrfl()
                   , x = ATO_Tenure_Range
                   , y = Percent
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    }
    
  })
  
})

#__________________________________________________________________________________________________#

# Job Family profile plot ####
output$jfPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x <- list(title = "")
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount" & input$wddSelOrg != "Job Family"){
      
      # plotly layout
      p <- plot_ly(data = wddJobPrfl()
                   , x = Job_Family
                   , y = HC
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage" & input$wddSelOrg != "Job Family"){
      
      # plotly layout
      p <- plot_ly(data = wddJobPrfl()
                   , x = Job_Family
                   , y = Percent
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Headcount" & input$wddSelOrg == "Job Family"){
      
      # plotly layout
      p <- plot_ly(wddOrgPrfl()
                   , x = Subplan
                   , y = HC
                   , group = BSL
                   , type = "bar") %>% 
            layout(barmode = "stack"
                   , xaxis = x
                   , yaxis = y
                   , margin = m
                   , showlegend = F) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage" & input$wddSelOrg == "Job Family"){
      
      # plotly layout
      p <- plot_ly(wddOrgPrfl()
                   , x = Subplan
                   , y = Percent
                   , group = BSL
                   , type = "bar") %>% 
            layout(barmode ="stack"
                   , xaxis = x
                   , yaxis = y
                   , margin = m
                   , showlegend = F) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    }
  
  })
  
})

#__________________________________________________________________________________________________#

# Position Location profile plot
output$locnPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    x <- list(title = "")
    y <- list(title = input$wddSelView)
    m <- list(t = 10, r = 30)
    
    if (input$wddSelView == "Headcount" & input$wddSelOrg != "Site"){
      
      # plotly layout
      p <- plot_ly(data = wddLocnPrfl()
                   , x = Position_Location
                   , y = HC
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage" & input$wddSelOrg != "Site"){
      
      # plotly layout
      p <- plot_ly(data = wddLocnPrfl()
                   , x = Position_Location
                   , y = Percent
                   , type = "bar") %>% 
            layout(xaxis = x
                   , yaxis = y
                   , margin = m) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Headcount" & input$wddSelOrg == "Site"){
      
      # local margin 
      m <- list(t = 10, b = 30, l = 60, r = 30)
      
      # plotly layout 
      p <- plot_ly(wddOrgPrfl()
                   , x = Subplan
                   , y = HC
                   , group = BSL
                   , type = "bar") %>% 
            layout(barmode ="stack"
                   , xaxis = x
                   , yaxis = y
                   , margin = m
                   , showlegend = F) %>% 
            config(displayModeBar = F)
      
      # print plotly build
      p
      
    } else if (input$wddSelView == "Percentage" & input$wddSelOrg == "Site"){
      
      # local margin 
      m <- list(t = 10, b = 30, l = 60, r = 30)
      
      # plotly layout
      p <- plot_ly(wddOrgPrfl()
                   , x = Subplan
                   , y = Percent
                   , group = BSL
                   , type = "bar") %>% 
           layout(barmode ="stack"
                  , xaxis = x
                  , yaxis = y
                  , margin = m
                  , showlegend = F) %>% 
           config(displayModeBar = F)
      
      # print plotly build
      p
    
    }
    
  })
  
})

#__________________________________________________________________________________________________#

# Diversity plots ####
output$gndrPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = gndrPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

output$nesbPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # error message to user where less than 100 records 
    prvcy.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = nesbPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

output$dsblPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # error message to user where less than 100 records 
    prvcy.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = dsblPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

output$indgPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # error message to user where less than 100 records 
    prvcy.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = indgPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

#__________________________________________________________________________________________________#

# Learning and development plots ####
output$mdpPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
    
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = mdpPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

output$ldPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    m <- list(t = 25, b = 40, l = 80)  
    
    # plotly layout
    p <- plot_ly(data = ldPrfl()
                 , x = value
                 , y = key
                 , type = "bar"
                 , orientation = "h") %>% 
         layout(margin = m
                , yaxis = list(title = "")
                , xaxis = list(title = "Events per HC")
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
  
  })
    
})

# External training cost plot
output$costTbl <- renderTable({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    costPrfl()
    
  })
      
})

#__________________________________________________________________________________________________#

# Mobility plots
output$mobPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = mobPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m
                , showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

output$oomPlot <- renderPlotly({
  
  # Take a dependency on action button
  input$buildDashboard
  
  isolate({
  
    # error message to user where no data exists in selection
    data.msg()
    
    # plot variables
    m <- list(t = 25, b = 15)
    
    # plotly layout
    p <- plot_ly(data = oomPrfl()
                 , labels = Var1
                 , values = Freq
                 , type = "pie") %>% 
         layout(margin = m,
                showlegend = F) %>% 
         config(displayModeBar = F)
    
    # print plotly build
    p
    
  })
  
})

#__________________________________________________________________________________________________#  

}