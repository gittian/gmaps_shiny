#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    
    titlePanel("Spatial analysis using Google maps"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        selectInput("city", "Select City",
                    c('Bangalore' = 1, 'Mumbai' = 2, 	'Delhi' = 3, 	'Hyderabad' = 4, 	'Chennai' = 5,
                      'Kolkata' = 6, 	'Mohali' = 7, 	'Jaipur' = 8, 	'Gurgaon' = 9, 	
                      'Pune' = 10, 	'Noida' = 11, 	'Lucknow' = 12, 	'Patna' = 13, 	
                      'Chandigarh' = 14, 	'Raipur' = 15, 	'Goa' = 16, 	'Ahmedabad' = 17,
                      'Shimla' = 18, 	'Jammu' = 19, 	'Ranchi' = 20, 	'Trivandrum' = 21,
                      'Bhopal' = 22, 	'Bhubaneswar' = 23, 	'Dehradun' = 24, 	
                      'Visakhapatnam' = 25, 	'Surat' = 26, 	'Rajkot' = 27, 	
                      'Nashik' = 28, 	'Nagpur' = 29, 	'Coimbatore' = 30, 	'Mysore' = 31,
                      'Kochi' = 32, 	'Jamshedpur' = 33, 	'Ghaziabad' = 34),selected = 4),
        numericInput('k','Number of Clusters',10),
        checkboxGroupInput("entity", 
                           label = h3("Select Entities"), 
                           choices = c('ATM' = 1,'Bank' = 2, 	'Shopping Mall' = 3, 
                                       'Restaurants' = 4, 	'Hospitals' = 5, 
                                       'Hotels' = 6, 	'Schools' = 7, 
                                       'Universities' = 8, 	'Play schools' = 9, 
                                       'Pharmacy' = 10, 	'Surgical Store' = 11, 
                                       'Reliance Fresh' = 12, 
                                       'Ratnadeep Super Market' = 13, 	
                                       'Ghanshyam Super Market' = 14, 
                                       'More Super Market' = 15, 	
                                       'Sampoorna Super Market' = 16, 
                                       'Reliance Digital' = 17, 	
                                       'reliance footprint' = 18, 	
                                       'max fashion' = 19, 	'pantaloons' = 20, 
                                       'shoppers stop' = 21, 	'reliance trends' = 22,
                                       'Cafe Coffee Day' = 23, 	"McDonald's" = 24, 
                                       "domino's pizza" = 25, 	'Pizza hut' = 26, 	'KFC' = 27,
                                       'Starbucks Coffee' = 28, 	'subway' = 29),
                           selected = c(15:18))
      ),
      
      # end of sidebar panel
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview"),
                    
                    tabPanel("Locations",
                             uiOutput('plotLocations')),
                    
                    tabPanel("Clusters",
                             uiOutput('plotClusters')),
                    
                    tabPanel("Clustered locations",
                             uiOutput('plotClusteredLocations'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



