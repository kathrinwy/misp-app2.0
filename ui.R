
# This is the user-interface definition of the MISP Calculator 2.0 2020 edition
# created by Kathrin Weny, deployed 27 May 2020

library(shiny)
library(rsconnect)
library(tidyverse)
library(readxl)
library(rhandsontable)
library(DT)
library(shinythemes)
library(openxlsx)

data.misp      <- read.csv("data.misp.2020.csv")

# Define UI for application that draws a histogram
shinyUI <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  tags$head(tags$script(src="format_numbers.js")),
  
  navbarPage("MISP Calculator 2.0",

  tabPanel("Input Data for MISP Calculations",
           
           fluidRow(style='padding-left:50px',
             
             br(),
                    tags$head(
                      tags$style(".btn { vertical-align: middle; height: 50px; width: 35%; font-size: 18px;}"
                      )),
                    
                    tags$h1("Selections"),
             
             br(),
             
             p("In this tab you can enter all required inputs for your MISP calculations, such as the", strong("Country"),
             "you work in or the", strong("Number of affected persons"), "in your context."),
                    
             br(),
                    selectInput("country", 
                                label = "Choose a country",
                                choices = data.misp$country,
                                selected = "Algeria"),
                    
                    #numericInput("num", 
                     #            "Number of affected persons", 
                      #           100000), 

                    br(),
             
             numericInput('n2', 'Number of affected persons', 100000),
             br(),

                    tags$h1("Input Data for MISP Calculator"),
                    
             br(),
             
                    p("This section lays out the input data for the MISP Calculator through", strong("Global constants"), ",", strong("Country-specific"), "or", strong("Site specific"), "indicators."),
                    p(strong("Global constants"),"are determined based on expert group assessments of low- and middle-income countries and/or humanitarian and fragile countries' averages."), 
                    p(strong("Country specific"), "data are drawn from our database based on the country you selected earlier."), 
                    p("You can enter", strong("site-specific data"), "in the green fields if more precise information is available to you."),
                    
             br(),
             
                    tags$h4("Demographic Indicators"),  rHandsontableOutput("table1.1"), rHandsontableOutput("table1.2"),
                    br(),
                    tags$h4("Maternal and Newborn Health"),  rHandsontableOutput("table2.1"), rHandsontableOutput("table2.2"),
                    br(),
                    tags$h4("Access to Sexual and Reproductive Health"),  rHandsontableOutput("table3.1"),  rHandsontableOutput("table3.2"),
             br(),
             br(),
             
             p("You can download your input data (whatever you see on this screen) as well as the full database running in the background below:"), 
             
             downloadButton("downloadData", "Download your input data"), 
             
             br(),
             br(),
             
             downloadButton("downloadDatafull", "Download full dataset"),
             
             br(),
             br(),
             
             tags$em("Last database update: June 2020"),
             
             br(),
             br()
             
                    )),
  
  tabPanel("MISP Calculations based on input data", 
           
           fluidRow(style='padding-left:50px',
             
                    tags$h1("Results"),
                    
           br(),
                    
                    p("In this tab, you can view and download MISP Calculations, based on the inputs in the previous tab."),
                    p("Again, estimates are available base on either", strong("Global constants, Country data"), "or", strong("site specific"), 
                      "inputs you may have provided."),
           br(),
                    
                    tags$h4("Demographic Indicators"),  
                    rHandsontableOutput("table4"),
       
           br(),

                    tags$h4("Maternal and Newborn Health"),  
                    rHandsontableOutput("table5"),
                    br(),
                    tags$h4("Access to Sexual and Reproductive Health"),  
                    rHandsontableOutput("table6"),
           br(),
           br(),
           
           p("You can download your resuts by clicking on the button below:"),
           downloadButton("downloadResultsCSV", "Download results (CSV)")),
           
           br(),
  
  fluidRow(style='padding-left:50px',
           
           downloadButton("downloadResultsExcel", "Download results (Excel)"),
           br(),
           br()
           
           )),
  
  tabPanel("Sources & Support", style='padding-left:50px',
           
           column(6,
           tags$h1("Sources"),
           br(),
           tags$h4("Demographic indicators"),
           p("United Nations Population Division - World Population Prospect: 2019 Revision, accessible", 
             tags$a(href="https://population.un.org/wpp/", "here"),"."),
           br(),
           tags$h4("Modelled family planning indicators"),
           p("UN Population Division - World Contraceptive Use 2019, accessible", 
             tags$a(href="https://www.un.org/en/development/desa/population/publications/dataset/contraception/wcu2019.asp", "here"),"."),
           br(),
           tags$h4("STI prevalence"),
           p("Global Burden of Disease Study 2017 (GBD 2017) Results, accessible", 
             tags$a(href="http://ghdx.healthdata.org/gbd-2017/data-input-sources", "here"),"."),
           br(),
           tags$h4("HIV indicators"),
           p("UN AIDS - AIDS Info - 2019 Estimates, accessible", 
             tags$a(href="https://aidsinfo.unaids.org/", "here"),"."),
           br(),
           tags$h4("Child mortality estimates"),
           p("United Nations Inter-agency Group for Child Mortality Estimation (UN IGME), 2019, accessible", 
             tags$a(href="https://www.unicef.org/reports/levels-and-trends-child-mortality-report-2019", "here"),"."),
           br(),
           tags$h4("Maternal mortality estimates"),
           p("Trends in Maternal Mortality: 2000 to 2017, WHO, UNICEF, UNFPA, World Bank Group, UNFPA, 2019, accessible", 
             tags$a(href="https://www.who.int/reproductivehealth/publications/maternal-mortality-2000-2017/en/", "here"),"."),
           br(),
           tags$h4("Abortion laws"),
           p("Center for Reproductive Rights, The World's Abortion Laws, 2019, accessible", 
             tags$a(href="https://reproductiverights.org/worldabortionlaws", "here"),".")),
          
           column(6,
           tags$h1("MISP Calculator 2.0 app support"),
           br(),
           p("If you have questions regarding the implemenation of the MISP or would like to provide feedback regaring the usage of the MISP Calculator 2.0 app, please contact XYZ XYZ. 
           For questions regaring calculations and underlying data, please reach out to xxxxx xxxx.")
           ))))
