
# This is the server logic of the MISP Calculator 2.0 2020 edition
# created by Kathrin Weny, deployed 27 May 2020

library(shiny)
data.misp     <- read.csv("data.misp.2020.csv")
data.misp.tbl1 <- data.misp[c("country", "wra", "t.18.up", "f.10.14", "f.10.19", "t.10.24", "m.18.up", "cbr", "sti", "nnmr", "mmr")]
data.misp.tbl2 <- data.misp[c("country", "mpds.all.f")]
data.misp.tbl3 <- data.misp[c("country", "hiv", "art")]
data.misp.tbl4 <- data.misp[c("country", "abortion")]

input.dem.1 <- read.csv("input.dem.1.csv")
input.dem.2 <- read.csv("input.dem.2.csv")
input.srh.1 <- read.csv("input.srh.1.csv")
input.srh.2 <- read.csv("input.srh.2.csv")
input.fp.1 <- read.csv("input.fp.1.csv")
input.fp.2 <- read.csv("input.fp.2.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # create reactive object to be used in multiple places
  test.1 <- reactive({
    t1 <- input.dem.1 # dplyr::filter(b, b$Category %in% input$cat & b$Group %in% input$group)
    return(t1)
  })
  
  test.2 <- reactive({
    t2 <- input.dem.2 # dplyr::filter(b, b$Category %in% input$cat & b$Group %in% input$group)
    return(t2)
  })
  
  test.3 <- reactive({
    t3 <- input.srh.1 # dplyr::filter(b, b$Category %in% input$cat & b$Group %in% input$group)
    return(t3)
  })
  
  test.4 <- reactive({
    t4 <- input.srh.2 # dplyr::filter(b, b$Category %in% input$cat & b$Group %in% input$group)
    return(t4)
  })
  
  test.5 <- reactive({
    t5 <- input.fp # dplyr::filter(b, b$Category %in% input$cat & b$Group %in% input$group)
    return(t5)
  })
  
  test.num <- reactive({
    tnum <- input$num 
    return(tnum)
  })
  
  output$table1.1               <- renderRHandsontable({
    data.select1                <- data.misp.tbl1[data.misp.tbl1$country == input$country,] 
    newvar1                     <- c(t(data.select1[c(2:7)]))
    input.dem.1$country         <- newvar1
    input.dem.1$site.specific   <- rep("-", nrow(input.dem.1))
    
    input.dem.1 <- input.dem.1 %>% column_to_rownames(var = "Basic.statistics")
    
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = 'lightgreen';
    }"
    
    rhandsontable(input.dem.1, rowHeaderWidth = 450, colHeaders = c("Global constants", "Country data", "Site specific")) %>%
      hot_col("Site specific", width = 100, valign = "htCenter", renderer = color_renderer) %>%
      hot_col("Country data", width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_col("Global constants", width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_validate_numeric("Site specific", min = 0, max = 1, allowInvalid = T)
  })
  
  output$table1.2             <- renderRHandsontable({
    data.select2              <- data.misp.tbl1[data.misp.tbl1$country == input$country,]
    newvar2                   <- c(t(data.select2[c(8)]))
    input.dem.2$country       <- newvar2
    input.dem.2$site.specific <- rep("-", nrow(input.dem.2))
    
    input.dem.2 <- input.dem.2 %>%
      column_to_rownames(var = "Basic.statistics")
    
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = 'lightgreen';}"
    
    rhandsontable(input.dem.2, rowHeaderWidth = 450, colHeaders = NULL) %>%
      hot_col(3, width = 100, valign = "htCenter", renderer = color_renderer) %>%
      hot_col(2, width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(1, width = 100, readOnly = TRUE, valign = "htCenter")%>%
      hot_validate_numeric(3, min = 0, max = 200, allowInvalid = T)
  })
  
  contentsTableDat.1 <- reactive({
    req(input$table1.1)
    hot_to_r(input$table1.1)
  })
  
  contentsTableDat.2 <- reactive({
    req(input$table1.2)
    hot_to_r(input$table1.2)
  })
  
  output$table4 <- renderRHandsontable({
    
    output.table1 <- data.frame(cbind(
      
      c(
        paste0(contentsTableDat.1()[1, 1]*input$num),
        paste0(contentsTableDat.1()[2, 1]*input$num),
        paste0(contentsTableDat.1()[3, 1]*input$num),
        paste0(contentsTableDat.1()[4, 1]*input$num),
        paste0(contentsTableDat.1()[5, 1]*input$num),
        paste0(contentsTableDat.1()[6, 1]*input$num), 
        round(contentsTableDat.2()[1, 1]*input$num/1000, 0),
        round(contentsTableDat.2()[1, 1]*input$num/12000),
        round((contentsTableDat.2()[1, 1]*input$num/12000)*9)),
      
      c(
        paste0(contentsTableDat.1()[1, 2]*input$num),
        paste0(contentsTableDat.1()[2, 2]*input$num),
        paste0(contentsTableDat.1()[3, 2]*input$num),
        paste0(contentsTableDat.1()[4, 2]*input$num),
        paste0(contentsTableDat.1()[5, 2]*input$num),
        paste0(contentsTableDat.1()[6, 2]*input$num),
        round(contentsTableDat.2()[1, 2]*input$num/1000, 0),
        round(contentsTableDat.2()[1, 2]*input$num/12000),
        round((contentsTableDat.2()[1, 2]*input$num/12000)*9)),
      
      c(ifelse((contentsTableDat.1()[1, 3] != "-" & contentsTableDat.1()[1, 3] != ""), paste0(as.numeric(contentsTableDat.1()[1, 3])*input$num), "-"),
        ifelse((contentsTableDat.1()[2, 3] != "-" & contentsTableDat.1()[2, 3] != ""), paste0(as.numeric(contentsTableDat.1()[2, 3])*input$num), "-"),
        ifelse((contentsTableDat.1()[3, 3] != "-" & contentsTableDat.1()[3, 3] != ""), paste0(as.numeric(contentsTableDat.1()[3, 3])*input$num), "-"),
        ifelse((contentsTableDat.1()[4, 3] != "-" & contentsTableDat.1()[4, 3] != ""), paste0(as.numeric(contentsTableDat.1()[4, 3])*input$num), "-"),
        ifelse((contentsTableDat.1()[5, 3] != "-" & contentsTableDat.1()[5, 3] != ""), paste0(as.numeric(contentsTableDat.1()[5, 3])*input$num), "-"),
        ifelse((contentsTableDat.1()[6, 3] != "-" & contentsTableDat.1()[6, 3] != ""), paste0(as.numeric(contentsTableDat.1()[6, 3])*input$num), "-"),
        ifelse((contentsTableDat.2()[1, 3] != "-" & contentsTableDat.2()[1, 3] != ""), round(as.numeric(contentsTableDat.2()[1, 3])*input$num/1000, 0), "-"),
        ifelse((contentsTableDat.2()[1, 3] != "-" & contentsTableDat.2()[1, 3] != ""), round(as.numeric(contentsTableDat.2()[1, 3])*input$num/12000), "-"),
        ifelse((contentsTableDat.2()[1, 3] != "-" & contentsTableDat.2()[1, 3] != ""), round((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9), "-")))) 
    
    row.names(output.table1) <- c("# of WRA", 
                                  "# of adults (18+)", 
                                  "# of young adolescent girls (10-14)", 
                                  "# of all adolescent girls (10-19)", 
                                  "# of young people (10-24)", 
                                  "# of adult men (18+)",
                                  "# of live births in the next 12 months",
                                  "# of live births in the next month",
                                  "# of pregnant women")
    
    rhandsontable(output.table1, rowHeaderWidth = 450, colHeaders = c("Global constants", "Country data", "Site specific")) %>%
      hot_col("Site specific", width = 200, readOnly = TRUE, valign = "htCenter") %>%
      hot_col("Country data", width = 200, readOnly = TRUE, valign = "htCenter") %>%
      hot_col("Global constants", width = 200, readOnly = TRUE, valign = "htCenter")
    
  })
  
  output$table2.1             <- renderRHandsontable({
    data.select1                <- data.misp.tbl1[data.misp.tbl1$country == input$country,]
    newvar2                     <- c(t(data.select1[c(9)]), rep("-",10))
    input.srh.1$country         <- newvar2
    input.srh.1$site.specific   <-  rep("-", nrow(input.srh.1))
    
    input.srh.1 <- input.srh.1 %>% column_to_rownames(var = "Basic.statistics")
    
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = 'lightgreen';}"
    
    rhandsontable(input.srh.1, rowHeaderWidth = 450, colHeaders = c("Global constants", "Country data", "Site specific")) %>%
      hot_col("Site specific", width = 100, valign = "htCenter", renderer = color_renderer) %>%
      hot_col("Country data", width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_col("Global constants", width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_validate_numeric("Site specific", min = 0, max = 1, allowInvalid = T)
  })
  
  output$table2.2             <- renderRHandsontable({
    data.select2              <- data.misp.tbl1[data.misp.tbl1$country == input$country,]
    newvar2                   <- c(t(data.select2[c(10,11)]))
    input.srh.2$country       <- newvar2
    input.srh.2$site.specific <- rep("-", nrow(input.srh.2))
    
    input.srh.2 <- input.srh.2 %>% column_to_rownames(var = "Basic.statistics")
    
    color_renderer <- "
      function(instance, td) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      td.style.background = 'lightgreen';}"
    
    rhandsontable(input.srh.2, rowHeaderWidth = 450, colHeaders = NULL) %>%
      hot_col(3, width = 100, valign = "htCenter", renderer = color_renderer) %>%
      hot_col(2, width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(1, width = 100, readOnly = TRUE, valign = "htCenter")%>%
      hot_validate_numeric(3, min = 0, max = 2000, allowInvalid = T)
  })
  
  contentsTableDat.3 <- reactive({
    req(input$table2.1)
    hot_to_r(input$table2.1)
  })
  
  contentsTableDat.4 <- reactive({
    req(input$table2.2)
    hot_to_r(input$table2.2)
  })
  
  output$table5 <- renderRHandsontable({
    
    output.table2 <- data.frame(cbind( 
      rbind("next 3 months/next month",
            paste0(round(contentsTableDat.3()[2, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[2, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[3, 1]*contentsTableDat.2()[1,1]*input$num*1.15/4000,0),"/", 
                   (round(contentsTableDat.3()[3, 1]*contentsTableDat.2()[1,1]*input$num*1.15/12000,0))),
            paste0(round(contentsTableDat.3()[4, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[4, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[5, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[5, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[6, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[6, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[7, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[7, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[8, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[8, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[9, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[9, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[10, 1]*contentsTableDat.2()[1,1]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[10, 1]*contentsTableDat.2()[1,1]*input$num/12000,0))),
            "-"),
      
      rbind("next 3 months/next month",
            paste0(round(contentsTableDat.3()[2, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[2, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[3, 1]*contentsTableDat.2()[1,2]*input$num*1.15/4000,0),"/", 
                   (round(contentsTableDat.3()[3, 1]*contentsTableDat.2()[1,2]*input$num*1.15/12000,0))),
            paste0(round(contentsTableDat.3()[4, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[4, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[5, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[5, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[6, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[6, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[7, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[7, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[8, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[8, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[9, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[9, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(contentsTableDat.3()[10, 1]*contentsTableDat.2()[1,2]*input$num/4000,0),"/", 
                   (round(contentsTableDat.3()[10, 1]*contentsTableDat.2()[1,2]*input$num/12000,0))),
            paste0(round(((contentsTableDat.2()[1, 2]*input$num/12000)*9 +  # Currently pregnant women
                            ((contentsTableDat.3()[2, 1]*contentsTableDat.2()[1,2]*input$num/4000)*3))*  # pregnancies that end in miscr/abortion
                           (as.numeric(contentsTableDat.4()[2,2])/100000)/3), "/",
                   round(((contentsTableDat.2()[1, 2]*input$num/12000)*9 +  # Currently pregnant women
                            ((contentsTableDat.3()[2, 1]*contentsTableDat.2()[1,2]*input$num/4000)*3))*  # pregnancies that end in miscr/abortion
                           (as.numeric(contentsTableDat.4()[2,2])/100000)/6))), 
      
      rbind("next 3 months/next month",
            
            # Pregnancies that end in miscarraige and unsafe abortion   
            ifelse(contentsTableDat.3()[2, 3] != "-"  & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000, 0),"/", 
                          round(as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0)), 
                   
                   ifelse(contentsTableDat.3()[2, 3] != "-" & (contentsTableDat.2()[1, 3] != "-"),
                          paste0(round(as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 round(as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0)),
                          
                          ifelse((contentsTableDat.3()[2, 3] == "-"  |  contentsTableDat.3()[2, 3] == "" ) & (contentsTableDat.2()[1, 3] == "-" |  contentsTableDat.2()[1, 3] == "" ), "-", 
                                 ifelse((contentsTableDat.3()[2, 3] == "-"  |  contentsTableDat.3()[2, 3] == "" ) & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Stillbirths
            ifelse(contentsTableDat.3()[3, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[3, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num*1.15/4000, 0),"/", 
                          round(as.numeric(contentsTableDat.3()[3, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num*1.15/12000,0)), 
                   
                   ifelse(contentsTableDat.3()[3, 3] != "-" & (contentsTableDat.2()[1, 3] != "-"),
                          paste0(round(as.numeric(contentsTableDat.3()[3, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num*1.15/4000,0),"/", 
                                 round(as.numeric(contentsTableDat.3()[3, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num*1.15/12000,0)),
                          
                          ifelse((contentsTableDat.3()[3, 3] == "-" |  contentsTableDat.3()[3, 3] == "") & (contentsTableDat.2()[1, 3] == "-" |  contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[3, 3] == "-" |  contentsTableDat.3()[3, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Pregnant women who will experience complications  
            ifelse(contentsTableDat.3()[4, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[4, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[4, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[4, 3] != "-" & contentsTableDat.2()[1, 3] != "-",      
                          paste0(round(as.numeric(contentsTableDat.3()[4, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[4, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[4, 3] == "-"  |  contentsTableDat.3()[4, 3] == "") & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[4, 3] == "-"  |  contentsTableDat.3()[4, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Newborns who will experience complications
            ifelse(contentsTableDat.3()[5, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[5, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[5, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[5, 3] != "-" & contentsTableDat.2()[1, 3] != "-",  
                          paste0(round(as.numeric(contentsTableDat.3()[5, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[5, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[5, 3] == "-" | contentsTableDat.3()[5, 3] == "") & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[5, 3] == "-" | contentsTableDat.3()[5, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Babies who will weigh less than 2,500g at birth
            ifelse(contentsTableDat.3()[6, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[6, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[6, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[6, 3] != "-" & contentsTableDat.2()[1, 3] != "-",  
                          paste0(round(as.numeric(contentsTableDat.3()[6, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[6, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[6, 3] == "-"  | contentsTableDat.3()[6, 3] == "") & (contentsTableDat.2()[1, 3] == "-"  | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[6, 3] == "-"  | contentsTableDat.3()[6, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Pregnant women who will have access to a health center
            ifelse(contentsTableDat.3()[7, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[7, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[7, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[7, 3] != "-" & contentsTableDat.2()[1, 3] != "-",  
                          paste0(round(as.numeric(contentsTableDat.3()[7, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[7, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[7, 3] == "-" | contentsTableDat.3()[7, 3] == "") & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[7, 3] == "-" | contentsTableDat.3()[7, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Pregnant women who will need suturing of vaginal tears
            ifelse(contentsTableDat.3()[8, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[8, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[8, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[8, 3] != "-" & contentsTableDat.2()[1, 3] != "-",  
                          paste0(round(as.numeric(contentsTableDat.3()[8, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[8, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[8, 3] == "-" | contentsTableDat.3()[8, 3] == "") & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[8, 3] == "-" | contentsTableDat.3()[8, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Deliveries requirng a C-section (min)
            ifelse(contentsTableDat.3()[9, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[9, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[9, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[9, 3] != "-" & contentsTableDat.2()[1, 3] != "-",  
                          paste0(round(as.numeric(contentsTableDat.3()[9, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[9, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[9, 3] == "-" | contentsTableDat.3()[9, 3] == "") & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[9, 3] == "-" | contentsTableDat.3()[9, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Deliveries requiring a C-section (max)
            ifelse(contentsTableDat.3()[10, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""),
                   paste0(round(as.numeric(contentsTableDat.3()[10, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000,0),"/", 
                          (round(as.numeric(contentsTableDat.3()[10, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/12000,0))),
                   
                   ifelse(contentsTableDat.3()[10, 3] != "-" & contentsTableDat.2()[1, 3] != "-",  
                          paste0(round(as.numeric(contentsTableDat.3()[10, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000,0),"/", 
                                 (round(as.numeric(contentsTableDat.3()[10, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/12000,0))),
                          
                          ifelse((contentsTableDat.3()[10, 3] == "-" | contentsTableDat.3()[10, 3] == "") & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == ""), "-", 
                                 ifelse((contentsTableDat.3()[10, 3] == "-" | contentsTableDat.3()[10, 3] == "") & (contentsTableDat.2()[1, 3] != "-"), "-", "Error")))),
            
            # Lives saved if all women have access to MISP
            ifelse(contentsTableDat.3()[2, 3] != "-" & 
                     (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == "") &
                     (contentsTableDat.4()[2, 3] == "-" | contentsTableDat.4()[2, 2] == ""),  # miscr/abortion site-specific
                   
                   paste0(round(((contentsTableDat.2()[1, 2]*input$num/12000)*9 +
                                   ((as.numeric(contentsTableDat.3()[2, 3])*contentsTableDat.2()[1, 2]*input$num/4000)*3))*  
                                  (as.numeric(contentsTableDat.4()[2, 2])/100000)/3), "/",
                          round(((contentsTableDat.2()[1, 2]*input$num/12000)*9 +
                                   ((as.numeric(contentsTableDat.3()[2, 3])*contentsTableDat.2()[1, 2]*input$num/4000)*3))*
                                  (as.numeric(contentsTableDat.4()[2, 2])/100000)/6)), 
                   
                   ifelse(contentsTableDat.3()[2, 3] != "-" & 
                            contentsTableDat.2()[1, 3] != "-" &
                            (contentsTableDat.4()[2, 3] == "-" | contentsTableDat.4()[2, 2] == ""),  # miscr/abortion and CBR site-specific
                          
                          paste0(round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                          ((as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1, 3])*input$num/4000)*3))*  
                                         (as.numeric(contentsTableDat.4()[2, 2])/100000)/3), "/",
                                 round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                          ((as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1, 3])*input$num/4000)*3))*
                                         (as.numeric(contentsTableDat.4()[2, 2])/100000)/6)),
                          
                          
                          ifelse(contentsTableDat.3()[2, 3] != "-" & contentsTableDat.2()[1, 3] != "-" & contentsTableDat.4()[2, 3] != "-",
                                 
                                 paste0(round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                                 ((as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000)*3))*  
                                                (as.numeric(contentsTableDat.4()[2,3])/100000)/3), "/",
                                        round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                                 ((as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000)*3))*  
                                                (as.numeric(contentsTableDat.4()[2,3])/100000)/6)),  
                                 
                                 ifelse((contentsTableDat.3()[2, 3] == "-"  | contentsTableDat.3()[2, 3] == "")& 
                                          contentsTableDat.2()[1, 3] != "-" & contentsTableDat.4()[2, 3] != "-", 
                                        
                                        paste0(round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                                        ((as.numeric(contentsTableDat.3()[2, 1])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000)*3))*  
                                                       (as.numeric(contentsTableDat.4()[2,3])/100000)/3), "/",
                                               round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                                        ((as.numeric(contentsTableDat.3()[2, 1])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000)*3))*  
                                                       (as.numeric(contentsTableDat.4()[2,3])/100000)/6)),
                                        
                                        ifelse((contentsTableDat.3()[2, 3] == "-"  | contentsTableDat.3()[2, 3] == "")& 
                                                 (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == "") &  contentsTableDat.4()[2, 3] != "-", 
                                               
                                               paste0(round(((as.numeric(contentsTableDat.2()[1, 2])*input$num/12000)*9 +
                                                               ((as.numeric(contentsTableDat.3()[2, 1])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000)*3))*  
                                                              (as.numeric(contentsTableDat.4()[2,3])/100000)/3), "/",
                                                      round(((as.numeric(contentsTableDat.2()[1, 2])*input$num/12000)*9 +
                                                               ((as.numeric(contentsTableDat.3()[2, 1])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000)*3))*  
                                                              (as.numeric(contentsTableDat.4()[2,3])/100000)/6)),
                                               
                                               
                                               ifelse(contentsTableDat.3()[2, 3] != "-" & (contentsTableDat.2()[1, 3] == "-" | contentsTableDat.2()[1, 3] == "")&
                                                        contentsTableDat.4()[2, 3] != "-",                                  # abortion and MMR site-specific
                                                      
                                                      paste0(round(((as.numeric(contentsTableDat.2()[1, 2])*input$num/12000)*9 +
                                                                      ((as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000)*3))*  
                                                                     (as.numeric(contentsTableDat.4()[2,3])/100000)/3), "/",
                                                             round(((as.numeric(contentsTableDat.2()[1, 2])*input$num/12000)*9 +
                                                                      ((as.numeric(contentsTableDat.3()[2, 3])*as.numeric(contentsTableDat.2()[1,2])*input$num/4000)*3))*  
                                                                     (as.numeric(contentsTableDat.4()[2,3])/100000)/6)),
                                                      
                                                      ifelse((contentsTableDat.3()[2, 3] == "-"  | contentsTableDat.3()[2, 3] == "")&     contentsTableDat.2()[1, 3] != "-" &
                                                               (contentsTableDat.4()[2, 3] == "-"| contentsTableDat.4()[2, 3] == ""), 
                                                             
                                                             paste0(round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                                                             ((as.numeric(contentsTableDat.3()[2, 1])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000)*3))*  
                                                                            (as.numeric(contentsTableDat.4()[2,2])/100000)/3), "/",
                                                                    round(((as.numeric(contentsTableDat.2()[1, 3])*input$num/12000)*9 +
                                                                             ((as.numeric(contentsTableDat.3()[2, 1])*as.numeric(contentsTableDat.2()[1,3])*input$num/4000)*3))*  
                                                                            (as.numeric(contentsTableDat.4()[2,2])/100000)/6)),  
                                                             
                                                             ifelse((contentsTableDat.3()[2, 3] == "-"  | contentsTableDat.3()[2, 3] == "")& 
                                                                      (contentsTableDat.2()[1, 3] == "-"  | contentsTableDat.2()[1, 3] == "")&
                                                                      (contentsTableDat.4()[2, 3] == "-"  | contentsTableDat.4()[2, 3] == ""), paste0("-"), "Error")))))))))))  
    
    row.names(output.table2) <- c("Time",
                                  "# Pregnancies that end in miscarriage or unsafe abortion", 
                                  "# Stillbirths", 
                                  "# Pregnant women who will experience complications", 
                                  "# Newborns who will experience complications", 
                                  "# Babies who will weigh less than 2,500 g at birth", 
                                  "# Pregnant women who will have access to a health center",
                                  "# Pregnant women who will need suturing of vaginal tears",
                                  "# Deliveries requiring a C-section (min)",
                                  "# Deliveries requiring a C-section (max)",
                                  "Maternal deaths averted")
    
    rhandsontable(output.table2, rowHeaderWidth = 450, 
                  colHeaders = c("Global constants", "Country data", "Site specific")) %>%
      hot_col(3, width = 200, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(2, width = 200, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(1, width = 200, readOnly = TRUE, valign = "htCenter")
    
  })
  
  output$table3.1 <- renderRHandsontable({
    data.select2 <-  data.misp.tbl2[data.misp.tbl2$country == input$country,] 
    data.select3 <-  data.misp.tbl3[data.misp.tbl3$country == input$country,]
    
    input.fp.1$Global <- as.character(input.fp.1$Global.constants..default.)
    
    #newvar3      <- 
    input.fp.1$country <- c( rep("-", 2), 
                             t(data.select2[c(2)]),  rep("-", 5), 
                             t(data.select3[c(2,3)]), rep("-", 3))
    
    input.fp.1$site.specific   <-  rep("-", nrow(input.fp.1))
    
    input.fp.1 <- input.fp.1[-c(2)]
    
    input.fp.1 <- input.fp.1 %>% column_to_rownames(var = "Basic.statistics")
    
    colnames(input.fp.1) <- c("Global.constants..default.", "country", "site.specific")
    
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = 'lightgreen';}"
    
    rhandsontable(input.fp.1, rowHeaderWidth = 450, colHeaders = c("Global constants", "Country data", "Site specific")) %>%
      hot_col("Site specific", width = 100, valign = "htCenter", renderer = color_renderer) %>%
      hot_col("Country data", width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_col("Global constants", width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_validate_numeric("Site specific", min = 0, max = 1, allowInvalid = T)
    
  })
  
  output$table3.2             <- renderRHandsontable({
    data.select2              <- data.misp.tbl4[data.misp.tbl4$country == input$country,] 
    newvar2                   <- c(t(data.select2[c(2)]))
    input.fp.2$country       <- newvar2
    input.fp.2$site.specific <- rep("-", nrow(input.fp.2))
    
    input.fp.2 <- input.fp.2 %>% column_to_rownames(var = "Basic.statistics")
    
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = 'lightgreen';}"
    
    rhandsontable(input.fp.2, rowHeaderWidth = 450, colHeaders = NULL) %>%
      hot_col(3, width = 100, valign = "htCenter", renderer = color_renderer) %>%
      hot_col(2, width = 100, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(1, width = 100, readOnly = TRUE, valign = "htCenter")%>%
      hot_validate_character(3, choices = LETTERS[1:100])
  })
  
  contentsTableDat.5 <- reactive({
    req(input$table3.1)
    hot_to_r(input$table3.1)
  })
  
  contentsTableDat.6 <- reactive({
    req(input$table3.2)
    hot_to_r(input$table3.2)
  })
  
  output$table6 <- renderRHandsontable({
    
    output.table3 <- data.frame(cbind( 
      
      rbind( 
        paste0(round(as.numeric(contentsTableDat.5()[1, 1])*input$num,0)),                             # Sexually active men
        paste0(round(as.numeric(contentsTableDat.5()[1, 1])*as.numeric(contentsTableDat.5()[2, 1])*input$num,0)),  # Sexually active men who use condoms
        paste0(round(as.numeric(contentsTableDat.5()[3, 1])*contentsTableDat.1()[1, 1]*input$num,0)),  # mPDS
        paste0(round(as.numeric(contentsTableDat.5()[4, 1])*contentsTableDat.1()[1, 1]*input$num,0)),  # Female condoms
        paste0(round(as.numeric(contentsTableDat.5()[5, 1])*contentsTableDat.1()[1, 1]*input$num,0)),  # Implant
        paste0(round(as.numeric(contentsTableDat.5()[6, 1])*contentsTableDat.1()[1, 1]*input$num,0)),  # Combined oral contraceptives
        paste0(round(as.numeric(contentsTableDat.5()[7, 1])*contentsTableDat.1()[1, 1]*input$num,0)),  # Injectable contractptives
        paste0(round(as.numeric(contentsTableDat.5()[8, 1])*contentsTableDat.1()[1, 1]*input$num,0)),  # IUD
        paste0("-"),  # HIV
        paste0("-"),  # HIV/ART
        paste0(round(as.numeric(contentsTableDat.5()[11,1])*contentsTableDat.1()[2, 1]*input$num,0)),  # People seeking care for STI syndroms
        paste0(round(as.numeric(contentsTableDat.5()[12,1])*contentsTableDat.1()[1, 1]*input$num,0)),  # Sexual violence
        paste0("-"),                                                                       # Safe induced abortion rate
        paste0("-")),                                                                      # Abortion legislation
      
      rbind(
        paste0("-"),                             # Sexually active men
        paste0("-"),  # Sexually active men who use condoms
        paste0(round(as.numeric(contentsTableDat.5()[3, 2])*contentsTableDat.1()[1, 2]*input$num,0)),  # mPDS
        paste0("-"),  # Female condoms
        paste0("-"),  # Implant
        paste0("-"),  # Combined oral contraceptives
        paste0("-"),  # Injectable contractptives
        paste0("-"),  # IUD
        paste0(ifelse(is.na(round(as.numeric(contentsTableDat.5()[9, 2])*contentsTableDat.1()[2, 2]*input$num,2)), "-",  # HIV
                       round(as.numeric(contentsTableDat.5()[9, 2])*contentsTableDat.1()[2, 2]*input$num,2))),
        paste0(ifelse(is.na(round(as.numeric(contentsTableDat.5()[10,2])*as.numeric(contentsTableDat.5()[9, 2])*contentsTableDat.1()[2, 1]*input$num,0)), "-",  # HIV/ART
                      round(as.numeric(contentsTableDat.5()[10,2])*as.numeric(contentsTableDat.5()[9, 2])*contentsTableDat.1()[2, 1]*input$num,0))),
        paste0("-"),  # People seeking care for STI syndroms
        paste0("-"),  # Sexual violence
        paste0("-"),                                                                       # Safe induced abortion rate
        paste0(contentsTableDat.6()[1, 2])),                                               # Abortion legislation
      
      
      rbind(
        
        # Sexually active men  in population
        ifelse(contentsTableDat.5()[1, 3] != "-",
               paste0(round(as.numeric(contentsTableDat.5()[1, 3])*input$num,0)),
               "-"), # otherwise no site-specific data is available
        
        # Sexually active men who use condoms
        ifelse(contentsTableDat.5()[1, 3] != "-" & contentsTableDat.5()[2, 3] != "-",                                        # if user has entered site-specific men of pop and condom use
               paste0(round(as.numeric(contentsTableDat.5()[1, 3])*as.numeric(contentsTableDat.5()[2, 3])*input$num,0)),
               
               ifelse((contentsTableDat.5()[1, 3] == "-" & contentsTableDat.5()[1, 3] == "") & contentsTableDat.5()[2, 3] != "-",   # if user has only entered site-specific condom use
                      paste0(round(as.numeric(contentsTableDat.5()[1, 1])*as.numeric(contentsTableDat.5()[2, 3])*input$num,0)), 
                      "-")),         
        
        # mPDS               
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[3, 3] != "-",                        
               paste0(round(as.numeric(contentsTableDat.5()[3, 3])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == "") & contentsTableDat.5()[3, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[3, 3])*as.numeric(contentsTableDat.1()[1,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[3, 3] == "-" & contentsTableDat.5()[3, 3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[3, 2])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)), "-"))),      
        
        # Female condoms
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[4, 3] != "-",                        
               paste0(round(as.numeric(contentsTableDat.5()[4, 3])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == "") & contentsTableDat.5()[4, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[4, 3])*as.numeric(contentsTableDat.1()[1,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[4, 3] == "-" & contentsTableDat.5()[4, 3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[4, 1])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)), "-"))),
        
        # Implant
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[5, 3] != "-",                        # if user has enter site-specific WRA
               paste0(round(as.numeric(contentsTableDat.5()[5, 3])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] != "") & contentsTableDat.5()[5, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[5, 3])*as.numeric(contentsTableDat.1()[1,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[5, 3] == "-" & contentsTableDat.5()[5, 3] != ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[5, 1])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)), "-"))),
        
        
        # Combined oral contraceptives
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[6, 3] != "-",                        # if user has enter site-specific WRA
               paste0(round(as.numeric(contentsTableDat.5()[6, 3])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == "-") & contentsTableDat.5()[6, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[6, 3])*as.numeric(contentsTableDat.1()[1,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[6, 3] == "-" & contentsTableDat.5()[6, 3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[6, 1])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)), "-"))),
        
        
        # Injectable contractptives
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[7, 3] != "-",                        # if user has enter site-specific WRA
               paste0(round(as.numeric(contentsTableDat.5()[7, 3])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == "") & contentsTableDat.5()[7, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[7, 3])*as.numeric(contentsTableDat.1()[1,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[7, 3] == "-" & contentsTableDat.5()[7, 3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[7, 1])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)), "-"))),
        
        # IUD
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[8, 3] != "-",                        # if user has enter site-specific WRA
               paste0(round(as.numeric(contentsTableDat.5()[8, 3])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == " ") & contentsTableDat.5()[8, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[8, 3])*as.numeric(contentsTableDat.1()[1,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[8, 3] == "-" & contentsTableDat.5()[8, 3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[8, 1])*as.numeric(contentsTableDat.1()[1,3])*input$num,0)), "-"))),
        
        # HIV
        ifelse(contentsTableDat.1()[2, 3] != "-" & contentsTableDat.5()[9, 3] != "-",                       
               paste0(round(as.numeric(contentsTableDat.5()[9, 3])*as.numeric(contentsTableDat.1()[2,3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[2, 3] == "-" & contentsTableDat.1()[2, 3] == "") & contentsTableDat.5()[9, 3] != "-", 
                      paste0(round(as.numeric(contentsTableDat.5()[9, 3])*as.numeric(contentsTableDat.1()[2,2])*input$num,0)),
                      
                      ifelse(contentsTableDat.1()[2, 3] != "-" & (contentsTableDat.5()[9, 3] == "-" & contentsTableDat.5()[9, 3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[9, 2])*as.numeric(contentsTableDat.1()[2, 3])*input$num,0)),
                             "-"))), 
        
        # HIV/ART
        ifelse(contentsTableDat.5()[10, 3] != "-" & contentsTableDat.1()[2, 3] != "-" & contentsTableDat.5()[9, 3] != "-",     # If the user has entered site-specific HIV prevalence and 18+
               paste0(round(as.numeric(contentsTableDat.5()[10,3])*as.numeric(contentsTableDat.5()[9, 3])*
                              as.numeric(contentsTableDat.1()[2, 3])*input$num,0)), 
               
               ifelse((contentsTableDat.5()[10, 3] == "-" & contentsTableDat.5()[10, 3] == "") & 
                        contentsTableDat.5()[9, 3] != "-" & contentsTableDat.1()[2, 3] != "-" ,    # user has only entered site specific 18+ but no site-specific HIV prevalence
                      paste0(round(as.numeric(contentsTableDat.5()[10,2])*as.numeric(contentsTableDat.5()[9, 3])*
                                     as.numeric(contentsTableDat.1()[2, 3])*input$num,0)),
                      
                      
                      ifelse((contentsTableDat.5()[10, 3] == "-" & contentsTableDat.5()[10, 3] == "") & 
                               (contentsTableDat.5()[9, 3] == "-"  & contentsTableDat.5()[9, 3] == "")& 
                               contentsTableDat.1()[2, 3] != "-" ,  # user has only entered site-specific HIV prevalence, but not 18+
                             paste0(round(as.numeric(contentsTableDat.5()[10,2])*as.numeric(contentsTableDat.5()[9, 2])*
                                            as.numeric(contentsTableDat.1()[2, 3])*input$num,0)),
                             
                             ifelse(contentsTableDat.5()[10, 3] != "-" & contentsTableDat.5()[9, 3] == "-" & contentsTableDat.1()[2, 3] != "-" , 
                                    paste0(round(as.numeric(contentsTableDat.5()[10,3])*as.numeric(contentsTableDat.5()[9, 2])*
                                                   as.numeric(contentsTableDat.1()[2, 3])*input$num,0)),
                                    
                                    ifelse(contentsTableDat.5()[10, 3] != "-" & contentsTableDat.5()[9, 3] != "-" & 
                                             (contentsTableDat.1()[2, 3] == "-"  & contentsTableDat.1()[2, 3] == ""), 
                                           paste0(round(as.numeric(contentsTableDat.5()[10,3])*as.numeric(contentsTableDat.5()[9, 3])*
                                                          as.numeric(contentsTableDat.1()[2, 2])*input$num,0)),
                                           
                                           ifelse(contentsTableDat.5()[10, 3] != "-" & 
                                                    (contentsTableDat.5()[9, 3] == "-" & contentsTableDat.5()[9, 3] == "") & 
                                                    (contentsTableDat.1()[2, 3] == "-" & contentsTableDat.1()[2, 3] == ""), 
                                                  paste0(round(as.numeric(contentsTableDat.5()[10,3])*as.numeric(contentsTableDat.5()[9, 2])*
                                                                 as.numeric(contentsTableDat.1()[2, 2])*input$num,0)),
                                                  
                                                  ifelse((contentsTableDat.5()[10, 3] == "-" & contentsTableDat.5()[10, 3] == "" )& 
                                                           (contentsTableDat.5()[9, 3] == "-"  & contentsTableDat.5()[9 , 3] == "" )& 
                                                           (contentsTableDat.1()[2, 3] == "-"  & contentsTableDat.1()[2 , 3] == ""), 
                                                         "-", "-"))))))),   # otherwise use only countrylevel HIV and 18+
        
        # People seeking care for STI syndroms
        ifelse(contentsTableDat.1()[2, 3] != "-" & contentsTableDat.5()[11,3] != "-",                                                      
               paste0(round(as.numeric(contentsTableDat.5()[11,3])*as.numeric(contentsTableDat.1()[2, 3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[2, 3] == "-" & contentsTableDat.1()[2, 3] == "") & contentsTableDat.5()[11,3] != "-",
                      paste0(round(as.numeric(contentsTableDat.5()[11,3])*as.numeric(contentsTableDat.1()[2, 2])*input$num,0)), 
                      
                      ifelse(contentsTableDat.1()[2, 3] != "-" & (contentsTableDat.5()[11,3] == "-" & contentsTableDat.5()[11,3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[11,1])*as.numeric(contentsTableDat.1()[2, 3])*input$num,0)), "-"))),     
        
        # Sexual violence
        ifelse(contentsTableDat.2()[1, 3] != "-" & contentsTableDat.5()[12,3] != "-",        
               paste0(round(as.numeric(contentsTableDat.5()[12,3])*as.numeric(contentsTableDat.1()[1, 3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == "") & contentsTableDat.5()[12,3] != "-",
                      paste0(round(as.numeric(contentsTableDat.5()[12,3])*as.numeric(contentsTableDat.1()[1, 2])*input$num,0)), 
                      
                      ifelse(contentsTableDat.1()[1, 3] != "-" & (contentsTableDat.5()[12,3] == "-" & contentsTableDat.5()[12,3] == ""), 
                             paste0(round(as.numeric(contentsTableDat.5()[12,1])*as.numeric(contentsTableDat.1()[1, 3])*input$num,0)), "-"))),     
        
        
        # Safe induced abortion rate
        ifelse(contentsTableDat.1()[1, 3] != "-" & contentsTableDat.5()[13,3] != "-",          
               paste0(round(as.numeric(contentsTableDat.5()[13,3])*as.numeric(contentsTableDat.1()[1, 3])*input$num,0)),
               
               ifelse((contentsTableDat.1()[1, 3] == "-" & contentsTableDat.1()[1, 3] == "")  & contentsTableDat.5()[13,3] != "-",
                      paste0(round(as.numeric(contentsTableDat.5()[13,3])*as.numeric(contentsTableDat.1()[1, 2])*input$num,0)), "-")),    
        
        
        # Abortion legislation
        "-")))
    
    row.names(output.table3) <- c("Sexually active men",
                                  "Sexually active men who use condoms", 
                                  "WRA who use modern contraceptives", 
                                  "WRA who use female condoms", 
                                  "WRA who use an implant", 
                                  "WRA who use combined oral contraceptive pills", 
                                  "WRA who use injectable contraception",
                                  "WRA who use an IUD",
                                  "People living with HIV",
                                  "People living with HIV, receiving ART",
                                  "People seeking care for STI syndroms",
                                  "Number of cases of sexual violence who will seek care",
                                  "Abortions per 1,000 women of reproductive age",
                                  "Status of abortion legislation")
    
    rhandsontable(output.table3, rowHeaderWidth = 450, 
                  colHeaders = c("Global constants", "Country data", "Site specific")) %>%
      hot_col(3, width = 200, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(2, width = 200, readOnly = TRUE, valign = "htCenter") %>%
      hot_col(1, width = 200, readOnly = TRUE, valign = "htCenter")
    
  })
  
  output$downloadData <- downloadHandler(
    
    
    filename = function() {
      paste0(input$country, Sys.Date(), "-input-data.csv")
    },
    
    content = function(file) {
      
      a <- hot_to_r(input$table1.1)
      b <- hot_to_r(input$table1.2)
      
      c <- hot_to_r(input$table2.1)
      d <- hot_to_r(input$table2.2)
      
      e <- hot_to_r(input$table3.1)
      f <- hot_to_r(input$table3.2)
      
      data <- rbind(a, b, c, d, e, f)
      
      write.csv(data, file)
      
    }
  )
  
  output$downloadDatafull <- downloadHandler(
    
    
    filename = function() {
      paste0("Fulldata", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      
      write.csv(data.misp, file, row.names = F)
      
    }
  )
  
  contentsTableDat.7 <- reactive({
    req(input$table4)
    hot_to_r(input$table4)
  })
  
  
  contentsTableDat.8 <- reactive({
    req(input$table5)
    hot_to_r(input$table5)
  })
  
  
  contentsTableDat.9 <- reactive({
    req(input$table6)
    hot_to_r(input$table6)
  })
  
  
  output$downloadResultsCSV <- downloadHandler(
    
    
    filename = function() {
      paste0(input$country, Sys.Date(), "-results.csv")
    },
    
    content = function(file) {
      
      a <- hot_to_r(input$table4)
      b <- hot_to_r(input$table5)
      c <- hot_to_r(input$table6)
      
      data <- rbind(a, b, c)
      
      colnames(data) <- c("Global constants", "Country data", "Site-specific estimates")
      
      write.csv(data, file)
      
    }
  )
  
  output$downloadResultsExcel <- downloadHandler(
    filename = function() {
      paste0(input$country, Sys.Date(), "-results.xlsx")
    },
    content = function(file) {
      
      a <- hot_to_r(input$table4)
      b <- hot_to_r(input$table5)
      c <- hot_to_r(input$table6)
      
      colnames(a) <- c("Global constants", "Country data", "Site-specific estimates")
      colnames(b) <- c("Global constants", "Country data", "Site-specific estimates")
      colnames(c) <- c("Global constants", "Country data", "Site-specific estimates")
      
      sheets <- list("Demographic Indicators" = a,
                     "Maternal and Newborn Health" = b,
                     "Sexual and Reproductive Health" = c)
      write_xlsx(sheets, file)
    }
  )
  
})
