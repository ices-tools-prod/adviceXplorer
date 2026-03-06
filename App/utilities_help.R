#' Data.table used by rintro.js to document each elements on each tab of the app.
#'
#' @param null
#'
#' @return a data.table object
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' help_datatable()
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
help_datatable <- function() {
    data.table(
        tab = c(
            #data filtering
            "help_tab1",# 1
            "help_tab1",#2
            "help_tab1", #3
            "help_tab1", # 4
            "help_tab1", # 5
            "help_tab1", # 6
            "help_tab1", # 7
            "help_tab1", # 8
            "help_tab1", # 9
            "help_tab1", # 10
            "help_tab1", # 11
            # stock selection
            "help_tab2", # 12
            # stock assessment trends - development over time
            "help_tab3", # 13
            "help_tab3", # 14
            "help_tab3", # 15
            "help_tab3", # 16
            # stock assessment trends - quality of assessemnt
            "help_tab4", # 17
            "help_tab4", # 18
            "help_tab4", # 19
            # Advice
            "help_tab5", # 20
            # "help_tab5", # 21
            # "help_tab5", # 22
            "help_tab5", # 23
            "help_tab5", # 24
            "help_tab5", # 25
            "help_tab5",  # 26
            "help_tab5", # 27
            "help_tab5", #28
            "help_tab5", #29
            "help_tab5", #30
            "help_tab5", #31
            "help_tab5" #32

        ),
        step = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),

        element = c(
            #data filtering
            "#help_tab1",
            "#tabset",
            "#map1",
            "#selected_locations + .selectize-control",
            "#selected_years + .selectize-control",
            "#my-filters-StockKeyLabel-label",
            "#my-filters-SpeciesCommonName-label",
            "#my-filters-ExpertGroup-label",
            "#my-filters-DataCategory-label",
            "#my-filters-YearOfLastAssessment-label",
            "#my-filters-AdviceCategory-label",
            # stock selection
            "#tbl",
            # stock assessment trends - development over time
            "#plot1",
            "#plot2",
            "#plot3",
            "#plot4",
            # stock assessment trends - quality of assessemnt
            "#plot5",
            "#plot6",
            "#plot7",
            # Advice
            "#Advice_Summary",

            # "#preview",
            # "#advice_view_link",

            "#catch_scenario_plot_3",
            "#catch_choice-label",
            "#TAC_timeline",
            "#catch_scenarios_radial",
            "#Radial_plot",
            "#catch_indicators_lollipop",
            "#Lollipop_plot",
            "#Advice_Headline",         
            "#table",
            "#footnotes"
        ),
        intro = c(
            #data filtering
            "<center><img src= 'ICES_logo.png' width= '100%'/></br></br></center><b><font size='5'>Welcome to the beta-version of the single-stock Online Advice!</font></b></br></br><font size='4'>You can get a quick tour of each page by clicking this symbol</font></br></br><center><img src= 'info.png' height= '50px'/></center>",
            "<ul><li><b>Data Filtering:</b> You can filter the data based on different criteria</li><li><b>Stock Selection:</b> Select a fish stock and get access to different ICES products</li><li><b>Stock Assessment Trends:</b> Check the present and past trends of the fish stock</li><li><b>Advice:</b> Check the future projections for the stock</li><li><b>Resources:</b> Give us feedback! Abd acess more ICES resources.</li></ul>",
            "<font size='4'>This map is interactive and connected to the Data filtering panel on the right. Click on one or more Ecoregions to start filtering the data</font><br/><br/><img src= 'Animation.gif' height= '150px'/>",
            "<font size='4'>You can also select an Ecoregion from this drop-down menu</font>",
            "<font size='4'>Select the assessment year you are interested in</font>",
            "<font size='4'>If you know already the stock code you are looking for, you can select it from here</font>",
            "<font size='4'>...or look for the common species name...</font>",
            "<font size='4'>You can also filter the data based on an Expert Group (to know more about Expert Groups, you can check this <a href='https://vocab.ices.dk/?ref=1424' target='_blank'>list</a>)</font>",
            "<font size='4'>You can choose also the data category of stocks (to know more about Data Categories, check page 7 of this <a href='https://www.ices.dk/sites/pub/Publication%20Reports/Advice/2019/2019/Introduction_to_advice_2019.pdf' target='_blank'>document</a>)</font>",
            "Year of last assessment",
            "<font size='4'>You can also filter the data based on the Advice Category (to know more about Advice Categories, you can check this <a href='https://vocab.ices.dk/?ref=1570' target='_blank'>list</a>)</font>",
            # stock selection
            "<b><font size='4'>To select a stock, simply click on its row and move to one of the other tabs on the right</font><b/></br><img src= 'stock_selection.gif' height= '100px'/>",
            # stock assessment trends - development over time
            "catches lorem ipsum",
            "Recruitment lorem ipsum",
            "fishing pressure lorem ipsum",
            "SSB lorem ipsum",
            # stock assessment trends - quality of assessemnt
            "SSB lorem ipsum",
            "F lorem ipsum",
            "REC lorem ipsum",
           # Advice
            "<font size='4'>A few information about the stock, the assessment year and the advice requesters</font>",

            # "<font size='4'>You can click here to see the calendar (advice release date, advice validity, WG dates etcc) of the stock you selected</font>",
            # "<font size='4'>You can click here to see the full advice view record for this stock</font>",

            "<font size='4'>Relationshp between F, SSB, Total Catches and the differen basis of advice..... lorem ipsum</font>",
            "<font size='4'>Here you can choose one or multiple scenarios of advice to be diplayed below.....lorem ipsum</font>",
            "<font size='4'>Catches plot and effect on catches of different scenarios</font>",
            "<font size='4'>Here you can choose one or multiple scenarios of advice to be diplayed below.....lorem ipsum</font>",
            "<font size='4'>Radial plot to compare the different scenarios in % of advice change</font>",           
            "<font size='4'>Here you can choose one or multiple scenarios of advice to be diplayed below.....lorem ipsum</font>",
            "<font size='4'>Lollipop plot to compare the different scenarios in % of advice change</font>", 
            "<font size='4'>Headline of the advice....lorem ipsun</font>",
            "<font size='4'>chatch scenario table....lorem ipsun</font>",
            "<font size='4'>Footnotes of catch scenario table</font>"
        ),
        position = c(
            # data filtering
            "left", # 1
            # "bottom", # 2
            # "right", # 3
            "left", # 4
            "left", # 5
            "left", # 6
            "left", # 7
            "left", # 8
            "left", # 9
            "left", # 10
            "left", # 11
            # stock selection
            "left", # 12
            # stock assessment trends - development over time
            "right", # 13
            "left", # 14
            "right", # 15
            "left", # 16
            # stock assessment trends - quality of assessemnt
            "bottom", # 17
            "bottom", # 18
            "bottom", # 19
            # Advice

            "bottom", # 20
            "right", # 21
            "top", # 22
            "right", # 23
            "left", # 24
            "top", # 25
            "left", # 26
            "top", # 27
            "top", # 28
            "top", # 29
            "left", # 30
            "left", # 31
            "left" # 32

        )
    )
}

help_server <- function(input, output, session) {
  observe({
    click("help_tab1")
    
  })
  
  helptext <- reactive(
    help_datatable()
  )
  
  observeEvent(
    eventExpr = input$help_tab1,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                "setDontShowAgain" = "true",
                steps=helptext()[tab == "help_tab1"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab2,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab2"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab3,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab3"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab4,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab4"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab5,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab5"]
              )
      )
    }
  )
  
}