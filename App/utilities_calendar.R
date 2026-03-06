#' This function collates the information necessary for the calandar for a specific stock/year.
#'
#' @param timeL (df dowloaded from advice view info record)
#' @param tbl_sid (SID df)
#' @param radio_button (input from radio button used to select a stock in the SID table)
#'
#' @return a character string (that will need to be converted with HTML())
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' html_calendar(advice_view_info(), res_mod(), input$rdbtn)
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
html_calendar <- function(timeL, tbl_sid, radio_button) {
    ## this gets the initial dates from the advice view
    
    release_date <- strptime(as.character(timeL$adviceReleasedDate), "%Y-%m-%d")
    release_date <- format(release_date, "%d/%m/%Y")

    applicable_from <- strptime(as.character(timeL$adviceApplicableFrom), "%Y-%m-%d")
    applicable_from <- format(applicable_from, "%d/%m/%Y")

    applicable_until <- strptime(as.character(timeL$adviceApplicableUntil), "%Y-%m-%d")
    applicable_until <- format(applicable_until, "%d/%m/%Y")

    ## This block gets the name of the working group from the currently selected row
    filtered_row <- tbl_sid[str_detect(tbl_sid$Select, regex(paste0("\\b", radio_button,"\\b"))), ]
    WG <- filtered_row$ExpertGroup
    
    ## This block scrapes the meeting-calendar webpage to find the dates of the upcoming WG meeting
    page <- read_html(paste0("https://www.ices.dk/news-and-events/meeting-calendar/Pages/ICES-CalendarSearch.aspx?k=", WG))
    
    start_date <- page %>%
        html_nodes("td") #%>%
        # html_text()
        
    start_date <- gsub("<td>", "", start_date)
    start_date <- gsub("</td>", "", start_date)


    title_meeting <- start_date[1]
    descr_group <- start_date[4]
    meeting_loc <- start_date[5]
    ## This block extracts and formats the dates as above
    start_WG <- strapplyc(start_date[2], "\\d+/\\d+/\\d+", simplify = TRUE)
    end_WG <- strapplyc(start_date[3], "\\d+/\\d+/\\d+", simplify = TRUE)

    start_WG <- strptime(as.character(start_WG), "%d/%m/%Y")
    start_WG <- format(start_WG, "%d/%m/%Y")
    end_WG <- strptime(as.character(end_WG), "%d/%m/%Y")
    end_WG <- format(end_WG, "%d/%m/%Y")

    html_timeline_string <- paste0("
                                <style>
                            /* (A) TIMELINE CONTAINER */
                        .vtl {
                        /* (A1) RELATIVE POSITION REQUIRED TO PROPERLY POSITION THE TIMELINE */
                        position: relative;

                        /* (A2) RESERVE MORE SPACE TO THE LEFT FOR THE TIMELINE */
                        padding: 10px 10px 10px 50px;

                        /* (A3) OPTIONAL WIDTH RESTRICTION */
                        max-width: 400px;
                        }
                        .vtl, .vtl * { box-sizing: border-box; }

                        /* (B) DRAW VERTICAL LINE USING ::BEFORE */
                        .vtl::before {
                        /* (B1) VERTICAL LINE */
                        content: '';
                        width: 5px;
                        background-color: #de421a;

                        /* (B2) POSITION TO THE LEFT */
                        position: absolute;
                        top: 0; bottom: 0; left: 15px;
                        }

                        /* (C) COSMETICS FOR EVENTS */
                        div.event {
                        padding: 20px 30px;
                        background-color: #ffebeb;
                        position: relative;
                        border-radius: 6px;
                        margin-bottom: 10px;
                        }

                        /* (D) COSMETICS FOR EVENT DATE & TEXT */
                        p.date {
                        font-size: 1.1em;
                        font-weight: 700;
                        color: #ff6a00;
                        }
                        p.txt {
                        margin: 10px 0 0 0;
                        color: #222;
                        }

                        /* (E) EVENT 'SPEECH BUBBLE CALLOUT' */
                        div.event::before {
                        /* (E1) 'MAGIC TRIANGLE' */
                        content: '';
                        border: 10px solid transparent;
                        border-right-color: #ffebeb;
                        border-left: 0;

                        /* (E2) POSITION TO THE LEFT */
                        position: absolute;
                        top: 20%; left: -10px;
                        }

                        /* (F) CIRCLE ON TIMELINE */
                        div.event::after {
                        /* (F1) 'MAGIC CIRCLE' */
                        content: '';
                        background: #fff;
                        border: 4px solid #DE421A;
                        width: 16px; height: 16px;
                        border-radius: 50%;

                        /* (F2) POSITION TO THE LEFT */
                        position: absolute;
                        top: 20%; left: -40px;
                        }
                        </style>


                        <h1>Stockcode: ", timeL$stockCode, "</h1>
                        <div class='vtl'>
                        <div class='event'>
                            <p class='date'>", "Not Available", "</p>
                            <p class='txt'>", "Previous Benchmark", "</p>
                        </div>
                        <div class='event'>
                            <p class='date'>", release_date, "</p>
                            <p class='txt'>Advice release</p>
                        </div>
                        <div class='event'>
                            <p class='date'>", applicable_from,  " - ", applicable_until, "</p>
                            <p class='txt'>Advice validity</href>
                        </div>
                        <div class='event'>
                            <p class='date'>", start_WG, " - ", end_WG, "</p>
                            <p class='txt'>", title_meeting, "<br/>", descr_group, "<br/>", "Location: ", meeting_loc, "</p>
                        </div>
                        <div class='event'>
                            <p class='date'>Not Available</p>
                            <p class='txt'>Next benchmark</p>
                        </div>
                        </div>")
return(html_timeline_string)
}
