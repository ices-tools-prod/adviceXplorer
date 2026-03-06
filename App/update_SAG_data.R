#' Returns data from the ICES Stock Assessment database.
#'
#' Downloads summary and reference points from the ICES Stock Assessment database for a specific year
#' and places the two csv files in a year-specific folder called "SAG_xxxx" (for example: "SAG_2021")
#'
#' @param year the year required#'
#'
#' @return 2 csv files
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' years <- c(2021, 2020, 2019, 2018, 2017)
#' for (year in years) {
#'    update_SAG(year)
#' }
#' }
#'
#' @references
#'
#' The ICES stock assessment graphs Database web sevices: \url{http://standardgraphs.ices.dk/stockList.aspx}
#' ICES Transparent Assessment Framework: \url{https://taf.ices.dk}
#'
#' @export
#'
# options(icesSAG.use_token = FALSE)
update_SAG <- function(year) {
  mkdir(paste0("Data/SAG_", year))
  year <- 2024
  # lookup for assessment key for summary
  sag <-
    getListStocks(year = year) %>%
    select(AssessmentKey, StockKeyLabel, AssessmentYear, Purpose, StockDescription, ModifiedDate, SAGStamp, LinkToAdvice, AssessmentComponent) # %>%
  # rename(FishStock = StockKeyLabel)

  summary <- getStockDownloadData(sag$AssessmentKey) %>%
    select(-AssessmentComponent, -Purpose, -AssessmentYear, -StockDescription) %>%
    # change AssessmentKey to integer
    mutate(AssessmentKey = as.integer(AssessmentKey)) %>%
    left_join(sag, by = c("StockKeyLabel", "AssessmentKey")) %>%
    mutate(across(
      c(
        CustomRefPointName1,
        CustomRefPointName2,
        CustomRefPointName3,
        CustomRefPointName4,
        CustomRefPointName5
      ), standardiseRefPoints
    ))


  # write.taf(summary, file = "SAG_summary.csv", dir = paste0("Data/SAG_", year), quote = TRUE)

  refpts <- getFishStockReferencePoints(sag$AssessmentKey)
  refpts <- refpts %>% mutate(across(
    c(
      CustomRefPointName1,
      CustomRefPointName2,
      CustomRefPointName3,
      CustomRefPointName4,
      CustomRefPointName5
    ), standardiseRefPoints
  ))

  # Perform the merge with suffixes to handle duplicate column names
  sagMerged <- merge(summary, refpts, by = "AssessmentKey", suffixes = c(".summary", ""))

  # Select only the columns from summary
  sagMerged <- sagMerged[, !grepl(".summary$", names(sagMerged))]
  # write.taf(SAG, file = "SAG_refpts.csv", dir = paste0("Data/SAG_", year), quote = TRUE)
  write.taf(sagMerged, file = "SAG.csv", dir = paste0("Data/SAG_", year), quote = TRUE)

  # write.taf(refpts, file = "SAG_refpts.csv", dir = paste0("Data/SAG_", year), quote = TRUE)
}

#' Returns the data summary from the ICES Stock Assessment database.
#' This is an internal version of icesFO functions, to run on one year only.
#'
#' @param year the year required
#'
#' @return SAG summary
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'load_sag_summary(2021)
#' }
#'
#' @references
#'
#' The ICES stock assessment graphs Database web sevices: \url{http://standardgraphs.ices.dk/stockList.aspx}
#'
#' @export
#'

# load_sag_summary <- function(year) {
#   out <- icesSAG::getSAG(
#     stock = NULL, year, data = "summary",
#     purpose = "Advice", combine = TRUE
#   )
#   sid <- load_sid(year)
#   sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
#   sid <- dplyr::select(
#     sid, StockKeyLabel, YearOfLastAssessment,
#     PreviousStockKeyLabel
#   )
#   colnames(sid) <- c("FishStock", "AssessmentYear", "PreviousStockKeyLabel")
#   old <- dplyr::filter(sid, AssessmentYear < 2017)

#   out1 <- merge(out, sid,
#     by = c("FishStock", "AssessmentYear"),
#     all = FALSE
#   )

#   out2 <- merge(out, old,
#     by.x = c("FishStock", "AssessmentYear"),
#     by.y = c("PreviousStockKeyLabel", "AssessmentYear"),
#     all = FALSE
#   )
#   out2$FishStock <- out2$FishStock.y
#   out2 <- subset(out2, select = -FishStock.y)
#   out <- merge(out1, out2, all = TRUE)
#   out <- subset(out, select = -PreviousStockKeyLabel)

#   out_replaced <- StockList(year) %>% filter(Purpose == "Replaced") %>% select(c(StockKeyLabel,LinkToAdvice)) %>% rename("ReplacedLinkToAdvice" = "LinkToAdvice")
  


#   unique(out)
# }
# year <- 2022
#' Returns the reference points from the ICES Stock Assessment database.
#' This is an internal version of icesFO functions, to run on one year only.
#'
#' @param year the year required
#'
#' @return SAG summary
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'load_sag_refpts(2021)
#' }
#'
#' @references
#'
#' The ICES stock assessment graphs Database web sevices: \url{http://standardgraphs.ices.dk/stockList.aspx}
#'
#' @export
#'
# load_sag_refpts <- function(year) {
#   out <- icesSAG::getSAG(
#     stock = NULL, year, purpose = "Advice",
#     data = "refpts", combine = TRUE
#   )
#   sid <- load_sid(year)
#   sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
#   sid <- dplyr::select(
#     sid, StockKeyLabel, YearOfLastAssessment,
#     PreviousStockKeyLabel
#   )
#   colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
#   old <- dplyr::filter(sid, AssessmentYear < 2017)
#   out1 <- merge(out, sid,
#     by = c("StockKeyLabel", "AssessmentYear"),
#     all = FALSE
#   )
#   out2 <- merge(out, old,
#     by.x = c("StockKeyLabel", "AssessmentYear"),
#     by.y = c("PreviousStockKeyLabel", "AssessmentYear"),
#     all = FALSE
#   )
#   out2$StockKeyLabel <- out2$StockKeyLabel.y
#   out2 <- subset(out2, select = -StockKeyLabel.y)
#   out <- merge(out1, out2, all = TRUE)
#   out <- subset(out, select = -PreviousStockKeyLabel)
#   unique(out)
# }




# get_CI <- function(df) {
#   out <- data.frame()
#   for (AssessmentKey in df$AssessmentKey) {
#     out_temp <- jsonlite::fromJSON(
#       URLencode(
#         sprintf("https://sag.ices.dk/SAG_API/api/FishStockReferencePoints?assessmentKey=%s", AssessmentKey) 
#       )
#     )
#     out_temp <- out_temp %>% select(AssessmentKey,ConfidenceIntervalDefinition)
#     out <- rbind(out, out_temp)
    
#   }

#   # colnames(out)[which(names(out) == "assessmentKey")] <- "AssessmentKey"
  
#   return(out)
# }

standardiseRefPoints <- function(totrefpoints) {
  if (any(totrefpoints %in% c(
    
    "F_{cap}",
    "Fcap"
  ))) {
    totrefpoints[totrefpoints %in% c(
      
      "F_{cap}",
      "Fcap"
    )] <- "FCap"
  }
  if (any(totrefpoints %in% c(
    "F_(MSY proxy)",
    "FMSY proxy",
    "Fmsy proxy",
    "F_{MSY proxy}",
    "F_{MSYproxy}",
    "F_(MSY proxy)"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_(MSY proxy)",
      "FMSY proxy",
      "Fmsy proxy",
      "F_{MSY proxy}",
      "F_{MSYproxy}",
      "F_(MSY proxy)"
    )] <- "FMSY<sub>proxy</sub>"
  }

  if (any(totrefpoints %in% c(
    "I_{trigger}",
    "I (trigger)",
    "I_{trigger}",
    "I_(trigger)",
    "I_trigger)",
    "Itrigger"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "I_{trigger}",
      "I (trigger)",
      "I_{trigger}",
      "I_(trigger)",
      "I_trigger)",
      "Itrigger"
    )] <- "I<sub>trigger</sub>"
  }

  if (any(totrefpoints %in% c(
    "F_{eco}",
    "Feco"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{eco}",
      "Feco"
    )] <- "F<sub>Eco</sub>"
  }

  if (any(totrefpoints %in% c(
    "F_{lim}"
    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{lim}"
    
    )] <- "FLim"
  }

  if (any(totrefpoints %in% c(
    "I_{loss}",
    "Iloss"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "I_{loss}",
      "Iloss"
    )] <- "I<sub>loss</sub>"
  }

  if (any(totrefpoints %in% c(
    "F_{msy}",
    
    "Fmsy"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{msy}",
      
      "Fmsy"
    )] <- "FMSY"
  }

  if (any(totrefpoints %in% c(
    "F_{pa}",
    
    "FPa"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F_{pa}",
      
      "FPa"
    )] <- "Fpa"
  }

  if (any(totrefpoints %in% c(
    "Fmgt",
    "F_{MGT}"    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "Fmgt",
      "F_{MGT}"
    )] <- "Fmanagement"
  }

  if (any(totrefpoints %in% c(
    "HR_{mgt}",
    "HR_{mgt}",
    "HR (mgt)",
    "HR_{MGT}",
    "HRmgt",
    "HRMGT"
    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HR_{mgt}",
      "HR_{mgt}",
      "HR (mgt)",
      "HR_{MGT}",
      "HRmgt",
      "HRMGT"
      
    )] <- "HR<sub>MGT</sub>"
  }

  if (any(totrefpoints %in% c(
    "HR_{msy}",
    "HR_{MSY}",
    "HR_{MSY}"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HR_{msy}",
      "HR_{MSY}",
      "HR_{MSY}"
    )] <- "HR<sub>MSY</sub>"
  }

  if (any(totrefpoints %in% c(
    "HRmsy proxy",
    "HRMSY proxy",
    "HR_{MSY proxy}"    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HRmsy proxy",
      "HRMSY proxy",
      "HR_{MSY proxy}"      
    )] <- "HR MSY<sub>proxy</sub>"
  }

  if (any(totrefpoints %in% c(
    "HR_{MSY proxy} (W)",
    "HR_{MSY proxy} W" 
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HR_{MSY proxy} (W)",
      "HR_{MSY proxy} W"       
    )] <- "HR MSY<sub>proxy</sub> W"
  }

  if (any(totrefpoints %in% c(
    "HR_{MSY proxy} (S)",
    "HR_{MSY proxy} S"
  ))) {
    totrefpoints[totrefpoints %in% c(
      "HR_{MSY proxy} (S)",
      "HR_{MSY proxy} S"
      )] <- "HR MSY<sub>proxy</sub> S"
  }

  if (any(totrefpoints %in% c(
    "MSY Btrigger"
    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "MSY Btrigger"
      
    )] <- "MSYBtrigger"
  }

  if (any(totrefpoints %in% c(
    "MGT B_{trigger}",
    "MGT B {trigger}",
    "MGT B (trigger)"
  ))) {
    totrefpoints[totrefpoints %in% c(
    "MGT B_{trigger}",
    "MGT B {trigger}",
    "MGT B (trigger)"
    )] <- "MGTB<sub>trigger</sub>"
  }

  if (any(totrefpoints %in% c(
    "Bmgt",
    "BMGT"
    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "Bmgt",
      "BMGT"
      
    )] <- "Bmanagement"
  }

  if (any(totrefpoints %in% c(
    "HR_{pa}",
    "HRpa",
    "HR {pa}"
  ))) {
    totrefpoints[totrefpoints %in% c(
     "HR_{pa}",
    "HRpa",
    "HR {pa}"
    )] <- "HR<sub>pa</sub>"
  }

  if (any(totrefpoints %in% c(
    "F/F_{MSY}"    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "F/F_{MSY}"      
    )] <- "F/F<sub>MSY</sub>"
  }

  if (any(totrefpoints %in% c(
    "B/B_{MSY}"    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "B/B_{MSY}"      
    )] <- "B/B<sub>MSY</sub>"
  }

  if (any(totrefpoints %in% c(
    "L_{F=M}/L_{mean}"    
  ))) {
    totrefpoints[totrefpoints %in% c(
      "L_{F=M}/L_{mean}"      
    )] <- "L<sub>F=M</sub>/L<sub>mean</sub>"
  }

  return(totrefpoints)
}
