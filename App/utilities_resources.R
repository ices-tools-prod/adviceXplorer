# #'This function create an HTML output for the Resources tab of the app. It includes the Citation
# #'
# #' @param null
# #'
# #' @return
# #'
# #' @note
# #' Can add some helpful information here
# #'
# #' @seealso
# #'
# #' @examples
# #' \dontrun{
# #' make_citation()
# #' }
# #'
# #' @references
# #'
# #'
# #'
# #' @export
# #'
# make_citation <- function() {
#   string_citation <- HTML(
#     paste0(

#       "<b>", "<font size=", 5, ">", "Citation", "</font>", "</b>", "<br/>",
#       "<font size=", 3, ">",
#       "Please refer to ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>",
#       " for full conditions and guidance on citation.<br/>
#       When publishing results from the app the minimum citation should include: <br/>
#       <br/>

#       <i><b>ICES adviceXplorer. ", Sys.Date(),". ICES, Copenhagen, Denmark.",
#       "<a href='https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=", getQueryString()$assessmentkey, "&assessmentcomponent=", getQueryString()$assessmentcomponent,
#       "'target='_blank'> https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=",  getQueryString()$assessmentkey, "&assessmentcomponent=", getQueryString()$assessmentcomponent, "</a>","</b></i></font><br/><br/>"

#     )
#   )

#   return(string_citation)
# }


# ---- helpers ----
#' Build a generic “resource card” for the Resources page
#'
#' Construct a standardised card used on the fisheriesXplorer Resources page
#' to present datasets, services, and related links in a consistent layout.
#' The card shows a title, short description, optional list of links
#' (dataset page, metadata, web services, repository, application), and an
#' optional notes field.
#'
#' @param title Character scalar. Short title for the resource, displayed as
#'   the card header.
#'
#' @param description Character scalar. One–two sentence description of the
#'   resource; rendered as a paragraph under the title.
#'
#' @param dataset_url Optional character scalar. URL to the main dataset
#'   landing page. When supplied, a \dQuote{Dataset page} link is added to
#'   the card.
#'
#' @param metadata_url Optional character scalar. URL to the corresponding
#'   metadata / catalogue record. When supplied, a \dQuote{Metadata record}
#'   link is added.
#'
#' @param services Optional set of additional service links. Can be either:
#'   \itemize{
#'     \item a named list, where each element value is a URL and the name is
#'           used as the link label; or
#'     \item a character vector of URLs, optionally named. Unnamed elements
#'           are automatically labelled as \dQuote{Service 1}, \dQuote{Service 2},
#'           etc.
#'   }
#'
#' @param repo_url Optional character scalar. URL for a related source code
#'   repository (e.g. GitHub). When provided, a \dQuote{GitHub repository}
#'   link is added.
#'
#' @param app_url Optional character scalar. URL to a related application
#'   (e.g. a companion Shiny app). When provided, an \dQuote{Application link}
#'   is added.
#'
#' @param notes Optional character scalar with additional notes or caveats
#'   about the resource. Rendered as a paragraph at the bottom of the card
#'   using the CSS class \code{source-notes}.
#'
#' @return A \link[shiny]{tags$div} object with class \code{"source-card"},
#'   containing the title, description, a list of links (if any), and optional
#'   notes. Intended to be used directly in a Shiny UI, for example by
#'   placing several cards within a \code{fluidRow()} or layout container.
#'
#' @details
#' All external links are opened in a new tab (\code{target = "_blank"}) and
#' use \code{rel = "noopener"} for security. The function itself does not
#' apply any styling beyond the CSS class names
#' \code{"source-card"}, \code{"source-title"}, \code{"source-links"},
#' and \code{"source-notes"}, which should be defined in the app's stylesheet.
#'
#' @examples
#' \dontrun{
#' resource_card(
#'   title = "ICES Stock Assessment Database",
#'   description = "Core database with time-series of stock assessment outputs.",
#'   dataset_url = "https://sd.ices.dk",
#'   metadata_url = "https://metadata.ices.dk",
#'   services = c("OData service" = "https://sd.ices.dk/odata/"),
#'   repo_url = "https://github.com/ices-tools-prod/fisheriesXplorer",
#'   notes = "Access subject to ICES data policy."
#' )
#' }
#'
#' @importFrom shiny tags tagList
#' @noRd
resource_card <- function(title, description,
                          dataset_url = NULL,
                          metadata_url = NULL,
                          services = NULL, # named list OR (named/unnamed) character vector
                          repo_url = NULL,
                          app_url = NULL,
                          notes = NULL) {
  # normalize services to a named list
  svc <- NULL
  if (!is.null(services)) {
    if (is.list(services)) {
      svc <- services
    } else if (is.character(services)) {
      if (is.null(names(services)) || any(names(services) == "")) {
        names(services) <- paste("Service", seq_along(services))
      }
      svc <- as.list(services)
    }
  }

  # build the list items
  items <- list()
  if (!is.null(dataset_url)) {
    items <- c(items, list(tags$li(a("Dataset page", href = dataset_url, target = "_blank", rel = "noopener"))))
  }
  if (!is.null(metadata_url)) {
    items <- c(items, list(tags$li(a("Metadata record", href = metadata_url, target = "_blank", rel = "noopener"))))
  }
  if (!is.null(svc)) {
    items <- c(items, lapply(seq_along(svc), function(i) {
      nm <- names(svc)[i]
      url <- unname(svc[[i]])
      tags$li(a(nm, href = url, target = "_blank", rel = "noopener"))
    }))
  }
  if (!is.null(repo_url)) {
    items <- c(items, list(tags$li(a("GitHub repository", href = repo_url, target = "_blank", rel = "noopener"))))
  }
  if (!is.null(app_url)) {
    items <- c(items, list(tags$li(a("Application link", href = app_url, target = "_blank", rel = "noopener"))))
  }

  tags$div(
    class = "source-card",
    tags$div(class = "source-title", title),
    tags$p(description),
    if (length(items)) tags$ul(class = "source-links", do.call(tagList, items)),
    if (!is.null(notes)) tags$p(class = "source-notes", notes)
  )
}


# `%||%` <- function(x, y) if (is.null(x)) y else x



#' Build the fisheriesXplorer data disclaimer block
#'
#' Construct a reusable UI block containing the fisheriesXplorer-specific
#' data disclaimer, links to the ICES general data disclaimer and data
#' policy, and a short summary of access conditions and exclusions.
#' The block includes some light inline CSS for styling the card.
#'
#' @return A \link[shiny]{tagList} with:
#' \itemize{
#'   \item embedded CSS rules for the \code{.disclaimer-card} and
#'         related classes, and
#'   \item a styled card with headings, explanatory text, lists of
#'         exclusions, and links to the fisheriesXplorer disclaimer,
#'         ICES data disclaimer, ICES data policy, and the CC BY 4.0 licence.
#' }
#' This is intended to be inserted directly into a Shiny UI (e.g. within
#' a sidebar or resources tab).
#'
#' @details
#' The function does not take any arguments and always returns the same
#' disclaimer block. External links are opened in a new browser tab and
#' use \code{rel = "noopener"} for security. The small CSS snippet is
#' injected via \code{tags$style()} and affects elements with classes
#' \code{.disclaimer-card} and \code{.cc-strip}.
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   make_disclaimer_block()
#' )
#' }
#'
#' @importFrom shiny tagList tags div p a h3 img br HTML
#' @noRd
make_disclaimer_block <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .disclaimer-card{border:1px solid #e5e7eb;border-radius:12px;background:#fff;padding:12px 16px;margin-bottom:16px;}
      .disclaimer-card h3{margin-top:0}
      .cc-strip{display:flex;align-items:center;gap:.5rem;margin-top:.25rem}
      .cc-strip img{height:36px}
      .disclaimer-card ul{margin:0 0 0 1.1rem}
    ")),
    shiny::div(
      class = "disclaimer-card",
      shiny::h3(shiny::HTML("<b>Data disclaimer</b>")),
      shiny::p(
        "The adviceXplorer Data Disclaimer can be found ",
        shiny::a("here",
          href = "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt",
          target = "_blank", rel = "noopener"
        ),
        "."
      ),
      shiny::p(
        "The general ICES Data Disclaimer can be found ",
        shiny::a("here",
          href = "https://www.ices.dk/Pages/Disclaimer.aspx",
          target = "_blank", rel = "noopener"
        ),
        "."
      ),
      shiny::br(),
      shiny::h3(shiny::HTML("<b>ICES Data Policy</b>")),
      shiny::p(
        "Under the revised ICES Data Policy (2021), public data are available under ",
        shiny::a("CC BY 4.0",
          href = "https://creativecommons.org/licenses/by/4.0/",
          target = "_blank", rel = "noopener"
        ),
        ", and data products are by default publicly available."
      ),
      shiny::p(
        "See the full policy on the ICES website. ",
        shiny::a("ICES Data Policy",
          href = "https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx",
          target = "_blank", rel = "noopener"
        ),
        "."
      ),
      shiny::div(
        class = "cc-strip",
        shiny::tags$img(src = "by.png", alt = "CC BY 4.0"),
        shiny::a("creativecommons.org/licenses/by/4.0/",
          href = "https://creativecommons.org/licenses/by/4.0/",
          target = "_blank", rel = "noopener"
        )
      )
    )
  )
}



#' Build citation strings for the adviceXplorer application
#'
#' Construct a set of citation variants (plain text, BibTeX, and CSL JSON)
#' for the adviceXplorer Shiny application, including optional version,
#' commit hash, and DOI information. This is intended to provide users
#' with a standard way to cite the app in reports, articles, and other
#' publications.
#'
#' @param app_name Character scalar. Name of the application to appear in
#'   the citation title. Default is \code{"adviceXplorer"}.
#'
#' @param org Character scalar. Organisation responsible for hosting or
#'   producing the app, used as the container title in the CSL record.
#'   Default is \code{"ICES"}.
#'
#' @param authors Character vector of author or corporate-author names.
#'   Defaults to \code{c("ICES")}. Names are concatenated with
#'   \code{", "} for the plain-text / BibTeX formats and mapped to CSL
#'   \code{author} entries.
#'
#' @param year Character (or numeric) year of publication / release.
#'   Defaults to the current calendar year, derived from
#'   \code{format(Sys.Date(), "%Y")}.
#'
#' @param app_url Character scalar giving the public URL where the app
#'   is hosted. Used in all three citation formats.
#'
#' @param repo_url Character scalar giving the URL of the source-code
#'   repository (e.g. GitHub). Included in the free-text, BibTeX
#'   \code{note} field, and CSL \code{note}.
#'
#' @param version Character scalar giving the application version
#'   string. By default this is taken from the environment variable
#'   \env{APP_VERSION} (if set). The version is appended in parentheses
#'   after the app name in the citation text and BibTeX title, and
#'   stored in the BibTeX \code{version} and CSL \code{version} fields
#'   when available.
#'
#' @param commit Character scalar giving a short commit hash or other
#'   revision identifier. By default this is taken from the environment
#'   variable \env{APP_COMMIT} (if set). When present, it is appended
#'   alongside the version in parentheses after the app name in the
#'   human-readable citation.
#'
#' @param license Character scalar describing the licence under which
#'   the app is released (e.g. \code{"CC BY 4.0"}). Currently not used
#'   directly in the output, but can be supplied for future extensions.
#'
#' @param access_date Character scalar giving the date the app was
#'   accessed by the user, typically \code{as.character(Sys.Date())}.
#'   This is included in the free-text citation and BibTeX note, and in
#'   the CSL \code{note} field.
#'
#' @param doi Optional character scalar containing a DOI for the app or
#'   a related software record. When supplied, it is appended at the
#'   end of the free-text citation, added to the BibTeX note, and
#'   placed in the CSL \code{DOI} field.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{\code{text}}{Single character string with a human-readable
#'       citation, e.g. \dQuote{ICES (2025). fisheriesXplorer (v1.2.3)
#'       [Shiny application]. https://... Accessed: 2025-11-18. Repository:
#'       https://github.com/...}.}
#'     \item{\code{bibtex}}{Character string containing a complete
#'       \code{@software} BibTeX entry. The entry key is constructed
#'       from \code{app_name} and \code{year} with non-word characters
#'       removed.}
#'     \item{\code{csl}}{A list representing a CSL-JSON item of type
#'       \code{"software"}, suitable for use with pandoc / citeproc
#'       (e.g. via the \pkg{citr}, \pkg{rmarkdown}, or \pkg{quarto}
#'       ecosystems).}
#'   }
#'
#' @details
#' Version and commit information are combined into a short suffix
#' (e.g. \code{" (v1.2.3, 1a2b3c4)"}) appended to the app name in the
#' text and BibTeX title when available. If neither is set (i.e. both
#' environment variables are missing or \code{NA}), the app name is
#' used without additional qualifiers.
#'
#' The CSL \code{author} field is populated using \code{literal} names
#' (no attempt is made to parse given/family names), which works well
#' for corporate authors such as \dQuote{ICES}.
#'
#' @examples
#' \dontrun{
#' cite <- build_app_citation()
#' cat(cite$text, "\n\n")
#' cat(cite$bibtex, "\n")
#' }
#'
#' @noRd
build_app_citation <- function(
    app_name = "adviceXplorer",
    org = "ICES",
    authors = c("ICES"),
    year = format(Sys.Date(), "%Y"),
    app_url = "https://ices-taf.shinyapps.io/adviceXplorer/",
    repo_url = "https://github.com/ices-tools-dev/adviceXplorer",
    version = Sys.getenv("APP_VERSION", unset = NA),
    commit = Sys.getenv("APP_COMMIT", unset = NA),
    license = "CC BY 4.0",
    access_date = as.character(Sys.Date()),
    doi = NULL) {
  verbits <- na.omit(c(version, commit))
  verbits <- if (length(verbits)) paste0(" (", paste(verbits, collapse = ", "), ")") else ""

  text <- sprintf(
    "%s (%s). %s%s [Shiny application]. %s. Accessed: %s. Repository: %s.%s",
    paste(authors, collapse = ", "), year, app_name, verbits, app_url, access_date, repo_url,
    if (!is.null(doi)) paste0(" DOI: ", doi) else ""
  )

  bibkey <- gsub("\\W+", "", paste0(app_name, year))
  bibtex <- sprintf(
    "@software{%s,
  author  = {%s},
  title   = {%s},
  year    = {%s},
  url     = {%s},
  version = {%s},
  note    = {Repository: %s; Accessed: %s%s}
}",
    bibkey, paste(authors, collapse = " and "), paste0(app_name, verbits), year,
    app_url, ifelse(is.na(version), "", version), repo_url, access_date,
    if (!is.null(doi)) paste0("; DOI: ", doi) else ""
  )

  csl <- list(
    type = "software",
    id = bibkey,
    title = paste0(app_name, verbits),
    author = lapply(authors, function(a) list(literal = a)),
    issued = list("date-parts" = list(list(as.integer(year)))),
    URL = app_url,
    version = if (!is.na(version)) version else NULL,
    `container-title` = org,
    note = paste("Repository:", repo_url, "| Accessed:", access_date, if (!is.null(doi)) paste("| DOI:", doi) else ""),
    DOI = doi
  )

  list(text = text, bibtex = bibtex, csl = csl)
}

# ---- module UI/server ----

mod_resources_ui <- function(id) {
  ns <- shiny::NS(id)

  # adviceXplorer repo + citation
  repo_url <- "https://github.com/ices-tools-dev/adviceXplorer"
  app_cite <- build_app_citation()

  bslib::navset_tab(
    id = ns("resources_nav"),

    # --- Contact & Feedback
    bslib::nav_panel(
      shiny::tagList(shiny::icon("envelope"), "Contact & Feedback"),
      shiny::div(
        class = "resources-page",
        shiny::h3(shiny::HTML("<b>Contact & Feedback</b>")),
        shiny::p("We’d love to hear from you. For questions, bug reports, or suggestions:"),
        shiny::tags$ul(
          shiny::tags$li(
            shiny::HTML("Email: <a href='mailto:luca.lamoni@ices.dk'>luca.lamoni@ices.dk</a>")
          )
        ),
        shiny::p("Or fill out the form below:"),
        shiny::div(
          class = "msform-embed",
          shiny::tags$iframe(
            src = "https://forms.office.com/Pages/ResponsePage.aspx?id=ziCy4DVXaESR3wXK5f8f3DhC6UPgEXpGqPSwaKKiWWRUNDBLOTMwQTdRVTNZQlFET041N0FYWkw3SC4u&embed=true",
            width = "820px", height = "580px", frameborder = "0",
            marginwidth = "0", marginheight = "0",
            style = "border: none; max-width:100%; max-height:100vh",
            loading = "lazy", allowfullscreen = NA, webkitallowfullscreen = NA,
            mozallowfullscreen = NA, msallowfullscreen = NA
          )
        ),
        shiny::p(
          shiny::em("Having trouble submitting inside the app? "),
          shiny::a(
            "Open the form in a new tab.",
            href = "https://forms.office.com/Pages/ResponsePage.aspx?id=ziCy4DVXaESR3wXK5f8f3DhC6UPgEXpGqPSwaKKiWWRUNDBLOTMwQTdRVTNZQlFET041N0FYWkw3SC4u",
            target = "_blank", rel = "noopener"
          )
        )
      )
    ),

    # --- Data Sources
    bslib::nav_panel(
      shiny::tagList(shiny::icon("database"), "Data Sources"),
      shiny::div(
        class = "resources-page",

        # Intro
        shiny::div(
          class = "intro-card",
          shiny::h3(shiny::HTML("<b>About these data</b>")),
          shiny::p(
            "adviceXplorer displays (and cites) data from ICES services.",
            " Each source below lists what is used, where it comes from, how to access it, and how to cite/reuse it."
          )
        ),

        # Grid of standardized cards (UNCHANGED)
        shiny::div(
          class = "resources-grid",
          resource_card(
            title = "ICES Spatial Facility (maps)",
            description = "Ecoregion layers and spatial context used across the app.",
            dataset_url = "https://gis.ices.dk/sf/index.html",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/4745e824-a612-4a1f-bc56-b540772166eb?language=all",
            services = list("Shapefiles (zip)" = "https://gis.ices.dk/shapefiles/ICES_ecoregions.zip"),
            notes = "See dataset page for data access and conditions."
          ),
          resource_card(
            title = "Stock Information Database (SID)",
            description = "Used to resolve stock metadata and keys used across the app.",
            dataset_url = "http://sid.ices.dk",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/ec374765-55e8-401a-b219-e011b231ae1b?language=all",
            services = list("APIs" = "http://sid.ices.dk/services/"),
            notes = "See dataset page for data access and conditions."
          ),
          resource_card(
            title = "Stock Assessment Graphs (SAG)",
            description = "Stock assessment outputs used across the app.",
            dataset_url = "https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/f5992b7d-b9da-40d4-81b9-d6db9e87e759?language=all",
            services = list("APIs" = "https://sag.ices.dk/sag_api/docs/swagger/index.html"),
            notes = "See dataset page for data access and conditions."
          ),
          resource_card(
            title = "Advice and Scenarios Database (ASD)",
            description = "Stock assessment catch scenarios and advice metadata.",
            dataset_url = "https://www.ices.dk/data/assessment-tools/Pages/advice-scenarios-database.aspx",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/710f7368-7f51-43dc-b0ac-8cc1788eb23b?language=all",
            services = list("APIs" = "https://asd.ices.dk/webservices"),
            notes = "See dataset page for data access and conditions."
          ),
          resource_card(
            title = "Application source code",
            description = "Code for this application (versioning, issues, reproducibility).",
            services = list("GitHub" = repo_url),
            notes = "See the repository for code, issues, and contribution guidelines."
          )
        ),

        # FAIR checklist
        shiny::div(
          class = "fair-card",
          shiny::h4("FAIR at a glance"),
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML("<b>Findable</b>: clear titles, landing pages, and a metadata record.")),
            shiny::tags$li(shiny::HTML("<b>Accessible</b>: direct links to landing pages and services.")),
            shiny::tags$li(shiny::HTML("<b>Interoperable</b>: machine-readable formats (CSV/JSON) and documented schemas.")),
            shiny::tags$li(shiny::HTML("<b>Reusable</b>: citation text, version/build info, and licensing notes."))
          )
        )
      ),

      # Optional JSON-LD (kept as-is; just namespaced)
      shiny::tags$script(
        type = "application/ld+json",
        shiny::HTML(jsonlite::toJSON(
          list(
            "@context" = "https://schema.org", "@type" = "Dataset",
            "name" = "ICES Fish catch and stock assessment dataset",
            "url" = "https://www.ices.dk/data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx",
            "identifier" = "https://gis.ices.dk/geonetwork/srv/api/records/7d242743-1069-417b-81e3-57f25c791a26",
            "publisher" = list("@type" = "Organization", "name" = "ICES"),
            "license" = "See dataset page",
            "isAccessibleForFree" = TRUE
          ),
          auto_unbox = TRUE, pretty = TRUE
        ))
      )
    ),

    # --- Data disclaimer & policy
    bslib::nav_panel(
      shiny::tagList(shiny::icon("exclamation-triangle"), "Data disclaimer & policy"),
      make_disclaimer_block()
    ),

    # --- Citation
    bslib::nav_panel(
      shiny::tagList(shiny::icon("quote-right"), "Citation"),
      shiny::div(
        class = "cite-card",
        shiny::div(class = "cite-title", shiny::h3(shiny::HTML("<b>How to cite this application</b>"))),
        shiny::p("Please cite the application to support reproducibility:"),
        shiny::div(class = "codeblock", app_cite$text),
        shiny::tags$details(
          shiny::tags$summary("BibTeX"),
          shiny::div(class = "codeblock", app_cite$bibtex)
        ),
        shiny::tags$details(
          shiny::tags$summary("CSL-JSON"),
          shiny::div(class = "codeblock", jsonlite::toJSON(app_cite$csl, auto_unbox = TRUE, pretty = TRUE))
        ),
        shiny::div(
          class = "cite-actions",
          shiny::downloadButton(ns("dl_citations_txt"), "Download TXT"),
          shiny::downloadButton(ns("dl_citations_bib"), "Download BibTeX"),
          shiny::downloadButton(ns("dl_citations_csl"), "Download CSL-JSON")
        ),

        # Optional: machine-readable JSON-LD for the app (kept; namespaced)
        shiny::tags$script(
          type = "application/ld+json",
          shiny::HTML(jsonlite::toJSON(
            list(
              "@context" = "https://schema.org",
              "@type" = "SoftwareApplication",
              "name" = "adviceXplorer",
              "applicationCategory" = "DataVisualization",
              "url" = "https://ices-taf.shinyapps.io/advicexplorer/",
              "publisher" = list("@type" = "Organization", "name" = "ICES"),
              "license" = "https://creativecommons.org/licenses/by/4.0/"
            ),
            auto_unbox = TRUE, pretty = TRUE
          ))
        )
      )
    )
  )
}

# mod_resources_ui <- function(id) {
#   ns <- shiny::NS(id)
#   shiny::tabsetPanel(
#     shiny::tabPanel("Test", shiny::div("OK"))
#   )
# }


mod_resources_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    app_cite <- build_app_citation()

    output$dl_citations_txt <- shiny::downloadHandler(
      filename = function() sprintf("adviceXplorer_citation_%s.txt", Sys.Date()),
      content  = function(file) writeLines(app_cite$text, file)
    )
    output$dl_citations_bib <- shiny::downloadHandler(
      filename = function() sprintf("adviceXplorer_citation_%s.bib", Sys.Date()),
      content  = function(file) writeLines(app_cite$bibtex, file)
    )
    output$dl_citations_csl <- shiny::downloadHandler(
      filename = function() sprintf("adviceXplorer_citation_%s.json", Sys.Date()),
      content = function(file) {
        writeLines(jsonlite::toJSON(app_cite$csl, auto_unbox = TRUE, pretty = TRUE), file)
      }
    )
  })
}

