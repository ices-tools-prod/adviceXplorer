show_startup_modal <- function(session, title = "Loading data") {
  showModal(modalDialog(
    title = title,
    easyClose = FALSE,
    footer = NULL,
    size = "m",
    tagList(
      tags$div(
        style = "margin-top: 6px;",
        tags$div(id = "startup-progress-text", style = "margin-bottom: 8px;", "Starting…"),
        tags$div(
          style = "height: 12px; background: #eee; border-radius: 6px; overflow: hidden;",
          tags$div(
            id = "startup-progress-bar",
            style = "height: 100%; width: 0%; background: #f15d22;"
          )
        )
      )
    )
  ))
}

update_startup_progress <- function(session, value, text) {
    session$sendCustomMessage("startupProgress", list(value = value, text = text))
  }

close_startup_modal <- function() removeModal()


