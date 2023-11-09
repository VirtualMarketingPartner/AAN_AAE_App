# UI FUNCTIONS ----

# The following script contains functions used in the UI components of the 
# Asthma Equity Explorer Dashboard application. These functions provide custom
# HTML to the UI to ensure certain functionality and add custom themes.

# Custom Modal Styling, adapted from https://github.com/cran/shinyBS/blob/master/R/bsModal.R
shinyBSDep <- htmltools::htmlDependency(
  "shinyBS", packageVersion("shinyBS"), 
  src = c("href" = "sbs"), 
  script = "shinyBS.js", 
  stylesheet = "shinyBS.css")


customModal <- function(id, title, trigger, ..., size) {
  if (!missing(size)) {
    if (size == "large") {
      size = "modal-lg"
    } else if (size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
  } else {
    size <- "modal-dialog"
  }
  bsTag <-
    shiny::tags$div(
      class = "modal sbs-modal fade",
      id = id,
      tabindex = "-1",
      "data-sbs-trigger" = trigger,
      "data-backdrop"="static",
      "data-keyboard"="false",
      shiny::tags$div(
        class = size,
        shiny::tags$div(
          class = "modal-content",
          style = "color: #0B2338;",
          shiny::tags$div(
            class = "modal-header",
            shiny::tags$h4(class = "modal-title", title, style = "font-size:150%;")
          ),
          shiny::tags$div(class = "modal-body", list(...)),
          shiny::tags$div(
            class = "modal-footer"
          )
        )
      )
    )
  
  htmltools::attachDependencies(bsTag, shinyBSDep)
}

MITREfooter <- function(){
  HTML(paste0(
    "<span class = 'copyright-footer'>&copy; ",
    format(Sys.Date(), "%Y"),
    ", The MITRE Corporation</span>"
  ))
}

MITREnavbarPage2 <- function (title, ..., windowTitle = title) 
{
  fluidPage(
    style = "padding:0px; margin:0px", 
    navbarPage(
      title = div(
        a(
          href = "https://www.mitre.org/", 
          img(src = "MITRE_logo.png", height = 30),
          target = "blank"),
        a(href = "https://allergyasthmanetwork.org/",
          img(src = "Horizongtal-logo-white-750.png", height = 50), 
          target = "blank"),
        title), 
      theme = "app.css", 
      windowTitle = windowTitle, 
      ...), 
    MITREfooter())
}
