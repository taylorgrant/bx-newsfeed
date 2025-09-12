pacman::p_load(shiny, bslib, DT, tidyverse, googlesheets4)

# helper function to set up links in DT
mk_links <- function(df, text = "link") {
  df %>%
    mutate(
      link = if_else(
        !is.na(url) & nzchar(url),
        sprintf(
          '<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>',
          url,
          text
        ),
        NA_character_
      )
    )
}
idify <- function(x) gsub("[^a-z0-9]+", "_", tolower(x))


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  titlePanel("Blackstone News Feed"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Today", DTOutput("tbl_today")) # we'll append the rest
  ),
  tags$style(HTML(
    ".dataTables_wrapper .dataTables_paginate { margin-top: .5rem; }"
  ))
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Set up creds
  options(gargle_oauth_cache = ".secrets")
  gs4_auth(cache = ".secrets", email = TRUE)
  sheet_id <- Sys.getenv("GS_ID")
  target_tab <- "mc_results"

  # Read ONCE at startup
  data_all <- suppressMessages(
    read_sheet(ss = sheet_id, sheet = target_tab)
  ) %>%
    mutate(publish_date = as.Date(publish_date))

  # Today table
  output$tbl_today <- renderDT({
    df <- data_all %>%
      filter(publish_date == Sys.Date()) %>%
      arrange(desc(publish_date)) %>%
      mk_links()
    if (nrow(df) == 0) {
      df <- data_all[0, ]
    }
    datatable(
      df[, c("publish_date", "query", "source", "title", "link", "url")],
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 25, order = list(list(0, "desc")))
    )
  })

  # Split once and append tabs programmatically (no cards)
  by_query <- split(data_all, data_all$query)
  query_names <- names(by_query)

  lapply(query_names, function(q) {
    out_id <- paste0("tbl_", idify(q))

    # append a tab for this query
    appendTab("tabs", tabPanel(q, DTOutput(out_id)), select = FALSE)

    # render its table
    df_q <- by_query[[q]] %>%
      arrange(desc(publish_date)) %>%
      mk_links()

    output[[out_id]] <- renderDT({
      datatable(
        df_q[, c("publish_date", "query", "source", "title", "link", "url")],
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 50,
          order = list(list(0, "desc")),
          scrollX = TRUE
        )
      )
    })
  })
}

shinyApp(ui, server)
