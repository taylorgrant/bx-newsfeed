# Function to pull stories from MediaCloud #
get_news <- function() {
  mc_news_pull <- function(qq) {
    library(reticulate)
    # libraries
    os <- import("os")
    mediacloud.api <- import("mediacloud.api")
    dt <- import("datetime")
    # Set the date of the query
    today_r <- Sys.Date()
    today_py <- dt$date(
      as.integer(format(today_r, "%Y")),
      as.integer(format(today_r, "%m")),
      as.integer(format(today_r, "%d"))
    )
    # Get API key
    MC_API_KEY = Sys.getenv("MEDIACLOUD_API_KEY")
    search_api = mediacloud.api$SearchApi(MC_API_KEY)
    # Set query parameters
    my_query <- qq
    start_date <- today_py
    end_date <- today_py
    sources = as.integer(c(
      18482,
      1755,
      1089,
      1096,
      18710,
      19508,
      1,
      2,
      18886,
      19562
    ))
    stories <- search_api$story_list(
      my_query,
      start_date,
      end_date,
      source_ids = sources
    )[[1]]
  }

  # Set empty DF in case of empty query
  EMPTY_DF <- tibble::tibble(
    language = character(),
    media_name = character(),
    publish_date = character(),
    source = character(),
    title = character(),
    url = character()
  )

  # Convert list of lists to tibble
  stories_to_df <- function(stories) {
    if (is.null(stories) || length(stories) == 0) {
      return(EMPTY_DF)
    }

    df <- purrr::map_dfr(stories, function(x) {
      tibble::tibble(
        language = as.character(rlang::`%||%`(x$language, NA_character_)),
        media_name = as.character(rlang::`%||%`(x$media_name, NA_character_)),
        publish_date = as.character(rlang::`%||%`(
          x$publish_date,
          NA_character_
        )), # <-- force chr
        source = as.character(rlang::`%||%`(x$media_name, NA_character_)),
        title = as.character(rlang::`%||%`(x$title, NA_character_)),
        url = as.character(rlang::`%||%`(x$url, NA_character_))
      )
    })
    df
  }

  # Queries to use
  queries <- c(
    "blackstone language:en",
    "kkr language:en",
    '"apollo global" language:en',
    '"carlyle group" language:en',
    "tpg language:en",
    '"blue owl" language:en',
    "blackrock language:en",
    '"ares management" language:en',
    '(("private market" NOT "private market value") OR "private assets" OR ("alternative investment" OR "alternative investments" OR "alternative investing" OR "alternative investor")) language:en'
  )

  results <- purrr::map(queries, function(q) {
    message(sprintf("[get_news] %s — pulling: %s", Sys.time(), q))
    stories <- mc_news_pull(q) # pull
    out <- stories_to_df(stories) # convert to tibble
    Sys.sleep(60)
    out
  }) |>
    purrr::set_names(
      nm = c(
        "Blackstone",
        "KKR",
        "Apollo Management",
        "Carlyle Group",
        "TPG",
        "Blue Owl",
        "BlackRock",
        "Ares Management",
        "Private Markets"
      )
    )

  all_results <- purrr::imap_dfr(results, ~ dplyr::mutate(.x, query = .y)) |>
    dplyr::mutate(publish_date = as.Date(publish_date)) |>
    dplyr::distinct(url, .keep_all = TRUE) |>
    dplyr::arrange(publish_date)

  sheet_id <- Sys.getenv("MEDIACLOUD_GS_ID")
  target_tab <- "mc_results"

  # Read only existing URLs (fast)
  existing_urls <- tryCatch(
    googlesheets4::read_sheet(
      sheet_id,
      sheet = target_tab,
      range = "F:F",
      col_types = "c"
    ) |>
      dplyr::pull(url),
    error = function(e) character()
  )

  # all_df is your combined, already-deduped + sorted DF
  new_rows <- all_results |>
    dplyr::filter(!is.na(url), url != "") |>
    dplyr::anti_join(tibble::tibble(url = existing_urls), by = "url")

  if (nrow(new_rows) > 0) {
    googlesheets4::sheet_append(sheet_id, data = new_rows, sheet = target_tab)
  }

  # return something useful
  invisible(list(
    appended = nrow(new_rows),
    first_appended = head(new_rows, 1)
  ))
}
