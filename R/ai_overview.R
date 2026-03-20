# ── R/ai_overview.R ────────────────────────────────────────────────────────────
# Calls GPT-4o to generate an AI overview + recommended studies for a query.
# Returns a parsed list: list(overview = "...", recommendations = list(...))
# Returns NULL silently on any error or missing API key.

build_ai_overview <- function(query, studies_df) {

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (is.null(api_key) || nchar(trimws(api_key)) == 0) return(NULL)
  if (nrow(studies_df) == 0) return(NULL)

  # Send the full catalog so the AI picks independently (cap at 60 studies by relevance proxy)
  ctx_rows <- studies_df |>
    dplyr::arrange(dplyr::desc(gender_score), dplyr::desc(year)) |>
    head(60) |>
    dplyr::mutate(
      safe_url  = tidyr::replace_na(as.character(url),           ""),
      safe_abs  = tidyr::replace_na(as.character(abstract_card), "No abstract."),
      safe_year = tidyr::replace_na(as.character(year),          "?"),
      entry = paste0(
        "TITLE: ",           title,
        " | YEAR: ",         safe_year,
        " | SERIES: ",       collection,
        " | GENDER_SCORE: ", gender_score, "/10",
        " | ACCESS: ",       access_clean,
        " | URL: ",          safe_url,
        " | ABSTRACT: ",     substr(safe_abs, 1, 200)
      )
    ) |>
    dplyr::pull(entry) |>
    paste(collapse = "\n---\n")

  prompt <- paste0(
    'You are a gender data expert for Rwanda supporting CSOs and policy advocates.',
    ' A researcher has typed the search query: "', query, '".\n\n',
    'Below is the COMPLETE catalog of Rwanda NISR national surveys (', nrow(studies_df), ' studies total).',
    ' Your job is to independently read every entry, decide which studies are most relevant',
    ' to the search query, and recommend the 2-3 BEST ones — regardless of any other filter or ranking.',
    ' Do NOT just repeat the top entries. Read the abstracts and titles carefully and pick',
    ' the studies that would genuinely help someone researching "', query, '".\n\n',
    'CATALOG:\n', ctx_rows, '\n\n',
    'Return ONLY a valid JSON object — no markdown, no code fences, no extra text.',
    ' Use exactly this structure:\n',
    '{\n',
    '  "overview": "2-3 sentence expert insight: what Rwanda data exists for this specific topic,',
    ' which survey series cover it best, and why it matters for gender advocacy or policy.",\n',
    '  "recommendations": [\n',
    '    {\n',
    '      "title": "exact study title from the catalog",\n',
    '      "year": "year string",\n',
    '      "series": "survey series name",\n',
    '      "gender_score": "N/10",\n',
    '      "reason": "1-2 specific sentences explaining WHY this study directly answers the query",\n',
    '      "url": "exact url from the catalog entry, empty string if none"\n',
    '    }\n',
    '  ]\n',
    '}\n\n',
    'Rules: pick the 2-3 genuinely most useful studies for the query;',
    ' write reasons that are specific to the query, not generic descriptions;',
    ' return pure JSON only.'
  )

  tryCatch({

    response <- httr::POST(
      url  = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ),
      body = jsonlite::toJSON(
        list(
          model       = "gpt-4o",
          messages    = list(list(role = "user", content = prompt)),
          max_tokens  = 750,
          temperature = 0.3
        ),
        auto_unbox = TRUE
      ),
      httr::timeout(20)
    )

    if (httr::status_code(response) != 200) return(NULL)

    raw <- httr::content(response, "parsed", encoding = "UTF-8")
    raw <- raw$choices[[1]]$message$content

    # Strip accidental markdown code fences if present
    raw <- gsub("^```(json)?\\s*|\\s*```$", "", trimws(raw))

    parsed <- jsonlite::fromJSON(raw, simplifyVector = FALSE)

    # Validate structure
    if (is.null(parsed$overview) || is.null(parsed$recommendations)) return(NULL)

    parsed

  }, error = function(e) {
    message("[GDDP AI] Overview error: ", conditionMessage(e))
    NULL
  })
}
