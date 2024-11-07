#' Search for entities using the Open Targets GraphQL API
#'
#' This function queries the Open Targets Platform using GraphQL based on the provided
#' keyword and returns entities related to the search term. The function supports
#' limiting the number of returned results due to API limitations.
#'
#' @param keywords A string representing the search term to query the Open Targets Platform.
#' @param size An integer representing the number of results to return (default is 10000).
#' Maximum value allowed by the Open Targets API is 10000.
#'
#' @return A list containing the search results, including entity names and their associated categories.
#' The list also contains search hits with detailed information about each hit. If the query fails,
#' an error message is returned.
#'
#' @details
#' The Open Targets API has a limit of 10000 results per query. If the total number of hits exceeds 10000,
#' the function will only return the first 10000 results, and a message will be printed to inform the user.
#' The results include entity names and categories, which are stored in a list, and a separate hits data frame.
#'
#' If the size parameter exceeds 10000, the function will automatically adjust it to 10000 and provide
#' a message to notify the user of this adjustment.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- search("cancer")
#'
#' # Query with a specific size:
#' result <- search("diabetes", size = 5000)
#' }
#'
#' @import httr jsonlite
#' @export
search <- function(keywords = NULL, size = 10000) {

  # Check if keyword is empty
  if (is.null(keywords)) {
    stop("Please provide a keyword to query.")
  }

  # If the size entered by the user exceeds 10000, force it to be set to 10000
  if (size > 10000) {
    size <- 10000
    message("Size exceeds the limit of the Open Targets API and has been automatically set to 10000.")
  }

  # GraphQL query string
  query_string <- '
  query searchEntities($keywords: String!, $size: Int!) {
    search(queryString: $keywords, page: {index: 0, size: $size}) {
      aggregations {
        total
        entities {
          name
          total
          categories {
            name
            total
          }
        }
      }
      hits {
        id
        entity
        name
      }
      total
    }
  }
  '

  # variables
  variables <- list(keywords = keywords, size = size)

  # base URL
  base_url <- "https://api.platform.opentargets.org/api/v4/graphql"

  # Send request
  tryCatch({
    response <- POST(
      url = base_url,
      body = list(query = query_string, variables = variables),
      encode = "json"
    )

    # Check response status code
    if (http_status(response)$category != "Success") {
      stop("Request failed: ", http_status(response)$message)
    }

    # Parse JSON response
    api_response <- fromJSON(content(response, "text"))

    # Get the total number of results
    total_hits <- api_response[["data"]][["search"]][["total"]]

    # If the total number of results exceeds 10,000, prompt the user
    if (total_hits > 10000) {
      message(sprintf("%d results found. Due to API limitations, only the first 10,000 are shown.", total_hits))
    }

    # Number of entities of each type
    entities_df <- api_response[["data"]][["search"]][["aggregations"]][["entities"]]

    # Create an empty list to store the categories column in the data.frame
    results_list <- list()

    # Traverse api_response, store each data.frame in categories into a list, and name it with the value of the name column
    for (i in seq_along(entities_df$name)) {
      category_name <- entities_df$name[i]   # Get the value of the name column
      categories_df <- entities_df$categories[[i]]  # Get the data.frame of categories column

      # Save data.frame into a list and name it the value of the name column
      results_list[[category_name]] <- categories_df
    }

    # hit list
    hits_df <- api_response[["data"]][["search"]][["hits"]]
    results_list$hits <- hits_df

    # Returns the parsed data list
    return(results_list)

  }, error = function(e) {
    # Capture request exceptions and return error information
    message <- paste("Request failed for keywords", keywords, ":", e$message)
    message(message)
    return(list(error = e$message))

  }, warning = function(w) {
    # Catching parsing warnings
    message <- paste("Warning occurred:", w$message)
    message(message)
    return(list(error = w$message))
  })
}
