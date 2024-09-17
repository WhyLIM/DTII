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
search <- function(keywords, size = 10000) {

  # 如果用户输入的 size 超过 10000，强制将其设置为 10000
  if (size > 10000) {
    size <- 10000
    message("Size exceeds the limit of the Open Targets API and has been automatically set to 10000.")
  }

  # GraphQL 查询字符串
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

  # 变量，包括 size 和 keywords
  variables <- list(keywords = keywords, size = size)

  # 基础 URL
  base_url <- "https://api.platform.opentargets.org/api/v4/graphql"

  # 发送请求
  tryCatch({
    response <- POST(
      url = base_url,
      body = list(query = query_string, variables = variables),
      encode = "json"
    )

    # 检查响应状态码
    if (http_status(response)$category != "Success") {
      stop("Request failed: ", http_status(response)$message)
    }

    # 解析 JSON 响应
    api_response <- fromJSON(content(response, "text"))

    # 获取总结果数
    total_hits <- api_response[["data"]][["search"]][["total"]]

    # 如果总结果数超过 10000，提示用户
    if (total_hits > 10000) {
      message(sprintf("%d results found. Due to API limitations, only the first 10,000 are shown.", total_hits))
    }

    # 每种类型实体的数量
    entities_df <- api_response[["data"]][["search"]][["aggregations"]][["entities"]]

    # 创建一个空列表，用于存储 categories 列中的 data.frame
    results_list <- list()

    # 遍历 api_response，将 categories 中的每个 data.frame 存入列表，并以 name 列的值命名
    for (i in seq_along(entities_df$name)) {
      category_name <- entities_df$name[i]   # 取出 name 列的值
      categories_df <- entities_df$categories[[i]]  # 取出 categories 列的 data.frame

      # 将 data.frame 存入列表，并命名为 name 列的值
      results_list[[category_name]] <- categories_df
    }

    # 命中列表
    hits_df <- api_response[["data"]][["search"]][["hits"]]
    results_list$hits <- hits_df

    # 返回解析后的数据列表
    return(results_list)

  }, error = function(e) {
    # 捕捉请求异常并返回错误信息
    message <- paste("Request failed for keywords", keywords, ":", e$message)
    message(message)
    return(list(error = e$message))

  }, warning = function(w) {
    # 捕捉解析警告
    message <- paste("Warning occurred:", w$message)
    message(message)
    return(list(error = w$message))
  })
}
