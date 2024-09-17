#' Query interactions between drugs, targets, and diseases from Open Targets API
#'
#' This function queries the Open Targets Platform to retrieve interactions between drugs,
#' targets, and diseases based on the provided identifier and type (drug, target, or disease).
#' The function sends a GraphQL query to the Open Targets API and returns relevant interaction data.
#'
#' @param query_id A string representing the unique identifier for the query. This could be
#' a ChEMBL ID (for drugs), Ensembl ID (for targets), or EFO ID (for diseases).
#' @param id_type A string representing the type of the identifier. Must be one of "drug",
#' "target", or "disease".
#'
#' @return A data frame containing interaction details between drugs, targets, and diseases, including:
#' \describe{
#'   \item{approvedName}{The approved name of the drug or target.}
#'   \item{approvedSymbol}{The approved symbol for the target.}
#'   \item{targetId}{The target identifier.}
#'   \item{diseaseId}{The disease identifier.}
#'   \item{drugId}{The drug identifier.}
#' }
#' If the query fails, the function returns a list with an error message.
#'
#' @details
#' The function sends a POST request to the Open Targets API with the appropriate GraphQL query
#' based on the provided `id_type`. The function handles the query results and returns relevant
#' data about the interactions between drugs, targets, and diseases.
#'
#' The function supports a maximum of 10,000 results due to API limitations. If an invalid
#' `id_type` is provided, the function will return an error message.
#'
#' @examples
#' \dontrun{
#' # Query interactions for a drug with ChEMBL ID
#' result <- get_interactions("CHEMBL25", "drug")
#'
#' # Query interactions for a target with Ensembl ID
#' result <- get_interactions("ENSG00000169083", "target")
#'
#' # Query interactions for a disease with EFO ID
#' result <- get_interactions("EFO_0000222", "disease")
#' }
#'
#' @import httr jsonlite
#' @export
get_interactions <- function(query_id, id_type) {

  # 构建 GraphQL 查询字符串函数
  get_query_string <- function(id_type) {
    queries <- list(
      drug = '
        query drugTargetIndication($query_id: String!) {
          drug(chemblId: $query_id) {
            name
            description
            isApproved
            knownDrugs(size: 10000) {
              uniqueDrugs
              uniqueTargets
              uniqueDiseases
              count
              rows {
                approvedName
                approvedSymbol
                targetId
                diseaseId
                drugId
              }
            }
          }
        }
      ',
      target = '
        query targetIndicationDrug($query_id: String!) {
          target(ensemblId: $query_id) {
            biotype
            approvedName
            functionDescriptions
            knownDrugs(size: 10000) {
              uniqueDrugs
              uniqueTargets
              uniqueDiseases
              count
              rows {
                approvedName
                approvedSymbol
                targetId
                diseaseId
                drugId
              }
            }
          }
        }
      ',
      disease = '
        query indicationDrugTarget($query_id: String!) {
          disease(efoId: $query_id) {
            id
            name
            description
            knownDrugs(size: 10000) {
              uniqueDrugs
              uniqueTargets
              uniqueDiseases
              count
              rows {
                approvedName
                approvedSymbol
                targetId
                diseaseId
                drugId
              }
            }
          }
        }
      '
    )
    return(queries[[id_type]])
  }

  # 获取对应的 GraphQL 查询字符串
  query_string <- get_query_string(id_type)

  if (is.null(query_string)) {
    message(sprintf("Invalid id_type: %s", id_type))
    return(list(error = sprintf("Invalid id_type: %s", id_type)))
  }

  # 变量
  variables <- list(query_id = query_id)

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
      stop(sprintf("Request failed: %s", http_status(response)$message))
    }

    # 解析 JSON 响应
    api_response <- fromJSON(content(response, "text"))

    # 根据 id_type 动态选择字段
    result <- switch(id_type,
                     drug = api_response[["data"]][["drug"]][["knownDrugs"]][["rows"]],
                     target = api_response[["data"]][["target"]][["knownDrugs"]][["rows"]],
                     disease = api_response[["data"]][["disease"]][["knownDrugs"]][["rows"]])

    # 返回解析后的数据
    return(result)

  }, error = function(e) {
    # 捕捉请求异常并返回错误信息
    message(sprintf("Request failed for query_id %s, id_type %s: %s", query_id, id_type, e$message))
    return(list(error = e$message))

  }, warning = function(w) {
    # 捕捉解析警告
    message(sprintf("Warning occurred for query_id %s, id_type %s: %s", query_id, id_type, w$message))
    return(list(error = w$message))
  })
}
