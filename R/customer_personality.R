#' Customer Personality
#' 
#' @description 
#' 고객의 개인화 분석을 위한 데이터 셋으로 고객의 인구통계, 제품구매, 할인구매, 구매 장소들의 정보를 포함합니다. 
#' 
#' @details 
#' 고객의 개인화 분석은 기업이 고객을 더 잘 이해하고 다양한 유형의 고객의 특정 요구, 
#' 행동 및 우려 사항에 따라 제품의 개선 및 효율적인 마케팅을 수행할 수 있도록 도와줍니다.
#' 
#' @format 이 데이터 프레임은 29개의 변수를 갖는 2240개의 관측치로 구성되었습니다. 개별 변수들은 다음과 같습니다.:
#' \describe{
#'   \item{고객}
#'   \itemize{
#'      \item{ID}: {고객의 고유 식별자}
#'      \item{Year_Birth}: {고객의 생년월일}
#'      \item{Education}: {고객의 학력}
#'      \item{Marital_Status}: {고객의 결혼 상태}
#'      \item{Income}: {고객의 연간 가구 소득}
#'      \item{Kidhome}: {고객 가구의 자녀 수}
#'      \item{Teenhome}: {고객 가구의 청소년 수}
#'      \item{Dt_Customer}: {고객의 회원 가입일자}
#'      \item{Recency}: {고객의 마지막 구매 이후 일수}
#'      \item{Complain}: {고객의 지난 2년 동안 불만 제기 여부. 불만을 제기한 경우 1, 그렇지 않은 경우 0}
#'   }
#'   \item{제품 구매}
#'   \itemize{
#'      \item{MntWines}: {지난 2년 동안 와인 구매 금액}
#'      \item{MntFruits}: {지난 2년 동안 과일 구매 금액}
#'      \item{MntMeatProducts}: {지난 2년 동안 육류 구매 금액}
#'      \item{MntFishProducts}: {지난 2년 동안 생선 구매 금액}
#'      \item{MntSweetProducts}: {지난 2년 동안 제과 구매 금액}
#'      \item{MntGoldProds}: {지난 2년 동안 귀금속 구매 금액}
#'   }
#'   \item{캠페인}
#'   \itemize{
#'     \item{NumDealsPurchases}: {할인상품 구매 횟수}
#'     \item{AcceptedCmp1}: {고객이 첫 번째 캠페인에서 제안을 수락한 경우 1, 그렇지 않은 경우 0}
#'     \item{AcceptedCmp2}: {고객이 두 번째 캠페인에서 제안을 수락한 경우 1, 그렇지 않은 경우 0}
#'     \item{AcceptedCmp3}: {고객이 세 번째 캠페인에서 제안을 수락한 경우 1, 그렇지 않은 경우 0}
#'     \item{AcceptedCmp4}: {고객이 네 번째 캠페인에서 제안을 수락한 경우 1, 그렇지 않은 경우 0}
#'     \item{AcceptedCmp5}: {고객이 다섯 번째 캠페인에서 제안을 수락한 경우 1, 그렇지 않은 경우 0}
#'     \item{Response}: {고객이 지난 캠페인에서 제안을 수락한 경우 1, 그렇지 않은 경우 0}
#'   }   
#'   \item{구매 장소}
#'   \itemize{
#'     \item{NumWebPurchases}: {회사 웹사이트를 통한 구매 건수}
#'     \item{NumCatalogPurchases}: {카칼로그를 사용한 구매 건수}
#'     \item{NumStorePurchases}: {매장에서 직접 구매한 구매 건수}
#'     \item{NumWebVisitsMonth}: {지난 달 회사 웹사이트를 방문한 횟수}
#'   }
#' }
#' @docType data
#' @keywords datasets
#' @name customer_personality
#' @usage data(customer_personality)
#' @source 
#' "Customer Personality Analysis" in Kaggle <https://www.kaggle.com/datasets/imakash3011/customer-personality-analysis>, License : CC0(Public Domain)
NULL

# library(dplyr)
# 
# customer_personality <- readr::read_tsv("inst/data/marketing_campaign.csv")
# 
# customer_personality <- customer_personality %>%
#   mutate(
#     Education = as.factor(Education),
#     Marital_Status = as.factor(Marital_Status),
#     Dt_Customer = as.Date(Dt_Customer, format = "%d-%m-%Y")
#   )
# 
# save(customer_personality, file = "data/customer_personality.rda")
