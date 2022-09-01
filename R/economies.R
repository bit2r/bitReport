#' Top 6 Economies in the world by GDP
#' 
#' @description 
#' 세계 상위 6개 경제(GDP 기준)의 주요 지표에 대한 정보를 포함합니다. 
#' 
#' @details 
#' 이 데이터 세트에는 1990년부터 2020년까지 30년의 기간 동안 미국, 중국, 일본, 독일, 영국, 인도를 포함하는 
#' 세계 상위 6개 경제(GDP 기준)의 주요 지표에 대한 데이터가 포함되어 있습니다.
#' 이 데이터 세트는 시계열 분석 및 예측을 수행하는 데 사용할 수 있습니다.
#' 
#' @format 이 데이터 프레임은 17개의 변수를 갖는 180개의 관측치로 구성되었습니다. 개별 변수들은 다음과 같습니다.:
#' \describe{
#'   \item{country_name}{국가 이름}
#'   \item{year}{년도}
#'   \item{gdp}{GDP(미국 달러)}
#'   \item{ppp}{GDP, PPP(국제 달러)}
#'   \item{gdp_per_capita}{1인당 GDP(미국 달러)}
#'   \item{gdp_growth}{GDP 성장률(연간 \%)}
#'   \item{imports_gs}{상품 및 서비스 수입(GDP 대비 \%)}
#'   \item{exports_gs}{상품 및 서비스 수출(GDP 대비 \%)}
#'   \item{debt}{중앙정부 부채 총액(GDP 대비 \%)}
#'   \item{total_reserves}{총 준비금(금, 현재 US$ 포함)}
#'   \item{unemployment}{실업률(총 노동력의 \%)(모델링된 ILO 추정치)}
#'   \item{consumer_prices}{인플레이션, 소비자 물가(연간 \%)}
#'   \item{personal_remittances}{개인 송금, 수령(GDP 대비 \%)}
#'   \item{population}{인구}
#'   \item{population_growth}{인구 증가율(연간 \%)}
#'   \item{life_expectancy}{출생 시 기대 수명, 총계(년)}
#'   \item{poverty_ratio}{하루 $1.90의 빈곤 인구 비율(2011 PPP)(인구 대비 \%)}
#' }
#' @docType data
#' @keywords datasets
#' @name economies
#' @usage data(economies)
#' @source 
#' "Top 6 Economies in the world by GDP" in Kaggle <https://www.kaggle.com/datasets/charanchandrasekaran/top-6-economies-in-the-world-by-gdp>, License : World Bank Dataset Terms of Use(CC BY 4.0)
NULL

# library(dplyr)
# 
# economies <- readr::read_csv("inst/data/top_six_economies.csv")
# 
# economies <- economies %>%
#   select(-1) %>% 
#   rename(
#     "country_name" = 1,
#     "year" = 2,
#     "gdp" = 3,
#     "ppp" = 4,
#     "gdp_per_capita" = 5,
#     "gdp_growth" = 6,
#     "imports_gs" = 7,
#     "exports_gs" = 8,
#     "debt" = 9,
#     "total_reserves" = 10,
#     "unemployment" = 11,
#     "consumer_prices" = 12,
#     "personal_remittances" = 13,
#     "population" = 14,
#     "population_growth" = 15,
#     "life_expectancy" = 16,
#     "poverty_ratio" = 17
#   )
# 
# save(economies, file = "data/economies.rda")
