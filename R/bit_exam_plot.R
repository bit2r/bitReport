#' @importFrom htmltools div
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4",
                      background = NULL) {
  bar <- htmltools::div(style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "8px",
                                       background = background), bar)
  
  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}


gradient_color <- function(x) {
  if (length(x)) return("#416ea4")
  
  blue_pal <- function(x) rgb(colorRamp(c("#9fc7df", "#416ea4"))(x),
                              maxColorValue = 255)
  normalized <- (x - min(x)) / (max(x) - min(x))
  
  blue_pal(normalized)
}

#' @import reactable
#' @import dplyr
#' @export
plot_edu_metric <- function() {
  tab <- customer_personality %>% 
    group_by(Education) %>% 
    summarise(
      n_cust = n(),
      cost_wine = mean(MntWines),
      cost_fruit = mean(MntFruits),
      cost_meat = mean(MntMeatProducts),
      pct_response = round(sum(Response) / n() * 100, 1),
      response_color = gradient_color(pct_response)
    ) 
  
  tab %>% 
    reactable(
      style = list(fontFamily = "NanumSquare"),
      highlight = TRUE,
      outlined = TRUE,
      defaultColDef = colDef(
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        Education = reactable::colDef(name = "교육수준", align = "left", width = 100),
        n_cust = reactable::colDef(name = "고객수", align = "left",
                                   cell = function(value) {
                                     width <- paste0(value / (max(tab$n_cust) + 0) * 100, "%")
                                     value <- value %>% formatC(format = "d", big.mark = ",")
                                     bar_chart(value, width = width, fill = "#008cec", background = "#e1e1e1")
                                   }),
        cost_wine = reactable::colDef(name = "와인 구매금액", align = "left",
                                      cell = function(value) {
                                        width <- paste0(value / (max(tab$cost_wine) + 0) * 100, "%")
                                        value <- value %>% formatC(format = "d", big.mark = ",")
                                        bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
                                      }),
        cost_fruit = reactable::colDef(name = "과일 구매금액", align = "left",
                                       cell = function(value) {
                                         width <- paste0(value / (max(tab$cost_fruit) + 0) * 100, "%")
                                         value <- value %>% formatC(format = "f", digits = 0)
                                         bar_chart(value, width = width, fill = "#32cd32", background = "#e1e1e1")
                                       }),
        cost_meat = reactable::colDef(name = "육류 구매금액", align = "left",
                                      cell = function(value) {
                                        width <- paste0(value / (max(tab$cost_meat) + 0) * 100, "%")
                                        value <- value %>% formatC(format = "d", width = 3,  digits = 0)
                                        bar_chart(value, width = width, fill = "#b070c8", background = "#e1e1e1")
                                      }),
        pct_response = reactable::colDef(
          name = "캠페인 응답률",
          defaultSortOrder = "desc",
          cell = htmlwidgets::JS("function(cellInfo) {
        const sliceColor = cellInfo.row['response_color']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value / 100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\">' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '%' + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }"),
      html = TRUE,
      align = "center",
      width = 140,
      class = "user-score"
        ),
      response_color = reactable::colDef(show = FALSE)
      ),
      theme = reactable::reactableTheme(
        highlightColor = "#f3fafb",
        borderColor = "hsl(0, 0%, 93%)",
        headerStyle = list(borderColor = "hsl(0, 0%, 90%)"),
        # Vertically center cells
        cellStyle = list(display = "flex", flexDirection = "column",
                         justifyContent = "center")
      )
    )
}

# Render a bar chart with positive and negative values
#' @importFrom htmltools div tagAppendChild
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "16px",
                              pos_fill = "#005ab5", neg_fill = "#dc3220") {
  neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
  pos_chart <- htmltools::div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")
  
  if (value < 0) {
    bar <- htmltools::div(style = list(marginLeft = "8px", background = neg_fill, width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
    neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
  } else {
    bar <- htmltools::div(style = list(marginRight = "8px", background = pos_fill, width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
  }
  
  htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)
}

#' @export
#' @import dplyr
#' @import reactable
#' @importFrom tidyr pivot_wider pivot_longer
plot_edu_increse <- function(bar_ratio = 1, page_size = 20) {
  tab <- customer_personality %>% 
    mutate(yearmonth = substr(Dt_Customer, 1, 7)) %>% 
    group_by(Education, yearmonth) %>% 
    summarise_at(vars(starts_with("Mnt")), sum) %>% 
    tidyr::pivot_longer(cols = starts_with("Mnt"), 
                        names_to = "product",
                        values_to = "amount") %>% 
    filter(yearmonth %in% c("2014-05", "2014-06")) %>% 
    tidyr::pivot_wider(names_from = "yearmonth",
                       values_from = "amount") %>% 
    mutate(product = case_when(
      product %in% "MntWines" ~ "와인 구매금액",
      product %in% "MntFruits" ~ "과일 구매금액",
      product %in% "MntMeatProducts" ~ "육류 구매금액",
      product %in% "MntFishProducts" ~ "생선 구매금액",      
      product %in% "MntSweetProducts" ~ "제과 구매금액",      
      product %in% "MntGoldProds" ~ "귀금속 구매금액")
    ) %>%
    filter(!product %in% c("제과 구매금액", "귀금속 구매금액")) %>% 
    mutate(profit = round(`2014-06` - `2014-05`) / `2014-05`) %>% 
    ungroup()
  
  tab %>% reactable(
    bordered = TRUE,
    highlight = TRUE,
    striped = TRUE,
    style = list(fontFamily = "NanumSquare"),
    defaultPageSize = page_size,
    defaultColDef = colDef(
      headerStyle = list(background = "#f7f7f8")
    ),
    rowStyle = function(index) {
      if (index == 1) 
        list(background = "rgba(255, 165, 0, 0.05)")
    },
    columns = list(
      Education = colDef(name = "교육수준", minWidth = 70),
      product = colDef(name = "구매금액 종류", minWidth = 120),
      `2014-05` = colDef(name = "2014-05", format = colFormat(separators = TRUE),
                    minWidth = 70),
      `2014-06` = colDef(name = "2014-06", format = colFormat(separators = TRUE),
                    minWidth = 70),
      profit = colDef(
        name = "증감비율 (%)",
        defaultSortOrder = "desc",
        cell = function(value) {
          label <- paste0(round(value * 100), "%")
          bar_chart_pos_neg(label, value * bar_ratio)
        },
        align = "center",
        minWidth = 180
      )
    )
  )
}


#' 국가 통계의 년간 트랜드 플롯
#'
#' @description plot_country_trend()은 국가의 메트릭을 년간 시계열 플롯으로 시각화한다.
#'
#' @details 국가 통계의 년간 트랜드 플롯은 World Bank 데이터인 economies를 이용한 시계열 라인플롯이다.
#'
#' @param start_year character. 통계 시작 년도.
#' @param end_year character. 통계 종료 년도.
#' @param country character. 시각화할 국가 이름
#' @param metric character. 시각화할 메트릭.
#' @details
#' country 종류는 다음과 같다.
#' \itemize{
#'   \item "United States" : 미국
#'   \item "China" : 중국
#'   \item "Japan" : 일본
#'   \item "Germany" : 독일
#'   \item "United Kingdom" : 영국
#'   \item "India" : 인도
#' }
#' metric 종류는 다음과 같다.
#' \itemize{
#'   \item "gdp" : GDP
#'   \item "gdp_per_capita" : 인당 GDP
#'   \item "gdp_growth" : GDP 성장률
#'   \item "imports_gs" : 상품/서비스 수입
#'   \item "exports_gs" : 상품/서비스 수출
#'   \item "debt" : 국개 채무
#'   \item "unemployment" : 실업률
#'   \item "population" : 인구
#'   \item "population_growth" : 인구증가율
#'   \item "life_expectancy" : 기대수명
#' }
#' @examples
#' plot_country_trend()
#'
#' plot_country_trend(metric = "population")
#'
#' plot_country_trend(start_year = 1995, metric = "life_expectancy")
#'
#' plot_country_trend(metric = "unemployment", country = "United States")
#' plot_country_trend(metric = "unemployment", country = c("China", "Japan"))
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom scales label_number cut_short_scale breaks_pretty
#' @export
plot_country_trend <- function(start_year = 1990, end_year = 2020, country = NULL,
                               metric = c("gdp", "gdp_per_capita", "gdp_growth",
                                          "imports_gs", "exports_gs", "debt", 
                                          "unemployment", "population", 
                                          "population_growth", "life_expectancy")) {
  metric <-match.arg(metric)
  
  main_title <- case_when(
    metric %in% "gdp" ~ "GDP 추이현황",
    metric %in% "gdp_per_capita" ~ "인당 GDP 추이 현황",
    metric %in% "gdp_growth" ~ "GDP 성장률 추이 현황",
    metric %in% "imports_gs" ~ "상품/서비스 수입 추이 현황",
    metric %in% "exports_gs" ~ "상품/서비스 수출 추이현황",
    metric %in% "debt" ~ "국개 채무 추이 현황",
    metric %in% "unemployment" ~ "실업률 추이 현황",
    metric %in% "population" ~ "인구 추이 현황",
    metric %in% "population_growth" ~ "인구증가율 추이 현황",
    metric %in% "life_expectancy" ~ "기대수명 추이 현황"  
  )
  
  cap_title <- "Designed by bitReport with World Bank datas"
  
  if (is.null(country)) {
    country <- c("United States", "China", "Japan", "Germany", 
                 "United Kingdom", "India")
  }
  
  if (length(country) > 1) {
    economies %>%
      select(year, country_name, metric = metric) %>%
      filter(country_name %in% country) %>%
      filter(year >= start_year) %>%
      filter(year <= end_year) %>%
      mutate(country_name = case_when(
        country_name %in% "United States" ~ "미국",
        country_name %in% "China" ~ "중국",
        country_name %in% "Japan" ~ "일본",
        country_name %in% "Germany" ~ "독일",
        country_name %in% "United Kingdom" ~ "영국",
        country_name %in% "India" ~ "인도"
      )) %>%
      ggplot2::ggplot(aes(x = year, y = metric, color = country_name)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, size = 1, colour = bit_grey) +
      labs(title = main_title,
           caption = cap_title) +
      scale_y_continuous(label = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_continuous(breaks = pretty(economies$year, n = 7)) +      
      bit_theme(grid = "Y", base_family = "NanumSquare") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  } else {
    country_name <- case_when(
      country %in% "United States" ~ "미국",
      country %in% "China" ~ "중국",
      country %in% "Japan" ~ "일본",
      country %in% "Germany" ~ "독일",
      country %in% "United Kingdom" ~ "영국",
      country %in% "India" ~ "인도"
    )
    
    main_title <- paste(country_name, main_title)
    
    economies %>%
      select(year, country_name, metric = metric) %>%
      filter(country_name %in% country) %>%
      filter(year >= start_year) %>%
      filter(year <= end_year) %>%
      ggplot2::ggplot(aes(x = year, y = metric)) +
      geom_line(colour = bit_blue, size = 1) +
      geom_hline(yintercept = 0, size = 1, colour = bit_grey) +
      labs(title = main_title,
           caption = cap_title) +
      scale_y_continuous(scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_continuous(breaks = pretty(economies$year, n = 7)) +
      bit_theme(grid = "Y", base_family = "NanumSquare") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  }
}





gradient_color <- function(x) {
  idx_pos <- which(x >= 0)
  idx_neg <- which(x < 0)  

  blue_pal <- function(x) rgb(colorRamp(c("#9fc7df", "#416ea4"))(x),
                              maxColorValue = 255)
  
  red_pal <- function(x) rgb(colorRamp(c("#ffcccc", "#e60000"))(x),
                             maxColorValue = 255)
  
  if (length(idx_pos) == 1) {
    normalized_pos <- "#416ea4"
  } else if (length(idx_pos) > 1) {
    normalized_pos <- blue_pal((x[idx_pos] - min(x[idx_pos])) / 
      (max(x[idx_pos]) - min(x[idx_pos])))
  }
  
  if (length(idx_neg) == 1) {
    normalized_neg <- "#e60000"
  } else if (length(idx_neg) > 1) {
    normalized_neg <- red_pal((x[idx_neg] - min(x[idx_neg])) / 
      (max(x[idx_neg]) - min(x[idx_neg])))
  }
  
  if (length(idx_pos) >= 1) {
    x[idx_pos] <- normalized_pos
  }
  
  if (length(idx_neg) >= 1) {
    x[idx_neg] <- normalized_neg  
  }

  x
}

#' @importFrom htmltools div
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4",
                      background = NULL) {
  bar <- htmltools::div(style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "8px",
                                       background = background), bar)
  
  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}



#' 국가 통계의 집계 테이블 출력
#'
#' @description table_country_metric()는 국가의 메트릭을 플롯을 포함한 테이블로 표현한다.
#'
#' @details 집계 테이블은 시각화를 포함한 집계표로 인구, 기대수명, GDP, 1인당 GDP, 
#' GDP 증가율을 출력한다.
#'
#' @param base_year character. 통계 년도.
#' @param country character. 시각화할 국가 이름.
#' @details
#' country 종류는 다음과 같다.
#' \itemize{
#'   \item "United States" : 미국
#'   \item "China" : 중국
#'   \item "Japan" : 일본
#'   \item "Germany" : 독일
#'   \item "United Kingdom" : 영국
#'   \item "India" : 인도
#' }
#' @examples
#' table_country_metric()
#'
#' table_country_metric(base_year = 2015)
#'
#' table_country_metric(country = c("United States", "Germany", "Japan"))
#'
#' @import dplyr
#' @importFrom reactable reactable colDef reactableTheme
#' @importFrom htmlwidgets JS
#' @export
table_country_metric <- function(base_year = 2020, country = NULL) {
  metrics <- c("population", "life_expectancy", "gdp", "gdp_per_capita", "gdp_growth")
  
  if (is.null(country)) {
    country <- c("United States", "China", "Japan", "Germany", 
                 "United Kingdom", "India")
  }
  
  tab <- economies %>%
    filter(year %in% base_year) %>%
    filter(country_name %in% country) %>%
    mutate(country_name = case_when(
      country_name %in% "United States" ~ "미국",
      country_name %in% "China" ~ "중국",
      country_name %in% "Japan" ~ "일본",
      country_name %in% "Germany" ~ "독일",
      country_name %in% "United Kingdom" ~ "영국",
      country_name %in% "India" ~ "인도"
    )) %>%
    select(country_name, matches(metrics)) %>%
    mutate(gdp_growth_color = gradient_color(gdp_growth),
           gdp = round(gdp / 1000000),
           gdp_growth = round(gdp_growth, 1)) %>%
    select(country_name, population, life_expectancy, gdp, gdp_per_capita,
           gdp_growth, gdp_growth_color) %>%
    arrange(country_name)
  
  tab %>%
    reactable(
      style = list(fontFamily = "NanumSquare"),
      highlight = TRUE,
      outlined = TRUE,
      defaultColDef = colDef(
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        country_name = reactable::colDef(name = "국가", align = "left", width = 100),
        population = reactable::colDef(name = "인구", align = "left",
                                   cell = function(value) {
                                     width <- paste0(value / (max(tab$population) + 20000) * 100, "%")
                                     value <- value %>% formatC(format = "d", big.mark = ",")
                                     bar_chart(value, width = width, fill = "#008cec", background = "#e1e1e1")
                                   }),
        life_expectancy = reactable::colDef(name = "기대수명", align = "left",
                                      cell = function(value) {
                                        width <- paste0(value / (max(tab$life_expectancy) + 0) * 100, "%")
                                        value <- value %>% formatC(format = "d", big.mark = ",")
                                        bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
                                      }),
        gdp = reactable::colDef(name = "GDP (단위 1백만$)", align = "left",
                                             cell = function(value) {
                                               width <- paste0(value / (max(tab$gdp) + 0.5) * 100, "%")
                                               value <- value %>% formatC(format = "d", big.mark = ",")
                                               bar_chart(value, width = width, fill = "#32cd32", background = "#e1e1e1")
                                             }),
        gdp_per_capita = reactable::colDef(name = "1인당 GDP", align = "left",
                                         cell = function(value) {
                                           width <- paste0(value / max(tab$gdp_per_capita) * 100, "%")
                                           value <- value %>% formatC(format = "d", big.mark = ",")
                                           bar_chart(value, width = width, fill = "#b070c8", background = "#e1e1e1")
                                         }),
        gdp_growth = reactable::colDef(
          name = "GDP 증가율",
          defaultSortOrder = "desc",
          cell = htmlwidgets::JS("function(cellInfo) {
        const sliceColor = cellInfo.row['gdp_growth_color']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value / 100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\">' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '%' + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }"),
      html = TRUE,
      align = "center",
      width = 140,
      class = "user-score"
        ),
      gdp_growth_color = reactable::colDef(show = FALSE)
      ),
      theme = reactable::reactableTheme(
        highlightColor = "#f3fafb",
        borderColor = "hsl(0, 0%, 93%)",
        headerStyle = list(borderColor = "hsl(0, 0%, 90%)"),
        # Vertically center cells
        cellStyle = list(display = "flex", flexDirection = "column",
                         justifyContent = "center")
      ))
}

custom_color_bar <- function (color = "lightgray", fixed_width = 150, ...) {
  formatter("span", style = function(x) {
    if (stringr::str_detect(x, ":") %>% any()) {
      widths <- paste(fixed_width * proportion(as.integer(hms2sec(x))),
                      "px", sep = "")
    } else {
      widths <- paste(fixed_width * proportion(as.numeric(gsub("%|,", "", x))),
                      "px", sep = "")
    }
    style(display = "inline-block", direction = "rtl", `border-radius` = "4px",
          `padding-right` = "2px", `background-color` = csscolor(color),
          width = widths, ...)
  })
}



#' 국가 통계의 formattable 테이블 출력
#'
#' @description formatable_country_metric()은 국가 통계를 formattable 패키지를 사용하여, 테이블로 표현한다.
#'
#' @details 집계 테이블은 시각화를 포함한 집계표로 인구, 기대수명, GDP, 1인당 GDP, 
#' GDP 증가율을 출력한다.
#'
#' @param base_year character. 통계 년도.
#' @param country character. 시각화할 국가 이름.
#' @param fixed_width integer. 컬럼의 너비
#' @details
#' country 종류는 다음과 같다.
#' \itemize{
#'   \item "United States" : 미국
#'   \item "China" : 중국
#'   \item "Japan" : 일본
#'   \item "Germany" : 독일
#'   \item "United Kingdom" : 영국
#'   \item "India" : 인도
#' }
#' @examples
#' formatable_country_metric()
#'
#' formatable_country_metric(base_year = 2015)
#'
#' formatable_country_metric(country = c("United States", "Germany", "Japan"))
#'
#' @import dplyr
#' @import formattable
#' @export
formatable_country_metric <- function(base_year = 2020, country = NULL,
                             fixed_width = 100) {
  metrics <- c("population", "life_expectancy", "gdp", "gdp_per_capita", "gdp_growth")
  
  if (is.null(country)) {
    country <- c("United States", "China", "Japan", "Germany", 
                 "United Kingdom", "India")
  }
  
  economies %>%
    filter(year %in% base_year) %>%
    filter(country_name %in% country) %>%
    mutate(country_name = case_when(
      country_name %in% "United States" ~ "미국",
      country_name %in% "China" ~ "중국",
      country_name %in% "Japan" ~ "일본",
      country_name %in% "Germany" ~ "독일",
      country_name %in% "United Kingdom" ~ "영국",
      country_name %in% "India" ~ "인도"
    )) %>%
    select(country_name, metrics) %>%
    mutate(population = comma(population / 1000, digits = 0),
           life_expectancy = comma(life_expectancy, digits = 1),
           gdp = comma(gdp / 1000000, digits = 0),
           gdp_per_capita = comma(gdp_per_capita, digits = 0),
           gdp_growth = percent(gdp_growth / 100, format = "f", digits = 1)) %>%
    arrange(country_name) %>%
    rename("국가" = country_name,
           "인구(천명)" = population,
           "기대수명" = life_expectancy,
           "GDP (단위 1백만$)" = gdp,
           "1인당 GDP" = gdp_per_capita,
           "GDP 증가율" = gdp_growth) %>%
    formattable(list(
      "인구(천명)" = custom_color_bar("#29BF1255", fixed_width),
      "기대수명" = custom_color_bar("#00A5CF55", fixed_width),
      "GDP (단위 1백만$)" = custom_color_bar("#DE1A1A55", fixed_width),
      "1인당 GDP" = custom_color_bar("#574AE255", fixed_width),
      "GDP 증가율" = custom_color_bar("#FFBF0055", fixed_width)))
}



# Render a bar chart with positive and negative values
#' @importFrom htmltools div
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "16px",
                              pos_fill = "#005ab5", neg_fill = "#dc3220") {
  neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
  pos_chart <- htmltools::div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")
  
  if (value < 0) {
    bar <- htmltools::div(style = list(marginLeft = "8px", background = neg_fill, width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- htmltools::div(style = list(marginRight = "8px", background = pos_fill, width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)
}


#' KPI 메트릭의 전월과의 성장률 비교
#'
#' @description compare_country_metric()은 월별로 집계된 마트의 KPI 메트릭을 전월과 비교하여, 그 증감률을 테이블로 표현한다.
#'
#' @details 집계 테이블은 월별로 집계된 GA KPI 마트를 이용한 시각화를 포함한 집계표로 일평균 사용자수,
#' 일평균 세션수, 평균 세션시간, 일평균 신규 사용자수, 일평균 신규 세션수, 신규세션 비율을 출력한다.
#'
#' @param prev_year numeric. 이전 년도.
#' @param curr_year numeric. 기준 년도.
#' @param platform_cd character. 표시할 대상의 플랫폼 코드.
#' @param metric character. 집계할 메트릭.
#' @param bar_ratio numeric. 증감비율로 표현할 막대의 가중치. 기본값은 1이다.
#' @param page_size integer. 한 페이지에 출력할 레코드 수.
#' @param country_band logical. 플랫폼 이름에 밴드를 지정할지의 여부. 기본값은 FALSE, TRUE이면, 짝수번쨰 플랫폼에 회색 밴드를 표시한다.
#' @details
#' platform_cd 종류는 다음과 같다.
#' \itemize{
#'   \item "ONS" : 온슈어
#'   \item "HMP" : 홈페이지
#'   \item "IWL" : 보험월렛
#' }
#' metric은 집계 지표로 다음과 같다.
#' \itemize{
#'   \item "n_user" : 일평균 사용자수 (명)
#'   \item "n_session" : 일평균 세션수 (건)
#'   \item "timeonsite" : 평균 세션시간 (초)
#'   \item "n_newuser" : 일평균 신규 사용자수 (명)
#'   \item "n_newsession" : 일평균 신규 세션수 (건)
#'   \item "newsession_rate" : 신규세션 비율 (%)
#' }
#' @examples
#' compare_country_metric()
#'
#' compare_country_metric(bar_ratio = 2, page_size = 18)
#'
#' compare_country_metric(metrics = c("n_user", "n_session"))
#'
#' compare_country_metric(bar_ratio = 1.5, page_size = 18, country_band = TRUE)
#'
#' @import lubridate
#' @import htmltools
#' @import reactable
#' @export
#'
compare_country_metric <- function(prev_year = 2019, curr_year = 2020,
                                   country = NULL,
                          metrics = c("population", "life_expectancy", "gdp_per_capita", "unemployment",
                                      "imports_gs", "exports_gs"),
                          bar_ratio = 1, page_size = 10, country_band = FALSE) {
  if (is.null(country)) {
    country <- c("United States", "China", "Japan", "Germany", 
                 "United Kingdom", "India")
  }
  
  country_nm <- case_when(
    country %in% "United States" ~ "미국",
    country %in% "China" ~ "중국",
    country %in% "Japan" ~ "일본",
    country %in% "Germany" ~ "독일",
    country %in% "United Kingdom" ~ "영국",
    country %in% "India" ~ "인도") %>%
    sort()
    
  band <- country_nm[!(seq(length(country_nm)) %% 2)]
  if (!country_band) {
    band <- NULL
  }
  
  tab <- economies %>%
    filter(year %in% c(prev_year, curr_year)) %>%
    filter(country_name %in% country) %>%
    select(country_name, year, metrics) %>%
    mutate(country_name = case_when(
      country_name %in% "United States" ~ "미국",
      country_name %in% "China" ~ "중국",
      country_name %in% "Japan" ~ "일본",
      country_name %in% "Germany" ~ "독일",
      country_name %in% "United Kingdom" ~ "영국",
      country_name %in% "India" ~ "인도"
    )) %>%
    arrange(country_name, year) %>%
    group_by(country_name) %>%
    mutate(
      prev_n_user = round(lag(n_user) / nday_prev),
      curr_n_user = round(n_user / nday_curr),
      prev_n_session = round(lag(n_session) / nday_prev),
      curr_n_session = round(n_session / nday_curr),
      prev_n_newuser = round(lag(n_newuser) / nday_prev),
      curr_n_newuser = round(n_newuser / nday_curr),
      prev_n_newsession = round(lag(n_newsession) / nday_prev),
      curr_n_newsession = round(n_newsession / nday_curr),
      prev_timeonsite = round(lag(timeonsite) / lag(n_session)),
      curr_timeonsite = round(timeonsite / n_session),
      prev_newsession_rate = round(lag(n_newsession) / lag(n_session) * 100),
      curr_newsession_rate = round(n_newsession / n_session * 100)
    ) %>%
    select(platform, prev_n_user:curr_newsession_rate) %>%
    filter(!is.na(prev_n_user)) %>%
    mutate(profit_n_user = (curr_n_user - prev_n_user) / prev_n_user,
           profit_n_session = (curr_n_session - prev_n_session) / prev_n_session,
           profit_n_newuser = (curr_n_newuser - prev_n_newuser) / prev_n_newuser,
           profit_n_newsession = (curr_n_newsession - prev_n_newsession) / prev_n_newsession,
           profit_timeonsite = (curr_timeonsite - prev_timeonsite) / prev_timeonsite,
           profit_newsession_rate = (prev_newsession_rate - curr_newsession_rate) /
             curr_newsession_rate) %>%
    select(platform, contains(metrics)) %>%
    ungroup()
  
  tab <- tab %>%
    select(platform, contains("prev_")) %>%
    rename_all(function(x) sub("prev_", "", x)) %>%
    tidyr::gather(key = "metric", value = "prev", -platform) %>%
    inner_join(
      tab %>%
        select(platform, contains("curr_")) %>%
        rename_all(function(x) sub("curr_", "", x)) %>%
        tidyr::gather(key = "metric", value = "curr", -platform),
      by = c("platform", "metric")
    ) %>%
    inner_join(
      tab %>%
        select(platform, contains("profit_")) %>%
        rename_all(function(x) sub("profit_", "", x)) %>%
        tidyr::gather(key = "metric", value = "profit", -platform),
      by = c("platform", "metric")
    ) %>%
    mutate(metric = case_when(
      metric %in% "n_user" ~ "일평균 사용자수",
      metric %in% "n_session" ~ "일평균 세션수",
      metric %in% "n_newuser" ~ "일평균 신규 사용자수",
      metric %in% "n_newsession" ~ "일평균 신규 세션수",
      metric %in% "newsession_rate" ~ "신규세션 비율",
      metric %in% "timeonsite" ~ "평균 세션시간(초)"
    )) %>%
    arrange(platform)
  
  tab %>% reactable(
    bordered = TRUE,
    highlight = TRUE,
    striped = !country_band,
    style = list(fontFamily = "NanumSquare"),
    defaultPageSize = page_size,
    defaultColDef = colDef(
      headerStyle = list(background = "#f7f7f8")
    ),
    rowStyle = function(index) {
      if (index == 2) list(fontWeight = "bold")
      else if (tab[index, "platform"] %in% band)
        list(background = "rgba(255, 165, 0, 0.05)")
    },
    columns = list(
      platform = colDef(name = "플랫폼", minWidth = 70),
      metric = colDef(name = "KPIs", minWidth = 120),
      prev = colDef(name = prev_year, format = colFormat(separators = TRUE),
                    minWidth = 70),
      curr = colDef(name = curr_year, format = colFormat(separators = TRUE),
                    minWidth = 70),
      profit = colDef(
        name = "증감비율 (%)",
        defaultSortOrder = "desc",
        cell = function(value) {
          label <- paste0(round(value * 100), "%")
          bar_chart_pos_neg(label, value * bar_ratio)
        },
        align = "center",
        minWidth = 180
      )
    )
  )
}



