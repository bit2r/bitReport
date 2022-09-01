#' KPI 메트릭의 일 트랜드 플롯
#'
#' @description plot_d_kpi()은 일별로 집계된 마트의 KPI 메트릭을 시계열 플롯으로 시각화한다.
#'
#' @details 일 트랜드 플롯은 일별로 집계된 GA KPI 마트를 이용한 시계열 라인플롯이다.
#'
#' @param mart_kpi data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param start_date character. 통계 시작 일자.
#' @param end_date character. 통계 종료 일자.
#' @param platform_cd character. 시각화할 플랫폼 코드.
#' @param metric character. 시각화할 메트릭.
#' @param moving character. 메트릭 집계 주기
#' @details
#' platform_cd 종류는 다음과 같다.
#' \itemize{
#'   \item "ONS" : 온슈어
#'   \item "HMP" : 홈페이지
#'   \item "IWL" : 보험월렛
#' }
#' metric 종류는 다음과 같다.
#' \itemize{
#'   \item "n_user" : 활성 사용자
#'   \item "n_newuser" : 신규 사용자
#'   \item "n_session" : 세션수
#'   \item "n_newsession" : 신규 세션수
#'   \item "n_screen" : 화면수
#'   \item "n_page" : 페이지수
#'   \item "tot_timeonsite" : 세션 시간
#'   \item "tot_timeonscreen" : 스크린 시간
#' }
#' moving 종류는 집계 기간으로 다음과 같다.
#' \itemize{
#'   \item "d1" : 일 집계
#'   \item "w1" : 7일 집계
#'   \item "w2" : 14일 집계
#'   \item "w3" : 21일 집계
#'   \item "w4" : 28일 집계
#'   \item "m1" : 월 집계
#' }
#' @examples
#' plot_d_kpi(TB_D_KPIS)
#'
#' plot_d_kpi(TB_D_KPIS, metric = "n_session", moving = "w4")
#'
#' plot_d_kpi(TB_D_KPIS, start_date = "2021-01-01", metric = "n_session",
#'            moving = "w1")
#'
#' plot_d_kpi(TB_D_KPIS, metric = "n_session", platform_cd = "IWL")
#' plot_d_kpi(TB_D_KPIS, metric = "n_session", platform_cd = c("IWL", "HMP"))
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom scales label_number cut_short_scale breaks_pretty
#' @export
plot_d_kpi <- function(mart_kpi,
                       start_date = "2021-03-01", end_date = "2021-03-31",
                       platform_cd = NULL,
                       metric = c("n_user", "n_newuser", "n_session",
                                  "n_newsession", "n_screen", "n_page",
                                  "tot_timeonsite", "tot_timeonscreen")[1],
                       moving = c("d1", "w1", "w2", "w3", "w4", "m1")[5]) {
  main_title <- case_when(
    metric %in% "n_user" ~ "활성 사용자 추이현황",
    metric %in% "n_newuser" ~ "신규 사용자 추이 현황",
    metric %in% "n_session" ~ "세션 추이 현황",
    metric %in% "n_newsession" ~ "신규세션 추이 현황",
    metric %in% "n_screen" ~ "스크린 뷰 추이현황",
    metric %in% "n_page" ~ "페이지 뷰 추이 현황",
    metric %in% "tot_timeonsite" ~ "세션 시간 추이 현황",
    metric %in% "tot_timeonscreen" ~ "페이지 시간 추이 현황"
  )

  sub_title <- case_when(
    moving %in% "d1" ~ "일별 집계",
    moving %in% "w1" ~ "일별 7일(1W) 이동 집계",
    moving %in% "w2" ~ "일별 14일(2W) 이동 집계",
    moving %in% "w3" ~ "일별 21일(3W) 이동 집계",
    moving %in% "w4" ~ "일별 28일(4W) 이동 집계",
    moving %in% "m1" ~ "일별 월 누적 집계"
  )
  cap_title <- "Designed by Data Analytics Team with GA360 logs"

  metric <- paste(metric, moving, sep = "_")

  if (is.null(platform_cd)) {
    platform_cd <- c("ONS", "HMP", "IWL")
  }

  if (length(platform_cd) > 1) {
    mart_kpi %>%
      select(date, platform, metric = starts_with(metric)) %>%
      filter(platform %in% platform_cd) %>%
      filter(date >= start_date) %>%
      filter(date <= end_date) %>%
      mutate(platform = case_when(
        platform %in% "ONS" ~ "온슈어",
        platform %in% "HMP" ~ "홈페이지",
        platform %in% "IWL" ~ "보험월렛")) %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
      ggplot2::ggplot(aes(x = date, y = metric, color = platform)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, size = 1, colour = bit_grey) +
      labs(title = main_title,
           subtitle = sub_title,
           caption = cap_title) +
      scale_y_continuous(label = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%m/%d") +
      bit_theme(grid = "Y", base_family = "NanumSquare") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  } else {
    platform <- case_when(
      platform_cd %in% "ONS" ~ "온슈어",
      platform_cd %in% "HMP" ~ "홈페이지",
      platform_cd %in% "IWL" ~ "보험월렛"
    )

    main_title <- paste(platform, main_title)

    mart_kpi %>%
      select(date, platform, metric = starts_with(metric)) %>%
      filter(platform %in% platform_cd) %>%
      filter(date >= start_date) %>%
      filter(date <= end_date) %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
      ggplot2::ggplot(aes(x = date, y = metric)) +
      geom_line(colour = bit_blue, size = 1) +
      geom_hline(yintercept = 0, size = 1, colour = bit_grey) +
      labs(title = main_title,
           subtitle = sub_title,
           caption = cap_title) +
      scale_y_continuous(scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%m/%d") +
      bit_theme(grid = "Y", base_family = "NanumSquare") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  }
}

gradient_color <- function(x) {
  if (length(x)) return("#416ea4")

  blue_pal <- function(x) rgb(colorRamp(c("#9fc7df", "#416ea4"))(x),
                              maxColorValue = 255)
  normalized <- (x - min(x)) / (max(x) - min(x))

  blue_pal(normalized)
}

#' @importFrom htmltools div
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4",
                      background = NULL) {
  bar <- htmltools::div(style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "8px",
                            background = background), bar)

  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}



#' KPI 메트릭의 집계 테이블 출력
#'
#' @description table_d_kpi()은 일별로 집계된 마트의 KPI 메트릭을 플롯을 포함한 테이블로 표현한다.
#'
#' @details 집계 테이블은 일별로 집계된 GA KPI 마트를 이용한 시각화를 포함한 집계표로 활성 사용자수,
#' 세션수, 사용자당 세션수, 평균 세션시간, 신규세션 비율을 출력한다.
#'
#' @param mart_kpi data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param base_date character. 통계 기준 일자.
#' @param platform_cd character. 표시할 대상의 플랫폼 코드.
#' @param moving character. 메트릭 집계 주기
#' @details
#' platform_cd 종류는 다음과 같다.
#' \itemize{
#'   \item "ONS" : 온슈어
#'   \item "HMP" : 홈페이지
#'   \item "IWL" : 보험월렛
#'   \item "LPS" : 라이프플러스
#'   \item "HLO" : HELLO
#'   \item "HRT" : 마음건강
#'   \item "SMP" : 스마트플래너
#' }
#' moving 종류는 집계 기간으로 다음과 같다.
#' \itemize{
#'   \item "d1" : 일 집계
#'   \item "w1" : 7일 집계
#'   \item "w2" : 14일 집계
#'   \item "w3" : 21일 집계
#'   \item "w4" : 28일 집계
#'   \item "m1" : 월 집계
#' }
#' @examples
#' table_d_kpi(TB_D_KPIS)
#'
#' table_d_kpi(TB_D_KPIS, moving = "d1")
#'
#' table_d_kpi(TB_D_KPIS, base_date = "2021-03-01")
#'
#' table_d_kpi(TB_D_KPIS, platform_cd = c("IWL", "HMP"))
#'
#' @import dplyr
#' @importFrom reactable reactable colDef reactableTheme
#' @importFrom htmlwidgets JS
#' @export
table_d_kpi <- function(mart_kpi, base_date = "2021-03-31", platform_cd = NULL,
                        moving = c("d1", "w1", "w2", "w3", "w4", "m1")[5]) {
  metrics <- c("n_user", "n_session", "tot_timeonsite", "n_newsession") %>%
    paste(moving, sep = "_")

  if (is.null(platform_cd)) {
    platform_cd <- c("ONS", "HMP", "IWL", "LPS", "HLO", "HRT", "SMP")
  }

  tab <- mart_kpi %>%
    filter(date %in% base_date) %>%
    filter(platform %in% platform_cd) %>%
    mutate(platform = case_when(
      platform %in% "ONS" ~ "온슈어",
      platform %in% "HMP" ~ "홈페이지",
      platform %in% "IWL" ~ "보험월렛",
      platform %in% "LPS" ~ "라이프플러스",
      platform %in% "HLO" ~ "HELLO",
      platform %in% "HRT" ~ "마음건강",
      platform %in% "SMP" ~ "스마트플래너")) %>%
    select(platform, starts_with(metrics)) %>%
    rename_at(metrics, function(x) substr(x, 1, nchar(x) - 3)) %>%
    mutate(avg_duration = round(tot_timeonsite / n_session),
           pct_newsession = round(n_newsession / n_session * 100),
           session_per_user = round(n_session / n_user, 1),
           newsession_color = gradient_color(pct_newsession)) %>%
    select(platform, n_user, n_session, session_per_user, avg_duration,
           pct_newsession, newsession_color) %>%
    arrange(platform)

  tab %>%
    reactable(
      style = list(fontFamily = "NanumSquare"),
      highlight = TRUE,
      outlined = TRUE,
      defaultColDef = colDef(
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
    platform = reactable::colDef(name = "플랫폼", align = "left", width = 100),
    n_user = reactable::colDef(name = "활성 사용자", align = "left",
       cell = function(value) {
         width <- paste0(value / (max(tab$n_user) + 20000) * 100, "%")
         value <- value %>% formatC(format = "d", big.mark = ",")
         bar_chart(value, width = width, fill = "#008cec", background = "#e1e1e1")
    }),
    n_session = reactable::colDef(name = "세션수", align = "left",
       cell = function(value) {
         width <- paste0(value / (max(tab$n_session) + 20000) * 100, "%")
         value <- value %>% formatC(format = "d", big.mark = ",")
         bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
    }),
    session_per_user = reactable::colDef(name = "사용자당 세션수", align = "left",
      cell = function(value) {
        width <- paste0(value / (max(tab$session_per_user) + 0.5) * 100, "%")
        value <- value %>% formatC(format = "f", digits = 1)
        bar_chart(value, width = width, fill = "#32cd32", background = "#e1e1e1")
    }),
    avg_duration = reactable::colDef(name = "평균 세션 시간", align = "left",
      cell = function(value) {
        width <- paste0(value / max(tab$avg_duration) * 100, "%")
        value <- sec2hms(value)
        bar_chart(value, width = width, fill = "#b070c8", background = "#e1e1e1")
    }),
    pct_newsession = reactable::colDef(
      name = "신규 세션",
      defaultSortOrder = "desc",
      cell = htmlwidgets::JS("function(cellInfo) {
        const sliceColor = cellInfo.row['newsession_color']
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
    newsession_color = reactable::colDef(show = FALSE)
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

#' KPI 메트릭의 formattable 테이블 출력
#'
#' @description formatable_d_kpi()은 일별로 집계된 마트의 KPI 메트릭을 formattable 패키지를 사용하여, 테이블로 표현한다.
#'
#' @details 집계 테이블은 일별로 집계된 GA KPI 마트를 이용한 시각화를 포함한 집계표로 활성 사용자수,
#' 세션수, 사용자당 세션수, 평균 세션시간, 신규세션 비율을 출력한다.
#'
#' @param mart_kpi data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param base_date character. 통계 기준 일자.
#' @param platform_cd character. 표시할 대상의 플랫폼 코드.
#' @param moving character. 메트릭 집계 주기
#' @param fixed_width integer. 컬럼의 너비
#' @details
#' platform_cd 종류는 다음과 같다.
#' \itemize{
#'   \item "ONS" : 온슈어
#'   \item "HMP" : 홈페이지
#'   \item "IWL" : 보험월렛
#' }
#' moving 종류는 집계 기간으로 다음과 같다.
#' \itemize{
#'   \item "d1" : 일 집계
#'   \item "w1" : 7일 집계
#'   \item "w2" : 14일 집계
#'   \item "w3" : 21일 집계
#'   \item "w4" : 28일 집계
#'   \item "m1" : 월 집계
#' }
#' @examples
#' formatable_d_kpi(TB_D_KPIS)
#'
#' formatable_d_kpi(TB_D_KPIS, moving = "d1")
#'
#' formatable_d_kpi(TB_D_KPIS, base_date = "2021-03-01")
#'
#' formatable_d_kpi(TB_D_KPIS, platform_cd = c("IWL", "HMP"))
#'
#' @import dplyr
#' @import formattable
#' @export
formatable_d_kpi <- function(mart_kpi, base_date = "2021-03-31", platform_cd = NULL,
                             moving = c("d1", "w1", "w2", "w3", "w4", "m1")[5],
                             fixed_width = 100) {
  metrics <- c("n_user", "n_session", "tot_timeonsite", "n_newsession") %>%
    paste(moving, sep = "_")

  if (is.null(platform_cd)) {
    platform_cd <- c("ONS", "HMP", "IWL")
  }

  mart_kpi %>%
    filter(date %in% base_date) %>%
    filter(platform %in% platform_cd) %>%
    mutate(platform = case_when(
      platform %in% "ONS" ~ "온슈어",
      platform %in% "HMP" ~ "홈페이지",
      platform %in% "IWL" ~ "보험월렛")) %>%
    select(platform, starts_with(metrics)) %>%
    rename_at(metrics, function(x) substr(x, 1, nchar(x) - 3)) %>%
    mutate(session_per_user = round(n_session / n_user, 1),
           n_user = comma(n_user, digits = 0),
           n_session = comma(n_session, digits = 0),
           avg_duration = sec2hms(round(tot_timeonsite / n_session)),
           pct_newsession = percent(n_newsession / n_session, format = "d")) %>%
    select(platform, n_user, n_session, session_per_user, avg_duration,
           pct_newsession) %>%
    arrange(platform) %>%
    rename("플랫폼" = platform,
           "활성 사용자" = n_user,
           "세션수" = n_session,
           "사용자당 세션수" = session_per_user,
           "평균 세션시간" = avg_duration,
           "신규 세션 비율" = pct_newsession) %>%
    formattable(list(
      "활성 사용자" = custom_color_bar("#29BF1255", fixed_width),
      "세션수" = custom_color_bar("#00A5CF55", fixed_width),
      "사용자당 세션수" = custom_color_bar("#DE1A1A55", fixed_width),
      "평균 세션시간" = custom_color_bar("#574AE255", fixed_width),
      "신규 세션 비율" = custom_color_bar("#FFBF0055", fixed_width)))
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
#' @description compare_m_kpi()은 월별로 집계된 마트의 KPI 메트릭을 전월과 비교하여, 그 증감률을 테이블로 표현한다.
#'
#' @details 집계 테이블은 월별로 집계된 GA KPI 마트를 이용한 시각화를 포함한 집계표로 일평균 사용자수,
#' 일평균 세션수, 평균 세션시간, 일평균 신규 사용자수, 일평균 신규 세션수, 신규세션 비율을 출력한다.
#'
#' @param mart_kpi data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param prev_month character. 이전 월도.
#' @param curr_month character. 기준 월도.
#' @param platform_cd character. 표시할 대상의 플랫폼 코드.
#' @param metric character. 집계할 메트릭.
#' @param bar_ratio numeric. 증감비율로 표현할 막대의 가중치. 기본값은 1이다.
#' @param page_size integer. 한 페이지에 출력할 레코드 수.
#' @param platform_band logical. 플랫폼 이름에 밴드를 지정할지의 여부. 기본값은 FALSE, TRUE이면, 짝수번쨰 플랫폼에 회색 밴드를 표시한다.
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
#' compare_m_kpi(TB_D_KPIS)
#'
#' compare_m_kpi(TB_D_KPIS, bar_ratio = 2, page_size = 18)
#'
#' compare_m_kpi(TB_D_KPIS, metrics = c("n_user", "n_session"))
#'
#' compare_m_kpi(TB_D_KPIS, bar_ratio = 1.5, page_size = 18, platform_band = TRUE)
#'
#' @import lubridate
#' @import htmltools
#' @import reactable
#' @export
#'
compare_m_kpi <- function(mart_kpi, prev_month = "2021-02", curr_month = "2021-03",
                          platform_cd = NULL,
                          metrics = c("n_user", "n_session", "timeonsite", "n_newuser",
                                      "n_newsession", "newsession_rate"),
                          bar_ratio = 1, page_size = 10, platform_band = FALSE) {
  sdate <- as.Date(paste(prev_month, "01", sep = "-")) + months(1) - 1
  edate <- as.Date(paste(curr_month, "01", sep = "-")) + months(1) - 1

  nday_prev <-  substr(sdate, 9, 10) %>%  as.integer()
  nday_curr <-  substr(edate, 9, 10) %>%  as.integer()

  if (is.null(platform_cd)) {
    platform_cd <- c("ONS", "HMP", "IWL")
  }

  platform_nm <- case_when(
    platform_cd %in% "ONS" ~ "온슈어",
    platform_cd %in% "HMP" ~ "홈페이지",
    platform_cd %in% "IWL" ~ "보험월렛") %>%
    sort()

  band <- platform_nm[!(seq(length(platform_nm)) %% 2)]
  if (!platform_band) {
    band <- NULL
  }

  tab <- mart_kpi %>%
    filter(as.Date(date) %in% c(sdate, edate)) %>%
    filter(platform %in% platform_cd) %>%
    select(platform, date,
           n_user = n_user_m1,
           n_session = n_session_m1,
           timeonsite = tot_timeonsite_m1,
           n_newuser = n_newuser_m1,
           n_newsession = n_newsession_m1) %>%
    mutate(platform = case_when(
      platform %in% "ONS" ~ "온슈어",
      platform %in% "HMP" ~ "홈페이지",
      platform %in% "IWL" ~ "보험월렛")) %>%
    arrange(platform, date) %>%
    group_by(platform) %>%
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
      striped = !platform_band,
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
        prev = colDef(name = prev_month, format = colFormat(separators = TRUE),
                      minWidth = 70),
        curr = colDef(name = curr_month, format = colFormat(separators = TRUE),
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


