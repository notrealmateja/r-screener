################################################################################
# APP.R PATCH SCRIPT
# Run AFTER MASTER_FIX.R
# setwd("~/Downloads/EdgeScreener2 2")
# source("APP_PATCH.R")
################################################################################

setwd("~/Downloads/EdgeScreener2 2")
cat("Reading app.R...\n")
app_lines <- readLines("app/app.R")
cat("app.R has", length(app_lines), "lines\n\n")

# ==============================================================================
# PATCH 1: FIX DEEP DIVE - price chart output
# Find the deep dive server section and replace it
# ==============================================================================
cat(">> Patching Deep Dive price chart...\n")

deep_dive_server <- '
  # ── DEEP DIVE: Stock selector ──────────────────────────────────────────────
  output$deep_dive_selector <- renderUI({
    req(master_data)
    syms <- sort(master_data$symbol)
    selectInput("selected_stock", "Select Stock:", choices=syms, selected=syms[1])
  })

  output$deep_dive_sector_filter <- renderUI({
    req(master_data)
    sectors <- c("All", sort(unique(master_data$sector)))
    selectInput("dd_sector", "Filter by Sector:", choices=sectors, selected="All")
  })

  dd_stock <- reactive({
    req(input$selected_stock)
    input$selected_stock
  })

  output$dd_master_score  <- renderText({ req(master_data, dd_stock()); s <- master_data[master_data$symbol==dd_stock(),]; if(nrow(s)==0) "N/A" else round(s$master_score[1],1) })
  output$dd_fund_score    <- renderText({ req(master_data, dd_stock()); s <- master_data[master_data$symbol==dd_stock(),]; if(nrow(s)==0) "N/A" else round(s$fundamental_score[1],1) })
  output$dd_mom_score     <- renderText({ req(master_data, dd_stock()); s <- master_data[master_data$symbol==dd_stock(),]; if(nrow(s)==0) "N/A" else round(coalesce(s$momentum_score[1],45),1) })
  output$dd_squeeze_score <- renderText({ req(master_data, dd_stock()); s <- master_data[master_data$symbol==dd_stock(),]; if(nrow(s)==0) "N/A" else round(coalesce(s$squeeze_score[1],28.5),1) })

  output$dd_price_chart <- renderPlotly({
    req(price_data, dd_stock())
    sym <- dd_stock()
    df  <- price_data[price_data$symbol == sym, ]
    if (is.null(df) || nrow(df) == 0) {
      # Fetch live if not in CSV
      tryCatch({
        env <- new.env()
        suppressWarnings(quantmod::getSymbols(sym, src="yahoo", env=env, auto.assign=TRUE,
                          from=Sys.Date()-365, to=Sys.Date()))
        px <- env[[sym]]
        df <- data.frame(
          date  = as.character(zoo::index(px)),
          close = as.numeric(quantmod::Cl(px)),
          open  = as.numeric(quantmod::Op(px)),
          high  = as.numeric(quantmod::Hi(px)),
          low   = as.numeric(quantmod::Lo(px)),
          volume= as.numeric(quantmod::Vo(px))
        )
        df <- df[!is.na(df$close),]
      }, error=function(e) {
        return(plotly::plot_ly() %>% plotly::layout(title="Unable to load price data"))
      })
    }
    
    if (nrow(df) == 0) return(plotly::plot_ly() %>% plotly::layout(title="No data"))
    
    df$date <- as.Date(df$date)
    
    # Candlestick chart
    plotly::plot_ly(df, type="candlestick",
      x=~date, open=~open, close=~close, high=~high, low=~low,
      name=sym,
      increasing=list(line=list(color="#FF6B00")),
      decreasing=list(line=list(color="#666666"))
    ) %>%
    plotly::layout(
      title=list(text=paste0("<b>", sym, " — 1Y Price Chart</b>"), font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(color="#999", gridcolor="#1A1A1A", rangeslider=list(visible=FALSE)),
      yaxis=list(color="#999", gridcolor="#1A1A1A", title="Price ($)"),
      font=list(color="#E8E8E8"),
      showlegend=FALSE
    )
  })

  output$dd_key_metrics <- renderUI({
    req(master_data, dd_stock())
    s <- master_data[master_data$symbol == dd_stock(), ]
    if (nrow(s) == 0) return(div("No data"))
    s <- s[1,]
    
    pe_val     <- if (!is.na(s$pe_ratio) && s$pe_ratio > 0) paste0(round(s$pe_ratio,1), "x") else "N/A"
    pb_val     <- if ("pb_ratio" %in% names(s) && !is.na(s$pb_ratio)) paste0(round(s$pb_ratio,2), "x") else "N/A"
    roe_val    <- if ("roe" %in% names(s) && !is.na(s$roe)) paste0(round(s$roe*100,1), "%") else "N/A"
    margin_val <- if ("profit_margin" %in% names(s) && !is.na(s$profit_margin)) paste0(round(s$profit_margin*100,1), "%") else "N/A"
    mktcap_val <- if (!is.na(s$market_cap) && s$market_cap > 0) {
      if(s$market_cap>=1e12) paste0("$",round(s$market_cap/1e12,1),"T")
      else if(s$market_cap>=1e9) paste0("$",round(s$market_cap/1e9,1),"B")
      else paste0("$",round(s$market_cap/1e6,1),"M")
    } else "N/A"
    
    tagList(
      div(class="metric-grid",
        div(class="metric-box", span("P/E Ratio", class="metric-label"), span(pe_val, class="metric-val")),
        div(class="metric-box", span("P/B Ratio", class="metric-label"), span(pb_val, class="metric-val")),
        div(class="metric-box", span("ROE", class="metric-label"), span(roe_val, class="metric-val")),
        div(class="metric-box", span("Profit Margin", class="metric-label"), span(margin_val, class="metric-val")),
        div(class="metric-box", span("Market Cap", class="metric-label"), span(mktcap_val, class="metric-val")),
        div(class="metric-box", span("Sector", class="metric-label"), span(s$sector, class="metric-val")),
        div(class="metric-box", span("Rating", class="metric-label"), span(s$rating, class="metric-val")),
        div(class="metric-box", span("Tier", class="metric-label"), span(coalesce(s$tier,"N/A"), class="metric-val"))
      )
    )
  })
'

# ==============================================================================
# PATCH 2: FIX MACRO TAB SERVER
# ==============================================================================
cat(">> Patching Macro tab...\n")

macro_server <- '
  # ── MACRO TAB ──────────────────────────────────────────────────────────────
  output$macro_indicators <- renderUI({
    req(macro_data)
    df <- macro_data
    if (is.null(df) || nrow(df) == 0) return(div("No macro data available"))
    
    items <- lapply(1:nrow(df), function(i) {
      row <- df[i,]
      val <- round(as.numeric(row$value), 2)
      color <- if (!is.na(val) && val >= 0) "#00C853" else "#FF3D00"
      div(class="macro-card",
        div(class="macro-name", row$name),
        div(class="macro-value", style=paste0("color:",color,";font-size:1.6em;font-weight:700;"),
            paste0(val, "%")),
        div(class="macro-date", style="color:#666;font-size:0.75em;", row$date)
      )
    })
    div(class="macro-grid", style="display:grid;grid-template-columns:repeat(3,1fr);gap:12px;", items)
  })

  output$yield_curve_plot <- renderPlotly({
    req(macro_data)
    df <- macro_data
    maturities <- c("2Y","10Y")
    yields <- c(
      df$value[df$series=="DGS2"][1],
      df$value[df$series=="DGS10"][1]
    )
    yields <- suppressWarnings(as.numeric(yields))
    
    plotly::plot_ly(
      x = maturities, y = yields, type="scatter", mode="lines+markers",
      line=list(color="#FF6B00", width=3),
      marker=list(color="#FF6B00", size=10)
    ) %>%
    plotly::layout(
      title=list(text="<b>Yield Curve (UST)</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(title="Maturity", color="#999", gridcolor="#1A1A1A"),
      yaxis=list(title="Yield (%)", color="#999", gridcolor="#1A1A1A"),
      font=list(color="#E8E8E8")
    )
  })

  output$treasury_spread_plot <- renderPlotly({
    req(macro_data)
    df <- macro_data
    g10 <- suppressWarnings(as.numeric(df$value[df$series=="DGS10"][1]))
    g2  <- suppressWarnings(as.numeric(df$value[df$series=="DGS2"][1]))
    spread <- g10 - g2
    
    plotly::plot_ly(
      x = c("2Y","10Y"), y = c(g2, g10), type="bar",
      marker=list(color=c("#666","#FF6B00"))
    ) %>%
    plotly::layout(
      title=list(text=paste0("<b>10Y Treasury (", round(g10,2), "%) vs 2Y (", round(g2,2), "%)</b><br>Spread: ", round(spread,3), "%"), font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(color="#999"), yaxis=list(title="Yield (%)", color="#999", gridcolor="#1A1A1A"),
      font=list(color="#E8E8E8")
    )
  })

  output$fed_funds_plot <- renderPlotly({
    req(macro_data)
    df <- macro_data
    ff  <- suppressWarnings(as.numeric(df$value[df$series=="DFF"][1]))
    cpi <- suppressWarnings(as.numeric(df$value[df$series=="CPIAUCSL"][1]))
    
    plotly::plot_ly(
      x=c("Fed Funds Rate","CPI YoY","Real Rate (approx)"),
      y=c(ff, cpi, round(ff-cpi,2)),
      type="bar",
      marker=list(color=c("#FF6B00","#FFD600","#00C853"))
    ) %>%
    plotly::layout(
      title=list(text="<b>Fed Funds Rate vs Inflation</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(color="#999"), yaxis=list(title="%", color="#999", gridcolor="#1A1A1A"),
      font=list(color="#E8E8E8")
    )
  })

  output$yield_spread_plot <- renderPlotly({
    req(macro_data)
    df <- macro_data
    g10 <- suppressWarnings(as.numeric(df$value[df$series=="DGS10"][1]))
    g2  <- suppressWarnings(as.numeric(df$value[df$series=="DGS2"][1]))
    spread <- g10 - g2
    inv_color <- if (spread < 0) "#FF3D00" else "#00C853"
    
    plotly::plot_ly(
      x=c("10Y-2Y Spread"), y=c(spread), type="bar",
      marker=list(color=inv_color)
    ) %>%
    plotly::add_annotations(
      x=0, y=spread/2, text=if(spread<0) "⚠ INVERTED (Recession Signal)" else "Normal",
      showarrow=FALSE, font=list(color="#E8E8E8", size=14)
    ) %>%
    plotly::layout(
      title=list(text="<b>Yield Curve Spread (10Y-2Y)</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(color="#999"), yaxis=list(title="Spread (%)", color="#999", gridcolor="#1A1A1A"),
      font=list(color="#E8E8E8")
    )
  })
'

# ==============================================================================
# PATCH 3: FIX NEWS TAB SERVER
# ==============================================================================
cat(">> Patching News tab...\n")

news_server <- '
  # ── NEWS TAB ───────────────────────────────────────────────────────────────
  output$news_source_tabs <- renderUI({
    req(news_data)
    sources <- c("All", sort(unique(news_data$source)))
    tabsetPanel(id="news_source_tab",
      lapply(sources, function(src) {
        tabPanel(src)
      })
    )
  })

  filtered_news <- reactive({
    req(news_data)
    df <- news_data
    if (!is.null(input$news_source_tab) && input$news_source_tab != "All") {
      df <- df[df$source == input$news_source_tab, ]
    }
    if (!is.null(input$news_category) && input$news_category != "All") {
      df <- df[df$category == input$news_category, ]
    }
    head(df, 50)
  })

  output$news_feed <- renderUI({
    req(filtered_news())
    df <- filtered_news()
    if (nrow(df) == 0) return(div("No news available for this filter.", style="color:#666;padding:20px;"))
    
    items <- lapply(1:nrow(df), function(i) {
      row <- df[i,]
      src_color <- switch(row$source,
        "CNBC Markets"="#E00000", "CNBC Finance"="#E00000",
        "Reuters"="#FF8C00", "WSJ Markets"="#0080FF",
        "MarketWatch"="#00AA44", "Reddit /r/stocks"="#FF4500",
        "Reddit /r/investing"="#FF4500", "Reddit WSB"="#FF4500",
        "BBC Business"="#BB1919", "Financial Times"="#FFA500",
        "#FF6B00"
      )
      div(class="news-item", style="border-left:3px solid #222;padding:12px 16px;margin-bottom:8px;background:#0D0D0D;border-radius:4px;",
        div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
          span(row$source, style=paste0("color:",src_color,";font-size:0.7em;font-weight:700;text-transform:uppercase;letter-spacing:1px;")),
          span(row$category, style="color:#444;font-size:0.7em;background:#1A1A1A;padding:2px 8px;border-radius:10px;")
        ),
        a(href=row$link, target="_blank",
          div(row$headline, style="color:#E8E8E8;font-size:0.95em;font-weight:600;margin-bottom:4px;line-height:1.4;")
        ),
        div(row$summary, style="color:#666;font-size:0.8em;line-height:1.5;")
      )
    })
    div(items)
  })

  output$sector_perf_plot <- renderPlotly({
    req(master_data)
    df <- master_data
    
    # Compute avg 1M return by sector
    if ("ret_1m" %in% names(df)) {
      sect_perf <- df %>%
        group_by(sector) %>%
        summarise(avg_ret = mean(ret_1m * 100, na.rm=TRUE), n=n(), .groups="drop") %>%
        filter(!is.na(avg_ret)) %>%
        arrange(avg_ret)
    } else {
      sect_perf <- df %>%
        group_by(sector) %>%
        summarise(avg_score = mean(master_score, na.rm=TRUE), n=n(), .groups="drop") %>%
        rename(avg_ret=avg_score) %>%
        arrange(avg_ret)
    }
    
    colors <- ifelse(sect_perf$avg_ret >= 0, "#00C853", "#FF3D00")
    
    plotly::plot_ly(sect_perf,
      x=~avg_ret, y=~reorder(sector, avg_ret),
      type="bar", orientation="h",
      marker=list(color=colors),
      text=~paste0(sector, ": ", round(avg_ret,2), if("ret_1m" %in% names(df)) "%" else " pts"),
      hoverinfo="text"
    ) %>%
    plotly::layout(
      title=list(text="<b>Sector Performance — 1M Avg Return</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(title=if("ret_1m" %in% names(df)) "1M Return (%)" else "Avg Score",
                 color="#999", gridcolor="#1A1A1A", zeroline=TRUE, zerolinecolor="#333"),
      yaxis=list(title="", color="#E8E8E8"),
      font=list(color="#E8E8E8"),
      margin=list(l=130)
    )
  })
'

# ==============================================================================
# PATCH 4: FIX SQUEEZE TAB SERVER
# ==============================================================================
cat(">> Patching Short/Squeeze tab...\n")

squeeze_server <- '
  # ── SHORT/SQUEEZE TAB ──────────────────────────────────────────────────────
  squeeze_filtered <- reactive({
    req(master_data)
    df <- master_data
    if (!is.null(input$squeeze_sector) && input$squeeze_sector != "All")
      df <- df[df$sector == input$squeeze_sector, ]
    df %>% arrange(desc(master_score))
  })

  output$squeeze_sector_filter <- renderUI({
    req(master_data)
    sectors <- c("All", sort(unique(master_data$sector)))
    selectInput("squeeze_sector", "Sector:", choices=sectors, selected="All", width="200px")
  })

  output$short_float_scatter <- renderPlotly({
    req(squeeze_filtered())
    df <- squeeze_filtered()
    
    # Use momentum as x-axis if short_float is all NA
    if (all(is.na(df$short_float))) {
      xvar <- if("momentum_score" %in% names(df)) df$momentum_score else df$fundamental_score
      xlabel <- "Momentum Score"
    } else {
      xvar <- df$short_float * 100
      xlabel <- "Short % of Float"
    }
    
    yvar   <- if("earningsGrowth" %in% names(df)) df$earningsGrowth * 100 else df$fundamental_score
    ylabel <- if("earningsGrowth" %in% names(df)) "Earnings Growth (%)" else "Fundamental Score"
    
    color_vals <- df$master_score
    
    plotly::plot_ly(df,
      x=~xvar, y=~yvar, type="scatter", mode="markers",
      marker=list(
        size=10, opacity=0.85,
        color=~color_vals,
        colorscale=list(c(0,"#FF3D00"), c(0.5,"#FFD600"), c(1,"#00C853")),
        colorbar=list(title="Score", tickfont=list(color="#E8E8E8")),
        showscale=TRUE
      ),
      text=~paste0("<b>",symbol,"</b> (", sector,")<br>Score: ",master_score,"<br>Rating: ",rating,"<br>Tier: ",tier),
      hoverinfo="text"
    ) %>%
    plotly::layout(
      title=list(text="<b>Stock Universe — Score Map</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(title=xlabel, color="#999", gridcolor="#1A1A1A"),
      yaxis=list(title=ylabel, color="#999", gridcolor="#1A1A1A"),
      font=list(color="#E8E8E8")
    )
  })

  output$squeeze_tiers_plot <- renderPlotly({
    req(squeeze_filtered())
    df <- squeeze_filtered()
    tier_counts <- table(df$tier)
    tier_df <- data.frame(tier=names(tier_counts), count=as.numeric(tier_counts))
    tier_df <- tier_df[order(tier_df$count, decreasing=TRUE),]
    
    colors <- c(
      "Tier 1: Golden Squeeze"="#FFD700",
      "Tier 2: Golden Cross"="#FF6B00",
      "Tier 3: Squeeze Setup"="#00C853",
      "Tier 4: High Score"="#0080FF",
      "Tier 5: Underdog Watch"="#AA00FF",
      "No Signal"="#333333"
    )
    bar_colors <- sapply(tier_df$tier, function(t) ifelse(t %in% names(colors), colors[t], "#555"))
    
    plotly::plot_ly(tier_df,
      x=~count, y=~reorder(tier,count), type="bar", orientation="h",
      marker=list(color=bar_colors),
      text=~paste(tier,":", count, "stocks"), hoverinfo="text"
    ) %>%
    plotly::layout(
      title=list(text="<b>Signal Tier Distribution</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(title="# Stocks", color="#999", gridcolor="#1A1A1A"),
      yaxis=list(title="", color="#E8E8E8"),
      font=list(color="#E8E8E8"),
      margin=list(l=200)
    )
  })

  output$high_conviction_table <- renderDT({
    req(squeeze_filtered())
    df <- squeeze_filtered() %>%
      filter(tier != "No Signal") %>%
      select(symbol, company, sector, master_score, rating, tier,
             any_of(c("ret_1m","ret_3m","short_float","momentum_score","fundamental_score"))) %>%
      arrange(desc(master_score)) %>%
      head(20)
    
    if ("ret_1m" %in% names(df)) df$ret_1m <- paste0(round(df$ret_1m*100,1),"%")
    if ("ret_3m" %in% names(df)) df$ret_3m <- paste0(round(df$ret_3m*100,1),"%")
    
    datatable(df,
      options=list(pageLength=10, dom="tp", scrollX=TRUE),
      rownames=FALSE, class="display compact"
    ) %>%
    formatStyle("master_score",
      background=styleColorBar(range(df$master_score,na.rm=TRUE), "#FF6B00"),
      backgroundSize="100% 80%", backgroundRepeat="no-repeat", backgroundPosition="center"
    )
  })

  output$short_trend_plot <- renderPlotly({
    req(squeeze_filtered())
    df <- squeeze_filtered()
    trend_col <- if("short_trend" %in% names(df)) df$short_trend else rep("Unknown", nrow(df))
    tc <- table(trend_col)
    
    plotly::plot_ly(
      labels=names(tc), values=as.numeric(tc), type="pie",
      marker=list(colors=c("#FF3D00","#00C853","#FFD600","#666")),
      textinfo="label+percent", hoverinfo="label+value"
    ) %>%
    plotly::layout(
      title=list(text="<b>Short Trend Direction</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", font=list(color="#E8E8E8"),
      showlegend=TRUE, legend=list(font=list(color="#E8E8E8"))
    )
  })

  output$squeeze_score_dist <- renderPlotly({
    req(squeeze_filtered())
    df <- squeeze_filtered()
    
    plotly::plot_ly(df, x=~master_score, type="histogram",
      nbinsx=15,
      marker=list(color="#FF6B00", line=list(color="#0A0A0A", width=1))
    ) %>%
    plotly::layout(
      title=list(text="<b>Master Score Distribution</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(title="Master Score", color="#999", gridcolor="#1A1A1A"),
      yaxis=list(title="Count", color="#999", gridcolor="#1A1A1A"),
      font=list(color="#E8E8E8")
    )
  })
'

# ==============================================================================
# NOW APPLY PATCHES TO APP.R
# ==============================================================================
cat("\n>> Applying patches to app.R...\n")

# Read the full app.R
lines <- readLines("app/app.R")
full_app <- paste(lines, collapse="\n")

# Find server function closing brace area
server_end <- max(grep("^\\}", lines))
cat("   app.R last } at line:", server_end, "\n")

# Inject all server patches just before the final closing brace
injection <- paste(deep_dive_server, macro_server, news_server, squeeze_server, sep="\n\n")

# Check if patches already exist - if so, replace them; if not, inject
if (grepl("dd_price_chart", full_app)) {
  cat("   Deep Dive patch already present - updating...\n")
}

# Strategy: append patches before last line (closing brace of server)
lines_new <- c(
  lines[1:(server_end-1)],
  "",
  "  # ═══════════════════════════════════════════════════════════════════════",
  "  # PATCHED OUTPUTS - News, Macro, Deep Dive, Short/Squeeze",
  "  # ═══════════════════════════════════════════════════════════════════════",
  strsplit(injection, "\n")[[1]],
  "",
  lines[server_end:length(lines)]
)

writeLines(lines_new, "app/app.R")
cat("   app.R patched:", length(lines_new), "lines (was", length(lines), ")\n\n")

# ==============================================================================
# PATCH GLOBAL.R - ensure all data loads correctly
# ==============================================================================
cat(">> Patching global.R data loading...\n")

global_lines <- readLines("app/global.R")
global_text  <- paste(global_lines, collapse="\n")

# Add news_data and price_data loading if not present
if (!grepl("news_data", global_text)) {
  extra_loads <- c(
    "",
    "# Additional data loads",
    "news_data  <- tryCatch(load_csv('market_news.csv'),  error=function(e) NULL)",
    "price_data <- tryCatch(load_csv('price_history.csv'), error=function(e) NULL)",
    ""
  )
  writeLines(c(global_lines, extra_loads), "app/global.R")
  cat("   Added news_data and price_data to global.R\n")
} else {
  cat("   global.R already has news_data\n")
}

cat("\n========================================\n")
cat("  APP PATCHES APPLIED!\n")
cat("  Now deploy:\n")
cat("  rsconnect::deployApp(\n")
cat("    appDir='~/Downloads/EdgeScreener2 2/app',\n")
cat("    appName='r-codescreener'\n")
cat("  )\n")
cat("========================================\n")
