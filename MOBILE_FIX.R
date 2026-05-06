################################################################################
# EDGESCREENER — MOBILE RESPONSIVE PATCH
# setwd("~/Downloads/EdgeScreener2 2")
# source("MOBILE_FIX.R")
#
# Makes the full app iPhone/mobile friendly:
#  - Hamburger menu replaces horizontal nav on small screens
#  - KPI strip goes 2-column on mobile, 3-column on tablet
#  - All grids (g2, g3, g73, g84 etc) stack to single column on mobile
#  - Tables get horizontal scroll with larger tap targets
#  - Font sizes scale up for readability
#  - Touch-friendly buttons and inputs
#  - Viewport meta tag added
#  - Zero changes to server logic
################################################################################

setwd("~/Downloads/EdgeScreener2 2")
cat("Reading app.R...\n")
lines <- readLines("app/app.R")
app   <- paste(lines, collapse="\n")
cat("app.R:", length(lines), "lines\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: Add viewport meta tag to <head> so mobile browsers render correctly
# ─────────────────────────────────────────────────────────────────────────────
old_head <- 'tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600;700&display=swap"),
    tags$style(HTML(bloomberg_css))
  ),'

new_head <- 'tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600;700&display=swap"),
    tags$style(HTML(bloomberg_css)),
    tags$style(HTML(mobile_css))
  ),'

app <- sub(old_head, new_head, app, fixed=TRUE)
cat("✓ Added viewport meta tag\n")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: Replace topbar nav with hamburger version
# ─────────────────────────────────────────────────────────────────────────────
old_topbar <- '  # ── TOPBAR ───────────────────────────────────────────────────────────────
  div(class="topbar",
    div(class="topbar-logo", "EdgeScreener", tags$span("EQUITY TERMINAL")),
    div(class="topbar-nav",
      div(class="topbar-nav-item active", id="nav-overview",  onclick="showPane(\'overview\')",  "Overview"),
      div(class="topbar-nav-item",        id="nav-screener",  onclick="showPane(\'screener\')",  "Screener"),
      div(class="topbar-nav-item",        id="nav-squeeze",   onclick="showPane(\'squeeze\')",   "Short/Squeeze"),
      div(class="topbar-nav-item",        id="nav-deepdive",  onclick="showPane(\'deepdive\')",  "Deep Dive"),
      div(class="topbar-nav-item",        id="nav-macro",     onclick="showPane(\'macro\')",     "Macro"),
      div(class="topbar-nav-item",        id="nav-news",      onclick="showPane(\'news\')",      "News & Events")
    ),
    div(class="topbar-time",
      div(uiOutput("clock_display")),
      div(style="color:#444; font-size:9px;", paste("Updated:", meta$last_updated))
    )
  ),'

new_topbar <- '  # ── TOPBAR ───────────────────────────────────────────────────────────────
  div(class="topbar",
    div(class="topbar-logo", "EdgeScreener", tags$span("EQUITY TERMINAL")),
    div(class="topbar-nav", id="topbar-nav-desktop",
      div(class="topbar-nav-item active", id="nav-overview",  onclick="showPane(\'overview\')",  "Overview"),
      div(class="topbar-nav-item",        id="nav-screener",  onclick="showPane(\'screener\')",  "Screener"),
      div(class="topbar-nav-item",        id="nav-squeeze",   onclick="showPane(\'squeeze\')",   "Short/Squeeze"),
      div(class="topbar-nav-item",        id="nav-deepdive",  onclick="showPane(\'deepdive\')",  "Deep Dive"),
      div(class="topbar-nav-item",        id="nav-macro",     onclick="showPane(\'macro\')",     "Macro"),
      div(class="topbar-nav-item",        id="nav-news",      onclick="showPane(\'news\')",      "News & Events")
    ),
    div(class="topbar-right",
      div(class="topbar-time",
        div(uiOutput("clock_display")),
        div(style="color:#444; font-size:9px;", paste("Updated:", meta$last_updated))
      ),
      div(class="hamburger", id="hamburger-btn", onclick="toggleMobileMenu()",
        tags$span(), tags$span(), tags$span()
      )
    )
  ),
  div(class="mobile-nav", id="mobile-nav",
    div(class="mobile-nav-item active", id="mnav-overview",  onclick="showPane(\'overview\')",  "▸ Overview"),
    div(class="mobile-nav-item",        id="mnav-screener",  onclick="showPane(\'screener\')",  "▸ Screener"),
    div(class="mobile-nav-item",        id="mnav-squeeze",   onclick="showPane(\'squeeze\')",   "▸ Short / Squeeze"),
    div(class="mobile-nav-item",        id="mnav-deepdive",  onclick="showPane(\'deepdive\')",  "▸ Deep Dive"),
    div(class="mobile-nav-item",        id="mnav-macro",     onclick="showPane(\'macro\')",     "▸ Macro"),
    div(class="mobile-nav-item",        id="mnav-news",      onclick="showPane(\'news\')",      "▸ News & Events")
  ),'

app <- sub(old_topbar, new_topbar, app, fixed=TRUE)
cat("✓ Added hamburger mobile nav\n")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: Update JavaScript — showPane also updates mobile nav + closes menu
# ─────────────────────────────────────────────────────────────────────────────
old_js <- '  tags$script(HTML("
    function showPane(name) {
      document.querySelectorAll(\'.tab-pane\').forEach(p => p.classList.remove(\'active\'));
      document.querySelectorAll(\'.topbar-nav-item\').forEach(n => n.classList.remove(\'active\'));
      document.getElementById(\'pane-\' + name).classList.add(\'active\');
      document.getElementById(\'nav-\' + name).classList.add(\'active\');
      setTimeout(function(){ window.dispatchEvent(new Event(\'resize\')); }, 80);
    }
    // Live clock
    function updateClock() {
      var now = new Date();
      var timeStr = now.toLocaleTimeString(\'en-US\', {hour12:false, hour:\'2-digit\', minute:\'2-digit\', second:\'2-digit\'});
      var dateStr = now.toLocaleDateString(\'en-US\', {weekday:\'short\', month:\'short\', day:\'numeric\', year:\'numeric\'});
      Shiny.setInputValue(\'clock_tick\', timeStr + \'|\' + dateStr, {priority:\'event\'});
    }
    setInterval(updateClock, 1000);
    updateClock();
  "))'

new_js <- '  tags$script(HTML("
    function showPane(name) {
      document.querySelectorAll(\'.tab-pane\').forEach(p => p.classList.remove(\'active\'));
      document.querySelectorAll(\'.topbar-nav-item\').forEach(n => n.classList.remove(\'active\'));
      document.querySelectorAll(\'.mobile-nav-item\').forEach(n => n.classList.remove(\'active\'));
      document.getElementById(\'pane-\' + name).classList.add(\'active\');
      var desktopNav = document.getElementById(\'nav-\' + name);
      if (desktopNav) desktopNav.classList.add(\'active\');
      var mobileNav = document.getElementById(\'mnav-\' + name);
      if (mobileNav) mobileNav.classList.add(\'active\');
      // Close mobile menu after selection
      var mobileMenu = document.getElementById(\'mobile-nav\');
      if (mobileMenu) mobileMenu.classList.remove(\'open\');
      var hamburger = document.getElementById(\'hamburger-btn\');
      if (hamburger) hamburger.classList.remove(\'open\');
      // Scroll to top on mobile
      window.scrollTo(0, 0);
      setTimeout(function(){ window.dispatchEvent(new Event(\'resize\')); }, 80);
    }
    function toggleMobileMenu() {
      var menu = document.getElementById(\'mobile-nav\');
      var btn  = document.getElementById(\'hamburger-btn\');
      menu.classList.toggle(\'open\');
      btn.classList.toggle(\'open\');
    }
    // Close menu if tapping outside
    document.addEventListener(\'click\', function(e) {
      var menu = document.getElementById(\'mobile-nav\');
      var btn  = document.getElementById(\'hamburger-btn\');
      if (menu && btn && !menu.contains(e.target) && !btn.contains(e.target)) {
        menu.classList.remove(\'open\');
        btn.classList.remove(\'open\');
      }
    });
    // Live clock
    function updateClock() {
      var now = new Date();
      var timeStr = now.toLocaleTimeString(\'en-US\', {hour12:false, hour:\'2-digit\', minute:\'2-digit\', second:\'2-digit\'});
      var dateStr = now.toLocaleDateString(\'en-US\', {weekday:\'short\', month:\'short\', day:\'numeric\', year:\'numeric\'});
      Shiny.setInputValue(\'clock_tick\', timeStr + \'|\' + dateStr, {priority:\'event\'});
    }
    setInterval(updateClock, 1000);
    updateClock();
  "))'

app <- sub(old_js, new_js, app, fixed=TRUE)
cat("✓ Updated JavaScript for mobile nav\n")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: Inject mobile_css variable BEFORE the UI definition
# ─────────────────────────────────────────────────────────────────────────────
old_ui_marker <- '# =============================================================================
# UI
# ============================================================================='

new_ui_marker <- '# =============================================================================
# MOBILE CSS — injected separately so it\'s easy to update
# =============================================================================
mobile_css <- "

/* ═══════════════════════════════════════════════════════════════
   MOBILE-FIRST RESPONSIVE OVERRIDES
   Breakpoints: 480px (phone), 768px (tablet), 1024px (desktop)
═══════════════════════════════════════════════════════════════ */

/* ── HAMBURGER BUTTON ── */
.hamburger {
  display: none;
  flex-direction: column;
  justify-content: space-between;
  width: 28px;
  height: 20px;
  cursor: pointer;
  padding: 2px 0;
  flex-shrink: 0;
  margin-left: 12px;
}
.hamburger span {
  display: block;
  width: 100%;
  height: 2px;
  background: var(--orange);
  border-radius: 2px;
  transition: all 0.25s ease;
  transform-origin: center;
}
.hamburger.open span:nth-child(1) { transform: translateY(9px) rotate(45deg); }
.hamburger.open span:nth-child(2) { opacity: 0; transform: scaleX(0); }
.hamburger.open span:nth-child(3) { transform: translateY(-9px) rotate(-45deg); }

/* ── TOPBAR RIGHT GROUPING ── */
.topbar-right {
  display: flex;
  align-items: center;
  gap: 8px;
  flex-shrink: 0;
}

/* ── MOBILE DROPDOWN NAV ── */
.mobile-nav {
  display: none;
  position: fixed;
  top: 80px;
  left: 0;
  right: 0;
  background: var(--s1);
  border-bottom: 2px solid var(--orange);
  z-index: 997;
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.3s ease;
  box-shadow: 0 8px 24px rgba(0,0,0,0.6);
}
.mobile-nav.open {
  max-height: 400px;
}
.mobile-nav-item {
  display: block;
  padding: 14px 20px;
  font-family: var(--mono);
  font-size: 13px;
  font-weight: 500;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 1px;
  border-bottom: 1px solid var(--border);
  cursor: pointer;
  transition: all 0.15s;
  -webkit-tap-highlight-color: transparent;
}
.mobile-nav-item:hover,
.mobile-nav-item.active {
  color: var(--orange);
  background: rgba(255,107,0,0.08);
  padding-left: 28px;
}
.mobile-nav-item:last-child { border-bottom: none; }

/* ════════════════════════════════
   TABLET  (max-width: 1024px)
════════════════════════════════ */
@media (max-width: 1024px) {
  .g73  { grid-template-columns: 1fr; }
  .g84  { grid-template-columns: 1fr; }
  .g64  { grid-template-columns: 1fr; }
  .g442 { grid-template-columns: 1fr 1fr; }
  .kpi-strip { grid-template-columns: repeat(3, 1fr); }
}

/* ════════════════════════════════
   MOBILE  (max-width: 768px)
════════════════════════════════ */
@media (max-width: 768px) {

  /* Base font larger for readability */
  html, body { font-size: 14px; }

  /* Show hamburger, hide desktop nav */
  .hamburger { display: flex; }
  .mobile-nav { display: block; }
  #topbar-nav-desktop { display: none !important; }

  /* Topbar compact */
  .topbar {
    padding: 0 14px;
    height: 48px;
    top: 28px;
  }
  .topbar-logo {
    font-size: 14px;
    letter-spacing: 1px;
  }
  .topbar-logo span { display: none; }
  .topbar-time { display: none; }

  /* Ticker smaller */
  .ticker-wrap { height: 28px; }
  .ticker-item { padding: 0 14px; font-size: 10px; }
  .ticker-label { font-size: 10px; padding: 0 10px; }

  /* Body padding */
  .terminal-body { padding: 10px 12px; }

  /* KPI strip — 2 columns on phone */
  .kpi-strip { grid-template-columns: repeat(2, 1fr); }
  .kpi-v { font-size: 18px; }
  .kpi-k { font-size: 8px; }
  .kpi-cell { padding: 10px 12px; }

  /* All grids stack */
  .g2   { grid-template-columns: 1fr; }
  .g3   { grid-template-columns: 1fr; }
  .g55  { grid-template-columns: 1fr; }
  .g73  { grid-template-columns: 1fr; }
  .g84  { grid-template-columns: 1fr; }
  .g64  { grid-template-columns: 1fr; }
  .g442 { grid-template-columns: 1fr; }

  /* Panels */
  .panel { margin-bottom: 12px; }
  .panel-body { padding: 10px; }
  .panel-head { padding: 8px 10px; }
  .panel-head-title { font-size: 9px; }

  /* Tables — horizontal scroll, bigger tap targets */
  .dataTables_wrapper { overflow-x: auto; -webkit-overflow-scrolling: touch; }
  table.dataTable tbody td {
    padding: 10px 8px !important;
    font-size: 11px !important;
    white-space: nowrap !important;
  }
  table.dataTable thead th {
    padding: 10px 8px !important;
    font-size: 9px !important;
    white-space: nowrap !important;
  }
  .dataTables_filter input {
    width: 120px !important;
    font-size: 12px !important;
    padding: 6px 8px !important;
  }
  .dataTables_length select { font-size: 12px !important; }

  /* Inputs & dropdowns — bigger touch targets */
  .selectize-input {
    font-size: 13px !important;
    padding: 8px 10px !important;
    min-height: 40px !important;
  }
  .checkbox label {
    font-size: 12px !important;
    padding: 6px 0 !important;
  }
  .irs--shiny .irs-handle {
    width: 24px !important;
    height: 24px !important;
  }

  /* Filter bar stacks vertically */
  .filter-bar {
    flex-direction: column;
    align-items: stretch;
    gap: 10px;
  }
  .filter-grp { width: 100%; }
  .selectize-input { width: 100% !important; }

  /* Deep Dive score boxes */
  .vbox-row { grid-template-columns: repeat(2, 1fr); }
  .vbox-v   { font-size: 20px; }
  .vbox-k   { font-size: 8px; }
  .vbox     { padding: 10px 12px; }

  /* Deep Dive stock selector */
  div[style*=\"min-width:180px\"] { min-width: 100% !important; width: 100% !important; }
  div[style*=\"display:flex;align-items:center;gap:20px\"] {
    flex-direction: column !important;
    align-items: stretch !important;
    gap: 10px !important;
  }

  /* Charts — let them be full width */
  .plotly { width: 100% !important; }
  .js-plotly-plot { width: 100% !important; }

  /* Macro grid */
  .macro-grid { grid-template-columns: repeat(2, 1fr) !important; }
  .macro-v { font-size: 16px; }

  /* News feed */
  div[style*=\"max-height:650px\"] { max-height: 400px !important; }
  div[style*=\"max-height:320px\"] { max-height: 250px !important; }

  /* Score bar */
  .sbar-val { font-size: 10px; min-width: 22px; }

  /* Buttons */
  .btn, button {
    min-height: 36px;
    font-size: 12px !important;
  }
  .shiny-action-button {
    padding: 6px 12px !important;
    font-size: 11px !important;
  }

  /* Short info grid */
  .si-grid { grid-template-columns: 1fr 1fr; gap: 8px; }
  .si-v { font-size: 15px; }
}

/* ════════════════════════════════
   SMALL PHONE  (max-width: 480px)
════════════════════════════════ */
@media (max-width: 480px) {
  .kpi-strip { grid-template-columns: repeat(2, 1fr); }
  .kpi-v { font-size: 16px; }
  .terminal-body { padding: 8px; }

  /* Single column everything */
  .vbox-row { grid-template-columns: 1fr 1fr; }
  .macro-grid { grid-template-columns: 1fr 1fr !important; }

  /* Ticker tape more compact */
  .ticker-item { padding: 0 10px; font-size: 10px; gap: 5px; }
}

/* ════════════════════════════════
   TOUCH DEVICE OVERRIDES
════════════════════════════════ */
@media (hover: none) and (pointer: coarse) {
  /* Remove hover effects that don\'t work on touch */
  .topbar-nav-item:hover { background: transparent; color: var(--muted); }
  .kpi-cell:hover { background: var(--s1); }
  table.dataTable tbody tr:hover td { background: transparent !important; }

  /* Bigger tap targets */
  .mobile-nav-item { padding: 16px 20px; }
  .topbar-nav-item { padding: 0 14px; min-height: 44px; }

  /* Prevent text selection on tap */
  .topbar-nav-item, .mobile-nav-item, .hamburger {
    -webkit-user-select: none;
    user-select: none;
    -webkit-tap-highlight-color: transparent;
  }
}
"

# =============================================================================
# UI
# ============================================================================='

app <- sub(old_ui_marker, new_ui_marker, app, fixed=TRUE)
cat("✓ Injected mobile_css variable\n")

# ─────────────────────────────────────────────────────────────────────────────
# Write patched app.R
# ─────────────────────────────────────────────────────────────────────────────
writeLines(strsplit(app, "\n")[[1]], "app/app.R")
new_lines <- length(readLines("app/app.R"))
cat("✓ app.R saved:", new_lines, "lines\n\n")

cat("=================================================\n")
cat("  MOBILE FIX COMPLETE!\n")
cat("  Deploy with:\n\n")
cat("  rsconnect::deployApp(\n")
cat("    appDir='~/Downloads/EdgeScreener2 2/app',\n")
cat("    appName='r-codescreener'\n")
cat("  )\n")
cat("=================================================\n")
