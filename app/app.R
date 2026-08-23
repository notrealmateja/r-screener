# =============================================================================
# EDGESCREENER — Bloomberg Terminal Style Dashboard
# =============================================================================
source("global.R")

# Load new quant outputs (graceful fallback if not yet generated)
top15_data   <- tryCatch(read_csv("top15_sweetspot.csv", show_col_types=FALSE), error=function(e) NULL)
unicorn_data <- tryCatch(read_csv("top15_unicorns.csv",  show_col_types=FALSE), error=function(e) NULL)

# =============================================================================
# CSS — Pure Bloomberg DNA
# =============================================================================
bloomberg_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600;700&display=swap');

:root {
  --bg:      #0A0A0A;
  --s1:      #111111;
  --s2:      #181818;
  --s3:      #222222;
  --border:  #2A2A2A;
  --border2: #333333;
  --orange:  #FF6B00;
  --orange2: #FF8C00;
  --orange3: #FFA040;
  --text:    #E8E8E8;
  --text2:   #AAAAAA;
  --muted:   #666666;
  --green:   #00C853;
  --red:     #FF3D00;
  --yellow:  #FFD600;
  --cyan:    #00B8D9;
  --mono:    'IBM Plex Mono', monospace;
  --sans:    'IBM Plex Sans', sans-serif;
}

*, *::before, *::after { box-sizing: border-box; margin:0; padding:0; }
html, body { background:var(--bg); color:var(--text); font-family:var(--sans);
             font-size:13px; min-height:100vh; overflow-x:hidden; }

/* ── SCROLLBAR ── */
::-webkit-scrollbar { width:5px; height:5px; }
::-webkit-scrollbar-track { background:var(--bg); }
::-webkit-scrollbar-thumb { background:var(--border2); border-radius:2px; }

/* ── TICKER TAPE ── */
.ticker-wrap {
  background:var(--s1); border-bottom:1px solid var(--border);
  overflow:hidden; height:32px; display:flex; align-items:center;
  position:sticky; top:0; z-index:999;
}
.ticker-label {
  background:var(--orange); color:#000; font-family:var(--mono);
  font-size:11px; font-weight:600; padding:0 14px; height:100%;
  display:flex; align-items:center; white-space:nowrap; flex-shrink:0;
  letter-spacing:0.5px;
}
.ticker-scroll-wrap { overflow:hidden; flex:1; }
.ticker-content {
  display:flex; gap:0; white-space:nowrap;
  animation:ticker-scroll 60s linear infinite;
}
.ticker-content:hover { animation-play-state:paused; }
@keyframes ticker-scroll {
  0%   { transform:translateX(0); }
  100% { transform:translateX(-50%); }
}
.ticker-item {
  display:inline-flex; align-items:center; gap:8px;
  padding:0 20px; border-right:1px solid var(--border);
  height:32px; font-family:var(--mono); font-size:11px;
}
.ticker-sym  { color:var(--orange); font-weight:600; letter-spacing:0.3px; }
.ticker-price { color:var(--text); }
.ticker-up   { color:var(--green); }
.ticker-dn   { color:var(--red); }

/* ── TOPBAR ── */
.topbar {
  background:var(--s1); border-bottom:2px solid var(--orange);
  display:flex; align-items:center; justify-content:space-between;
  padding:0 20px; height:48px; position:sticky; top:32px; z-index:998;
}
.topbar-logo {
  font-family:var(--mono); font-size:18px; font-weight:600;
  color:var(--orange); letter-spacing:2px; text-transform:uppercase;
}
.topbar-logo span { color:var(--text2); font-size:11px; font-weight:300;
                    margin-left:12px; letter-spacing:1px; }
.topbar-nav { display:flex; gap:0; height:100%; min-width:0; }
/* The bar needs 1075px for the brand, seven nav items and the clock, so it
   spilled past the viewport on a 1024-wide laptop and forced the whole page
   to scroll sideways. Tighten it rather than letting it push the layout. */
@media (max-width:1200px) {
  .topbar { padding:0 12px; }
  .topbar-nav-item { padding:0 10px; font-size:10px; letter-spacing:0.2px; }
  .topbar-sub { display:none; }
  .topbar-time { font-size:10px; }
}
@media (max-width:1080px) {
  .topbar-nav-item { padding:0 7px; font-size:9px; }
  .topbar-logo { font-size:13px; }
}
.topbar-nav-item {
  display:flex; align-items:center; padding:0 18px;
  font-family:var(--mono); font-size:11px; font-weight:500;
  color:var(--muted); cursor:pointer; letter-spacing:0.5px;
  border-bottom:3px solid transparent; margin-bottom:-2px;
  text-transform:uppercase; transition:all 0.15s;
  border-right:1px solid var(--border);
  white-space:nowrap;
}
.topbar-nav-item:hover  { color:var(--orange); background:rgba(255,107,0,0.05); }
.topbar-nav-item.active { color:var(--orange); border-bottom-color:var(--orange);
                           background:rgba(255,107,0,0.08); }
.topbar-time {
  font-family:var(--mono); font-size:11px; color:var(--muted);
  text-align:right; line-height:1.6;
}

/* ── LAYOUT ── */
.terminal-body { padding:16px 20px; }
.tab-pane      { display:none; }
.tab-pane.active { display:block; }

/* ── SECTION HEADER ── */
/* ── KPI STRIP ── */
.kpi-strip {
  display:grid; grid-template-columns:repeat(6,1fr);
  gap:1px; background:var(--border); margin-bottom:16px;
  border:1px solid var(--border);
}
.kpi-cell {
  background:var(--s1); padding:12px 14px;
  transition:background 0.15s;
}
.kpi-cell:hover { background:var(--s2); }
.kpi-k { font-family:var(--mono); font-size:9px; color:var(--muted);
         text-transform:uppercase; letter-spacing:1px; margin-bottom:6px; }
.kpi-v { font-family:var(--mono); font-size:22px; font-weight:600;
         color:var(--orange); line-height:1; }
.kpi-s { font-size:10px; color:var(--muted); margin-top:3px; font-family:var(--mono); }

/* ── PANEL ── */
.panel {
  background:var(--s1); border:1px solid var(--border);
  margin-bottom:16px;
}
.panel-head {
  background:var(--s2); border-bottom:1px solid var(--border);
  padding:8px 14px; display:flex; align-items:center; justify-content:space-between;
}
.panel-head-title {
  font-family:var(--mono); font-size:10px; font-weight:600;
  color:var(--orange2); text-transform:uppercase; letter-spacing:1px;
}
.panel-head-meta {
  font-family:var(--mono); font-size:9px; color:var(--muted); letter-spacing:0.5px;
}
.panel-body { padding:14px; }

/* ── GRID LAYOUTS ── */
.g2  { display:grid; grid-template-columns:1fr 1fr;         gap:16px; }
.g3  { display:grid; grid-template-columns:1fr 1fr 1fr;     gap:16px; }
.g73 { display:grid; grid-template-columns:7fr 3fr;         gap:16px; }
.g64 { display:grid; grid-template-columns:6fr 4fr;         gap:16px; }
.g84 { display:grid; grid-template-columns:8fr 4fr;         gap:16px; }
.g55 { display:grid; grid-template-columns:5fr 5fr;         gap:16px; }
.g345 { display:grid; grid-template-columns:3fr 4fr 5fr;    gap:16px; }
.g442 { display:grid; grid-template-columns:4fr 4fr 2fr;    gap:16px; }
/* Grid items default to min-width:auto, so a column refuses to shrink below
   its content and a wide table pushes the whole grid past the viewport — the
   body measured 1778px inside a 1275px window, forcing a horizontal scroll on
   the page itself. min-width:0 lets the column shrink, which is what finally
   lets overflow-x on the inner wrapper do its job. */
.g2 > *, .g3 > *, .g73 > *, .g64 > *, .g84 > *, .g55 > *, .g345 > *, .g442 > * {
  min-width:0;
}
.panel { min-width:0; }
/* Anything that can be wider than its column scrolls inside itself. */
.panel-body { min-width:0; }
.ss-table-wrap, .panel-body .dataTables_wrapper { overflow-x:auto; max-width:100%; }
.panel-head { gap:10px; }
.panel-head-title { white-space:nowrap; }
.panel-head-meta {
  min-width:0; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;
}

/* ── DATA TABLE OVERRIDES ── */
.dataTables_wrapper { font-size:11px; }
table.dataTable { background:transparent !important; color:var(--text) !important;
                  border-collapse:collapse !important; width:100% !important; }
table.dataTable thead th {
  background:var(--s3) !important; color:var(--orange2) !important;
  border-bottom:1px solid var(--border2) !important;
  font-family:var(--mono) !important; font-size:9px !important;
  text-transform:uppercase !important; letter-spacing:1px !important;
  font-weight:600 !important; padding:8px 10px !important;
  white-space:nowrap !important;
}
/* A rule under every cell drew a full grid across the table and made 20 rows
   read as 20 boxes. Rows are separated by alternating tone instead, which
   groups a row visually without ruling it off. */
table.dataTable tbody td {
  padding:9px 12px !important; border:0 !important;
  background:transparent !important; font-family:var(--mono) !important;
  font-size:11px !important; vertical-align:middle !important;
  /* Cells were wrapping: company names, rating labels and driver names each
     broke onto two lines, and the signal list onto four, so every row rendered
     as a ragged stack of 2-4 text lines at uneven heights. One line per cell
     keeps rows uniform and scannable; the wide tables already scroll
     horizontally, and anything too long is elided. */
  white-space:nowrap !important;
  overflow:hidden !important; text-overflow:ellipsis !important;
  max-width:230px !important;
}
/* Signal lists are the longest field and the least essential to read in full. */
table.dataTable tbody td:nth-last-child(3) { max-width:170px !important; }
table.dataTable tbody tr { background:transparent !important; }
table.dataTable tbody tr:nth-child(even) td { background:rgba(255,255,255,0.018) !important; }
table.dataTable tbody tr td { transition:background .15s ease; }
table.dataTable tbody tr:hover td { background:rgba(255,107,0,0.07) !important; }
/* One hairline under the header keeps the columns anchored without a grid. */
table.dataTable thead th {
  border-bottom:1px solid var(--border2) !important;
  white-space:nowrap !important;
}
/* Rating pills and badges must not break across lines inside their cell. */
table.dataTable tbody td .pill,
table.dataTable tbody td .unicorn-badge { white-space:nowrap !important; }
/* Rows in the browse tables open that stock in Deep Dive, so they need to read
   as interactive rather than as static cells. */
/* Live TV channel buttons */
.tv-btn {
  background:#141414 !important; color:var(--text2) !important;
  border:1px solid var(--border) !important; border-radius:0 !important;
  font-family:var(--mono) !important; font-size:10px !important;
  letter-spacing:1px; text-transform:uppercase; padding:8px 16px !important;
  transition:background .18s ease, border-color .18s ease, color .18s ease;
}
.tv-btn.tv-on {
  background:#1C1608 !important; border-color:var(--orange) !important;
  color:var(--orange) !important;
}
.tv-btn:hover {
  background:#1C1608 !important; border-color:var(--orange) !important;
  color:var(--orange) !important;
}
/* Live wire rows */
.wire-item {
  padding:9px 0; border-bottom:1px solid var(--border);
  transition:background .16s ease, padding-left .16s ease;
}
.wire-item:hover { background:rgba(255,107,0,0.05); padding-left:6px; }
.wire-item a { text-decoration:none; }

table.dataTable.row-clickable tbody tr { cursor:pointer; }
table.dataTable.row-clickable tbody tr:hover td {
  background:rgba(255,107,0,0.10) !important;
  box-shadow:inset 2px 0 0 var(--orange);
}
.dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate {
  color:var(--muted) !important; font-size:10px !important;
  font-family:var(--mono) !important; margin-top:10px !important;
}
.dataTables_filter input, .dataTables_length select {
  background:var(--s2) !important; border:1px solid var(--border2) !important;
  color:var(--text) !important; padding:4px 8px !important; font-size:11px !important;
}
.paginate_button { color:var(--muted) !important; }
.paginate_button.current, .paginate_button.current:hover {
  background:rgba(255,107,0,0.2) !important; color:var(--orange) !important;
  border:1px solid var(--orange) !important;
}

/* ── RATING PILLS ── */
.r-sb { color:#00C853; font-weight:700; }
.r-b  { color:#64DD17; }
.r-h  { color:#FFD600; }
.r-u  { color:#FF6D00; }
.r-av { color:#FF3D00; }

/* ── SCORE BAR ── */
.sbar { display:flex; align-items:center; gap:6px; }
.sbar-track { flex:1; height:4px; background:var(--border2); }
.sbar-fill  { height:100%; }
.sbar-val   { font-family:var(--mono); font-size:11px; min-width:26px; text-align:right; color:var(--text); }

/* ── FILTER ROW ── */
.filter-bar { display:flex; gap:12px; align-items:flex-end; flex-wrap:wrap; margin-bottom:16px; }
.filter-grp { display:flex; flex-direction:column; gap:4px; }
.filter-lbl { font-family:var(--mono); font-size:9px; color:var(--muted);
              text-transform:uppercase; letter-spacing:1px; }
.selectize-input  { background:var(--s2) !important; border:1px solid var(--border2) !important;
                    color:var(--text) !important; font-size:11px !important;
                    font-family:var(--mono) !important; box-shadow:none !important; }
.selectize-dropdown { background:var(--s2) !important; border:1px solid var(--border2) !important;
                      color:var(--text) !important; font-family:var(--mono) !important; font-size:11px !important; }
.selectize-dropdown .option:hover, .selectize-dropdown .active { background:var(--s3) !important; color:var(--orange) !important; }
.irs--shiny .irs-bar, .irs--shiny .irs-bar--single { background:var(--orange) !important; border-color:var(--orange) !important; }
.irs--shiny .irs-handle { background:var(--orange) !important; border-color:var(--orange2) !important; }
.irs--shiny .irs-single { background:var(--s3) !important; color:var(--orange) !important; font-family:var(--mono) !important; font-size:10px !important; }
.irs--shiny .irs-line  { background:var(--border2) !important; }
.checkbox label { color:var(--text2) !important; font-family:var(--mono) !important; font-size:11px !important; }

/* ── METRIC TABLE ── */
.mt { width:100%; border-collapse:collapse; }
.mt td { padding:6px 0; border-bottom:1px solid var(--border); font-family:var(--mono); font-size:11px; }
.mt tr:last-child td { border:none; }
.mt .mk { color:var(--muted); }
.mt .mv { text-align:right; color:var(--text); font-weight:500; }

/* ── SHORT INFO GRID ── */
.si-grid { display:grid; grid-template-columns:1fr 1fr; gap:10px; margin-bottom:12px; }
.si-cell { background:var(--s2); border:1px solid var(--border); padding:12px; }
.si-k  { font-family:var(--mono); font-size:9px; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:5px; }
.si-v  { font-family:var(--mono); font-size:18px; font-weight:600; color:var(--text); }
.squeeze-badge { padding:12px; text-align:center; font-family:var(--mono); font-weight:600; font-size:13px; letter-spacing:1px; text-transform:uppercase; border:1px solid; }
.sq-hc  { color:var(--red);    border-color:var(--red);    background:rgba(255,61,0,0.1); }
.sq-wl  { color:var(--yellow); border-color:var(--yellow); background:rgba(255,214,0,0.1); }
.sq-ls  { color:var(--cyan);   border-color:var(--cyan);   background:rgba(0,184,217,0.1); }
.sq-ns  { color:var(--muted);  border-color:var(--border); background:var(--s2); }

/* Classes below are generated at runtime by DataTables, ionRangeSlider and
   selectize, and by the DT row callback (.comp-highlight) and the regime badge
   (.regime-*, built with paste0). They appear unused to a naive grep of this
   file — they are not. */

/* ── NEWS FEED ── */
/* ── WSB TRENDING ── */
.wsb-item { display:flex; align-items:center; gap:10px; padding:8px 12px; border-bottom:1px solid var(--border); font-family:var(--mono); font-size:11px; transition:background 0.15s; }
.wsb-item:hover { background:rgba(255,107,0,0.04); }
.wsb-item:last-child { border:none; }
.wsb-rank { color:var(--muted); min-width:24px; font-weight:600; font-size:10px; }
.wsb-sym  { color:var(--orange); font-weight:700; min-width:55px; letter-spacing:0.3px; }
.wsb-name { color:var(--text2); flex:1; font-size:10px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.wsb-mentions { color:var(--text); font-weight:600; min-width:45px; text-align:right; }
.wsb-upvotes  { color:var(--muted); min-width:50px; text-align:right; font-size:10px; }
.wsb-momentum { font-size:9px; font-weight:700; min-width:65px; text-align:right; letter-spacing:0.3px; }
.wsb-surge  { color:#FF3D00; }
.wsb-rise   { color:#00C853; }
.wsb-fade   { color:#FF6B00; }
.wsb-fall   { color:#FF3D00; }
.wsb-steady { color:#666666; }

/* ── STOCKTWITS ── */
.st-item { display:flex; align-items:center; gap:10px; padding:8px 12px; border-bottom:1px solid var(--border); font-family:var(--mono); font-size:11px; transition:background 0.15s; }
.st-item:hover { background:rgba(0,184,217,0.04); }
.st-item:last-child { border:none; }
.st-rank  { color:var(--muted); min-width:20px; font-weight:600; font-size:10px; }
.st-sym   { color:var(--cyan); font-weight:700; min-width:55px; letter-spacing:0.3px; }
.st-name  { color:var(--text2); flex:1; font-size:10px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.st-watch { color:var(--text); font-weight:600; min-width:50px; text-align:right; font-size:10px; }
.st-sent  { font-size:9px; font-weight:700; min-width:75px; text-align:right; letter-spacing:0.3px; }
.st-bull  { color:#00C853; }
.st-bear  { color:#FF3D00; }
.st-neut  { color:#666666; }

/* ── DISCLAIMER GATE ── */
/* Rendered as static markup so it paints before Shiny connects, and dismissed
   by plain JS — it holds no reactive state and cannot affect any output. */
#disclaimer-gate {
  position:fixed; inset:0; z-index:100000;
  background:rgba(6,6,6,0.97);
  display:flex; align-items:center; justify-content:center;
  padding:24px; overflow-y:auto;
}
#disclaimer-gate.hidden { display:none; }
.dg-card {
  background:var(--s1); border:1px solid var(--border2);
  border-top:3px solid var(--orange);
  max-width:620px; width:100%; padding:30px 32px 26px;
  box-shadow:0 24px 70px rgba(0,0,0,0.75);
}
.dg-eyebrow {
  font-family:var(--mono); font-size:9.5px; letter-spacing:2px;
  text-transform:uppercase; color:var(--orange); margin-bottom:14px;
}
.dg-title {
  font-family:var(--mono); font-size:19px; font-weight:600; color:var(--text);
  letter-spacing:0.5px; margin-bottom:16px; line-height:1.3;
}
.dg-card p {
  font-size:12.5px; line-height:1.65; color:var(--text2); margin-bottom:11px;
}
.dg-card strong { color:var(--text); font-weight:600; }
.dg-flag {
  border-left:3px solid var(--yellow); background:rgba(255,214,0,0.05);
  padding:11px 14px; margin:14px 0 16px;
}
.dg-flag p { margin:0; font-size:12px; color:var(--text2); }
.dg-btn {
  display:block; width:100%; margin-top:20px; padding:13px;
  background:var(--orange); color:#000; border:none; cursor:pointer;
  font-family:var(--mono); font-size:12px; font-weight:600;
  letter-spacing:1.5px; text-transform:uppercase; transition:background 0.15s;
}
.dg-btn:hover { background:var(--orange2); }
.dg-btn:focus-visible { outline:2px solid var(--cyan); outline-offset:2px; }
.dg-foot {
  margin-top:14px; font-family:var(--mono); font-size:10px;
  color:var(--muted); text-align:center; line-height:1.5;
}
@media (max-width:600px) {
  .dg-card { padding:22px 20px 20px; }
  .dg-title { font-size:16px; }
}

/* ── METHODOLOGY PAGE ── */
.doc { max-width:1100px; }
.doc h2 { font-family:var(--mono); font-size:13px; color:var(--orange); letter-spacing:1.5px;
          text-transform:uppercase; margin:26px 0 10px; padding-bottom:6px;
          border-bottom:1px solid var(--border); }
.doc h2:first-child { margin-top:0; }
.doc p  { font-size:12.5px; line-height:1.65; color:var(--text2); margin-bottom:10px; }
.doc strong { color:var(--text); font-weight:600; }
.doc code { font-family:var(--mono); font-size:11px; background:var(--s2);
            border:1px solid var(--border); padding:1px 5px; color:var(--orange3); }
.doc pre  { font-family:var(--mono); font-size:11px; background:var(--s2);
            border:1px solid var(--border); border-left:3px solid var(--orange);
            padding:12px 14px; margin:10px 0 14px; overflow-x:auto;
            color:var(--text2); line-height:1.6; }
.doc ul { margin:0 0 12px 18px; }
.doc li { font-size:12.5px; line-height:1.6; color:var(--text2); margin-bottom:6px; }
.dtable { width:100%; border-collapse:collapse; margin:8px 0 16px; font-size:11.5px; }
.dtable th { font-family:var(--mono); font-size:9.5px; text-transform:uppercase;
             letter-spacing:1px; color:var(--muted); text-align:left;
             padding:7px 10px; border-bottom:1px solid var(--border2); }
.dtable td { padding:7px 10px; border-bottom:1px solid var(--border);
             color:var(--text2); vertical-align:top; }
.dtable td.num { font-family:var(--mono); text-align:right;
                 font-variant-numeric:tabular-nums; color:var(--text); }
.dtable tr:last-child td { border-bottom:none; }
.callout { border:1px solid var(--border2); border-left:3px solid var(--yellow);
           background:rgba(255,214,0,0.04); padding:12px 14px; margin:12px 0 16px; }
.callout .ct { font-family:var(--mono); font-size:9.5px; color:var(--yellow);
               text-transform:uppercase; letter-spacing:1px; margin-bottom:6px; }
.callout p { margin-bottom:6px; font-size:12px; }
.callout p:last-child { margin-bottom:0; }
.statrow { display:grid; grid-template-columns:repeat(4,1fr); gap:1px;
           background:var(--border); margin:6px 0 16px; }
.statcell { background:var(--s2); padding:14px; }
.statcell .k { font-family:var(--mono); font-size:9px; color:var(--muted);
               text-transform:uppercase; letter-spacing:1px; margin-bottom:6px; }
.statcell .v { font-family:var(--mono); font-size:20px; font-weight:600; color:var(--orange); }
.statcell .s { font-size:10px; color:var(--muted); margin-top:3px; }
@media (max-width:860px){ .statrow { grid-template-columns:repeat(2,1fr); } }

/* ── EARNINGS ROW ── */
.earn-item { display:flex; align-items:center; gap:10px; padding:7px 0; border-bottom:1px solid var(--border); font-family:var(--mono); font-size:11px; }
.earn-item:last-child { border:none; }
.earn-date   { color:var(--muted); min-width:80px; }
.earn-time   { color:var(--text2); font-size:10px; }
.earn-eps    { margin-left:auto; color:var(--text); }

/* ── MACRO PANEL ── */
.macro-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:10px; margin-bottom:14px; }
.macro-cell { background:var(--s2); border:1px solid var(--border); padding:12px; }
.macro-k    { font-family:var(--mono); font-size:9px; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-bottom:6px; }
.macro-v    { font-family:var(--mono); font-size:20px; font-weight:600; color:var(--orange); }
.macro-sub  { font-family:var(--mono); font-size:9px; color:var(--muted); margin-top:3px; }

/* ── DCF TABLE ── */
.dcf-row { display:flex; justify-content:space-between; padding:6px 0; border-bottom:1px solid var(--border); font-family:var(--mono); font-size:11px; }
.dcf-row:last-child { border:none; }
.dcf-lbl { color:var(--muted); }
.dcf-val { color:var(--text); font-weight:500; }
.dcf-upside-pos { color:var(--green); font-size:15px; font-weight:700; }
.dcf-upside-neg { color:var(--red);   font-size:15px; font-weight:700; }

/* ── VBOX ROW ── */
.vbox-row { display:grid; grid-template-columns:repeat(4,1fr); gap:1px; background:var(--border); margin-bottom:16px; }
.vbox { background:var(--s1); padding:12px 14px; }
.vbox-v { font-family:var(--mono); font-size:26px; font-weight:600; color:var(--orange); }
.vbox-k { font-family:var(--mono); font-size:9px; color:var(--muted); text-transform:uppercase; letter-spacing:1px; margin-top:4px; }

/* ── PEER COMPS ── */
.comp-highlight td { background:rgba(255,107,0,0.08) !important; }

/* ── SWEET SPOT / UNICORN TABLES ── */
.ss-table-wrap  { overflow-x:auto; }
.conf-bar       { display:flex; align-items:center; gap:6px; }
.conf-track     { flex:1; height:4px; background:var(--s3); border-radius:2px; }
.conf-fill      { height:4px; border-radius:2px; }
.conf-val       { font-family:var(--mono); font-size:10px; color:var(--text2); min-width:28px; }
.exp-ret        { font-family:var(--mono); font-size:11px; font-weight:600; }
.exp-ret.pos    { color:var(--green); }
.exp-ret.neg    { color:var(--red); }
.regime-badge   { display:inline-block; font-family:var(--mono); font-size:9px;
                  font-weight:600; padding:2px 6px; border-radius:2px; letter-spacing:0.5px; }
.regime-NEUTRAL  { background:rgba(0,184,217,0.15); color:var(--cyan); border:1px solid rgba(0,184,217,0.3); }
.regime-HIGH_RATE{ background:rgba(255,61,0,0.15);  color:var(--red);  border:1px solid rgba(255,61,0,0.3); }
.regime-LOW_RATE { background:rgba(0,200,83,0.15);  color:var(--green);border:1px solid rgba(0,200,83,0.3); }
.regime-RISK_OFF { background:rgba(255,214,0,0.15); color:var(--yellow);border:1px solid rgba(255,214,0,0.3); }
.regime-INVERTED { background:rgba(255,61,0,0.15);  color:var(--red);  border:1px solid rgba(255,61,0,0.3); }
.unicorn-badge  { display:inline-block; font-family:var(--mono); font-size:9px;
                  font-weight:700; padding:2px 7px; border-radius:2px;
                  background:rgba(255,107,0,0.18); color:var(--orange);
                  border:1px solid rgba(255,107,0,0.4); letter-spacing:0.5px; }
.panel-head-regime { display:flex; align-items:center; gap:10px; }
.ss-rank        { font-family:var(--mono); font-size:13px; font-weight:700;
                  color:var(--orange); min-width:22px; }
"

# =============================================================================
# MOBILE CSS — injected separately so it's easy to update
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
  .g345 { grid-template-columns: 1fr; }
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
  /* Remove hover effects that don't work on touch */
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
# =============================================================================
ui <- fluidPage(
  tags$head(
    # The app shipped with no title at all, so the browser tab showed the raw
    # URL and nothing could build a link preview — LinkedIn, Slack and Twitter
    # all got a blank card, which is what made the URL look invalid when pasted.
    tags$title("EdgeScreener — Quantitative Equity Terminal"),
    tags$meta(name="description", content=PAGE_DESC),
    tags$meta(property="og:type",        content="website"),
    tags$meta(property="og:site_name",   content="EdgeScreener"),
    tags$meta(property="og:title",       content="EdgeScreener — Quantitative Equity Terminal"),
    tags$meta(property="og:description", content=PAGE_DESC),
    tags$meta(property="og:url",         content=PAGE_URL),
    tags$meta(name="twitter:card",        content="summary"),
    tags$meta(name="twitter:title",       content="EdgeScreener — Quantitative Equity Terminal"),
    tags$meta(name="twitter:description", content=PAGE_DESC),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600;700&display=swap"),
    tags$style(HTML(bloomberg_css)),
    tags$style(HTML(mobile_css)),

    # Motion One (motion.dev) — loaded as an ES module from a CDN.
    #
    # Deliberately additive: nothing starts hidden in CSS. Elements are animated
    # FROM opacity 0 in JS only once the library has actually loaded, so a
    # blocked CDN, an offline user or a failed import costs the animation and
    # nothing else. Starting them hidden in CSS would leave the page blank in
    # exactly those cases.
    #
    # Restrained on purpose: a terminal that bounces reads as a toy. Short
    # durations, small distances, no easing overshoot.
    tags$script(HTML("
      window.__motionReady = false;
      window.__reduceMotion =
        window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    ")),
    tags$script(type = "module", HTML("
      if (!window.__reduceMotion) {
        try {
          const m = await import('https://cdn.jsdelivr.net/npm/motion@11.11.13/+esm');
          window.__motion = m;
          window.__motionReady = true;

          // Panels enter on tab switch, staggered so the eye follows the page
          window.animatePane = function (pane) {
            if (!window.__motionReady || !pane) return;
            const items = pane.querySelectorAll('.panel, .kpi-cell, .statcell');
            if (!items.length) return;
            m.animate(items,
              { opacity: [0, 1], transform: ['translateY(6px)', 'translateY(0px)'] },
              { duration: 0.28, delay: m.stagger(0.025), easing: 'ease-out' });
          };

          // Count numeric KPI values up rather than snapping to them
          window.animateCounters = function (root) {
            if (!window.__motionReady) return;
            (root || document).querySelectorAll('.kpi-v, .statcell .v').forEach(function (el) {
              const txt = (el.textContent || '').trim();
              const match = txt.match(/^([+-]?)(\\d[\\d,]*\\.?\\d*)(.*)$/);
              if (!match) return;
              const sign = match[1], suffix = match[3];
              const target = parseFloat(match[2].replace(/,/g, ''));
              if (!isFinite(target)) return;
              const decimals = (match[2].split('.')[1] || '').length;
              m.animate(function (p) {
                el.textContent = sign + (target * p).toFixed(decimals) + suffix;
              }, { duration: 0.5, easing: 'ease-out' });
            });
          };

          window.animatePane(document.querySelector('.tab-pane.active'));
          window.animateCounters(document.querySelector('.tab-pane.active'));
        } catch (e) {
          // CDN unreachable or import failed — leave the page exactly as-is
          window.__motionReady = false;
        }
      }
    "))
  ),

  # ── DISCLAIMER GATE ──────────────────────────────────────────────────────
  # Static markup, shown once per browser session. Deliberately states the
  # model's own null result: the dashboard ranks stocks into score bands while the
  # Methodology tab documents an information coefficient of ~0, and a visitor
  # should meet that contradiction before the rankings, not after.
  div(id="disclaimer-gate",
    div(class="dg-card",
      div(class="dg-eyebrow", "Before you continue"),
      div(class="dg-title", "Model output is not a prediction, and not investment advice."),
      tags$p("EdgeScreener is a personal portfolio project demonstrating a quantitative ",
             "data pipeline. The operator is ", tags$strong("not a broker-dealer, not an ",
             "investment adviser"), ", and not licensed to give financial advice. Nothing ",
             "here is a recommendation, solicitation or offer to buy or sell any security, ",
             "and no advisory relationship is created by using this site."),
      div(class="dg-flag",
        tags$p(tags$strong("Rankings carry no demonstrated predictive power. "),
               "In out-of-sample testing across 32 rebalances, this model's rankings showed ",
               "no statistically significant relationship to forward returns (information ",
               "coefficient +0.003, t = 0.09). Stocks are labelled by score band — Very ",
               "Strong through Very Weak — describing where they sit in the model's own ",
               "distribution. They are not forecasts, not endorsements, and not a ",
               "basis for any investment decision. The Methodology tab shows the full ",
               "evidence.")),
      tags$p(tags$strong("Backtested results are hypothetical. "),
             "They do not represent actual trading, were produced with the benefit of ",
             "hindsight, and exclude trading costs, slippage and the effect of companies ",
             "that were delisted. No representation is made that any account will achieve ",
             "similar results. ", tags$strong("Past performance is not a guarantee of ",
             "future results."), ""),
      tags$p("Data comes from free public APIs, may be delayed, incomplete or inaccurate, ",
             "and is provided \"as is\" with no warranty of any kind. Do your own research ",
             "and consult a licensed financial professional before making any investment ",
             "decision. You are solely responsible for anything you do with this ",
             "information."),
      tags$button(id="disclaimer-accept", class="dg-btn",
                  onclick="dismissDisclaimer()", "I understand — continue"),
      div(class="dg-foot",
        "Shown once per session · Full disclosures on the Methodology tab · ",
        "Not affiliated with any exchange or data provider")
    )
  ),
  tags$script(HTML("
    // Runs before Shiny connects. sessionStorage means a returning visitor is
    // not nagged mid-session, but a fresh visit always sees it again.
    (function () {
      try {
        if (sessionStorage.getItem('edgescreener_disclaimer_v1') === 'ack') {
          var g = document.getElementById('disclaimer-gate');
          if (g) g.classList.add('hidden');
        }
      } catch (e) { /* private mode: just show it */ }
    })();
    function dismissDisclaimer() {
      var g = document.getElementById('disclaimer-gate');
      if (g) g.classList.add('hidden');
      try { sessionStorage.setItem('edgescreener_disclaimer_v1', 'ack'); } catch (e) {}
      window.dispatchEvent(new Event('resize'));  // let plots size correctly
    }
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' || e.key === 'Escape') {
        var g = document.getElementById('disclaimer-gate');
        if (g && !g.classList.contains('hidden')) dismissDisclaimer();
      }
    });
  ")),

  # ── TICKER TAPE ──────────────────────────────────────────────────────────
  div(class="ticker-wrap",
    div(class="ticker-label","▶ LIVE"),
    div(class="ticker-scroll-wrap",
      uiOutput("ticker_tape")
    )
  ),

  # ── TOPBAR ───────────────────────────────────────────────────────────────
  div(class="topbar",
    div(class="topbar-logo", "EdgeScreener",
        tags$span(class="topbar-sub", "EQUITY TERMINAL")),
    div(class="topbar-nav", id="topbar-nav-desktop",
      div(class="topbar-nav-item active", id="nav-overview",  onclick="showPane('overview')",  "Overview"),
      div(class="topbar-nav-item",        id="nav-screener",  onclick="showPane('screener')",  "Screener"),
      div(class="topbar-nav-item",        id="nav-squeeze",   onclick="showPane('squeeze')",   "Short/Squeeze"),
      div(class="topbar-nav-item",        id="nav-deepdive",  onclick="showPane('deepdive')",  "Deep Dive"),
      div(class="topbar-nav-item",        id="nav-macro",     onclick="showPane('macro')",     "Macro"),
      div(class="topbar-nav-item",        id="nav-news",      onclick="showPane('news')",      "News & Events"),
      div(class="topbar-nav-item",        id="nav-about",     onclick="showPane('about')",     "Methodology")
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
    div(class="mobile-nav-item active", id="mnav-overview",  onclick="showPane('overview')",  "▸ Overview"),
    div(class="mobile-nav-item",        id="mnav-screener",  onclick="showPane('screener')",  "▸ Screener"),
    div(class="mobile-nav-item",        id="mnav-squeeze",   onclick="showPane('squeeze')",   "▸ Short / Squeeze"),
    div(class="mobile-nav-item",        id="mnav-deepdive",  onclick="showPane('deepdive')",  "▸ Deep Dive"),
    div(class="mobile-nav-item",        id="mnav-macro",     onclick="showPane('macro')",     "▸ Macro"),
    div(class="mobile-nav-item",        id="mnav-news",      onclick="showPane('news')",      "▸ News & Events"),
    div(class="mobile-nav-item",        id="mnav-about",     onclick="showPane('about')",     "▸ Methodology")
  ),

  # ── BODY ─────────────────────────────────────────────────────────────────
  div(class="terminal-body",

    # ── OVERVIEW ───────────────────────────────────────────────────────────
    div(class="tab-pane active", id="pane-overview",
      div(class="kpi-strip",
        div(class="kpi-cell", div(class="kpi-k","Universe"),     div(class="kpi-v", uiOutput("kpi_n")),   div(class="kpi-s","stocks scored")),
        div(class="kpi-cell", div(class="kpi-k","Very Strong"),  div(class="kpi-v", uiOutput("kpi_sb")),  div(class="kpi-s","top score band")),
        div(class="kpi-cell", div(class="kpi-k","Strong"),       div(class="kpi-v", uiOutput("kpi_b")),   div(class="kpi-s","second band")),
        div(class="kpi-cell", div(class="kpi-k","Squeeze Setup"),div(class="kpi-v", uiOutput("kpi_sq")), div(class="kpi-s","candidates")),
        div(class="kpi-cell", div(class="kpi-k","Avg Score"),    div(class="kpi-v", uiOutput("kpi_avg")),div(class="kpi-s","universe avg")),
        div(class="kpi-cell", div(class="kpi-k","Golden Cross"), div(class="kpi-v", uiOutput("kpi_gc")), div(class="kpi-s","bullish MA signal"))
      ),
      div(class="g73",
        div(
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-regime",
                div(class="panel-head-title","TOP 20 — SWEET SPOT"),
                uiOutput("regime_badge_ui")
              ),
              # Previously claimed predicted returns and backtested confidence,
              # which is exactly what the Methodology tab disproves.
              div(class="panel-head-meta","HIGHEST MODEL SCORE · RANKING OUTPUT, NOT A RECOMMENDATION")),
            div(class="panel-body ss-table-wrap", DTOutput("top15_table"))
          ),
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","🦄 TOP 15 UNICORNS"),
              div(class="panel-head-meta","MARKET CAP < $5B · REVENUE GROWTH >15% · WEEKLY CONFIDENCE")),
            div(class="panel-body ss-table-wrap", DTOutput("unicorn_table"))
          )
        ),
        div(
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","LIVE TV"),
              div(class="panel-head-meta", uiOutput("tv_meta"))),
            div(class="panel-body",
              uiOutput("tv_player"),
              div(style="display:flex;gap:6px;flex-wrap:wrap;margin-top:10px;",
                lapply(names(TV_CHANNELS), function(n)
                  actionButton(paste0("tv_", make.names(n)), n, class="tv-btn")))
            )
          ),
          div(class="panel",
            div(class="panel-head", div(class="panel-head-title","SCORE DISTRIBUTION")),
            div(class="panel-body", plotlyOutput("score_dist", height="200px"))
          ),
          div(class="panel",
            div(class="panel-head", div(class="panel-head-title","RATING BREAKDOWN")),
            div(class="panel-body", plotlyOutput("rating_donut", height="200px"))
          )
        )
      ),
      div(class="g2",
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","SECTOR AVG SCORE")),
          div(class="panel-body", plotlyOutput("sector_bar", height="220px"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","SECTOR HEATMAP")),
          div(class="panel-body", plotlyOutput("sector_heat", height="220px"))
        )
      )
    ),

    # ── SCREENER ───────────────────────────────────────────────────────────
    div(class="tab-pane", id="pane-screener",
      div(class="panel",
        div(class="panel-head", div(class="panel-head-title","FILTERS")),
        div(class="panel-body",
          div(class="filter-bar",
            div(class="filter-grp", div(class="filter-lbl","Sector"),
              selectInput("f_sector","",choices=all_sectors,width="180px")),
            div(class="filter-grp", div(class="filter-lbl","Rating"),
              selectInput("f_rating","",choices=all_ratings,width="155px")),
            div(class="filter-grp", div(class="filter-lbl","Min Score"),
              sliderInput("f_score","",min=0,max=100,value=0,step=5,width="200px")),
            div(class="filter-grp", div(class="filter-lbl",""),
              checkboxInput("f_squeeze","Squeeze candidates only",FALSE)),
            div(class="filter-grp", div(class="filter-lbl",""),
              checkboxInput("f_golden","Golden cross only",FALSE))
          )
        )
      ),
      div(class="panel",
        div(class="panel-head",
          div(class="panel-head-title","FULL UNIVERSE SCREENER"),
          uiOutput("screener_badge")),
        div(class="panel-body", DTOutput("screener_table"))
      )
    ),

    # ── SHORT / SQUEEZE ────────────────────────────────────────────────────
    div(class="tab-pane", id="pane-squeeze",
      div(class="g84",
        div(class="panel",
          div(class="panel-head",
            div(class="panel-head-title","SHORT FLOAT VS EARNINGS GROWTH"),
            div(class="panel-head-meta","TOP-RIGHT QUADRANT = SQUEEZE SETUP")),
          div(class="panel-body", plotlyOutput("squeeze_scatter", height="420px"))
        ),
        div(
          div(class="panel",
            div(class="panel-head", div(class="panel-head-title","SQUEEZE TIERS")),
            div(class="panel-body", plotlyOutput("squeeze_tiers", height="180px"))
          ),
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","HIGH CONVICTION"),
              div(class="panel-head-meta","TOP SETUPS")),
            div(class="panel-body", DTOutput("squeeze_top"))
          )
        )
      ),
      div(class="g3",
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","SHORT TREND DIRECTION")),
          div(class="panel-body", plotlyOutput("short_trend", height="220px"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","DAYS TO COVER DIST.")),
          div(class="panel-body", plotlyOutput("days_cover", height="220px"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","SQUEEZE SCORE DIST.")),
          div(class="panel-body", plotlyOutput("squeeze_dist", height="220px"))
        )
      )
    ),

    # ── DEEP DIVE ──────────────────────────────────────────────────────────
    div(class="tab-pane", id="pane-deepdive",
      div(class="panel",
        div(class="panel-head", div(class="panel-head-title","SELECT STOCK")),
        div(class="panel-body",
          div(style="display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
            div(style="min-width:180px;",
              selectInput("dd_ticker","",
                choices=if(!is.null(master_data)) sort(master_data$symbol) else "AAPL",
                width="100%")),
            uiOutput("dd_header")
          )
        )
      ),
      div(class="vbox-row",
        div(class="vbox", div(class="vbox-v", uiOutput("dd_v_master")),  div(class="vbox-k","Master Score")),
        div(class="vbox", div(class="vbox-v", uiOutput("dd_v_fund")),    div(class="vbox-k","Fundamental")),
        div(class="vbox", div(class="vbox-v", uiOutput("dd_v_mom")),     div(class="vbox-k","Momentum")),
        div(class="vbox", div(class="vbox-v", uiOutput("dd_v_squeeze")), div(class="vbox-k","Squeeze Score"))
      ),
      div(class="g84",
        div(class="panel",
          div(class="panel-head",
            div(class="panel-head-title", uiOutput("dd_chart_title")),
            div(style="display:flex;gap:8px;",
              actionButton("btn_1y","1Y",style="background:var(--s3);color:var(--orange2);border:1px solid var(--border2);font-family:var(--mono);font-size:10px;padding:3px 8px;cursor:pointer;"),
              actionButton("btn_6m","6M",style="background:var(--s3);color:var(--muted);border:1px solid var(--border2);font-family:var(--mono);font-size:10px;padding:3px 8px;cursor:pointer;"),
              actionButton("btn_3m","3M",style="background:var(--s3);color:var(--muted);border:1px solid var(--border2);font-family:var(--mono);font-size:10px;padding:3px 8px;cursor:pointer;")
            )
          ),
          div(class="panel-body", plotlyOutput("dd_price", height="340px")),
          div(class="panel-body", style="padding-top:0;", plotlyOutput("dd_volume", height="80px")),
          div(class="panel-body", style="padding-top:0;", plotlyOutput("dd_macd",   height="100px")),
          div(class="panel-body", style="padding-top:0;", plotlyOutput("dd_rsi",    height="100px"))
        ),
        div(
          div(class="panel",
            div(class="panel-head", div(class="panel-head-title","KEY METRICS")),
            div(class="panel-body", uiOutput("dd_metrics"))
          ),
          div(class="panel",
            div(class="panel-head", div(class="panel-head-title","SHORT INTEREST")),
            div(class="panel-body", uiOutput("dd_short"))
          )
        )
      ),
      div(class="g3",
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","SCORE RADAR")),
          div(class="panel-body", plotlyOutput("dd_radar", height="240px"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","DCF VALUATION")),
          div(class="panel-body", uiOutput("dd_dcf"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","PEER COMPS")),
          div(class="panel-body", DTOutput("dd_comps"))
        )
      ),
      # Deep Dive was entirely price, technicals and valuation. News is already
      # collected tagged by ticker and social sentiment per symbol, but neither
      # was ever connected to the stock being examined.
      div(class="g64",
        div(class="panel",
          div(class="panel-head",
            div(class="panel-head-title","COMPANY NEWS"),
            div(class="panel-head-meta", uiOutput("dd_news_count"))),
          div(class="panel-body", style="max-height:320px;overflow-y:auto;",
            uiOutput("dd_news"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","CROWD SENTIMENT")),
          div(class="panel-body", uiOutput("dd_sentiment"))
        )
      ),
      div(class="panel",
        div(class="panel-head",
          div(class="panel-head-title","SEC FILINGS & INSIDER ACTIVITY"),
          div(class="panel-head-meta", uiOutput("dd_filings_meta"))),
        div(class="panel-body", uiOutput("dd_filings"))
      ),
      div(class="panel",
        div(class="panel-head",
          div(class="panel-head-title","LBO RETURN SENSITIVITY"),
          div(class="panel-head-meta","EQUITY IRR")),
        div(class="panel-body", uiOutput("dd_lbo"))
      ),
      div(class="panel",
        div(class="panel-head",
          div(class="panel-head-title","RESEARCH BRIEF"),
          div(class="panel-head-meta","GENERATED")),
        div(class="panel-body", uiOutput("dd_pitch"))
      )
    ),

    # ── MACRO ──────────────────────────────────────────────────────────────
    div(class="tab-pane", id="pane-macro",
      div(class="panel",
        div(class="panel-head", div(class="panel-head-title","MACRO INDICATORS")),
        div(class="panel-body", uiOutput("macro_kpis"))
      ),
      div(class="g2",
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","YIELD CURVE (UST)")),
          div(class="panel-body", plotlyOutput("yield_curve", height="260px"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","10Y TREASURY YIELD — 2 YEAR")),
          div(class="panel-body", plotlyOutput("treasury_10y", height="260px"))
        )
      ),
      div(class="g2",
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","FED FUNDS RATE")),
          div(class="panel-body", plotlyOutput("fed_funds", height="220px"))
        ),
        div(class="panel",
          div(class="panel-head", div(class="panel-head-title","YIELD CURVE SPREAD (10Y-2Y)")),
          div(class="panel-body", plotlyOutput("yield_spread", height="220px"))
        )
      ),
      # CPI and unemployment were pulled from FRED on every run and never
      # rendered. Rates alone are half a macro picture; inflation and employment
      # are the other half.
      div(class="g2",
        div(class="panel",
          div(class="panel-head",
            div(class="panel-head-title","INFLATION (CPI, YEAR-OVER-YEAR)"),
            div(class="panel-head-meta", uiOutput("cpi_latest"))),
          div(class="panel-body", plotlyOutput("cpi_yoy", height="220px"))
        ),
        div(class="panel",
          div(class="panel-head",
            div(class="panel-head-title","UNEMPLOYMENT RATE"),
            div(class="panel-head-meta", uiOutput("unrate_latest"))),
          div(class="panel-body", plotlyOutput("unemployment", height="220px"))
        )
      )
    ),

    # ── NEWS & EVENTS ──────────────────────────────────────────────────────
    div(class="tab-pane", id="pane-news",
      div(class="g345",
        div(
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","EARNINGS CALENDAR"),
              div(class="panel-head-meta","NEXT 60 DAYS")),
            div(class="panel-body", style="max-height:420px;overflow-y:auto;",
              uiOutput("earnings_feed"))
          ),
          div(class="panel",
            div(class="panel-head", div(class="panel-head-title","SECTOR PERFORMANCE TODAY")),
            div(class="panel-body", plotlyOutput("sector_today", height="240px"))
          )
        ),
        div(
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","LIVE WIRE"),
              div(class="panel-head-meta", uiOutput("live_news_meta"))),
            div(class="panel-body", style="max-height:420px;overflow-y:auto;",
              uiOutput("live_news_feed"))
          ),
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","MARKET NEWS FEED"),
              div(class="panel-head-meta", uiOutput("news_count"))),
            div(class="panel-body", style="max-height:420px;overflow-y:auto;",
              uiOutput("news_feed"))
          )
        ),
        div(
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","WSB TRENDING"),
              div(class="panel-head-meta", uiOutput("wsb_count"))),
            div(class="panel-body", style="max-height:380px;overflow-y:auto;",
              uiOutput("wsb_feed"))
          ),
          div(class="panel",
            div(class="panel-head",
              div(class="panel-head-title","STOCKTWITS TRENDING"),
              div(class="panel-head-meta", uiOutput("stwits_count"))),
            div(class="panel-body", style="max-height:380px;overflow-y:auto;",
              uiOutput("stwits_feed"))
          )
        )
      )
    ),

    # ── METHODOLOGY ────────────────────────────────────────────────────────
    div(class="tab-pane", id="pane-about",
      div(class="panel",
        div(class="panel-head",
          div(class="panel-head-title","METHODOLOGY & VALIDATION"),
          div(class="panel-head-meta", uiOutput("about_meta"))),
        div(class="panel-body", div(class="doc", uiOutput("about_body")))
      )
    )
  ),

  # ── JS ────────────────────────────────────────────────────────────────────
  tags$script(HTML("
    function showPane(name) {
      document.querySelectorAll('.tab-pane').forEach(p => p.classList.remove('active'));
      document.querySelectorAll('.topbar-nav-item').forEach(n => n.classList.remove('active'));
      document.querySelectorAll('.mobile-nav-item').forEach(n => n.classList.remove('active'));
      // The stream lives on the Overview pane now. Panes are hidden with CSS
      // rather than unmounted, so without this the video keeps playing — and
      // keeps making sound — while the viewer is on another tab.
      if (name !== 'overview') {
        var f = document.querySelector('#pane-overview iframe');
        if (f && f.src && f.src !== 'about:blank') { f.dataset.src = f.src; f.src = 'about:blank'; }
      } else {
        var f2 = document.querySelector('#pane-overview iframe');
        if (f2 && f2.src === 'about:blank' && f2.dataset.src) f2.src = f2.dataset.src;
      }
      var pane = document.getElementById('pane-' + name);
      pane.classList.add('active');
      // Guarded: these only exist once motion.dev has loaded successfully, so a
      // blocked CDN or reduced-motion preference simply skips the animation.
      if (window.animatePane) window.animatePane(pane);
      if (window.animateCounters) window.animateCounters(pane);
      var desktopNav = document.getElementById('nav-' + name);
      if (desktopNav) desktopNav.classList.add('active');
      var mobileNav = document.getElementById('mnav-' + name);
      if (mobileNav) mobileNav.classList.add('active');
      // Close mobile menu after selection
      var mobileMenu = document.getElementById('mobile-nav');
      if (mobileMenu) mobileMenu.classList.remove('open');
      var hamburger = document.getElementById('hamburger-btn');
      if (hamburger) hamburger.classList.remove('open');
      // Scroll to top on mobile
      window.scrollTo(0, 0);
      setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 80);
    }
    // Opens a stock in Deep Dive from any table row. Setting the value through
    // selectize is what notifies Shiny; assigning to .value alone does not.
    window.openDeepDive = function(sym) {
      if (!sym) return;
      var sel = document.getElementById('dd_ticker');
      if (sel && sel.selectize) {
        sel.selectize.setValue(sym, false);
      } else if (window.Shiny) {
        Shiny.setInputValue('dd_ticker', sym, {priority: 'event'});
      }
      showPane('deepdive');
    };
    Shiny.addCustomMessageHandler('tvActive', function(m) {
      document.querySelectorAll('.tv-btn').forEach(function(b) { b.classList.remove('tv-on'); });
      var el = document.getElementById(m.id);
      if (el) el.classList.add('tv-on');
    });
    function toggleMobileMenu() {
      var menu = document.getElementById('mobile-nav');
      var btn  = document.getElementById('hamburger-btn');
      menu.classList.toggle('open');
      btn.classList.toggle('open');
    }
    // Close menu if tapping outside
    document.addEventListener('click', function(e) {
      var menu = document.getElementById('mobile-nav');
      var btn  = document.getElementById('hamburger-btn');
      if (menu && btn && !menu.contains(e.target) && !btn.contains(e.target)) {
        menu.classList.remove('open');
        btn.classList.remove('open');
      }
    });
    // Live clock
    function updateClock() {
      var now = new Date();
      var timeStr = now.toLocaleTimeString('en-US', {hour12:false, hour:'2-digit', minute:'2-digit', second:'2-digit'});
      var dateStr = now.toLocaleDateString('en-US', {weekday:'short', month:'short', day:'numeric', year:'numeric'});
      // The interval starts before Shiny has initialised, so the first tick threw
      // an uncaught TypeError on every page load. Guarded so it waits quietly.
      if (window.Shiny && typeof Shiny.setInputValue === 'function') {
        Shiny.setInputValue('clock_tick', timeStr + '|' + dateStr, {priority:'event'});
      }
    }
    setInterval(updateClock, 1000);
    updateClock();
  "))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {


  # Auto-refresh: log data age (pipeline runs via GitHub Actions cron, not in-app)
  observe({
    meta_file <- "data/meta.rds"
    if (file.exists(meta_file)) {
      tryCatch({
        m <- readRDS(meta_file)
        age_hours <- as.numeric(difftime(Sys.time(), as.POSIXct(m$last_updated), units="hours"))
        if (age_hours > 8) {
          message("Data is ", round(age_hours,1), " hours old. Pipeline refreshes daily via cron.")
        }
      }, error = function(e) NULL)
    }
  })

  # Dark plotly base
  # Shared hover styling. The price chart used hovermode="x unified", which
  # stacks every trace — price, three moving averages and both Bollinger bands —
  # into one tall tooltip that jumps around as the cursor moves. A single
  # readout plus a thin vertical crosshair tracks the cursor smoothly instead.
  HOVER_LABEL <- list(bgcolor="#14140F", bordercolor="#FF6B00",
                      font=list(color="#E8E8E8", family="IBM Plex Mono", size=11))

  dk <- function(p, ml=60, mr=20, mt=20, mb=40, spike=FALSE) {
    xa <- list(gridcolor="#2A2A2A", zerolinecolor="#2A2A2A", color="#666",
               tickfont=list(size=10))
    if (spike) {
      xa$showspikes  <- TRUE
      xa$spikemode   <- "across"
      xa$spikethickness <- 1
      xa$spikecolor  <- "rgba(255,107,0,0.45)"
      xa$spikedash   <- "solid"
      xa$spikesnap   <- "cursor"
    }
    p %>%
      layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#666", family="IBM Plex Mono", size=10),
        xaxis=xa,
        yaxis=list(gridcolor="#2A2A2A", zerolinecolor="#2A2A2A", color="#666", tickfont=list(size=10)),
        margin=list(t=mt, b=mb, l=ml, r=mr),
        legend=list(font=list(color="#666", size=10), bgcolor="rgba(0,0,0,0)"),
        hoverlabel=HOVER_LABEL,
        transition=list(duration=250, easing="cubic-in-out")
      ) %>% config(displayModeBar=FALSE)
  }

  no_data <- function(msg="Run R/05_run_all.R to populate data") {
    plot_ly() %>% layout(
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      annotations=list(list(text=msg, x=0.5, y=0.5, xref="paper", yref="paper",
                            showarrow=FALSE, font=list(color="#444", size=11, family="IBM Plex Mono"))),
      xaxis=list(visible=FALSE), yaxis=list(visible=FALSE)
    ) %>% config(displayModeBar=FALSE)
  }

  # Chart period
  chart_days <- reactiveVal(365)
  observeEvent(input$btn_1y, chart_days(365))
  observeEvent(input$btn_6m, chart_days(180))
  observeEvent(input$btn_3m, chart_days(90))

  # Clock
  output$clock_display <- renderUI({
    req(input$clock_tick)
    parts <- strsplit(input$clock_tick, "\\|")[[1]]
    div(parts[1], style="font-family:IBM Plex Mono;font-size:13px;color:#FF6B00;font-weight:600;")
  })

  # ── LIVE TICKER TAPE ────────────────────────────────────────────────────
  # Refresh live quotes every 60 seconds via quantmod::getQuote (Yahoo Finance)
  ticker_timer <- reactiveTimer(60000)

  live_quotes <- reactive({
    ticker_timer()
    if (is.null(master_data)) return(NULL)
    top_syms <- master_data %>%
      arrange(desc(master_score)) %>%
      head(25) %>%
      pull(symbol)
    tryCatch({
      q <- getQuote(top_syms)
      tibble(
        symbol     = rownames(q),
        price      = as.numeric(q$Last),
        change     = as.numeric(q$Change),
        change_pct = as.numeric(q$`% Change`)
      ) %>% filter(!is.na(price), price > 0)
    }, error = function(e) {
      message("getQuote failed: ", e$message)
      NULL
    })
  })

  output$ticker_tape <- renderUI({
    lq <- live_quotes()

    make_item <- function(sym, price, chg) {
      cls <- if (chg >= 0) "ticker-up" else "ticker-dn"
      arrow <- if (chg >= 0) "▲" else "▼"
      div(class="ticker-item",
        span(class="ticker-sym", sym),
        span(class="ticker-price", paste0("$", formatC(price, format="f", digits=2, big.mark=","))),
        span(class=cls, paste0(arrow, " ", round(abs(chg), 2), "%"))
      )
    }

    if (!is.null(lq) && nrow(lq) > 0) {
      # Live data available
      items <- mapply(make_item, lq$symbol, lq$price, lq$change_pct, SIMPLIFY=FALSE)
    } else if (!is.null(master_data)) {
      # Fallback to static CSV data
      static <- master_data %>%
        filter(!is.na(close), !is.na(changesPercentage)) %>%
        arrange(desc(abs(changesPercentage))) %>%
        head(25)
      items <- mapply(make_item, static$symbol, static$close, static$changesPercentage, SIMPLIFY=FALSE)
    } else {
      return(div(class="ticker-content",
        div(class="ticker-item",
          span(class="ticker-sym","EDGESCREENER"),
          span(class="ticker-price","Run pipeline to load live data"))))
    }

    # Duplicate items for seamless infinite scroll
    div(class="ticker-content", do.call(tagList, c(items, items)))
  })

  # ── KPIs ─────────────────────────────────────────────────────────────────
  output$kpi_n   <- renderUI({ if(!is.null(master_data)) nrow(master_data) else "—" })
  output$kpi_sb  <- renderUI({ if(!is.null(master_data)) sum(master_data$rating %in% c("Very Strong","Strong Buy"),na.rm=TRUE) else "—" })
  output$kpi_b   <- renderUI({ if(!is.null(master_data)) sum(master_data$rating %in% c("Strong","Buy"),na.rm=TRUE) else "—" })
  output$kpi_sq  <- renderUI({ if(!is.null(master_data)) sum(master_data$squeeze_candidate,na.rm=TRUE) else "—" })
  output$kpi_avg <- renderUI({ if(!is.null(master_data)) round(mean(master_data$master_score,na.rm=TRUE),1) else "—" })
  output$kpi_gc  <- renderUI({ if(!is.null(master_data)) sum(master_data$golden_cross_flag,na.rm=TRUE) else "—" })

  # ── Rating pill HTML ──────────────────────────────────────────────────────
  pill <- function(r) {
    cls <- switch(r, "Very Strong"="r-sb","Strong"="r-b","Neutral"="r-h","Weak"="r-u",
                 "Strong Buy"="r-sb","Buy"="r-b","Hold"="r-h","Underperform"="r-u","r-av")
    as.character(tags$b(class=cls, r))
  }
  sbar <- function(s) {
    s <- round(replace_na(as.numeric(s),0),1)
    col <- if(s>=65)"#00C853" else if(s>=45)"#FFD600" else "#FF3D00"
    as.character(div(class="sbar",
      div(class="sbar-track", div(class="sbar-fill", style=glue("width:{s}%;background:{col};"))),
      div(class="sbar-val", s)))
  }

  # ── Regime badge ─────────────────────────────────────────────────────────
  output$regime_badge_ui <- renderUI({
    regime <- if (!is.null(top15_data) && "regime" %in% names(top15_data))
      top15_data$regime[1] else
      if (!is.null(master_data) && "regime" %in% names(master_data))
        master_data$regime[1] else "NEUTRAL"
    if (is.na(regime)) regime <- "NEUTRAL"
    tags$span(class=paste("regime-badge", paste0("regime-", regime)), regime)
  })

  # ── Confidence bar HTML helper ────────────────────────────────────────────
  # Currently unused. The confidence columns were dropped from both overview
  # tables because every row scored exactly 100 — one distinct value across the
  # whole universe — so the bar drew an identical green line on every row and
  # told the reader nothing. Kept for when confidence becomes a real spread.
  conf_bar_html <- function(conf) {
    conf <- round(replace_na(as.numeric(conf), 0), 1)
    col  <- if (conf >= 75) "#00C853" else if (conf >= 55) "#FFD600" else "#FF3D00"
    as.character(div(class="conf-bar",
      div(class="conf-track",
        div(class="conf-fill", style=glue("width:{conf}%;background:{col};"))),
      div(class="conf-val", paste0(conf, "%"))))
  }

  # ── Expected return HTML helper ───────────────────────────────────────────
  # Input is a 1-day expected return stored as a decimal fraction
  # (0.0044 = 0.44%/day), so scale by 100 before display.
  exp_ret_html <- function(ret) {
    ret <- replace_na(as.numeric(ret), 0) * 100
    cls <- if (ret >= 0) "pos" else "neg"
    pfx <- if (ret >= 0) "+" else ""
    as.character(tags$span(class=paste("exp-ret", cls),
      paste0(pfx, round(ret, 2), "%")))
  }

  # Clicking a row opens that stock in Deep Dive. The symbol column is located by
  # its header text rather than a fixed index, because it sits in a different
  # position in each table (first in the screener, third in the unicorn table).
  # JS() joins its arguments with newlines, so every line here must stand alone
  # as a complete statement.
  dd_click <- function() JS(
    "table.on('click', 'tbody tr', function() {",
    "  var idx = -1;",
    "  table.columns().header().each(function(h, i) {",
    "    if ($(h).text().trim() === 'Symbol') idx = i;",
    "  });",
    "  if (idx < 0) return;",
    "  var d = table.row(this).data();",
    "  if (!d) return;",
    "  var sym = $('<div>').html(String(d[idx])).text().trim();",
    "  if (sym && window.openDeepDive) window.openDeepDive(sym);",
    "});",
    "$(table.table().node()).addClass('row-clickable');"
  )

  # ── Top 20 Sweet Spot Table ───────────────────────────────────────────────
  output$top15_table <- renderDT({
    if (is.null(top15_data) || nrow(top15_data) == 0)
      return(datatable(data.frame(
        Message="Run 01_fundamentals.R then 04_master_score.R to generate Top 20")))

    d <- top15_data

    df <- d %>%
      mutate(
        `#`          = row_number(),
        Score        = sapply(sweet_spot_score, sbar),
        Rating       = sapply(rating, pill),
        `Exp Return` = sapply(expected_return_1d, exp_ret_html),
        Percentile   = paste0(round(master_percentile, 1), "th"),
        Driver       = primary_driver,
        Signals      = signal_matches,
        pe_fmt       = ifelse(is.na(pe_ratio)|pe_ratio<=0,"N/A",
                              as.character(round(pe_ratio,1))),
        mktcap_fmt   = case_when(
          is.na(market_cap)  ~ "N/A",
          market_cap >= 1e12 ~ paste0("$",round(market_cap/1e12,1),"T"),
          market_cap >= 1e9  ~ paste0("$",round(market_cap/1e9,1),"B"),
          TRUE               ~ "N/A"),
        ret_1m_fmt   = fmt_ret(ret_1m),
        ret_3m_fmt   = fmt_ret(ret_3m),
        company_fmt  = coalesce(company, symbol),
        sector_fmt   = coalesce(sector, "—")
      ) %>%
      select(`#`, Symbol=symbol, Company=company_fmt, Sector=sector_fmt,
             Score, Rating, `Exp Ret/D`=`Exp Return`,
             `1M`=ret_1m_fmt, `3M`=ret_3m_fmt,
             Percentile, Driver, `Sig Matches`=Signals,
             `P/E`=pe_fmt, `Mkt Cap`=mktcap_fmt)

    datatable(df, escape=FALSE, rownames=FALSE, selection="none", callback=dd_click(),
      options=list(dom="t", pageLength=20, ordering=FALSE, autoWidth=TRUE,
                   columnDefs=list(list(className="dt-left", targets="_all"))))
  })

  # ── Unicorn Table (Top 15) ────────────────────────────────────────────────
  output$unicorn_table <- renderDT({
    if (is.null(unicorn_data) || nrow(unicorn_data) == 0)
      return(datatable(data.frame(
        Message="Run 01_fundamentals.R then 04_master_score.R to generate Unicorns")))

    d <- unicorn_data

    if (nrow(d) == 0)
      return(datatable(data.frame(Msg="No unicorn candidates — re-run pipeline")))

    has_uconf   <- "unicorn_confidence"  %in% names(d)
    has_uscore  <- "unicorn_score"       %in% names(d)
    has_exp5d   <- "confidence_5d"       %in% names(d)
    has_driver  <- "primary_driver"      %in% names(d)

    df <- d %>%
      mutate(
        `#`          = row_number(),
        Badge        = as.character(tags$span(class="unicorn-badge","UNICORN")),
        Score        = sapply(if(has_uscore) unicorn_score else master_score, sbar),
        Rating       = sapply(rating, pill),
        Driver       = if(has_driver) primary_driver else "—",
        mktcap_fmt   = case_when(
          is.na(market_cap)  ~ "N/A",
          market_cap >= 1e9  ~ paste0("$",round(market_cap/1e9,2),"B"),
          market_cap >= 1e6  ~ paste0("$",round(market_cap/1e6,1),"M"),
          TRUE               ~ "N/A"),
        rev_g_fmt    = ifelse(is.na(revenue_growth),"N/A",
                              paste0(ifelse(revenue_growth>=0,"+",""),
                                     round(revenue_growth*100,1),"%")),
        ret_3m_fmt   = fmt_ret(ret_3m),
        ret_1m_fmt   = fmt_ret(ret_1m),
        pe_fmt       = ifelse(is.na(pe_ratio)|pe_ratio<=0,"N/A",
                              as.character(round(pe_ratio,1))),
        company_fmt  = coalesce(company, symbol),
        sector_fmt   = coalesce(sector, "—")
      ) %>%
      select(`#`, Badge, Symbol=symbol, Company=company_fmt, Sector=sector_fmt,
             Score, Rating, Driver,
             `1M`=ret_1m_fmt, `3M`=ret_3m_fmt,
             `Rev Growth`=rev_g_fmt, `Mkt Cap`=mktcap_fmt, `P/E`=pe_fmt)

    datatable(df, escape=FALSE, rownames=FALSE, selection="none", callback=dd_click(),
      options=list(dom="t", pageLength=15, ordering=FALSE, autoWidth=TRUE,
                   columnDefs=list(list(className="dt-left", targets="_all"))))
  })

  output$score_dist <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    plot_ly(master_data, x=~master_score, type="histogram", nbinsx=20,
      marker=list(color="#FF6B00",opacity=0.85,line=list(color="#0A0A0A",width=1))) %>%
      layout(xaxis=list(title="Score"), yaxis=list(title="Count")) %>% dk(mt=10,mb=30)
  })

  output$rating_donut <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>% count(rating) %>% filter(!is.na(rating))
    clrs <- c("Very Strong"="#00C853","Strong"="#64DD17","Neutral"="#FFD600","Weak"="#FF6D00","Very Weak"="#FF3D00")
    plot_ly(d, labels=~rating, values=~n, type="pie", hole=0.55,
      marker=list(colors=clrs[d$rating], line=list(color="#0A0A0A",width=2)),
      textfont=list(family="IBM Plex Mono",size=10), textinfo="label+percent") %>%
      dk(mt=5,mb=5,ml=5,mr=5)
  })

  output$sector_bar <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>% filter(!is.na(sector)) %>%
      group_by(sector) %>% summarize(avg=round(mean(master_score,na.rm=TRUE),1),.groups="drop") %>% arrange(avg)
    plot_ly(d, x=~avg, y=~reorder(sector,avg), type="bar", orientation="h",
      marker=list(color="#FF6B00",opacity=0.85)) %>%
      layout(xaxis=list(title="Avg Score"), yaxis=list(title=""), margin=list(l=160)) %>% dk(ml=160)
  })

  output$sector_heat <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>% filter(!is.na(sector)) %>%
      group_by(sector) %>%
      summarize(Fund=mean(fundamental_score,na.rm=TRUE),
                Mom=mean(momentum_score,na.rm=TRUE),
                Squeeze=mean(squeeze_score,na.rm=TRUE),
                Master=mean(master_score,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(Master))
    m <- as.matrix(d[,-1]); rownames(m) <- d$sector
    plot_ly(x=colnames(m), y=rownames(m), z=round(m,1), type="heatmap",
      colorscale=list(list(0,"#1A0500"),list(0.5,"#FF6B00"),list(1,"#FFE0B2")),
      text=round(m,1), texttemplate="%{text}",
      colorbar=list(tickfont=list(color="#666",size=9))) %>%
      layout(xaxis=list(title=""), yaxis=list(title=""), margin=list(l=160)) %>% dk(ml=160)
  })

  # ── Screener ──────────────────────────────────────────────────────────────
  filtered <- reactive({
    req(master_data)
    d <- master_data
    if (input$f_sector != "All") d <- filter(d, sector==input$f_sector)
    if (input$f_rating != "All") d <- filter(d, rating==input$f_rating)
    d <- filter(d, master_score >= input$f_score)
    if (input$f_squeeze) d <- filter(d, squeeze_candidate==TRUE)
    if (input$f_golden)  d <- filter(d, golden_cross_flag==TRUE)
    d
  })

  output$screener_badge <- renderUI({
    div(class="panel-head-meta", glue("{nrow(filtered())} STOCKS"))
  })

  output$screener_table <- renderDT({
    d <- filtered(); if (nrow(d)==0) return(datatable(data.frame(Msg="No matches")))
    df <- d %>%
      mutate(
        Score   = sapply(master_score, sbar),
        Rating  = sapply(rating, pill),
        pe_fmt       = ifelse(is.na(pe_ratio)|pe_ratio<=0,"N/A",as.character(round(pe_ratio,1))),
        mktcap_fmt   = case_when(is.na(market_cap)~"N/A",market_cap>=1e12~paste0("$",round(market_cap/1e12,1),"T"),market_cap>=1e9~paste0("$",round(market_cap/1e9,1),"B"),TRUE~"N/A"),
        ret_1m_fmt   = fmt_ret(ret_1m),
        ret_3m_fmt   = fmt_ret(ret_3m),
        ret_6m_fmt   = fmt_ret(ret_6m),
        ret_1y_fmt   = fmt_ret(ret_1y),
        company_fmt  = coalesce(company, symbol),
        sector_fmt   = coalesce(sector, "—"),
        short_float_pct = ifelse(is.na(short_percent_float),"N/A",paste0(round(short_percent_float*100,1),"%")),
        squeeze_tier = ifelse(is.null(squeeze_tier)|is.na(squeeze_tier),"No Signal",squeeze_tier),
        momentum_score = coalesce(momentum_score, 45),
        squeeze_score  = coalesce(squeeze_score,  28.5)
      ) %>%
      select(Symbol=symbol, Company=company_fmt, Sector=sector_fmt, Score, Rating,
             Fund=fundamental_score, Mom=momentum_score, Squeeze=squeeze_score,
             `P/E`=pe_fmt, `1M`=ret_1m_fmt, `3M`=ret_3m_fmt,
             `6M`=ret_6m_fmt, `1Y`=ret_1y_fmt,
             `Mkt Cap`=mktcap_fmt, `Short Float`=short_float_pct, Tier=squeeze_tier)
    datatable(df, escape=FALSE, rownames=FALSE, callback=dd_click(),
      options=list(pageLength=25, scrollX=TRUE,
                   columnDefs=list(list(className="dt-left",targets="_all"))))
  })

  # ── Squeeze ───────────────────────────────────────────────────────────────
  output$squeeze_scatter <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>%
      mutate(
        earningsGrowth_safe = coalesce(earningsGrowth, 0.03),
        eg = earningsGrowth_safe * 100,
        sf = replace_na(short_percent_float, 0) * 100
      )
    clrs <- c("High Conviction"="#FF3D00","Watch List"="#FFD600","Low Signal"="#00B8D9","No Signal"="#333")
    plot_ly(d, x=~eg, y=~sf, type="scatter", mode="markers",
      color=~squeeze_tier, colors=clrs,
      size=~squeeze_score, sizes=c(4,24),
      text=~paste0("<b>",symbol,"</b><br>",company,"<br>Squeeze: ",squeeze_score,
                   "<br>Short Float: ",short_float_pct,"<br>EPS Growth: ",round(earningsGrowth*100,1),"%"),
      hoverinfo="text",
      marker=list(opacity=0.82, sizemode="diameter", line=list(color="rgba(0,0,0,0.4)",width=0.5))) %>%
      layout(
        xaxis=list(title="Earnings Growth (%)"),
        yaxis=list(title="Short % of Float (%)"),
        shapes=list(
          list(type="line",x0=0,x1=0,y0=0,y1=100,line=list(color="#333",dash="dash",width=1)),
          list(type="line",x0=min(d$eg,na.rm=TRUE),x1=max(d$eg,na.rm=TRUE),
               y0=10,y1=10,line=list(color="#333",dash="dash",width=1))
        ),
        legend=list(orientation="h",y=-0.15)
      ) %>% dk()
  })

  output$squeeze_tiers <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>% count(squeeze_tier) %>% filter(!is.na(squeeze_tier))
    clrs <- c("High Conviction"="#FF3D00","Watch List"="#FFD600","Low Signal"="#00B8D9","No Signal"="#333")
    plot_ly(d, x=~squeeze_tier, y=~n, type="bar",
      marker=list(color=clrs[d$squeeze_tier],opacity=0.9)) %>%
      layout(xaxis=list(title=""),yaxis=list(title="")) %>% dk(mt=5,mb=30)
  })

  output$squeeze_top <- renderDT({
    if (is.null(master_data)) return(datatable(data.frame()))
    master_data %>% filter(squeeze_tier=="High Conviction") %>%
      select(Symbol=symbol, Score=squeeze_score, `Short Float`=short_float_pct, Trend=short_trend) %>%
      head(8) %>%
      datatable(rownames=FALSE, selection="none", callback=dd_click(),
        options=list(dom="t",pageLength=8,ordering=FALSE))
  })

  output$short_trend <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>% count(short_trend) %>% filter(!is.na(short_trend))
    clrs <- c("Decreasing"="#00C853","Stable"="#FFD600","Increasing"="#FF3D00","Unknown"="#333")
    plot_ly(d, x=~short_trend, y=~n, type="bar",
      marker=list(color=clrs[d$short_trend],opacity=0.9)) %>%
      layout(xaxis=list(title=""),yaxis=list(title="# Stocks")) %>% dk(mt=5)
  })

  output$days_cover <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    d <- master_data %>% filter(!is.na(short_ratio),short_ratio<30)
    plot_ly(d, x=~short_ratio, type="histogram", nbinsx=25,
      marker=list(color="#FF6B00",opacity=0.85,line=list(color="#0A0A0A",width=1))) %>%
      layout(xaxis=list(title="Days to Cover"),yaxis=list(title="")) %>% dk(mt=5)
  })

  output$squeeze_dist <- renderPlotly({
    if (is.null(master_data)) return(no_data())
    plot_ly(master_data, x=~squeeze_score, type="histogram", nbinsx=20,
      marker=list(color="#FF3D00",opacity=0.85,line=list(color="#0A0A0A",width=1))) %>%
      layout(xaxis=list(title="Squeeze Score"),yaxis=list(title="")) %>% dk(mt=5)
  })

  # ── Deep Dive ─────────────────────────────────────────────────────────────
  sel <- reactive({
    req(master_data, input$dd_ticker)
    master_data %>% filter(symbol==input$dd_ticker) %>% slice(1)
  })

  sel_prices <- reactive({
    req(input$dd_ticker, price_history)
    days <- chart_days()
    price_history %>%
      filter(symbol==input$dd_ticker, date >= Sys.Date()-days) %>%
      arrange(date)
  })

  output$dd_chart_title <- renderUI({
    s <- sel(); req(nrow(s)>0)
    div(paste(s$symbol,"— PRICE CHART"))
  })

  output$dd_header <- renderUI({
    s <- sel(); req(nrow(s)>0)
    chg_col <- if(!is.na(s$changesPercentage) && s$changesPercentage>=0) "#00C853" else "#FF3D00"
    chg_sym <- if(!is.na(s$changesPercentage) && s$changesPercentage>=0) "▲" else "▼"
    div(style="display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
      div(style="font-family:IBM Plex Mono;font-size:20px;font-weight:700;color:#FF6B00;",
          paste0("$", round(replace_na(s$close,0),2))),
      div(style=glue("font-family:IBM Plex Mono;font-size:13px;color:{chg_col};font-weight:600;"),
          paste0(chg_sym," ", round(replace_na(s$changesPercentage,0),2),"%")),
      div(style="font-size:13px;color:#AAA;", paste(s$company,"•",s$sector,"•",s$mktcap_fmt))
    )
  })

  output$dd_v_master  <- renderUI({ s <- sel(); round(replace_na(s$master_score,0),1) })
  output$dd_v_fund    <- renderUI({ s <- sel(); round(replace_na(s$fundamental_score,0),1) })
  output$dd_v_mom     <- renderUI({ s <- sel(); round(replace_na(s$momentum_score,0),1) })
  output$dd_v_squeeze <- renderUI({ s <- sel(); round(replace_na(s$squeeze_score,0),1) })

  output$dd_price <- renderPlotly({
    p <- sel_prices()
    if (is.null(p) || nrow(p) == 0) {
      # Try fetching live
      tryCatch({
        sym <- input$dd_ticker
        env <- new.env()
        suppressWarnings(quantmod::getSymbols(sym, src="yahoo", env=env, auto.assign=TRUE, from=Sys.Date()-400))
        px <- env[[sym]]; px <- na.omit(px); cl <- as.numeric(quantmod::Cl(px))
        p <- data.frame(
          date=as.Date(zoo::index(px)), close=cl,
          volume=as.numeric(quantmod::Vo(px)),
          ma20=as.numeric(TTR::SMA(cl,20)), ma50=as.numeric(TTR::SMA(cl,50)),
          ma200=as.numeric(TTR::SMA(cl,200)),
          bb_upper=as.numeric(TTR::BBands(cl,n=20)[,"up"]),
          bb_lower=as.numeric(TTR::BBands(cl,n=20)[,"dn"]),
          macd_line=as.numeric(TTR::MACD(cl)[,"macd"]),
          macd_signal=as.numeric(TTR::MACD(cl)[,"signal"]),
          rsi14=as.numeric(TTR::RSI(cl,n=14))
        )
        p <- p[p$date >= Sys.Date()-365,]
      }, error=function(e) { p <- data.frame() })
    }
    if (is.null(p) || nrow(p) == 0) return(no_data("No price data available"))
    
    plt <- plot_ly(p) %>%
      add_lines(x=~date, y=~close, name="Price",
        line=list(color="#FF6B00", width=2, shape="spline", smoothing=0.5),
        hovertemplate="%{x|%b %d, %Y}   <b>$%{y:.2f}</b><extra></extra>")
    # hoverinfo="skip" on the overlays: they are visual context, and including
    # them in the tooltip is what produced the six-line block.
    if ("ma20"  %in% names(p)) plt <- plt %>% add_lines(x=~date, y=~ma20,  name="MA20",  hoverinfo="skip", line=list(color="#FFD600",width=1,dash="dot"))
    if ("ma50"  %in% names(p)) plt <- plt %>% add_lines(x=~date, y=~ma50,  name="MA50",  hoverinfo="skip", line=list(color="#00B8D9",width=1.5,dash="dash"))
    if ("ma200" %in% names(p)) plt <- plt %>% add_lines(x=~date, y=~ma200, name="MA200", hoverinfo="skip", line=list(color="#FF3D00",width=1.5,dash="dash"))
    if (all(c("bb_lower","bb_upper") %in% names(p)))
      plt <- plt %>% add_ribbons(x=~date, ymin=~bb_lower, ymax=~bb_upper, hoverinfo="skip",
        fillcolor="rgba(255,107,0,0.06)", line=list(color="rgba(255,107,0,0.15)",width=1), name="BB")
    plt %>%
      layout(xaxis=list(title="",rangeslider=list(visible=FALSE)),
             yaxis=list(title="Price ($)"),
             hovermode="x", hoverdistance=-1,
             legend=list(orientation="h",y=-0.05)) %>% dk(mb=20, spike=TRUE)
  })

  output$dd_volume <- renderPlotly({
    p <- sel_prices(); req(nrow(p)>0)
    plot_ly(p, x=~date, y=~volume, type="bar",
      marker=list(color="rgba(255,107,0,0.5)")) %>%
      layout(xaxis=list(title="",showticklabels=FALSE),
             yaxis=list(title="Vol",tickformat=".2s")) %>% dk(mt=0,mb=10,ml=50)
  })

  output$dd_macd <- renderPlotly({
    p <- sel_prices(); req(nrow(p)>0)
    plot_ly(p) %>%
      add_lines(x=~date, y=~macd_line,   name="MACD",   line=list(color="#FF6B00",width=1.5)) %>%
      add_lines(x=~date, y=~macd_signal, name="Signal", line=list(color="#FFD600",width=1,dash="dash")) %>%
      add_bars(x=~date, y=~macd_hist, name="Hist",
        marker=list(color=~ifelse(macd_hist>=0,"rgba(0,200,83,0.7)","rgba(255,61,0,0.7)"))) %>%
      layout(xaxis=list(title="",showticklabels=FALSE),
             yaxis=list(title="MACD"),
             barmode="relative") %>% dk(mt=0,mb=5,ml=50)
  })

  output$dd_rsi <- renderPlotly({
    p <- sel_prices(); req(nrow(p)>0)
    plot_ly(p) %>%
      add_lines(x=~date, y=~rsi14, name="RSI(14)",
        line=list(color="#FF6B00",width=1.5)) %>%
      add_lines(x=~date, y=~rep(70,nrow(p)), name="OB",
        line=list(color="#FF3D00",width=0.8,dash="dot"), showlegend=FALSE) %>%
      add_lines(x=~date, y=~rep(30,nrow(p)), name="OS",
        line=list(color="#00C853",width=0.8,dash="dot"), showlegend=FALSE) %>%
      layout(xaxis=list(title=""),
             yaxis=list(title="RSI",range=c(0,100))) %>% dk(mt=0,mb=20,ml=50)
  })

  output$dd_metrics <- renderUI({
    s <- sel(); req(nrow(s)>0)
    rows <- list(
      c("Price",          paste0("$",round(replace_na(s$close,0),2))),
      c("Market Cap",     s$mktcap_fmt),
      c("52W High",       ifelse(is.na(s$yearHigh),"N/A",paste0("$",round(s$yearHigh,2)))),
      c("52W Low",        ifelse(is.na(s$yearLow), "N/A",paste0("$",round(s$yearLow,2)))),
      c("P/E (TTM)",      s$pe_fmt),
      c("P/B Ratio",      ifelse(is.na(coalesce(s$priceToBook,s$pb_ratio)),"N/A",round(coalesce(s$priceToBook,s$pb_ratio),1))),
      c("EV/EBITDA",      ifelse(is.na(s$enterpriseToEbitda),"N/A",round(s$enterpriseToEbitda,1))),
      c("Gross Margin",   fmt_pct(coalesce(s$grossMargin, s$gross_margin))),
      c("Op. Margin",     fmt_pct(coalesce(s$operatingMargin, s$operating_margin))),
      c("Net Margin",     fmt_pct(coalesce(s$profitMargins, s$profit_margin))),
      c("ROE",            fmt_pct(coalesce(s$returnOnEquity, s$roe))),
      c("Rev Growth",     fmt_pct(coalesce(s$revenueGrowth, s$revenue_growth))),
      c("EPS Growth",     fmt_pct(coalesce(s$earningsGrowth, 0))),
      c("Debt/Equity",    ifelse(is.na(coalesce(s$debtToEquity,s$debt_to_equity)),"N/A",round(coalesce(s$debtToEquity,s$debt_to_equity),2))),
      c("Current Ratio",  ifelse(is.na(coalesce(s$currentRatio,s$current_ratio)),"N/A",round(coalesce(s$currentRatio,s$current_ratio),2))),
      c("1M Return",      s$ret_1m_fmt),
      c("3M Return",      s$ret_3m_fmt),
      c("6M Return",      s$ret_6m_fmt),
      c("1Y Return",      s$ret_1y_fmt)
    )
    trs <- lapply(rows, function(r) tags$tr(tags$td(class="mk",r[1]),tags$td(class="mv",r[2])))
    tags$table(class="mt", do.call(tagList,trs))
  })

  output$dd_short <- renderUI({
    s <- sel(); req(nrow(s)>0)
    tier_cls <- switch(replace_na(s$squeeze_tier,"No Signal"),
      "High Conviction"="squeeze-badge sq-hc",
      "Watch List"="squeeze-badge sq-wl",
      "Low Signal"="squeeze-badge sq-ls",
      "squeeze-badge sq-ns")
    div(
      div(class="si-grid",
        div(class="si-cell", div(class="si-k","Short Float"),  div(class="si-v",replace_na(s$short_float_pct,"N/A"))),
        div(class="si-cell", div(class="si-k","Days to Cover"),div(class="si-v",ifelse(is.na(s$short_ratio),"N/A",round(s$short_ratio,1)))),
        div(class="si-cell", div(class="si-k","Short Trend"),  div(class="si-v",style="font-size:14px;",replace_na(s$short_trend,"N/A"))),
        div(class="si-cell", div(class="si-k","Fund. Improving"),div(class="si-v",style="font-size:14px;",ifelse(isTRUE(s$fundamentals_improving),"YES ✓","NO")))
      ),
      div(class=tier_cls, paste("TIER:", toupper(replace_na(s$squeeze_tier,"No Signal"))))
    )
  })

  output$dd_radar <- renderPlotly({
    s <- sel(); req(nrow(s)>0)
    cats <- c("Value","Quality","Growth","Safety","Momentum","Squeeze")
    vals <- c(s$value_score,s$quality_score,s$growth_score,
              s$safety_score,s$momentum_score,s$squeeze_score)
    vals <- replace(as.numeric(vals), is.na(vals), 0)
    plot_ly(type="scatterpolar", fill="toself",
      r=c(vals,vals[1]), theta=c(cats,cats[1]),
      line=list(color="#FF6B00",width=2),
      fillcolor="rgba(255,107,0,0.12)") %>%
      layout(
        polar=list(
          radialaxis=list(visible=TRUE,range=c(0,100),color="#2A2A2A",gridcolor="#2A2A2A",tickfont=list(size=9,color="#444")),
          angularaxis=list(color="#666",tickfont=list(size=10,color="#AAA"))),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#666",family="IBM Plex Mono",size=10),
        showlegend=FALSE, margin=list(t=20,b=20,l=40,r=40)
      ) %>% config(displayModeBar=FALSE)
  })

  output$dd_dcf <- renderUI({
    s <- sel(); req(nrow(s)>0)
    dcf <- tryCatch(compute_dcf(s), error=function(e) list(intrinsic_value=NA,upside=NA,dcf_rating="N/A"))
    upside_cls <- if (!is.na(dcf$upside) && dcf$upside>0) "dcf-upside-pos" else "dcf-upside-neg"
    price <- replace_na(s$close[1], 0)
    div(
      if (!is.na(dcf$intrinsic_value)) {
        div(class=upside_cls, style="margin-bottom:12px;",
          paste0(ifelse(replace_na(dcf$upside,0)>0,"▲ ","▼ "),
                 round(abs(replace_na(dcf$upside,0))*100,1),"% ",
                 ifelse(replace_na(dcf$upside,0)>0,"UPSIDE","DOWNSIDE")))
      },
      lapply(list(
        c("Current Price",   paste0("$",round(price,2))),
        c("Intrinsic Value", ifelse(is.na(dcf$intrinsic_value),"N/A",paste0("$",dcf$intrinsic_value))),
        c("Valuation",       dcf$dcf_rating),
        # These were hardcoded strings sitting next to a permanently-N/A
        # valuation. They now report what the model actually used.
        c("WACC Used",       fmt_pct(dcf$wacc)),
        c("Risk-Free (10Y)", fmt_pct(dcf$rf)),
        c("Terminal Growth", fmt_pct(dcf$terminal_growth)),
        c("Rev Growth Used", fmt_pct(dcf$growth)),
        c("Free Cash Flow",  fmt_mktcap(s$fcf))
      ), function(r) div(class="dcf-row", div(class="dcf-lbl",r[1]), div(class="dcf-val",r[2]))),
      div(style="color:#555;font-size:9px;font-family:IBM Plex Mono;margin-top:10px;line-height:1.5;",
        if (is.na(dcf$intrinsic_value))
          paste0("No valuation: this model needs positive free cash flow, share count ",
                 "and price. Names with negative or unreported FCF are left blank ",
                 "rather than estimated.")
        else
          paste0("Ten-year two-stage model: growth held five years, then fading to ",
                 "terminal, discounted at a CAPM-derived WACC. Free cash flow is ",
                 "averaged across available filing years to smooth the capex cycle; ",
                 "cash flow, debt and cash come from SEC XBRL filings. A DCF is ",
                 "highly sensitive to its assumptions — see Methodology."))
    )
  })

  output$dd_comps <- renderDT({
    s <- sel(); req(nrow(s)>0, !is.null(master_data))
    # Peers = same sector, sorted by market cap
    peers <- master_data %>%
      filter(sector==s$sector[1], !is.na(close)) %>%
      arrange(desc(replace_na(coalesce(market_cap, marketCap, 0), 0))) %>%
      head(8) %>%
      mutate(
        highlight = symbol == s$symbol[1],
        `P/E` = pe_fmt, `3M` = ret_3m_fmt, Score = round(master_score,1),
        # These were computed inside select(), which only accepts column
        # selections and cannot evaluate coalesce(). The call aborted with
        # "object 'profitMargins' not found" even though the column exists, so
        # Peer Comps rendered an error rather than a table.
        Margin = coalesce(profitMargins, profit_margin),
        ROE    = coalesce(returnOnEquity, roe)
      ) %>%
      select(Symbol=symbol, Price=close, `Mkt Cap`=mktcap_fmt,
             `P/E`, Margin, ROE, `3M`, Score)
    dt <- datatable(peers, rownames=FALSE, selection="none",
      options=list(dom="t",pageLength=8,ordering=FALSE),
      # JS() joins its arguments with newlines, so interpolating the ticker
      # mid-string split the quoted literal across three lines and produced
      # invalid JavaScript. Shiny threw "Invalid or unexpected token" when it
      # evaluated this, and that client-side error aborted rendering for every
      # other output delivered in the same batch — which is why the entire Deep
      # Dive tab was blank while the server computed all of it correctly.
      # Built as one string so the result is always valid.
      callback=JS(sprintf(
        "table.rows().every(function(i){ if(this.data()[0] === '%s') $(this.node()).addClass('comp-highlight'); })",
        s$symbol[1]))) %>%
      formatPercentage(c("Margin","ROE"),1)
    dt
  })

  # ── Macro ─────────────────────────────────────────────────────────────────
  output$macro_kpis <- renderUI({
    if (is.null(macro_data)) return(div("Run pipeline to load macro data"))
    latest <- macro_data %>% group_by(series) %>%
      filter(!is.na(price)) %>% slice_tail(n=1) %>% ungroup()
    cells <- lapply(split(latest, latest$series), function(row) {
      div(class="macro-cell",
        div(class="macro-k", row$series),
        div(class="macro-v", round(row$price,2)),
        div(class="macro-sub", format(as.Date(row$date),"%b %d, %Y"))
      )
    })
    div(class="macro-grid", do.call(tagList, cells))
  })

  make_macro_plot <- function(series_name, color="#FF6B00") {
    if (is.null(macro_data)) return(no_data())
    d <- macro_data %>% filter(series==series_name, !is.na(price)) %>% arrange(date)
    if (nrow(d)==0) return(no_data(paste("No data for",series_name)))
    plot_ly(d, x=~date, y=~price, type="scatter", mode="lines",
      line=list(color=color,width=2),
      fill="tozeroy", fillcolor=paste0(gsub("#","rgba(",color),",0.08)")) %>%
      layout(xaxis=list(title=""), yaxis=list(title="")) %>% dk()
  }

  output$yield_curve <- renderPlotly({
    if (is.null(macro_data)) return(no_data())
    latest <- macro_data %>% group_by(ticker) %>%
      filter(!is.na(price)) %>% slice_tail(n=1) %>% ungroup() %>%
      filter(ticker %in% c("DGS3MO","DGS2","DGS10")) %>%
      mutate(maturity=factor(ticker,
        levels=c("DGS3MO","DGS2","DGS10"),
        labels=c("3M","2Y","10Y")))
    if (nrow(latest)==0) return(no_data())
    plot_ly(latest, x=~maturity, y=~price, type="scatter", mode="lines+markers",
      line=list(color="#FF6B00",width=2),
      marker=list(color="#FF6B00",size=8)) %>%
      layout(xaxis=list(title="Maturity"), yaxis=list(title="Yield (%)")) %>% dk()
  })

  output$treasury_10y <- renderPlotly({ make_macro_plot("10Y Treasury","#FF6B00") })
  output$fed_funds    <- renderPlotly({ make_macro_plot("Fed Funds Rate","#FFD600") })
  output$yield_spread <- renderPlotly({
    if (is.null(macro_data)) return(no_data())
    d <- macro_data %>% filter(series=="Yield Curve Spread",!is.na(price)) %>% arrange(date)
    if (nrow(d)==0) return(no_data())
    plot_ly(d, x=~date, y=~price, type="scatter", mode="lines",
      line=list(color=~ifelse(price>=0,"#00C853","#FF3D00"),width=2),
      fill="tozeroy",
      fillcolor=~ifelse(price>=0,"rgba(0,200,83,0.08)","rgba(255,61,0,0.08)")) %>%
      add_lines(x=~date, y=~rep(0,nrow(d)), line=list(color="#333",width=1), showlegend=FALSE) %>%
      layout(xaxis=list(title=""), yaxis=list(title="Spread (%)")) %>% dk()
  })

  # ── Deep Dive: company news ───────────────────────────────────────────────
  # Prefer the per-ticker RSS feed. The market-wide Alpha Vantage feed carries
  # ~50 articles for the entire market, so it covered roughly 6 of 195 stocks on
  # any given day and most tickers showed nothing. RSS is per-symbol, so it
  # covers essentially the whole universe; AV remains the fallback.
  dd_news_rows <- reactive({
    req(input$dd_ticker)
    sym <- input$dd_ticker
    if (!is.null(stock_news) && nrow(stock_news) > 0 && "symbol" %in% names(stock_news)) {
      rss <- stock_news %>% filter(!is.na(symbol), symbol == sym)
      if (nrow(rss) > 0) {
        return(rss %>%
          transmute(title,
                    url,
                    source    = if ("publisher" %in% names(rss)) publisher else "Yahoo Finance",
                    sentiment = NA_character_,
                    sentiment_score = NA_real_,
                    publishedDate = if ("published_parsed" %in% names(rss))
                                      suppressWarnings(as.POSIXct(published_parsed)) else as.POSIXct(NA)))
      }
    }
    if (is.null(news_data) || nrow(news_data) == 0) return(NULL)
    if (!"symbol" %in% names(news_data)) return(NULL)
    news_data %>% filter(!is.na(symbol), symbol == sym)
  })

  # ── Deep Dive: SEC filings ────────────────────────────────────────────────
  dd_filings <- reactive({
    req(input$dd_ticker)
    sym <- input$dd_ticker
    if (is.null(sec_filings) || nrow(sec_filings) == 0) return(NULL)
    if (!"symbol" %in% names(sec_filings)) return(NULL)
    f <- sec_filings %>% filter(symbol == sym)
    if (nrow(f) == 0) NULL else f
  })

  output$dd_filings_meta <- renderUI({
    f <- dd_filings()
    if (is.null(f)) return(div("—"))
    div(glue("{nrow(f)} RECENT"))
  })

  output$dd_filings <- renderUI({
    f <- dd_filings()
    if (is.null(f))
      return(div(glue("No recent SEC filings found for {input$dd_ticker}."),
                 style="color:#666;padding:18px;font-family:IBM Plex Mono;font-size:11px;"))

    ins   <- suppressWarnings(as.numeric(f$insider_filings_90d[1]))
    mna   <- isTRUE(as.logical(f$merger_activity_1y[1]))
    evts  <- suppressWarnings(as.numeric(f$material_events_1y[1]))

    # Form 4 is an insider transaction report — the closest a free source gets
    # to "what has this company been buying". S-4 signals a merger; SC 13D/G a
    # large stake being built.
    flags <- tagList(
      div(class="si-cell",
        div(class="si-k", "Insider filings, 90d"),
        div(class="si-v", style="font-size:15px;", ifelse(is.na(ins), "—", ins))),
      div(class="si-cell",
        div(class="si-k", "M&A activity, 1y"),
        div(class="si-v", style=paste0("font-size:15px;color:", if (mna) "#00C853" else "#666", ";"),
            if (mna) "S-4 filed" else "none")),
      div(class="si-cell",
        div(class="si-k", "Material events, 1y"),
        div(class="si-v", style="font-size:15px;", ifelse(is.na(evts), "—", evts)))
    )

    form_label <- function(form, desc) {
      # EDGAR often echoes the form back as the description, sometimes prefixed
      # ("FORM 8-K"), which rendered as a redundant second copy of the code.
      norm <- gsub("^FORM\\s+", "", toupper(trimws(ifelse(is.na(desc), "", desc))))
      if (nzchar(norm) && norm != toupper(form)) return(desc)
      switch(as.character(form),
        "10-K"    = "Annual report",
        "10-Q"    = "Quarterly report",
        "8-K"     = "Material event disclosure",
        "4"       = "Insider transaction",
        "S-4"     = "Merger / acquisition registration",
        "SC 13D"  = "Activist stake above 5%",
        "SC 13G"  = "Passive stake above 5%",
        "DEF 14A" = "Proxy statement",
        "S-1"     = "Securities registration",
        "S-3"     = "Shelf registration",
        as.character(form))
    }

    rows <- lapply(seq_len(nrow(f)), function(i) {
      r <- f[i, ]
      form_col <- switch(as.character(r$form),
        "8-K" = "#FFD600", "S-4" = "#00C853", "10-K" = "#00B8D9", "10-Q" = "#00B8D9", "#FF6B00")
      div(style="display:flex;align-items:center;gap:10px;padding:7px 0;border-bottom:1px solid var(--border);font-family:var(--mono);font-size:11px;",
        div(style=paste0("min-width:56px;font-weight:700;color:", form_col, ";"), r$form),
        div(style="min-width:80px;color:var(--muted);", format(as.Date(r$filed), "%b %d, %Y")),
        div(style="flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
            tags$a(href = r$url, target = "_blank",
                   style="color:var(--text2);text-decoration:none;",
                   form_label(r$form, r$description)))
      )
    })

    tagList(
      div(class="si-grid", style="grid-template-columns:repeat(3,1fr);", flags),
      div(style="margin-top:10px;", do.call(tagList, rows)),
      div(style="color:#555;font-size:10px;font-family:IBM Plex Mono;margin-top:8px;line-height:1.5;",
          "Source: SEC EDGAR. Form 4 reports insider transactions; S-4 accompanies a ",
          "merger; 8-K discloses a material event. Counts are filings, not dollar amounts.")
    )
  })

  # ── Deep Dive: LBO return sensitivity ─────────────────────────────────────
  output$dd_lbo <- renderUI({
    s <- sel(); req(nrow(s) > 0)
    l <- tryCatch(compute_lbo(s), error = function(e) NULL)
    if (is.null(l))
      return(div(paste0("No LBO view for ", input$dd_ticker, ": this needs positive EBITDA, ",
                        "and a business already carrying more debt than the structure assumes ",
                        "leaves no sponsor equity to return on."),
                 style="color:#666;padding:18px;font-family:IBM Plex Mono;font-size:11px;line-height:1.6;"))

    cell <- function(v) {
      if (is.na(v)) return(tags$td("n/a", style="color:#444;text-align:right;padding:5px 10px;"))
      col <- if (v >= 0.25) "#00C853" else if (v >= 0.15) "#8BC34A"
             else if (v >= 0)   "#FFD600" else "#FF3D00"
      tags$td(paste0(round(v * 100), "%"),
              style=paste0("color:", col, ";text-align:right;padding:5px 10px;font-weight:600;"))
    }

    tbl <- tags$table(
      style="width:100%;border-collapse:collapse;font-family:var(--mono);font-size:11px;",
      tags$thead(tags$tr(
        tags$th("Exit multiple", style="text-align:left;padding:5px 10px;color:#666;font-weight:400;"),
        lapply(l$growth, function(g)
          tags$th(paste0(round(g * 100), "% growth"),
                  style="text-align:right;padding:5px 10px;color:#666;font-weight:400;")))),
      tags$tbody(lapply(seq_along(l$exit_mults), function(i) {
        xm <- l$exit_mults[i]
        entry <- abs(xm - l$entry_mult) < 0.05
        tags$tr(
          tags$td(paste0(xm, "x", if (entry) "  (entry)" else ""),
                  style=paste0("padding:5px 10px;color:", if (entry) "var(--orange)" else "var(--text2)",
                               ";font-weight:", if (entry) "700" else "400", ";")),
          lapply(l$grid[[i]], cell))
      })))

    tagList(
      div(class="si-grid", style="grid-template-columns:repeat(3,1fr);margin-bottom:12px;",
        div(class="si-cell", div(class="si-k","Entry EV / EBITDA"),
            div(class="si-v", style="font-size:15px;", paste0(l$entry_mult, "x"))),
        div(class="si-cell", div(class="si-k","Entry debt"),
            div(class="si-v", style="font-size:15px;", fmt_mktcap(l$entry_debt))),
        div(class="si-cell", div(class="si-k","Sponsor equity"),
            div(class="si-v", style="font-size:15px;", fmt_mktcap(l$equity_in)))),
      div(style="overflow-x:auto;", tbl),
      div(style="color:#555;font-size:10px;font-family:IBM Plex Mono;margin-top:10px;line-height:1.6;",
          paste0("Five-year hold at ", LBO_LEVERAGE, "x entry leverage, ", round(LBO_RATE*100),
                 "% cost of debt, ", round(LBO_TAX*100), "% tax, and capex plus working capital at ",
                 round(LBO_REINVEST*100), "% of EBITDA swept against the debt. Cells are equity IRR. ",
                 "The grid exists because the answer is dominated by two guesses — what you pay ",
                 "and what you sell for — so a single IRR would be false precision."))
    )
  })

  # ── Deep Dive: generated research brief ───────────────────────────────────
  output$dd_pitch <- renderUI({
    s <- sel(); req(nrow(s) > 0)
    sym <- input$dd_ticker
    f  <- dd_filings()
    nn <- tryCatch({ d <- dd_news_rows(); if (is.null(d)) NA else nrow(d) }, error = function(e) NA)
    w  <- if (!is.null(wsb_data) && "ticker" %in% names(wsb_data))
            wsb_data %>% filter(ticker == sym) else NULL
    p  <- tryCatch(pitch_bullets(s, f, nn, w), error = function(e) NULL)
    if (is.null(p))
      return(div("Brief unavailable for this name.",
                 style="color:#666;padding:18px;font-family:IBM Plex Mono;font-size:11px;"))

    side <- function(title, items, colour) {
      if (length(items) == 0)
        items <- list("Nothing in the current data argues this side.")
      div(style="flex:1;min-width:260px;",
        div(style=paste0("font-family:var(--mono);font-size:10px;letter-spacing:1px;",
                         "text-transform:uppercase;color:", colour, ";margin-bottom:8px;"), title),
        tags$ul(style="margin:0;padding-left:16px;",
          lapply(items, function(x)
            tags$li(x, style="color:var(--text2);font-size:12px;line-height:1.65;margin-bottom:7px;"))))
    }

    tagList(
      div(style="font-size:13px;color:var(--text);line-height:1.6;margin-bottom:14px;",
        tags$strong(p$company), glue(" ({p$symbol}) — {tolower(p$sector)}. ",
          "What follows is assembled from the same filings, prices and sentiment shown on this ",
          "page. It is a summary of the evidence, not advice, and it deliberately argues both ",
          "directions.")),
      div(style="display:flex;gap:26px;flex-wrap:wrap;",
        side("What supports the name", p$supports, "#00C853"),
        side("What argues against it", p$against,  "#FF3D00")),
      div(style="color:#555;font-size:10px;font-family:IBM Plex Mono;margin-top:14px;line-height:1.6;",
          paste0("Generated from this page's data at render time — no forecast, no price target, ",
                 "and no position. Every figure above appears in the panels on this page; read ",
                 "the linked filings and stories before relying on any of it."))
    )
  })

  # ── Live TV ───────────────────────────────────────────────────────────────
  # Resolved once per session: opening on a channel that happens to be off air
  # is the dead-player case this panel exists to avoid.
  tv_choice <- reactiveVal(tryCatch(tv_first_live(), error = function(e) TV_DEFAULT))
  lapply(names(TV_CHANNELS), function(n) {
    observeEvent(input[[paste0("tv_", make.names(n))]], { tv_choice(n) }, ignoreInit = TRUE)
  })

  output$tv_meta <- renderUI({
    ch <- tv_choice()
    div(toupper(ch %||% TV_DEFAULT))
  })

  observe({
    ch <- tv_choice() %||% TV_DEFAULT
    session$sendCustomMessage("tvActive", list(id = paste0("tv_", make.names(ch))))
  })

  output$tv_player <- renderUI({
    # Always mounts a channel. An empty "pick a channel" placeholder meant the
    # panel opened as a dead grey box on the landing tab.
    ch <- tv_choice() %||% TV_DEFAULT
    # Only refuse to mount when the channel is positively known to be off air.
    # Treating an unreachable YouTube as off air blanked every channel at once.
    if (identical(tryCatch(tv_live_state(ch), error = function(e) NA), FALSE))
      return(div(style=paste0("border:1px solid var(--border);background:#0D0D0D;",
                              "aspect-ratio:16/9;display:flex;align-items:center;",
                              "justify-content:center;color:#666;font-family:IBM Plex Mono;",
                              "font-size:11px;text-align:center;padding:20px;line-height:1.6;"),
                 paste0(ch, " is not streaming live right now. Pick another channel.")))
    tags$div(style="position:relative;width:100%;aspect-ratio:16/9;border:1px solid var(--border);background:#000;",
      tags$iframe(src = tv_embed_url(ch),
        style = "position:absolute;inset:0;width:100%;height:100%;border:0;",
        allow = "autoplay; encrypted-media; picture-in-picture",
        allowfullscreen = NA, referrerpolicy = "strict-origin-when-cross-origin"))
  })

  # ── Live wire: re-polls the RSS feeds on a timer ──────────────────────────
  # The nightly CSV cannot change during the day, so refreshing the panel from
  # it would redraw identical rows. This re-fetches the feeds instead. The
  # fetch itself is cached in global.R for LIVE_NEWS_TTL seconds and shared
  # across sessions, so many viewers polling still produce one request per TTL.
  live_news_tick <- reactiveTimer(30000)

  live_news <- reactive({
    live_news_tick()
    tryCatch(get_live_news(), error = function(e) NULL)
  })

  output$live_news_meta <- renderUI({
    d <- live_news()
    if (is.null(d) || nrow(d) == 0) return(div("—"))
    div(glue("{nrow(d)} · {format(Sys.time(), '%H:%M:%S')}"))
  })

  output$live_news_feed <- renderUI({
    d <- live_news()
    if (is.null(d) || nrow(d) == 0)
      return(div("Live feeds unreachable right now. The nightly news panel below is unaffected.",
                 style="color:#666;padding:18px;font-family:IBM Plex Mono;font-size:11px;line-height:1.6;"))

    ago <- function(ts) {
      if (is.na(ts)) return("")
      m <- as.numeric(difftime(Sys.time(), ts, units = "mins"))
      if (m < 1) "just now" else if (m < 60) paste0(round(m), "m ago")
      else if (m < 1440) paste0(round(m / 60), "h ago") else paste0(round(m / 1440), "d ago")
    }

    items <- lapply(seq_len(min(25, nrow(d))), function(i) {
      r <- d[i, ]
      div(class="wire-item",
        div(style="display:flex;justify-content:space-between;gap:10px;margin-bottom:3px;",
          span(toupper(r$source), style="color:var(--orange);font-size:9px;font-family:IBM Plex Mono;letter-spacing:1px;"),
          span(ago(r$ts), style="color:#555;font-size:9px;font-family:IBM Plex Mono;")),
        tags$a(href=r$url, target="_blank",
          div(r$title, style="color:#E8E8E8;font-size:11px;line-height:1.45;")))
    })
    div(do.call(tagList, items))
  })

  output$dd_news_count <- renderUI({
    d <- dd_news_rows()
    if (is.null(d) || nrow(d) == 0) return(div("—"))
    div(glue("{nrow(d)} {ifelse(nrow(d) == 1, 'STORY', 'STORIES')}"))
  })

  output$dd_news <- renderUI({
    d <- dd_news_rows()
    if (is.null(d) || nrow(d) == 0)
      return(div(glue("No recent stories found for {input$dd_ticker}."),
                 style="color:#666;padding:18px;font-family:IBM Plex Mono;font-size:11px;line-height:1.5;"))

    items <- lapply(seq_len(min(12, nrow(d))), function(i) {
      r      <- d[i, ]
      title  <- replace_na(r$title, "Untitled")
      url    <- if ("url" %in% names(r)) replace_na(r$url, "#") else "#"
      src    <- if ("source" %in% names(r)) replace_na(r$source, "") else ""
      sent   <- if ("sentiment" %in% names(r)) replace_na(r$sentiment, "") else ""
      score  <- if ("sentiment_score" %in% names(r)) suppressWarnings(as.numeric(r$sentiment_score)) else NA
      when   <- if ("publishedDate" %in% names(r) && !is.na(r$publishedDate))
                  format(as.POSIXct(r$publishedDate), "%b %d") else ""

      col <- if (!is.na(score)) {
        if (score >= 0.15) "#00C853" else if (score <= -0.15) "#FF3D00" else "#FFD600"
      } else "#666666"

      div(style=paste0("border-left:2px solid ", col, ";padding:8px 12px;margin-bottom:6px;background:#0D0D0D;"),
        div(style="display:flex;justify-content:space-between;margin-bottom:4px;",
          span(src, style="color:#666;font-size:9px;font-family:IBM Plex Mono;text-transform:uppercase;letter-spacing:1px;"),
          span(paste(when, if (nzchar(sent)) paste0("· ", sent) else ""),
               style=paste0("color:", col, ";font-size:9px;font-family:IBM Plex Mono;"))
        ),
        tags$a(href=url, target="_blank",
          div(title, style="color:#E8E8E8;font-size:11px;line-height:1.4;"))
      )
    })
    div(do.call(tagList, items))
  })

  # ── Deep Dive: crowd sentiment ────────────────────────────────────────────
  output$dd_sentiment <- renderUI({
    req(input$dd_ticker)
    sym <- input$dd_ticker

    st  <- if (!is.null(stwits_data) && "symbol" %in% names(stwits_data))
             stwits_data %>% filter(symbol == sym) else NULL
    wsb <- if (!is.null(wsb_data) && "ticker" %in% names(wsb_data))
             wsb_data %>% filter(ticker == sym) else NULL

    cell <- function(k, v, colour = "#E8E8E8") div(class="si-cell",
      div(class="si-k", k),
      div(class="si-v", style=paste0("color:", colour, ";font-size:15px;"), v))

    has_st  <- !is.null(st)  && nrow(st)  > 0
    has_wsb <- !is.null(wsb) && nrow(wsb) > 0

    if (!has_st && !has_wsb)
      return(div(glue("{sym} is not currently trending on StockTwits or r/wallstreetbets. ",
                      "Those feeds cover the ~50 most-discussed tickers, so absence is ",
                      "the normal state for most stocks."),
                 style="color:#666;padding:18px;font-family:IBM Plex Mono;font-size:11px;line-height:1.5;"))

    # stocktwits_trending stores raw bullish/bearish counts, not a ratio; the
    # ratio only exists in sentiment_history, which this panel does not read.
    num  <- function(x) suppressWarnings(as.numeric(x))
    bull <- NA_real_
    if (has_st) {
      b <- num(st$bullish[1]); r <- num(st$bearish[1])
      if (!is.na(b) && !is.na(r) && (b + r) > 0) bull <- b / (b + r)
    }
    tiles <- tagList(
      if (has_st) cell("StockTwits watchers",
                       formatC(num(st$watchlist_count[1]), format="d", big.mark=",")) else NULL,
      if (has_st && !is.na(bull)) cell("Bullish share", sprintf("%.0f%%", bull * 100),
                                       if (bull >= 0.6) "#00C853" else if (bull <= 0.4) "#FF3D00" else "#FFD600") else NULL,
      if (has_wsb) cell("WSB mentions", formatC(num(wsb$mentions[1]), format="d", big.mark=",")) else NULL,
      if (has_wsb) cell("WSB rank", paste0("#", wsb$rank[1])) else NULL
    )

    tagList(
      div(class="si-grid", tiles),
      div(style="color:#555;font-size:10px;font-family:IBM Plex Mono;line-height:1.5;margin-top:4px;",
          "Retail chatter, shown for context. It carries no weight in the score — ",
          "see Methodology.")
    )
  })

  # Panes are toggled by a CSS class rather than Shiny's own tab widgets, so
  # Shiny never learns a pane became visible and keeps its outputs suspended.
  outputOptions(output, "dd_header", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_v_master", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_v_fund", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_v_mom", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_v_squeeze", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_chart_title", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_price", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_volume", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_macd", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_rsi", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_metrics", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_short", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_radar", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_dcf", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_comps", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_news_count", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_news", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_sentiment", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_filings", suspendWhenHidden = FALSE)
  outputOptions(output, "tv_player", suspendWhenHidden = FALSE)
  outputOptions(output, "tv_meta", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_lbo", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_pitch", suspendWhenHidden = FALSE)
  outputOptions(output, "dd_filings_meta", suspendWhenHidden = FALSE)

  # CPI arrives as an index level near 300, which tells a reader nothing on its
  # own. Year-over-year change is the number people mean by "inflation".
  cpi_yoy_data <- reactive({
    if (is.null(macro_data)) return(NULL)
    d <- macro_data %>%
      filter(series == "CPI", !is.na(price)) %>%
      arrange(date)
    if (nrow(d) < 13) return(NULL)
    d %>% mutate(yoy = (price / dplyr::lag(price, 12) - 1) * 100) %>%
      filter(!is.na(yoy))
  })

  output$cpi_latest <- renderUI({
    d <- cpi_yoy_data()
    if (is.null(d) || nrow(d) == 0) return(div("—"))
    last <- tail(d, 1)
    div(glue("{sprintf('%+.1f%%', last$yoy)} · {format(last$date, '%b %Y')}"))
  })

  output$cpi_yoy <- renderPlotly({
    d <- cpi_yoy_data()
    if (is.null(d) || nrow(d) == 0) return(no_data())
    plot_ly(d, x = ~date, y = ~yoy, type = "scatter", mode = "lines",
            line = list(color = "#00B8D9", width = 2),
            fill = "tozeroy", fillcolor = "rgba(0,184,217,0.08)") %>%
      # 2% is the Fed's stated target; the chart is far easier to read against it
      add_lines(x = ~date, y = ~rep(2, nrow(d)), name = "2% target",
                line = list(color = "#FFD600", width = 1, dash = "dot"),
                showlegend = FALSE) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "YoY (%)")) %>% dk()
  })

  output$unrate_latest <- renderUI({
    if (is.null(macro_data)) return(div("—"))
    d <- macro_data %>% filter(series == "Unemployment", !is.na(price)) %>% arrange(date)
    if (nrow(d) == 0) return(div("—"))
    last <- tail(d, 1)
    div(glue("{sprintf('%.1f%%', last$price)} · {format(last$date, '%b %Y')}"))
  })

  output$unemployment <- renderPlotly({ make_macro_plot("Unemployment", "#64DD17") })

  # ── News & Events ──────────────────────────────────────────────────────────
  output$news_count <- renderUI({
    if (!is.null(news_data)) div(glue("{nrow(news_data)} STORIES")) else div("—")
  })

  output$news_feed <- renderUI({
    if (is.null(news_data) || nrow(news_data)==0) return(div("No news data available.", style="color:#666;padding:20px;font-family:IBM Plex Mono;"))
    # Determine which columns exist
    has_source   <- "source"   %in% names(news_data)
    has_headline <- "headline" %in% names(news_data)
    has_title    <- "title"    %in% names(news_data)
    has_link     <- "link"     %in% names(news_data)
    has_url      <- "url"      %in% names(news_data)
    has_summary  <- "summary"  %in% names(news_data)
    has_category <- "category" %in% names(news_data)

    items <- lapply(1:min(50, nrow(news_data)), function(i) {
      row <- news_data[i,]
      src      <- if(has_source)   replace_na(row$source,"")   else ""
      headline <- if(has_headline) replace_na(row$headline,"") else if(has_title) replace_na(row$title,"Untitled") else "Untitled"
      url      <- if(has_link)     replace_na(row$link,"#")    else if(has_url) replace_na(row$url,"#") else "#"
      summary  <- if(has_summary)  replace_na(row$summary,"")  else ""
      category <- if(has_category) replace_na(row$category,"") else ""

      src_color <- switch(src,
        "CNBC Markets"="#E00000","CNBC Finance"="#E00000",
        "Reuters"="#FF8C00","WSJ Markets"="#0080FF",
        "MarketWatch"="#00AA44","Reddit /r/stocks"="#FF4500",
        "Reddit /r/investing"="#FF4500","Reddit WSB"="#FF4500","StockTwits"="#00B8D9",
        "BBC Business"="#BB1919","Financial Times"="#FFA500",
        "#FF6B00"
      )
      # Mark stories about stocks in the tracked universe, the same way the
      # earnings calendar already does. Without it a headline about a name you
      # follow is indistinguishable from one about a company you do not.
      sym      <- if ("symbol" %in% names(row)) replace_na(row$symbol, "") else ""
      our_syms <- if (!is.null(master_data)) master_data$symbol else character(0)
      in_univ  <- nzchar(sym) && sym %in% our_syms

      div(style=paste0("border-left:3px solid ", if (in_univ) "#FF6B00" else "#1A1A1A",
                       ";padding:10px 14px;margin-bottom:6px;background:",
                       if (in_univ) "rgba(255,107,0,0.04)" else "#0D0D0D", ";"),
        div(style="display:flex;justify-content:space-between;margin-bottom:5px;",
          span(src, style=paste0("color:",src_color,";font-size:9px;font-weight:700;font-family:IBM Plex Mono;text-transform:uppercase;letter-spacing:1px;")),
          div(style="display:flex;gap:8px;align-items:center;",
            if (in_univ) span(sym, style="color:#FF6B00;font-size:9px;font-weight:700;font-family:IBM Plex Mono;border:1px solid #FF6B00;padding:1px 5px;") else NULL,
            span(category, style="color:#333;font-size:9px;font-family:IBM Plex Mono;")
          )
        ),
        tags$a(href=url, target="_blank",
          div(headline, style="color:#E8E8E8;font-size:11px;font-weight:600;line-height:1.4;margin-bottom:3px;")
        ),
        div(summary, style="color:#555;font-size:10px;font-family:IBM Plex Mono;line-height:1.4;")
      )
    })
    div(do.call(tagList, items))
  })

  output$earnings_feed <- renderUI({
    if (is.null(earnings_data) || nrow(earnings_data)==0)
      return(div("No earnings data. Run pipeline.",
                 style="color:#666;padding:20px;font-family:IBM Plex Mono;font-size:11px;"))
    upcoming <- earnings_data %>%
      filter(!is.na(date), date >= Sys.Date()) %>%
      arrange(date) %>% head(40)
    if (nrow(upcoming)==0) return(div("No upcoming earnings in next 60 days.",
                                      style="color:#666;padding:20px;font-family:IBM Plex Mono;"))
    # Flag stocks in our universe
    our_syms <- if (!is.null(master_data)) master_data$symbol else character(0)
    items <- lapply(1:nrow(upcoming), function(i) {
      row <- upcoming[i,]
      in_universe <- row$symbol %in% our_syms
      sym_style <- if (in_universe) "color:#FF6B00;font-weight:700;" else "color:#FF8C00;font-weight:600;"
      bg_style  <- if (in_universe) "background:rgba(255,107,0,0.05);" else ""
      eps_est   <- if (!is.na(row$epsEstimated)) paste0("Est $", round(row$epsEstimated, 2)) else ""
      rev_est   <- if ("revenueEstimated" %in% names(row) && !is.na(row$revenueEstimated))
                     paste0(" · Rev ", fmt_mktcap(row$revenueEstimated)) else ""
      time_str  <- replace_na(row$time, "")
      time_icon <- switch(time_str,
        "bmo" = "Pre-Market",
        "amc" = "After-Close",
        time_str
      )
      div(class="earn-item", style=bg_style,
        div(class="earn-date", format(row$date, "%b %d")),
        div(style=paste0(sym_style, "min-width:55px;font-family:var(--mono);font-size:11px;"), row$symbol),
        div(class="earn-time", time_icon),
        div(class="earn-eps", paste0(eps_est, rev_est))
      )
    })
    do.call(tagList, items)
  })

  # ── WSB Trending Feed ──────────────────────────────────────────────────
  output$wsb_count <- renderUI({
    if (!is.null(wsb_data) && nrow(wsb_data) > 0) {
      div(glue("TOP {nrow(wsb_data)} · r/WALLSTREETBETS"))
    } else {
      div("—")
    }
  })

  output$wsb_feed <- renderUI({
    if (is.null(wsb_data) || nrow(wsb_data) == 0) {
      return(div("No WSB data available. Pipeline will fetch on next run.",
                 style="color:#666;padding:20px;font-family:IBM Plex Mono;font-size:11px;"))
    }

    # Header row
    header <- div(class="wsb-item", style="border-bottom:2px solid #2A2A2A;",
      div(class="wsb-rank", "#"),
      div(class="wsb-sym", "TICKER"),
      div(class="wsb-name", "COMPANY"),
      div(class="wsb-mentions", "MENTIONS"),
      div(class="wsb-upvotes", "UPVOTES"),
      div(class="wsb-momentum", "TREND")
    )

    items <- lapply(1:min(30, nrow(wsb_data)), function(i) {
      row <- wsb_data[i,]
      rank_val    <- row$rank
      sym         <- row$ticker
      name_val    <- replace_na(row$name, "")
      mentions    <- row$mentions
      upvotes     <- row$upvotes
      momentum    <- if ("momentum" %in% names(row)) replace_na(row$momentum, "Steady") else "Steady"
      rank_chg    <- if ("rank_chg" %in% names(row)) replace_na(row$rank_chg, 0) else 0

      mom_cls <- switch(momentum,
        "Surging" = "wsb-surge",
        "Rising"  = "wsb-rise",
        "Falling" = "wsb-fall",
        "Fading"  = "wsb-fade",
        "wsb-steady"
      )

      rank_arrow <- if (rank_chg > 0) paste0("▲", rank_chg) else
                    if (rank_chg < 0) paste0("▼", abs(rank_chg)) else "—"

      div(class="wsb-item",
        div(class="wsb-rank", rank_val),
        div(class="wsb-sym", sym),
        div(class="wsb-name", name_val),
        div(class="wsb-mentions", formatC(mentions, format="d", big.mark=",")),
        div(class="wsb-upvotes", paste0("▲ ", formatC(upvotes, format="d", big.mark=","))),
        div(class=paste("wsb-momentum", mom_cls), paste0(rank_arrow, " ", momentum))
      )
    })

    div(header, do.call(tagList, items))
  })

  # ── StockTwits Trending Feed ──────────────────────────────────────────────
  output$stwits_count <- renderUI({
    if (!is.null(stwits_data) && nrow(stwits_data) > 0) {
      div(glue("TOP {nrow(stwits_data)} · SOCIAL"))
    } else {
      div("—")
    }
  })

  output$stwits_feed <- renderUI({
    if (is.null(stwits_data) || nrow(stwits_data) == 0) {
      return(div("No StockTwits data. Pipeline will fetch on next run.",
                 style="color:#666;padding:20px;font-family:IBM Plex Mono;font-size:11px;"))
    }

    header <- div(class="st-item", style="border-bottom:2px solid #2A2A2A;",
      div(class="st-rank", "#"),
      div(class="st-sym", "TICKER"),
      div(class="st-name", "NAME"),
      div(class="st-watch", "WATCHERS"),
      div(class="st-sent", "SENTIMENT")
    )

    items <- lapply(1:min(30, nrow(stwits_data)), function(i) {
      row <- stwits_data[i,]
      sym   <- row$symbol
      name  <- replace_na(row$title, "")
      watch <- if ("watchlist_count" %in% names(row) && !is.na(row$watchlist_count))
                 formatC(row$watchlist_count, format = "d", big.mark = ",") else "—"
      sent  <- if ("sentiment_label" %in% names(row)) replace_na(row$sentiment_label, "Neutral") else "Neutral"
      bull  <- if ("bullish" %in% names(row)) replace_na(row$bullish, 0) else 0
      bear  <- if ("bearish" %in% names(row)) replace_na(row$bearish, 0) else 0

      sent_cls <- if (grepl("Bull", sent)) "st-bull" else if (grepl("Bear", sent)) "st-bear" else "st-neut"
      sent_icon <- if (grepl("Very Bull", sent)) paste0("▲▲ ", sent) else
                   if (grepl("Bull", sent)) paste0("▲ ", sent) else
                   if (grepl("Very Bear", sent)) paste0("▼▼ ", sent) else
                   if (grepl("Bear", sent)) paste0("▼ ", sent) else sent

      div(class="st-item",
        div(class="st-rank", i),
        div(class="st-sym", sym),
        div(class="st-name", name),
        div(class="st-watch", watch),
        div(class=paste("st-sent", sent_cls), sent_icon)
      )
    })

    div(header, do.call(tagList, items))
  })

  # ── Methodology & Validation ──────────────────────────────────────────────
  output$about_meta <- renderUI({
    n <- if (!is.null(master_data)) nrow(master_data) else 0
    div(glue("{n} STOCKS · UPDATED {meta$last_updated}"))
  })

  output$about_body <- renderUI({
    pct <- function(x, d = 1) if (is.na(x)) "n/a" else sprintf("%+.*f%%", d, x * 100)

    # ── Validation block, driven by the live backtest output ────────────────
    validation <- if (is.null(bt_headline) || nrow(bt_headline) == 0) {
      div(class="callout",
        div(class="ct","Validation pending"),
        tags$p("The walk-forward backtest has not produced results yet. It needs at least ",
               "85 trading days of alpha history."))
    } else {
      h <- bt_headline[1, ]
      stats <- div(class="statrow",
        div(class="statcell", div(class="k","Q1 ann. alpha"),
            div(class="v", pct(h$q1_ann_alpha, 1)),
            div(class="s","top-scored quintile")),
        div(class="statcell", div(class="k","Q1 − Q5 spread"),
            div(class="v", pct(h$spread_ann, 1)),
            div(class="s","annualised")),
        div(class="statcell", div(class="k","Mean rank IC"),
            div(class="v", sprintf("%+.3f", h$mean_rank_ic)),
            div(class="s", sprintf("t = %.2f", h$ic_t_stat))),
        div(class="statcell", div(class="k","Rebalances"),
            div(class="v", h$rebalances),
            div(class="s", sprintf("%dd hold", h$hold_days)))
      )

      tbl <- if (!is.null(bt_summary) && nrow(bt_summary) > 0) {
        rows <- lapply(seq_len(nrow(bt_summary)), function(i) {
          r <- bt_summary[i, ]
          tags$tr(
            tags$td(r$bucket_label),
            tags$td(class="num", pct(r$ann_alpha, 2)),
            tags$td(class="num", sprintf("%.2f", r$alpha_ir)),
            tags$td(class="num", sprintf("%.0f%%", r$hit_rate * 100)),
            tags$td(class="num", pct(r$worst_period, 2)),
            tags$td(class="num", r$avg_n)
          )
        })
        tags$table(class="dtable",
          tags$thead(tags$tr(tags$th("Bucket"), tags$th("Ann. alpha"), tags$th("IR"),
                             tags$th("Hit rate"), tags$th("Worst period"), tags$th("Names"))),
          tags$tbody(do.call(tagList, rows)))
      } else NULL

      honest <- if (is.na(h$ic_t_stat) || abs(h$ic_t_stat) < 2) {
        div(class="callout",
          div(class="ct","How to read this"),
          tags$p(tags$strong("The spread is encouraging; the significance is not. "),
            sprintf("Top-quintile names returned %s annualised excess return against %s for the bottom quintile, but the mean rank information coefficient is %+.3f with a t-statistic of %.2f.",
                    pct(h$q1_ann_alpha,1), pct(h$q5_ann_alpha,1), h$mean_rank_ic, h$ic_t_stat)),
          tags$p("A t-statistic below 2 means that relationship is not statistically ",
                 "distinguishable from zero. With ", h$rebalances, " rebalances there is not ",
                 "enough independent evidence to claim the score has genuine predictive skill."),
          if (!isTRUE(h$monotonic)) tags$p(tags$strong("Bucket ordering is not monotonic. "),
            "Alpha does not decline cleanly from Q1 to Q5, which is what you would expect ",
            "from a signal with consistent power. Q1 separating from the rest, with the ",
            "middle buckets noisy, is the honest description.") else NULL)
      } else {
        div(class="callout",
          div(class="ct","How to read this"),
          tags$p(sprintf("Mean rank IC of %+.3f with t = %.2f clears the conventional |t| > 2 bar, so the relationship between score and forward alpha is statistically distinguishable from zero over this sample.",
                         h$mean_rank_ic, h$ic_t_stat)))
      }

      tagList(stats, tbl, honest)
    }

    tagList(
      tags$h2("What this is"),
      tags$p("A quantitative screener that ranks a ", tags$strong("195-stock universe"),
             " by risk-adjusted excess return against SPY. The pipeline runs unattended ",
             "every trading day at 21:30 UTC on GitHub Actions — pulling market data, ",
             "recomputing the model, and redeploying this dashboard without any manual step."),
      tags$p("The model ranks on ", tags$strong("alpha, not raw return"),
             ". A stock up 15% while the market is up 14% scores worse than one up 6% ",
             "while the market is up 2%."),

      tags$h2("Composite score"),
      # Show the weighting actually in force, read from the scored output rather
      # than hardcoded, so this page cannot drift from the model.
      (function() {
        live <- !is.null(master_data) && "options_live" %in% names(master_data) &&
                isTRUE(master_data$options_live[1])
        if (live) tagList(
          tags$pre(
"final_score = alpha_score    x 0.55
            + tech_filter    x 0.22
            + quality_gate   x 0.13
            + options_signal x 0.10"),
          tags$p("The options term is active: Polygon is returning options data ",
                 "that varies across the universe."))
        else tagList(
          tags$pre(
"final_score = alpha_score  x 0.61
            + tech_filter  x 0.24
            + quality_gate x 0.15"),
          tags$p("The options term is ", tags$strong("currently excluded"),
                 ". Polygon's free tier returns no options snapshot, so every stock ",
                 "falls back to an identical 50 — a constant that would take 10% of ",
                 "the weight while distinguishing nothing, compressing the score range ",
                 "by the same 10%. The model detects this and redistributes that weight ",
                 "across the three components that do carry signal. A paid Polygon key ",
                 "reactivates the term with no code change."))
      })(),
      tags$p("The alpha component is a percentile blend of five measures taken from each ",
             "stock's daily excess-return series:"),
      tags$pre(
"alpha_raw = annualised_alpha  x 0.30   Jensen's alpha, CAPM-adjusted
          + information_ratio x 0.25   alpha per unit of tracking error
          + hit_rate          x 0.20   % of days the stock beat SPY
          + alpha_63d         x 0.15   compounded alpha over one quarter
          + streak            x 0.10   consecutive positive-alpha days"),

      tags$h2("Confidence weighting"),
      tags$p("A short return series gives a noisy alpha estimate, so the raw score is ",
             "shrunk toward a neutral 50 in proportion to how much history supports it:"),
      tags$pre("alpha_score = alpha_raw x w + 50 x (1 - w),   w = min(days_tracked / 63, 1)"),
      tags$p("63 trading days is one quarter — the point at which an information ratio ",
             "becomes statistically meaningful. Below that the model deliberately refuses ",
             "to express conviction. Alpha history is rebuilt from the full one-year price ",
             "window on every run, so a missed run backfills itself and a newly added ticker ",
             "is judged on the same evidence as one tracked since day one."),

      tags$h2("Does it actually work?"),
      tags$p("Walk-forward test, out-of-sample by construction: at each rebalance the score ",
             "is computed from a 63-day formation window, stocks are sorted into quintiles, ",
             "and realised alpha is measured over the following 21 days — data the score ",
             "could not have seen. The weights are the live model's, not fitted here."),
      validation,

      # ── Per-component diagnostic ────────────────────────────────────────
      if (!is.null(bt_components) && nrow(bt_components) > 0) {
        rows <- lapply(seq_len(nrow(bt_components)), function(i) {
          r <- bt_components[i, ]
          sig <- isTRUE(r$significant)
          tags$tr(
            tags$td(r$label),
            tags$td(class="num", sprintf("%+.4f", r$mean_ic)),
            tags$td(class="num", sprintf("%.2f", r$t_stat)),
            tags$td(class="num", sprintf("%.0f%%", r$pct_positive * 100)),
            tags$td(style = if (sig) "color:#00C853;font-weight:600;" else "color:#666;",
                    if (sig) "significant" else "no")
          )
        })
        any_sig <- any(bt_components$significant, na.rm = TRUE)
        tagList(
          tags$p(tags$strong("Is the blend hiding a good signal? "),
                 "The composite could look flat because one informative component is ",
                 "being averaged away by four uninformative ones. Testing each ",
                 "separately against forward alpha rules that out — or finds it."),
          tags$table(class="dtable",
            tags$thead(tags$tr(tags$th("Component"), tags$th("Mean IC"),
                               tags$th("t"), tags$th("Correct sign"), tags$th("|t| > 2"))),
            tags$tbody(do.call(tagList, rows))),
          div(class="callout",
            div(class="ct","What this settles"),
            if (any_sig)
              tags$p("At least one component clears the significance bar on its own, ",
                     "which means the blend is diluting real signal and the weights ",
                     "are worth revisiting.")
            else tagList(
              tags$p(tags$strong("No component carries signal either. "),
                "Every measure sits within noise of zero, so the flat composite is not ",
                "a weighting problem — trailing daily alpha simply does not predict ",
                "forward excess return for this universe at a one-month horizon."),
              tags$p("That is a real finding rather than a bug, and it is the honest ",
                     "conclusion of the validation: a screener built on momentum in ",
                     "risk-adjusted excess return does not, on this evidence, identify ",
                     "stocks that go on to outperform.")))
        )
      },

      # ── Reversal hypothesis ─────────────────────────────────────────────
      if (!is.null(bt_reversal) && nrow(bt_reversal) > 0) {
        rows <- lapply(seq_len(nrow(bt_reversal)), function(i) {
          r <- bt_reversal[i, ]
          sig <- isTRUE(r$significant)
          tags$tr(
            tags$td(paste0(r$formation_days, " days")),
            tags$td(class="num", sprintf("%+.4f", r$mean_ic)),
            tags$td(class="num", sprintf("%.2f", r$ic_t)),
            tags$td(class="num", sprintf("%+.2f%%", r$ls_spread_ann * 100)),
            tags$td(style = if (sig) "color:#00C853;font-weight:600;" else "color:#666;",
                    r$effect)
          )
        })
        all_neg <- all(bt_reversal$mean_ic < 0, na.rm = TRUE)
        any_sig <- any(bt_reversal$significant, na.rm = TRUE)
        tagList(
          tags$h2("Testing the opposite hypothesis"),
          tags$p("The component diagnostic produced a result the model did not ",
                 "predict: of the six measures, the positive-alpha streak had the ",
                 "largest magnitude, it was ", tags$strong("negative"),
                 ", and it points against the momentum premise the score is built on. ",
                 "Short-term reversal is a documented effect, so the honest step is to ",
                 "test it rather than ignore a result that disagrees with the design."),
          tags$p("Same walk-forward harness, but correlating trailing alpha with forward ",
                 "alpha directly. The sign is the whole answer: ",
                 tags$strong("negative means losers outperform"), " (reversal), positive ",
                 "means winners do (momentum). Long the worst trailing quintile, short ",
                 "the best."),
          tags$table(class="dtable",
            tags$thead(tags$tr(tags$th("Formation"), tags$th("Mean IC"), tags$th("t"),
                               tags$th("Long/short, ann."), tags$th("Verdict"))),
            tags$tbody(do.call(tagList, rows))),
          div(class="callout",
            div(class="ct","Verdict"),
            if (any_sig)
              tags$p("A window clears the adjusted significance bar — worth pursuing.")
            else tagList(
              tags$p(tags$strong("No window clears significance either. "),
                "Testing four formation windows means four chances at a false positive, ",
                "so the bar here is |t| > 2.5 rather than the usual 2.0. Nothing comes ",
                "close; the strongest is the 10-day window at t = -1.53."),
              if (all_neg) tags$p(
                "Every window does carry a negative sign, which is the reversal ",
                "direction. That is suggestive but it is ", tags$strong("not"),
                " four independent confirmations — the windows overlap heavily and ",
                "read the same underlying returns, so their signs are correlated by ",
                "construction. Treating agreement across them as evidence would be a ",
                "mistake.") else NULL,
              tags$p("Two hypotheses tested, both rejected on the evidence. Neither ",
                     "momentum nor reversal in risk-adjusted excess return predicts ",
                     "forward performance for this universe at a one-month horizon.")))
        )
      },

      tags$h2("Data sources"),
      tags$table(class="dtable",
        tags$thead(tags$tr(tags$th("Source"), tags$th("Provides"), tags$th("Cadence"))),
        tags$tbody(
          tags$tr(tags$td("Yahoo Finance"), tags$td("Prices, market cap, P/E, EPS"), tags$td("Every run, all 195")),
          tags$tr(tags$td("Alpha Vantage OVERVIEW"), tags$td("P/B, ROE, margins, growth, analyst ratings, sector"), tags$td("22 tickers/run — free tier is 25 calls/day")),
          tags$tr(tags$td("Alpha Vantage NEWS"), tags$td("Market news with sentiment scoring"), tags$td("Daily")),
          tags$tr(tags$td("Alpha Vantage EARNINGS"), tags$td("Earnings dates and EPS estimates"), tags$td("Cached 3 days")),
          tags$tr(tags$td("FRED"), tags$td("Treasury curve, CPI, unemployment, Fed funds"), tags$td("Daily")),
          tags$tr(tags$td("ApeWisdom"), tags$td("r/wallstreetbets mentions and rank deltas"), tags$td("Daily")),
          tags$tr(tags$td("StockTwits"), tags$td("Trending symbols, bull/bear sentiment"), tags$td("Daily")),
          tags$tr(tags$td("SEC EDGAR (XBRL frames)"), tags$td("Free cash flow, debt, cash, revenue — the valuation inputs"), tags$td("Daily, whole universe")),
          tags$tr(tags$td("SEC EDGAR (submissions)"), tags$td("Recent filings, insider Form 4 counts, M&A flags"), tags$td("Daily, whole universe")),
          tags$tr(tags$td("Yahoo Finance RSS"), tags$td("Per-ticker company news"), tags$td("Daily, whole universe")),
          tags$tr(tags$td("Polygon.io"), tags$td("Options positioning, share counts"), tags$td("See limitations")))),

      tags$h2("Valuation model"),
      tags$p("The Deep Dive carries a discounted cash flow estimate. It is a ",
             tags$strong("ten-year, two-stage model"), ": free cash flow grows at the ",
             "observed rate for five years, then fades linearly to a 2.5% terminal rate, ",
             "with a Gordon terminal value. Cash flows are discounted at a WACC built from ",
             "CAPM — the live 10-year Treasury as the risk-free rate, a 5% equity risk ",
             "premium, the stock's beta, and an after-tax cost of debt at the risk-free ",
             "rate plus 200bp."),
      tags$p("Free cash flow is operating cash flow less capital expenditure, taken from ",
             "SEC XBRL filings and ", tags$strong("averaged across the available filing years"),
             ". A single year is misleading for any company in a heavy investment cycle: ",
             "Amazon's most recent year nets to roughly $8B against a share price in the ",
             "hundreds, while its three-year average is around $24B."),
      tags$p(tags$strong("What it will not do. "),
             "Companies with negative or unreported free cash flow are left blank rather ",
             "than estimated — a DCF on negative cash flow has no meaningful terminal ",
             "value. Growth is capped at 20% no matter what the filings show, because no ",
             "company compounds its trailing rate for a decade. Banks and insurers are ",
             "shown but should be ignored: a free-cash-flow DCF does not describe a ",
             "balance-sheet business."),
      tags$p(tags$strong("Treat the output as one input, not an answer. "),
             "A DCF is extremely sensitive to its assumptions — moving the discount rate ",
             "or terminal growth by a point moves the value by tens of percent. The ",
             "figure shown is what this specific set of conservative assumptions implies, ",
             "and on those assumptions most large-cap technology names screen well above ",
             "their computed value. That is a property of the method as much as a ",
             "statement about the companies."),

      tags$h2("Known limitations"),
      tags$p("Stated plainly, because they change how the output should be read."),
      tags$ul(
        tags$li(tags$strong("Survivorship bias in the backtest. "),
                "The universe is built from tickers that exist and have a year of history ",
                "today, so companies that were delisted or acquired over the test window are ",
                "absent. That biases measured returns upward, and it is the single largest ",
                "caveat on the numbers above."),
        tags$li(tags$strong("No options data on the free Polygon tier. "),
                "Options snapshots and SEC financials are paid-tier endpoints, so put/call ",
                "ratio, options sentiment and implied volatility are all empty. The score ",
                "drops the term rather than letting a constant dilute every stock, but the ",
                "signal itself is genuinely missing until a paid key is configured."),
        tags$li(tags$strong("Sector coverage is partial. "),
                "Sector comes only from Alpha Vantage, which refreshes 22 tickers per run, ",
                "so recently added names show an em dash until rotation reaches them."),
        tags$li(tags$strong("Short interest is a stub. "),
                "No free API provides reliable short-float data, so the field is explicitly ",
                "NA rather than filled with an unrelated proxy."),
        tags$li(tags$strong("Small sample. "),
                "One year of history supports only a handful of independent rebalances. ",
                "Treat every figure above as directional, not conclusive.")),

      tags$h2("Engineering"),
      tags$p("Five R modules run in sequence, each writing CSV that is committed back to the ",
             "repo and copied into the app before deploy — so the dashboard is a pure read of ",
             "pre-computed state and stays responsive regardless of upstream API latency."),
      tags$ul(
        tags$li("Incomplete price bars are dropped per symbol and every technical indicator ",
                "is individually guarded — one malformed series cannot halt a run."),
        tags$li("A fetch returning empty keeps the previous file rather than overwriting it, ",
                "so a transient API failure degrades a panel to stale instead of blank."),
        tags$li("Payloads with variable nested schemas are parsed field by field from atomic ",
                "vectors rather than bulk-coerced."),
        tags$li("Every run publishes a data-health table — row counts, file age, status — to ",
                "the workflow summary."),
        tags$li("Deploys retry three times, then fail the run loudly rather than reporting ",
                "success while the live site goes stale.")),
      tags$h2("Disclosures"),
      tags$p(tags$strong("Not investment advice. "),
             "EdgeScreener is a personal portfolio and educational project. The operator ",
             "is not a broker-dealer, not a registered investment adviser, and not ",
             "licensed to provide financial advice. Nothing on this site constitutes ",
             "investment, financial, legal or tax advice, nor a recommendation, ",
             "solicitation or offer to buy or sell any security. Using this site creates ",
             "no advisory or fiduciary relationship."),
      tags$p(tags$strong("Model output is not a recommendation. "),
             "Score bands — Very Strong, Strong, Neutral, Weak, Very Weak — are produced ",
             "mechanically by ranking a formula's output. They describe position in that ",
             "distribution, involve no human judgment about any company, and are ",
             "deliberately not phrased as buy or sell recommendations. As documented above, ",
             "this model shows no statistically significant ability to predict forward ",
             "returns."),
      # Language adapted from the CFTC Rule 4.41 hypothetical-performance
      # statement. That rule governs commodity trading advisors rather than an
      # equity screener, so it is not binding here — it is simply the industry
      # standard for presenting backtested results, and worth meeting.
      div(class="callout",
        div(class="ct","Hypothetical performance"),
        tags$p(style="font-family:var(--mono);font-size:11px;line-height:1.6;",
          "HYPOTHETICAL OR SIMULATED PERFORMANCE RESULTS HAVE CERTAIN INHERENT ",
          "LIMITATIONS. UNLIKE AN ACTUAL PERFORMANCE RECORD, SIMULATED RESULTS DO NOT ",
          "REPRESENT ACTUAL TRADING. SINCE THE TRADES HAVE NOT BEEN EXECUTED, THE RESULTS ",
          "MAY HAVE UNDER- OR OVER-COMPENSATED FOR THE IMPACT OF CERTAIN MARKET FACTORS ",
          "SUCH AS LACK OF LIQUIDITY. SIMULATED PROGRAMS ARE ALSO SUBJECT TO THE FACT ",
          "THAT THEY ARE DESIGNED WITH THE BENEFIT OF HINDSIGHT. NO REPRESENTATION IS ",
          "BEING MADE THAT ANY ACCOUNT WILL OR IS LIKELY TO ACHIEVE PROFITS OR LOSSES ",
          "SIMILAR TO THOSE SHOWN."),
        tags$p(style="margin-top:10px;",
          "Specific to this project: the backtest charges no commissions, spread or ",
          "slippage; assumes positions fill at closing prices; and draws its universe from ",
          "tickers listed today, so companies delisted or acquired during the test window ",
          "are absent. Each of those biases results upward.")),
      tags$p(tags$strong("Past performance is not a guarantee of future results. "),
             "Any historical figure shown, whether realised or simulated, may bear no ",
             "relationship to future outcomes. Investing involves risk, including the ",
             "possible loss of principal."),
      tags$p(tags$strong("No warranty. "),
             "Data is sourced from free public APIs, may be delayed, incomplete, ",
             "misattributed or simply wrong, and is provided \"as is\" and \"as available\" ",
             "without warranty of any kind, express or implied. No representation is made ",
             "as to accuracy, completeness, timeliness or fitness for any purpose."),
      tags$p(tags$strong("Limitation of liability. "),
             "To the maximum extent permitted by law, the operator accepts no liability ",
             "for any loss or damage, direct or indirect, arising from use of or reliance ",
             "on this site. You are solely responsible for your own investment decisions ",
             "and should consult a licensed financial professional before acting on any ",
             "information here."),
      tags$p(tags$strong("Third-party data. "),
             "Market data is retrieved from Yahoo Finance, Alpha Vantage, Polygon.io, ",
             "FRED, ApeWisdom and StockTwits, each subject to its own terms of use. This ",
             "project is not affiliated with, endorsed by, or sponsored by any of them, ",
             "nor by any exchange. All trademarks belong to their respective owners."),
      tags$p(tags$strong("No positions. "),
             "The operator holds no position in, and receives no compensation from, any ",
             "security displayed here, and receives no payment for its inclusion."),

      tags$p(style="margin-top:18px;color:#555;font-size:11px;font-family:var(--mono);",
             "Built in R — quantmod, dplyr, TTR, Shiny, plotly. CI/CD on GitHub Actions.")
    )
  })

  output$sector_today <- renderPlotly({
    if (is.null(sector_perf) || nrow(sector_perf)==0) {
      # Fallback: compute from master data
      if (is.null(master_data)) return(no_data())
      d <- master_data %>% filter(!is.na(sector),!is.na(ret_1m)) %>%
        group_by(sector) %>%
        summarize(changesPercentage=mean(ret_1m*100,na.rm=TRUE),.groups="drop") %>%
        rename(sector=sector) %>% arrange(changesPercentage)
      plot_ly(d, x=~changesPercentage, y=~reorder(sector,changesPercentage),
        type="bar", orientation="h",
        marker=list(color=~ifelse(changesPercentage>=0,"#00C853","#FF3D00"),opacity=0.9)) %>%
        layout(xaxis=list(title="1M Avg Return (%)"),yaxis=list(title=""),margin=list(l=160)) %>% dk(ml=160)
    } else {
      d <- sector_perf %>% arrange(changesPercentage)
      plot_ly(d, x=~as.numeric(changesPercentage), y=~reorder(sector,as.numeric(changesPercentage)),
        type="bar", orientation="h",
        marker=list(color=~ifelse(as.numeric(changesPercentage)>=0,"#00C853","#FF3D00"),opacity=0.9)) %>%
        layout(xaxis=list(title="Change (%)"),yaxis=list(title=""),margin=list(l=160)) %>% dk(ml=160)
    }
  })

  # ── ADDITIONAL OUTPUTS ──────────────────────────────────────────────────

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



  # ── MACRO TAB ──────────────────────────────────────────────────────────────
  output$macro_indicators <- renderUI({
    req(macro_data)
    df <- macro_data
    if (is.null(df) || nrow(df) == 0) return(div("No macro data available"))

    # Show latest value per series (not all rows)
    latest <- df %>% group_by(ticker) %>%
      arrange(desc(date)) %>% slice(1) %>% ungroup()

    items <- lapply(1:nrow(latest), function(i) {
      row <- latest[i,]
      val <- round(as.numeric(coalesce(row$value, row$price)), 2)
      color <- if (!is.na(val) && val >= 0) "#00C853" else "#FF3D00"
      display_name <- if ("series" %in% names(row)) row$series else row$ticker
      div(class="macro-card",
        div(class="macro-name", display_name),
        div(class="macro-value", style=paste0("color:",color,";font-size:1.6em;font-weight:700;"),
            paste0(val, "%")),
        div(class="macro-date", style="color:#666;font-size:0.75em;", as.character(row$date))
      )
    })
    div(class="macro-grid", style="display:grid;grid-template-columns:repeat(3,1fr);gap:12px;", items)
  })

  output$yield_curve_plot <- renderPlotly({
    req(macro_data)
    df <- macro_data
    maturities <- c("2Y","10Y")
    yields <- c(
      df$value[df$ticker=="DGS2"][1],
      df$value[df$ticker=="DGS10"][1]
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
    g10 <- suppressWarnings(as.numeric(df$value[df$ticker=="DGS10"][1]))
    g2  <- suppressWarnings(as.numeric(df$value[df$ticker=="DGS2"][1]))
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
    ff  <- suppressWarnings(as.numeric(df$value[df$ticker=="FEDFUNDS"][1]))
    cpi <- suppressWarnings(as.numeric(df$value[df$ticker=="CPIAUCSL"][1]))
    
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
    g10 <- suppressWarnings(as.numeric(df$value[df$ticker=="DGS10"][1]))
    g2  <- suppressWarnings(as.numeric(df$value[df$ticker=="DGS2"][1]))
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

  # An orphaned duplicate of output$news_feed used to sit here, shadowing
  # the real definition in the News & Events section above, so the universe
  # highlighting added there never took effect.

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
      text=~paste0(sector, ": ", round(avg_ret,2), ifelse("ret_1m" %in% names(df), "%", " pts")),
      hoverinfo="text"
    ) %>%
    plotly::layout(
      title=list(text="<b>Sector Performance — 1M Avg Return</b>", font=list(color="#E8E8E8")),
      paper_bgcolor="#0A0A0A", plot_bgcolor="#111111",
      xaxis=list(title=ifelse("ret_1m" %in% names(df), "1M Return (%)", "Avg Score"),
                 color="#999", gridcolor="#1A1A1A", zeroline=TRUE, zerolinecolor="#333"),
      yaxis=list(title="", color="#E8E8E8"),
      font=list(color="#E8E8E8"),
      margin=list(l=130)
    )
  })



  # ── SHORT/SQUEEZE TAB ──────────────────────────────────────────────────────
  squeeze_filtered <- reactive({
    req(master_data)
    df <- master_data
    # Base-R subsetting on a column containing NA yields all-NA phantom rows.
    # Harmless while every stock had a sector; now that sector is NA until
    # Alpha Vantage enriches a ticker, filter() is required so those drop out.
    if (!is.null(input$squeeze_sector) && input$squeeze_sector != "All")
      df <- dplyr::filter(df, !is.na(sector), sector == input$squeeze_sector)
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

}

shinyApp(ui=ui, server=server)
