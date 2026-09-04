# Ranking stocks by risk-adjusted return, in SQL

This is the ranking step from EdgeScreener, rewritten as a single BigQuery
query. The R version does the same thing across 195 stocks; this one works on a
16-stock sample so it is easy to read and cheap to run.

The question it answers: **which stocks beat the S&P 500 by the most, per unit
of the risk they took to do it?**

Beating the index is easy if you buy something that swings 8% a day. Dividing by
volatility asks whether the extra return was worth the ride.

## What is here

| File | What it is |
|---|---|
| `prices.csv` | Daily closes, 16 stocks plus SPY, 2 years, 8,534 rows |
| `schema.json` | Column types for the BigQuery load |
| `rank_stocks.sql` | The query |

The prices are real, pulled from Yahoo through the R pipeline in this repo. SPY
is the S&P 500 proxy and the benchmark.

## Setting up BigQuery, free

The sandbox needs a Google account and no credit card.

1. Go to https://console.cloud.google.com/bigquery and sign in.
2. Accept the sandbox prompt if it appears. You get 10 GB of storage and 1 TB of
   queries a month. This dataset is under 200 KB, so you will not come close.
3. Create a project if you do not have one. Note its ID — it is not the display
   name, and it appears in the project picker at the top.
4. In the Explorer panel, click the three dots next to your project and pick
   **Create dataset**. Name it `edgescreener`. Leave the location default.

Sandbox tables expire after 60 days. That is fine here; reload the CSV if it
lapses.

## Loading the data

In the console: click the three dots next to the `edgescreener` dataset,
**Create table**, then set

- Source: **Upload**, and choose `prices.csv`
- File format: **CSV**
- Table name: `prices`
- Schema: tick **Edit as text** and paste the contents of `schema.json`
- Under Advanced options, set **Header rows to skip** to `1`

Or with the `bq` command line tool, if you have it:

```
bq mk --dataset edgescreener
bq load --source_format=CSV --skip_leading_rows=1 \
  edgescreener.prices prices.csv schema.json
```

## Running it

Open `rank_stocks.sql`, paste it into the query editor, and change
`` `edgescreener.prices` `` to `` `your-project-id.edgescreener.prices` `` if
BigQuery cannot resolve the short name.

Output is the top 10 by score:

```
rank  symbol  as_of       excess_return_pct  volatility_pct  score
1     PANW    2026-09-02  137.76             51.56           2.672
2     AMD     2026-09-02  167.79             66.92           2.507
3     CRWD    2026-09-02  133.69             58.59           2.282
4     DUOL    2026-09-02   93.55             60.68           1.542
5     UNH     2026-09-02   44.00             31.04           1.418
```

Note AMD in second place. It earned more excess return than PANW — 167% against
138% — but its volatility was higher, so it scores lower. That trade-off is the
whole point of the ranking.

## How the query works

Five steps, each a CTE.

**`daily`** turns prices into returns. `LAG` fetches the previous row's close
for the same symbol. `PARTITION BY symbol` keeps stocks separate — without it,
the last row of one stock would be treated as the previous close of the next.

**`spy`** pulls the benchmark out as its own series, one row per date.

**`excess`** joins each stock to SPY on date and subtracts. This is the daily
return above the index.

**`rolling`** is where the window functions do the work. `AVG` and
`STDDEV_SAMP` run over a frame of `ROWS BETWEEN 125 PRECEDING AND CURRENT ROW`
— the last 126 trading days, about six months. The frame ends at the current
row, so every value uses only data available on that date. `COUNT` over the same
frame reports how full it is.

**`latest`** keeps the most recent date per symbol, using `ROW_NUMBER`, and
drops any window that is not yet full. A window with 20 days in it produces a
volatility number that cannot be compared to one with 126.

The final `SELECT` annualises. Returns scale with time, so the mean is
multiplied by 252 trading days. Volatility scales with the square root of time,
so it gets `SQRT(252)`. The score is the ratio, which makes it an information
ratio.

BigQuery also has `QUALIFY`, which would replace the `latest` CTE with one line.
The `ROW_NUMBER` version is written out here because it runs anywhere.

## Checking it

The query was run against this CSV and the numbers were reproduced
independently in R with `dplyr`. Every symbol, figure and position matched. If
you change the window length or the benchmark, re-check it the same way — an
off-by-one in a window frame does not throw an error, it just gives a wrong
answer quietly.

## What the score does not tell you

A high score means a stock beat the index steadily over the last six months. It
does not mean it will keep doing so.

Two things worth knowing before reading anything into the ranking:

- These 16 tickers were picked in 2026 and their prices pulled backward. Any
  company that was delisted or acquired is missing. In the full 195-stock
  version, holding every name equally with no ranking at all still beat the
  index by about 7 points a year, purely because of who was on the list.
- Six months of daily data is 126 numbers. That is not enough to separate skill
  from luck. In the full backtest the outperformance carried a t-statistic of
  about 1.3, short of the usual bar of 2.

The ranking is a starting point for looking at stocks, not a reason to buy them.
