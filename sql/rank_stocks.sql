-- Rank stocks by risk-adjusted return against the S&P 500.
--
-- Replace `edgescreener.prices` with your own project and dataset if they
-- differ. In the BigQuery sandbox the full name looks like:
--   `your-project-id.edgescreener.prices`
--
-- The score is mean excess return divided by the standard deviation of that
-- excess return, annualised. It answers "how much did this stock beat the
-- index by, per unit of the risk it took to do it." A stock that beat the
-- index by a lot while swinging wildly can score below one that beat it by
-- less but steadily.

WITH daily AS (
  -- LAG gives the previous trading day's close for the same symbol, so this
  -- is a one-day return. Rows are ordered by date inside each symbol, which
  -- matters: without PARTITION BY the last row of one stock would be used as
  -- the previous close of the next.
  SELECT
    symbol,
    date,
    close,
    close / LAG(close) OVER (PARTITION BY symbol ORDER BY date) - 1 AS ret
  FROM `edgescreener.prices`
),

spy AS (
  -- The benchmark, pulled out as its own one-row-per-date series.
  SELECT date, ret AS spy_ret
  FROM daily
  WHERE symbol = 'SPY'
),

excess AS (
  -- What the stock did beyond the index on the same day.
  SELECT
    d.symbol,
    d.date,
    d.ret - s.spy_ret AS excess_ret
  FROM daily d
  JOIN spy s USING (date)
  WHERE d.symbol != 'SPY'
    AND d.ret IS NOT NULL
    AND s.spy_ret IS NOT NULL
),

rolling AS (
  -- Trailing 126 trading days, about six months. The frame is the current row
  -- plus the 125 before it, so every value is known as of that date and no
  -- future data is used.
  SELECT
    symbol,
    date,
    AVG(excess_ret)         OVER w AS avg_excess,
    STDDEV_SAMP(excess_ret) OVER w AS vol_excess,
    COUNT(excess_ret)       OVER w AS n_obs
  FROM excess
  WINDOW w AS (
    PARTITION BY symbol
    ORDER BY date
    ROWS BETWEEN 125 PRECEDING AND CURRENT ROW
  )
),

latest AS (
  -- Keep only the most recent date per symbol, and only once the window is
  -- full. A partly filled window gives a volatility figure computed from a
  -- handful of days, which is not comparable across stocks.
  SELECT
    symbol,
    date,
    avg_excess,
    vol_excess,
    ROW_NUMBER() OVER (PARTITION BY symbol ORDER BY date DESC) AS rn
  FROM rolling
  WHERE n_obs = 126
    AND vol_excess > 0
)

SELECT
  RANK() OVER (ORDER BY avg_excess / vol_excess DESC)   AS rank,
  symbol,
  date                                                  AS as_of,
  -- Annualised so the numbers read like the percentages people expect.
  -- Returns scale with time, volatility with its square root.
  ROUND(avg_excess * 252 * 100, 2)                      AS excess_return_pct,
  ROUND(vol_excess * SQRT(252) * 100, 2)                AS volatility_pct,
  ROUND(avg_excess / vol_excess * SQRT(252), 3)         AS score
FROM latest
WHERE rn = 1
ORDER BY score DESC
LIMIT 10
