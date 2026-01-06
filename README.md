# Asymmetric Positive Feedback Trading and Factor-Based Strategy

This repository implements an empirical asset pricing study on **asymmetric positive feedback trading (AFT)** in the Chinese equity market, with a particular focus on small-cap stocks. The project constructs a novel AFT measure based on rolling regressions with realized volatility and incorporates it into a factor-based trading strategy.

The empirical analysis follows a standard research pipeline in asset pricing:
**data construction → factor estimation → portfolio sorting → statistical testing → strategy backtesting**.

---

## 1. Research Motivation

Positive feedback trading—buying after price increases and selling after price decreases—has been widely documented in financial markets. However, investor reactions to **positive and negative past returns may be asymmetric**, especially under varying volatility conditions.

This project aims to:
- Quantify **asymmetric feedback trading intensity (AFT)** at the stock level
- Examine whether AFT behaves like a priced characteristic
- Test whether AFT can be exploited in a **predictive trading strategy**

---

## 2. Data Description

- **Stock universe**: CSI 500 constituents listed before 2003
- **Frequency**: Daily (returns, volatility), aggregated to monthly where appropriate
- **Key variables**:
  - Daily stock returns
  - Realized volatility (RV)
  - Market capitalization (Size)
  - Price-to-earnings ratio (Value)
  - Amihud illiquidity measure (Liquidity)

All data are sourced from CSMAR and official index files.

---

## 3. Methodology Overview

### 3.1 Asymmetric Feedback Trading (AFT)

For each stock, AFT is estimated using rolling regressions of the form:

\[
r_t = \beta_0 + \beta_1 r_{t-1} + \beta_2 r_{t-1} \mathbb{I}(r_{t-1} > 0)
+ \beta_3 RV_t + \beta_4 r_{t-1} \times RV_t
+ \beta_5 r_{t-1} \mathbb{I}(r_{t-1} > 0) \times RV_t + \varepsilon_t
\]

The coefficient \(\beta_5\) captures **asymmetric feedback trading intensity**.

Rolling windows of 12 months are used to obtain time-varying AFT measures.

---

### 3.2 Factor Construction

Stocks are sorted monthly into quintiles based on:
- Market capitalization (Size)
- Price-to-earnings ratio (Value)
- Amihud illiquidity (Liquidity)
- Asymmetric feedback trading (AFT)

Value-weighted portfolio returns are computed, and factor spreads (High–Low or Low–High) are constructed.

---

### 3.3 Statistical Testing

Factor returns are evaluated using **Newey–West adjusted t-tests** to account for autocorrelation and heteroskedasticity.

---

### 3.4 Strategy Backtesting

A predictive trading strategy is implemented using:
- Rolling estimation of factor loadings
- Weekly return prediction
- Stock selection based on predicted returns
- Value-weighted portfolio construction
- Performance evaluation using cumulative returns, Sharpe ratio, and maximum drawdown

---

## 4. Repository Structure

```text
src/
├── 01_data_loading.R
├── 02_data_preprocessing.R
├── 03_volatility_measures.R
├── 04_AFT_estimation.R
├── 05_factor_construction.R
├── 06_factor_tests.R
├── 07_strategy_backtest.R
