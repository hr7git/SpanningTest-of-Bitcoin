import pandas as pd
import numpy as np
import statsmodels.api as sm
from scipy import stats

# 1. Load your data
# Requirements: A CSV with columns 'Date', 'BTC', 'Stocks', 'Bonds', 'Gold', 'REITs'
# Returns should be in percentage change (decimal or percentage)
df = pd.read_csv('portfolio_data.csv', index_index='Date', parse_dates=True)

# Define variables
# Bitcoin is the dependent variable (y)
y = df['BTC']
# Traditional assets are the independent variables (X)
X = df[['Stocks', 'Bonds', 'Gold', 'REITs']]

# Add a constant to X for the Alpha (intercept) calculation
X_with_const = sm.add_constant(X)

# 2. Fit the OLS Regression
model = sm.OLS(y, X_with_const).fit()

print("--- Regression Results ---")
print(model.summary())

# 3. Perform the Huberman and Kandel Spanning Test (Wald Test)
# We test two hypotheses:
# 1) The constant (const) is 0
# 2) The sum of all asset coefficients (beta_i) is 1
hypotheses = '(const = 0), (Stocks + Bonds + Gold + REITs = 1)'
wald_test = model.wald_test(hypotheses)

print("\n--- Wald Test for Spanning ---")
print(f"F-statistic: {wald_test.fvalue[0][0]:.4f}")
print(f"p-value: {wald_test.pvalue:.4f}")

# 4. Interpretation
if wald_test.pvalue < 0.05:
    print("\nResult: Reject the Null Hypothesis (p < 0.05)")
    print("Interpretation: Bitcoin provides significant expansion to the portfolio frontier.")
else:
    print("\nResult: Fail to Reject the Null Hypothesis")
    print("Interpretation: Bitcoin is spanned by existing assets; no significant expansion.")
