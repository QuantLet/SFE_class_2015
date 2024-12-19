<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SFECliquetPrice

Published in: Statistics of Financial Markets I

Description: Calculates and plots BS price of a cliquet call option as a function of S, r, D, sigma and points of reset. The cliquet option is priced as sum of aligned start forward options with strike price equal to the underlying at the reset points.

Keywords: black-scholes, plot, option, exotic-option, option-price

See also: SFECliquetPayoff

Author: Weongi Woo, Thorsten Disser

Submitted: 2015/12/20

Input: 

- S0: Stock price at t = 0

- r: Risk free interest rate

- D: Continuous dividend payment

- sigma: Volatility

- reset: reset points

Output: A plot of the underlying and a plot of the price of a cliquet call option with one pay-off at the final maturity.

Example: 'Plots are generated for the following parameter values: S0 = 100, r = 0.3, D = 0.01, sigma = 0.5, reset = c(0.3, 0.7)'

```
