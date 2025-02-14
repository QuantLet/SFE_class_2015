<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SFECliquetPayoff

Published in: Statistics of Financial Markets I

Description: Calculates and plot the payoff of a cliquet put/call option. When there is no reset points this will generate the pay off graph of a European put/call option.

Keywords: plot, option, call, put, exotic-option, european-option

See also: SFECliquetPrice

Author: Weongi Woo, Thorsten Disser

Submitted: 2015/12/20

Input: 
- S0 : Stock price at t = 0
- r : Risk free interest rate
- D : Continuous dividend payment
- sigma : Volatility
- reset : Reset points
- typ : The type of options to plot. "call" and "put" are available.

Output: A plot of simulated underlying with the payoff of the desired option type considered at the desired resets.

Example: Plots are generated for the following parameter values: S0 = 100, r = 0.3, D = 0.01, sigma = 0.5, reset = c(0.3, 0.6, 0.9), typ = "call

```
