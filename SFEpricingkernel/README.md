<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of QuantLet: SFEpricingkernel

Published in: Statistics of Financial Markets I

Description: Estimates and plots the empirical pricing kernel (EPK) for historical index and option data (code applicable only for calls) of the DAX. The historical density is estimated by simulating a GARCH process which is chosen according to the index data. To estimate the state price density nonparametric regression is used.

Keywords: black-scholes, option-price, state-price-density, Rookley, garch

See also: SFEVolSurfPlot, XFGSPDcb, XFGSPDcb2, XFGSPDcom

Author: Georg Keilbar, Lucas Rohde

Datafile: DAX.txt, odata20142212.txt

Input: Data of spot prices of the underlying asset for the last years. Columns in the option data file are defined in the following way: [,1] spot price [,2] strike [,3] interest rate [,4] maturity [,5] option price [,6] type [,7] implied volatlity [,8] date.

Output: Plot of the empirical pricing kernels for all maturities given in the option data file. As well as a plot of the empirical pricing kernel next to the estimated state price densities and estimated historical densities.

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2015/master/SFEpricingkernel/All_EPKs.jpg" alt="Image" />
</div>

