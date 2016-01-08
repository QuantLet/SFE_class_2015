% ---------------------------------------------------------------------
% Quantlet:     SFEPutConsistency
% ---------------------------------------------------------------------
% Description:  SFEPutConsistency computes the exercise price of
%               American put option using a binomial tree for assets
%               without dividends.In order to see whether the pricing is
%               reliable or not.
% ---------------------------------------------------------------------
% Inputs:       s0 - Stock Price
%               k - Exercise Price
%               i - Interest Rate
%               sig - Volatility
%               t - Time to Expiration
%               n - Number of Intervals
% ---------------------------------------------------------------------
% Output:       Option's price with different steps under the same time 
%               to maturity.
% ---------------------------------------------------------------------
% Author:       Yaojun Liu, Yinan Wu 20151228
% ---------------------------------------------------------------------
clear,clc;clear all;
%% User inputs parameters
disp('Please input Price of Underlying Asset s0, Exercise Price k, Domestic Interest Rate per Year i');
disp('Volatility per Year sig, Time to Expiration (Years) t, Number of steps n');
disp('as: [230, 210, 0.04545, 0.25, 0.5, 200]');
disp(' ') ;
para=input('[s0, k, i, sig, t, n]=');
while length(para) < 6
    disp('Not enough input arguments. Please input in 1*6 vector form like [230, 210, 0.04545, 0.25, 0.5, 50]');
    disp(' ') ;
    para=input('[s0, k, i, sig, t, n]=');
end
s0=para(1);             % Stock price
k=para(2);              % Exercise price
i=para(3);              % Interest rate
sig=para(4);         	% Volatility
t=para(5);              % Time to expiration
n=para(6);              % Number of intervals
%Check conditions
if s0<=0
    disp('SFEBiTree: Price of Underlying Asset should be positive! Please input again')
    s0=input('s0=');
end
if k<0
    disp('SFEBiTree: Exercise price couldnot be negative! Please input again')
    k=input('k=');
end
if sig<0
    disp('SFEBiTree: Volatility should be positive! Please input again')
    sig=input('sig=');
end
if t<=0
    disp('SFEBiTree: Time to expiration should be positive! Please input again')
    t=input('t=');
end
if n<1
    disp('SFEBiTree: Number of steps should be at least equal to 1! Please input again')
    n=input('n=');
end
%% Main computation
price_nn=zeros(1,n);
for nn=1:n                                    % Different steps
    dt=t/nn;                                  % Interval of step
    u=exp(sig*sqrt(dt));                      % Up movement parameter u
    d=1/u;                                    % Down movement parameter d
    b=i;                                      % Costs of carry
    p=0.5+0.5*(b-sig^2/2)*sqrt(dt)/sig;       % Probability of up movement
    % Pricing by using different steps
    s=ones(nn+1,nn+1)*s0;
    un=zeros(nn+1,1);
    un(nn+1,1)=1;
    dm=un';
    um=[];
    j=1;
    l=1;
    while j<nn+1
        d1=[zeros(1,nn-j) (ones(1,j+1)*d).^((1:j+1)-1)];
        dm=[dm; d1];                                       % Down movement dynamics
        u1=[ones(1,nn-j)-1 (ones(1,j+1)*u).^((j:-1:0))];
        um=[um; u1];                                       % Up movement dynamics
        j=j+1;
    end
    um=[un';um]';
    dm=dm';
    s=s(1,1).*um.*dm;                                  % Stock price development
    Stock_Price=s;
    s=flipud(s);                                       % Rearangement
    opt = zeros(size(s));
    % Option is an american put
    opt(:,nn+1) = max(k-s(:,nn+1),0);                  % Determine option values from prices
    for j = nn:-1:1
        l = 1:j;
        % Probable option values discounted back one time step
        discopt = ((1-p)*opt(l,j+1)+p*opt(l+1,j+1))*exp(-b*dt);
        % Option value is max of X - current price or discopt
        opt(:,j) = [max(k-s(1:j,j),discopt);zeros(nn+1-j,1)];
    end
    American_Put_Price = flipud(opt);
    price_nn(nn)=American_Put_Price(end,1);            % The price of the option under different steps
end
%% The figure of option price with different steps(the same time to maturity)
plot(1:n,price_nn,'b-');
title('American put option price with different steps')
xlabel('Steps')
ylabel('Option rice')





