%%
%--------------------------------------------------------------------------
% Data Preparation
%--------------------------------------------------------------------------

clear all
clc
close all

% read the data
x=csvread('surf_2010-2014.csv');

% calculate moneyness
x(:,8)=x(:,1)./x(:,2);

% exclude date with time to maturity>1, moneyness<0.7 as well as >1.2
ex1=find(x(:,4)>1);
ex2=find(x(:,8)<0.8 |x(:,8)>1.2);
ex=[ex1;ex2];
x(ex,:)=[];

% assign vectors
Price=x(:,1);       
Strike=x(:,2);      
Rate=x(:,3);
Time=x(:,4);
Value=x(:,5);
Class=x(:,6);
Index=x(:,7);

% calculate the IV with the Black-Scholes-Formula
iv = blsimpv(Price, Strike, Rate, Time, Value,[],[], [],Class);
x(:,9)=iv;

% exclude missing values
ex3=find(isnan(iv));
x(ex3,:)=[];

% determine number of trading days in data set
t=max(Index);

%%

% loop over the different trading days
for s=1:t
    a=find(x(:,7)==s);
    A=x(a,:);

%--------------------------------------------------------------------------
% Grid Preparations
%--------------------------------------------------------------------------

    % set bounderies for moneyness and maturity
    firstmon=0.8;
    lastmon=1.2;
    firstmat=0;
    lastmat=1;

    % set spaces in grid: moneyness in steps of 0.02, maturity in steps of 1 week
    stepwidth=[0.02 1/52];
    lengthmon=ceil((lastmon-firstmon)/stepwidth(1));
    lengthmat=ceil((lastmat-firstmat)/stepwidth(2));

    mongrid=linspace(0.8,1.2,lengthmon+1);
    matgrid=linspace(0,1,lengthmat+1);

    [MON, MAT]=meshgrid(mongrid,matgrid);

%%
%--------------------------------------------------------------------------
% Smoothing the Surface with the Nadaraya-Watson-Kernel Estimation
%--------------------------------------------------------------------------

    gmon=lengthmon+1;
    gmat=lengthmat+1;
    uu=size(A);
    v=uu(1,1);
    ivsurf=zeros(gmat,gmon);

    j=1;
    while (j<gmat+1);
        k=1;
        while (k<gmon+1);
            i=1;
        
            % regression matrix
            X=zeros(v,3);
            while (i<v+1);
                X(i,:)=[1,A(i,8)-MON(j,k), A(i,4)-MAT(j,k)];
                i=i+1;
            end
        
            % implied volatility
            Y=A(:,9);
        
            % optimal bandwidth by Silverman's rule of thumb
            a=unique(A(:,8));
            h1=0.9*min(std(a),iqr(a)/1.34)*length(a)^(-1/5);
        
            b=unique(A(:,4));
            h2=0.9*min(std(b),iqr(b)/1.34)*length(b)^(-1/5);
        
            % kernel matrix - gaussian kernel
            W=zeros(v,v);
            i=1;
            while (i<v+1);
                u1=(A(i,8)-MON(j,k))/h1;
                u2=(A(i,4)-MAT(j,k))/h2;
                aa=sqrt(1/2*pi)*exp(-0.5*u1^2);
                bb=sqrt(1/2*pi)*exp(-0.5*u2^2);
                W(i,i)=aa*bb;
                i=i+1;
            end

            % Nadaraya Watson kernel estimation
            est=inv(X'*W*X)*X'*W*Y;
            ivsurf(j,k)=est(1);
            k=k+1;
        end
        j=j+1;
    end

    % surface
    IV=ivsurf;
%%
%--------------------------------------------------------------------------
% Plot surface and save 
%--------------------------------------------------------------------------

    % plot surface
    surf(MON,MAT,IV)
    colormap hsv
    alpha(0.3) 

    % scatter iv's for single time to maturities
    hold on
    Time=A(:,4);
    Moneyness=A(:,8);
    iv=A(:,9);
    %scatter3(Moneyness,Time,iv,'filled')

    % label and scale axis
    xlabel('Moneyness')
    ylabel('Time to Maturity')
    zlabel('Implied Volatility')
    zlim([0 0.8]);

    % title
    Month={'January','February','March','April','May','June','July','August','September','October','November','December'};
    Year={'2010','2011','2012','2013','2014'};
    Title={Month{mod(s+11,12)+1} Year{ceil(s/12)}};
    title(Title)

    hold off

    % uncomment to save single figures
    %saveas(gcf, sprintf('VolSurfPlot%d.fig', s))

    % save film fragments
    M(s)=getframe(gcf);
end

%%
%--------------------------------------------------------------------------
% Film
%--------------------------------------------------------------------------

% uncomment for instant play
%movie(gcf,M,1,2)

% save film
movie2avi(M, 'VolSurfFilm.avi','fps',1)

