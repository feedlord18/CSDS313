rng('default') % For reproducibility
%% matrix setup
matrix = zeros(100, 6);
for i = 1:100
    for j = 1:6
        matrix(i, j) = sample(j);
    end
end
%% part A
samp_10 = zeros(2, 6);
for i = 1:6
    n = 10;
    [mean, var] = stats(matrix, i, n);
    samp_10(1, i) = mean;
    samp_10(2, i) = var;
end
samp_100 = zeros(2, 6);
for i = 1:6
    n = 100;
    [mean, var] = stats(matrix, i, n);
    samp_100(1, i) = mean;
    samp_100(2, i) = var;
end
%% part B
figure(1)
boxplot(matrix, labels)
%% part C
% CDF
figure(2)
for i = 1:3
    switch (i)
        case 1
            h1 = cdfplot(matrix(:, i));
        case 2
            h2 = cdfplot(matrix(:, i));
        case 3
            h3 = cdfplot(matrix(:, i));
    end
    hold on
end
title('Cumulative Distribution Function')
legend('Bernoulli','Binomial', 'Uniform','Location','best')
hold off
% I-CDF
figure(3)
for i = 1:3
    switch (i)
        case 1
            x = h1.XData;
            y = h1.YData;
        case 2
            x = h2.XData;
            y = h2.YData;
        case 3
            x = h3.XData;
            y = h3.YData;
    end
    plot(y, x);
    hold on
end
title('Inverse Cumulative Distribution Function')
legend('Bernoulli','Binomial', 'Uniform','Location','best')
hold off
%% part D
figure(4)
for i = 4:6
    histogram(matrix(:, i))
    hold on
end
title('Histogram of Sampled Distribution')
legend('Normal','Exponential', 'Power Law','Location','best')
hold off
%% functions
function v = sample(i)
    v = -1;
    switch (i)
        case 1
            % bernoulli with p=0.3, outcome: (T/F)
            v = rand(1) < bern_p;
        case 2
            % binomial with n=10, p=0.4, outcome: total_num_success
            v = binornd(bino_trials, bino_p);
        case 3
            % uniform with a = 3, b = 10, outcome: rand_num
            v = unifrnd(uni_a, uni_b);
        case 4
            % normal with u = -1, var = 2, outcome: rand_num
            v = normrnd(norm_u, norm_sig);
        case 5
            % exponential with lambda = 2, outcome: rand_num
            v = exprnd(1/expo_lambda);
        case 6
            % power law (pareto) with alpha = 3, xmin = 1, outcome:
            % rand_num
            v = randht(1,'xmin',power_xmin,'powerlaw',power_alpha);
    end
end

function [mean, var] = stats(matrix, i, n)
    col = matrix(:, i);
    mean = sum(col)./n;
    var = 0;
    for i=1:length(col)
        var = var + (col(i) - mean)^2;
    end
    var = var / length(col);
end

%% constant variable (hidden from workspace)
function value = bern_p
    value = 0.3;
end

function value = bino_trials
    value = 10;
end

function value = bino_p
    value = 0.4;
end

function value = uni_a
    value = 3;
end

function value = uni_b
    value = 10;
end

function value = norm_u
    value = -1;
end

function value = norm_sig
    value = 2;
end

function value = expo_lambda
    value = 2;
end

function value = power_alpha
    value = 3;
end

function value = power_xmin
    value = 1;
end

function value = labels
    value = {'Bernoulli', 'Binomial', 'Uniform', 'Normal', 'Exponential', 'Power Law'};
%     g1 = repmat({value(1)},5,1);
%     g2 = repmat({value(2)},10,1);
%     g3 = repmat({value(3)},15,1);
%     g4 = repmat({value(4)},20,1);
%     g5 = repmat({value(5)},25,1);
%     g6 = repmat({value(6)},30,1);
%     value = [g1; g2; g3; g4; g5; g6];
end

function value = axis_length
    value = 100;
end



function x=randht(n, varargin)
    % RANDHT generates n observations distributed as some continous heavy-
    % tailed distribution. Options are power law, log-normal, stretched 
    % exponential, power law with cutoff, and exponential. Can specify lower 
    % cutoff, if desired.
    % 
    %    Example:
    %       x = randht(10000,'powerlaw',alpha);
    %       x = randht(10000,'xmin',xmin,'powerlaw',alpha);
    %       x = randht(10000,'cutoff',alpha, lambda);
    %       x = randht(10000,'exponential',lambda);
    %       x = randht(10000,'lognormal',mu,sigma);
    %       x = randht(10000,'stretched',lambda,beta);
    %
    %    See also PLFIT, PLVAR, PLPVA
    %
    %    Source: http://www.santafe.edu/~aaronc/powerlaws/

    % Version 1.0   (2007 May)
    % Version 1.0.1 (2007 September)
    % Version 1.0.2 (2008 April)
    % Copyright (C) 2007 Aaron Clauset (Santa Fe Institute)
    % Distributed under GPL 2.0
    % http://www.gnu.org/copyleft/gpl.html
    % RANDHT comes with ABSOLUTELY NO WARRANTY
    % 
    % Notes:
    % 

    type   = '';
    xmin   = 1;
    alpha  = 2.5;
    beta   = 1;
    lambda = 1;
    mu     = 1;
    sigma  = 1;
    persistent rand_state;

    % parse command-line parameters; trap for bad input
    i=1; 
    while i<=length(varargin), 
      argok = 1; 
      if ischar(varargin{i}) 
        switch varargin{i},
            case 'xmin',            xmin = varargin{i+1}; i = i + 1;
            case 'powerlaw',        type = 'PL'; alpha  = varargin{i+1}; i = i + 1;
            case 'cutoff',          type = 'PC'; alpha  = varargin{i+1}; lambda = varargin{i+2}; i = i + 2;
            case 'exponential',     type = 'EX'; lambda = varargin{i+1}; i = i + 1;
            case 'lognormal',       type = 'LN'; mu = varargin{i+1}; sigma = varargin{i+2};i = i + 2;
            case 'stretched',       type = 'ST'; lambda = varargin{i+1}; beta = varargin{i+2}; i = i + 2;
            otherwise, argok=0; 
        end
      end
      if ~argok, 
        disp(['(RANDHT) Ignoring invalid argument #' num2str(i+1)]); 
      end
      i = i+1; 
    end
    if (~isscalar(n) || n<1)
        fprintf('(RANDHT) Error: invalid ''n'' argument; using default.\n');
        n = 10000;
    end;
    if (~isscalar(xmin) || xmin<1)
        fprintf('(RANDHT) Error: invalid ''xmin'' argument; using default.\n');
        xmin = 1;
    end;
    if isempty(rand_state)
        rand_state = cputime;
        rand('twister',sum(100*clock));
    end;

    switch type
        case 'EX', x = xmin - (1/lambda)*log(1-rand(n,1));
        case 'LN',
            y = exp(mu+sigma*randn(10*n,1));
            while true
                y(y<xmin) = [];
                q = length(y)-n;
                if (q==0), break;
                end;
                if (q>0),
                    r = randperm(length(y));
                    y(r(1:q)) = [];
                    break;
                end;
                if (q<0),
                    y = [y; exp(mu+sigma*randn(10*n,1))];
                end;
            end;
            x = y;
        case 'ST', x = (xmin^beta - (1/lambda)*log(1-rand(n,1))).^(1/beta);
        case 'PC',
            x = [];
            y = xmin - (1/lambda)*log(1-rand(10*n,1));
            while true
                y(rand(10*n,1)>=(y./xmin).^(-alpha)) = [];
                x = [x; y];
                q = length(x)-n;
                if (q==0), break;
                end;
                if (q>0),
                    r = randperm(length(x));
                    x(r(1:q)) = [];
                    break;
                end;
                if (q<0),
                    y = xmin - (1/lambda)*log(1-rand(10*n,1));
                end;
            end;
        otherwise, x = xmin*(1-rand(n,1)).^(-1/(alpha-1));
    end;
end