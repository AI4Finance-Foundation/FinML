
%% get data
%sp500_price=readtable('1-SP500 Price 1990 to 2017.csv','TreatAsEmpty',{''});
%sp500_price(:,[1,2])=[];
%sp500_price=table2array(sp500_price);
%add a column based on existing column
%sp500_price.adj_price=sp500_price.prccd./sp500_price.ajexdi;
%sp500_price(:,[3,4])=[];
%universal same
load('sp500_price.mat');
sp500_stocks=unique(sp500_price.tic);

%universal same
fundamental_ml=readtable('fundamental_final_table.csv','TreatAsEmpty',{''});

%selected_stock=readtable('sector10_stock1.csv','TreatAsEmpty',{''});
%different
selected_stock=readtable('stocks_selected_total_user8.csv','TreatAsEmpty',{''});
fundamental_stocks=unique(selected_stock.tic);

%selected_stock(:,1)=[];
all_date=unique(sp500_price.datadate);
trade_date=unique(selected_stock.trade_date);


%% get daily 1 year return table in each 89 period
%30mins
for i=1:length(trade_date)
    %match trading date
    index=selected_stock.trade_date==trade_date(i);
    %get the corresponding trade period's selected stocks' name
    stocks_name=selected_stock(selected_stock.trade_date==trade_date(i),1);
    stocks_name=string(table2array(stocks_name));
    %selected_assets=table2array(selected_stock(index,1));
    %get the corresponding trade period's selected stocks' predicted return
    asset_expected_return=table2array(selected_stock(index,2));
    %get current trade date
    last_year_tradedate=(trade_date(i)-round(trade_date(i)/10000)*10000)+round(trade_date(i)/10000-1)*10000;
    convert_to_yyyymmdd=datetime(last_year_tradedate,'ConvertFrom','yyyymmdd');
    %determine the business date
    lastY_tradedate=yyyymmdd(busdate(convert_to_yyyymmdd,-1));
    %get 1 year historical daily date table before trading date
    get_date_index=(all_date<trade_date(i) & all_date>lastY_tradedate);
    get_date=all_date(get_date_index);
    %get adjusted price table
    price_table=[];
    return_table=[];
    for m =1:length(stocks_name)
        %get stocks's name
        index_tic=(sp500_price.tic==stocks_name(m));
        %get this stock's all historicall price from sp500_price
        sp500_temp=sp500_price(index_tic,{'datadate','adj_price'});
        %set left outer join table
        merge_left_main=table(get_date);
        merge_left_main.Properties.VariableNames={'datadate'};
        %perform left outer join, join all price
        temp_price=outerjoin(merge_left_main,sp500_temp,'Type','left');
        price_table(:,m)=temp_price.adj_price;
        return_table=tick2ret(price_table);
    end
    all_price_table(i)={price_table};
    all_return_table(i)={return_table};
end

%Store stocks name and predicted return
for i=1:length(trade_date)

    index=selected_stock.trade_date==trade_date(i);
    %get the corresponding trade period's selected stocks' name
    stocks_name=selected_stock(selected_stock.trade_date==trade_date(i),{'tic','predicted_return'});
    %stocks_name=string(table2array(stocks_name));
    %selected_assets=table2array(selected_stock(index,1));

    all_stocks_name(i)={stocks_name};
end

%% Portfolio allocation

selected_stock=readtable('stocks_selected_total_user8.csv','TreatAsEmpty',{''});
trade_date=unique(selected_stock.trade_date);


%mean-variance
for i=0:89
p1_alldata=(all_stocks_name{i});
p1_stock=table2array(p1_alldata(:,1));

p1_return_table=all_return_table{i};
p1_return_cov=nancov(p1_return_table);

p1_predicted_return=table2array(p1_alldata(:,2));

%set the assets name, set risk free to 0
%minimum-variance
%p = Portfolio('AssetList', 0, 'RiskFreeRate', 0);

%mean-variance
p = Portfolio('AssetList', p1_stock, 'RiskFreeRate', 0);
%set the portfolo mean to the predicted return
%set the portfolo covariance matrix
p = setAssetMoments(p, p1_predicted_return, p1_return_cov);
%buycost=0.001;
%sellcost=0.001;
%q=p.setCosts(buycost,sellcost);
%A "default" portfolio set has LowerBound = 0 and LowerBudget = UpperBudget = 1
% such that a portfolio Port must satisfy sum(Port) = 1 with Port >= 0.
p = setDefaultConstraints(p);
upper_bound=0.05;
p = setBounds(p,0,upper_bound);
%maximizes the Sharpe ratio among portfolios on the efficient frontier.
w1 = estimateMaxSharpeRatio(p);
[risk1, ret1] = estimatePortMoments(p, w1)

%error then use equally weighted


weights_table=p1_alldata;
weights_table(:,3)=table(w1);
weights_table(:,4)=table(trade_date(i));
weights_table.Properties.VariableNames{3}='weights';
weights_table.Properties.VariableNames{4}='trade_date';

weights_mean_table(i)={weights_table};
%weights_minimum_table(i)={weights_table};

end


%%
%minimum-variance
for i=0:89
p1_alldata=(all_stocks_name{i});
p1_stock=table2array(p1_alldata(:,1));

p1_return_table=all_return_table{i};
p1_return_cov=nancov(p1_return_table);

%set the assets name, set risk free to 0
p = Portfolio('AssetList', p1_stock, 'RiskFreeRate', 0);
%set the portfolo mean to the predicted return
%set the portfolo covariance matrix
%minimum-variance
p = setAssetMoments(p, 0, p1_return_cov);
%buycost=0.001;
%sellcost=0.001;
%q=p.setCosts(buycost,sellcost);
%A "default" portfolio set has LowerBound = 0 and LowerBudget = UpperBudget = 1
% such that a portfolio Port must satisfy sum(Port) = 1 with Port >= 0.
p = setDefaultConstraints(p);
upper_bound=0.05;
p = setBounds(p,0,upper_bound);
%maximizes the Sharpe ratio among portfolios on the efficient frontier.
w1 = estimateMaxSharpeRatio(p);
[risk1, ret1] = estimatePortMoments(p, w1)

weights_table=p1_alldata;
weights_table(:,3)=table(w1);
weights_table(:,4)=table(trade_date(i));
weights_table.Properties.VariableNames{3}='weights';
weights_table.Properties.VariableNames{4}='trade_date';

weights_minimum_table(i)={weights_table};

end


minimum_weighted=vertcat(weights_minimum_table{:});
writetable(minimum_weighted,'minimum_weighted_user8.xlsx','Sheet',1,'Range','A1')
