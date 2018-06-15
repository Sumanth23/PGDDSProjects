
##################################################################################
##################################################################################
#                             Time Series Case Study
##################################################################################
##################################################################################

##################################################################################
#                     Loading required libraries and packages                             
##################################################################################
# ----------------------------------------------------------------------


# Clearing Workspace
  rm(list = ls())

# Installing required packages 
  #install.packages("ggseas")
  #install.packages("seasonal")
  #library(seasonal)
  # Loading libraries

# Loading required libraries 
  library(dplyr)
  library(ggplot2)
  library(forecast)
  library(tseries)
  #library(ggseas)
  #library(astsa)
  
# Loading the data set
  global_mart <-
    read.csv('Global Superstore.csv', stringsAsFactors = FALSE)


##################################################################################
#                             Data Cleaning                              
##################################################################################
# ----------------------------------------------------------------------

  # Checking the structure of the data set
    str(global_mart)
  # Checking the unique list of segments in the data set
    unique(global_mart$Segment)
    # "Consumer"    "Corporate"   "Home Office"
  # Checking the unique list of markets in the data set
    unique(global_mart$Market)
    # "US"     "APAC"   "EU"     "Africa" "EMEA"   "LATAM"  "Canada"

  # Checking duplicate rows
    sum(duplicated(global_mart))    # 0 indicates that there are no duplicate rows

  # Checking empty and NA values
    sapply(global_mart, function(x)
        sum(is.na(x))) # Only postal code has 41296 NA values.
    global_mart$Postal.Code <- NULL


  # Converting necessary variables to factor
    factor_columns <-
      c("Ship.Mode", "Segment", "City",
        "State", "Country", "Market",
        "Region", "Category", "Sub.Category",
        "Order.Priority"
      )
    global_mart[factor_columns] <-
      lapply(global_mart[factor_columns], factor)

  # Converting the Order.Date and Ship.Date to date
    global_mart$Order.Date <- as.Date(global_mart$Order.Date, "%d-%m-%Y")
    global_mart$Ship.Date <- as.Date(global_mart$Ship.Date, "%d-%m-%Y")

  # Extracting Month and Year from the Order.Date
    global_mart$Year_Month <-
      paste(
            strftime(global_mart$Order.Date, "%Y"),
            strftime(global_mart$Order.Date, "%m"),
            sep = "-"
            )


##################################################################################
#                           Data Preperation                             
##################################################################################
# ----------------------------------------------------------------------

  # 1. Create segments of the data using the Market and Segment data fields.
    segment_global_mart <-
        group_by(global_mart, Market, Segment, Year_Month)
  
  # 2. Summarize the Profit for each segment.
    profit_per_market_segment <-
        summarize(segment_global_mart, Segment_Profit = sum(Profit))
    sales_per_market_segment <-
        summarise(segment_global_mart, Segment_Sales = sum(Sales))
    quantity_per_market_segment <-
        summarize(segment_global_mart, Segment_Quantity = sum(Quantity))
  
    summary(profit_per_market_segment)
    summary(sales_per_market_segment)
    summary(quantity_per_market_segment)
    
  # 3.  #Creating a dataset to aggregate yearly profit at segment and market level. 
    #   We will use this to plot and derive insights.
    profit_per_market_segment$year <- substr(profit_per_market_segment$Year_Month,1,4)
    profit_per_segment_yearly <- profit_per_market_segment %>% group_by(Segment,year) %>% 
        summarise(total_profits=sum(Segment_Profit))
    profit_per_market_yearly <- profit_per_market_segment %>% group_by(Market,year) %>% 
        summarise(total_profits=sum(Segment_Profit))
    
        ggplot(
      profit_per_market_yearly,
      aes(
        x = year,
        y = total_profits,
        color = Market,
        group = Market
      )
    ) + scale_y_continuous(breaks = seq(0, 150000, 10000), lim = c(0, 150000)) + geom_line() +
      labs(title = "Profits Across Markets From 2011 to 2014", x = "Year", y = "Profits")
        
        ggplot(
          profit_per_segment_yearly,
          aes(
            x = year,
            y = total_profits,
            color = Segment,
            group = Segment
          )
        ) + geom_line() +
          labs(title = "Profits Across Segments From 2011 to 2014", x = "Year", y = "Profits")
  
  # 4. COnvert the summary for the segement of Profit into time series
    profit_ts <-
      ts(
        profit_per_market_segment$Segment_Profit,
        start = c(2011, 1),
        end = c(2014, 12),
        frequency = 12
      )
    plot(profit_ts)
  
    sales_ts <-
      ts(
        sales_per_market_segment$Segment_Sales,
        start = c(2011, 1),
        end = c(2014, 12),
        frequency = 12
      )
    plot(sales_ts)
    
    quantity_ts <-
      ts(
        quantity_per_market_segment$Segment_Quantity,
        start = c(2011, 1),
        end = c(2014, 12),
        frequency = 12
      )
    plot(quantity_ts)
  
  
  # 5. Check the Coefficient of variation for the Profit.
    cv_segment_group <-
      group_by(profit_per_market_segment, Market, Segment)
    cv_segment_summary_sd <-
      summarize(cv_segment_group, sd_value = sd(Segment_Profit))
    cv_segment_summary_mean <-
      summarize(cv_segment_group, mean_value = mean(Segment_Profit))
    
    cv_segment <- merge(cv_segment_summary_mean, cv_segment_summary_sd)
    cv_segment$cv <- (cv_segment$sd_value / cv_segment$mean_value) * 100
    
    head(cv_segment[order(cv_segment$cv), ])
  
    # Market   Segment mean_value  sd_value       cv
    # 13     EU  Consumer   3930.994 2454.1398 62.43052
    # 4    APAC  Consumer   4642.033 2934.3785 63.21323
    # 16  LATAM  Consumer   2513.186 1662.4295 66.14828
    # 5    APAC Corporate   2702.859 1886.8305 69.80869
    # 14     EU Corporate   2570.708 1963.5252 76.38072
    # 17  LATAM Corporate   1205.738  978.0003 81.11217
      
    # We see that the coefficient of variation (CV) is the lowest for EU and APAC markets
    # for the consumer segment. 
    # We consider the lower CV values as they are more forecastable as higher %age of 
    # data lies closer to the mean
    
    
  # Considernig Consumer segment for APAC and EU as the leading markets based on profit values.
  # === === === === === === === === === === === === === === === === === === === === === === ===
  
    
  # filter the data for the APAC and EU from the global_mart
    global_mart_top <-
      global_mart[which(global_mart$Market %in% c('EU', 'APAC') &
                          global_mart$Segment == 'Consumer'), ]
  
  # Getting Sales and Demand Data for EU Consumer
    Sales_Demand_EU_Consumer <-
      aggregate(
        cbind(Sales, Quantity) ~ Market + Segment + Year_Month,
        data = global_mart_top[which(global_mart_top$Market == 'EU' &
                                       global_mart_top$Segment == 'Consumer'), ],
        FUN = sum
      )
  
  # Getting Sales and Demand Data for APAC Consumer
    Sales_Demand_APAC_Consumer <-
      aggregate(
        cbind(Sales, Quantity) ~ Market + Segment + Year_Month,
        data = global_mart_top[which(global_mart_top$Market == 'APAC' &
                                       global_mart_top$Segment == 'Consumer'), ],
        FUN = sum
      )
  
 
  # Preparing the indata and outdata for the data frame for Both EU and APAC consumer
    indata_Sales_Demand_EU_Consumer <-
        Sales_Demand_EU_Consumer[c(1:42), ]
    outdata_Sales_Demand_EU_Consumer <-
        Sales_Demand_EU_Consumer[c(43:48), ]
    indata_Sales_Demand_APAC_Consumer <-
        Sales_Demand_APAC_Consumer[c(1:42), ]
    outdata_Sales_Demand_APAC_Consumer <-
        Sales_Demand_APAC_Consumer[c(43:48), ]
  

##################################################################################
#                           Time Series Analysis
##################################################################################
# ----------------------------------------------------------------------

  ## Plot the time series for both the Market Regions.
    
  # Time series for EU market sales.
  ts_indata_Sales_EU_Consumer <-
    ts(
      indata_Sales_Demand_EU_Consumer$Sales,
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  plot(ts_indata_Sales_EU_Consumer)
  
  # Time series for EU market demand.
  ts_indata_Demand_EU_Consumer <-
    ts(
      indata_Sales_Demand_EU_Consumer$Quantity,
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  plot(ts_indata_Demand_EU_Consumer)
  
  # Time series for APAC market sales.
  ts_indata_Sales_APAC_Consumer <-
    ts(
      indata_Sales_Demand_APAC_Consumer$Sales,
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  plot(ts_indata_Sales_APAC_Consumer)
  
  # Time series for APAC market demand
  ts_indata_Demand_APAC_Consumer <-
    ts(
      indata_Sales_Demand_APAC_Consumer$Quantity,
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  plot(ts_indata_Demand_APAC_Consumer)
  
  ## Identify if the model is Additive or Multiplicative
  
  ## EU_Sales- Checking using the stl command how the various components are placed.-
    plot(stl(ts_indata_Sales_EU_Consumer, s.window = "periodic"))
    ## Additive model fits well for the given time series.
    ## Various components of the time series Trend, Seasonality and noise is present in the time series.
  
  ### EU_Demand - Checking using the stl command how the various components are placed.-
    plot(stl(ts_indata_Demand_EU_Consumer, s.window = "periodic"))
    ## Additive model fits well for the given time series. As the sesonal component show the additive behaviour
    ## Various components of the time series Trend, Seasonality and noise is present in the time series.
  
  ##  APAC_Sales - Checking using the stl command how the various components are placed.-
    plot(stl(ts_indata_Sales_APAC_Consumer, s.window = "periodic"))
    ## Additive model fits well for the given time series. As the sesonal component show the additive behaviour
    ## Various components of the time series Trend, Seasonality and noise is present in the time series.
  
  ##  APAC_Demand - Checking using the stl command how the various components are placed.-
    plot(stl(ts_indata_Demand_APAC_Consumer, s.window = "periodic"))
    ## Additive model fits well for the given time series. As the sesonal component show the additive behaviour
    ## Various components of the time series Trend, Seasonality and noise is present in the time series.
  
  ## Function to Smoothening of the time series
  
    smoothedseries <- function(time_series, width) {
    smooth_series <-
      stats::filter(
        time_series,
        filter = rep(1 / (2 * width + 1), (2 * width + 1)),
        method = "convolution",
        sides = 2
      )
    
    # Last Value of time series is containing NA. So replacing the NA by averaging out
    second_last_difference <-
      smooth_series[length(smooth_series) - 1] - smooth_series[length(smooth_series) - 2]
    smooth_series[length(smooth_series)] <-
      smooth_series[length(smooth_series) - 1] + second_last_difference
    return(smooth_series)
  }
  

##################################################################################
#                     Model Building - EU CONSUMER SALES
##################################################################################
# ----------------------------------------------------------------------


    ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-### 
    #       MODEL 1 - Classical Decomposition for the Model Building + 
    #                 Time Series Regression + MA Smoothening
    ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
      ## STEP 1. Smoothen and Decompose the series to check the various components, 
      
      # Time Series smoothing : ts_indata_Sales_EU_Consumer
      plot(ts_indata_Sales_EU_Consumer)
      title("smooth time series : ts_indata_Sales_EU_Consumer")
      lines(smoothedseries(ts_indata_Sales_EU_Consumer, 0.2),
            col = "blue",
            lwd = 2)
      lines(smoothedseries(ts_indata_Sales_EU_Consumer, 0.5),
            col = "red",
            lwd = 2)
      lines(smoothedseries(ts_indata_Sales_EU_Consumer, 0.8),
            col = "green",
            lwd = 2)
      lines(smoothedseries(ts_indata_Sales_EU_Consumer, 2),
            col = "yellow",
            lwd = 2)
      legend(
        "bottomright",
        col = c("blue", "red", "green", "yellow", "black"),
        lwd = c(1, 1, 1, 1),
        c("width=0.2", "width=0.5" , "width=0.8","width=2", "orignal"),
        cex = 0.25,
        ncol = 2
      )
      
      ## checking the smoothening values we can see 0.5 is the ideal value to be used.
      
      smooth_ts_indata_Sales_EU_Consumer <-
        smoothedseries(ts_indata_Sales_EU_Consumer, 0.5)
      
      plot(decompose(smooth_ts_indata_Sales_EU_Consumer))
      
      ## conclusion : clearly the value for the Tread, seasonality and noise
      # can be seen in the given time series.
      
      ## STEP 2. Build the model with treand and seasonality.
      
      EU_Sales_lmfit <-
        tslm(smooth_ts_indata_Sales_EU_Consumer ~ trend + season, data = smooth_ts_indata_Sales_EU_Consumer)
      
      ## STEP 3: Predict the global values for the time series using the above model.
      
      EU_Sales_lmfit_Predict <-
        ts(
          predict(EU_Sales_lmfit),
          start = c(2011, 1),
          end = c(2014, 6),
          frequency = 12
        )
      
      plot(ts_indata_Sales_EU_Consumer)
      lines(smooth_ts_indata_Sales_EU_Consumer,
            col = "blue",
            lwd = 2)
      lines(EU_Sales_lmfit_Predict,
            col = "red",
            lwd = 2)
      legend(
        "bottomright",
        col = c("blue", "red", "black"),
        lwd = c(1, 1, 1, 1),
        c("smoothSeries", "predicted", "orignal"),
        cex = 0.4,
        ncol = 2
      )
      
      ## Generated model fits closely to the actual series.
      
      ## Step 4: Remove the Trend and Seasonality component from the series.
      ## get the residual series.
      
      EU_Sales_lmfit_Residue <-
        ts_indata_Sales_EU_Consumer - EU_Sales_lmfit_Predict
      
      ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
      
      acf(EU_Sales_lmfit_Residue)
      acf(EU_Sales_lmfit_Residue, type = "partial")
      # As all the values in the acf and pacf values are within the margins the residual series is stationary.
      
      ## Step 6: Run the auto arima on the residual series.
      
      EU_Sales_armafit <- auto.arima(EU_Sales_lmfit_Residue)
      
      #tsdiag(EU_Sales_armafit)
      EU_Sales_armafit
      resi <- EU_Sales_lmfit_Residue - fitted(EU_Sales_armafit)
      
      ## Step 7: Check the stationarity of the series.
      adf.test(resi, alternative = "stationary")
      kpss.test(resi)
      # Both hypothesis testing results are in line to show that the residual is stationary
      
      # Step 8: Check the MAPE value for the series.
      Sales_forcast_EU <- forecast(EU_Sales_lmfit, h = 6)
      plot(ts(
        Sales_Demand_EU_Consumer$Sales,
        start = c(2011, 1),
        end = c(2014, 12),
        frequency = 12
      ))
      lines(Sales_forcast_EU$mean, col = "blue", lwd = 2)
      
      
      mape <- accuracy(Sales_forcast_EU$mean, outdata_Sales_Demand_EU_Consumer[, 4])[5]
      mape #22.02904
  
  
    ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
    ##            MODEL 2 - EXPONENTIAL SMOOTHING TECHNIQUE
    ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
    ## STEP 1. Soomthen  and Decompose the series to check the various components,
    
    Smoothn_EU_Sales_exp <-
      HoltWinters(
        ts_indata_Sales_EU_Consumer,
        alpha = 0.7,
        beta = FALSE,
        gamma = FALSE
      )
      
    # After evaluation of different values for alpha we identified the optimal value of 0.7
    
    # Time Series smoothing : ts_indata_Sales_EU_Consumer
    plot(ts_indata_Sales_EU_Consumer)
    title("smooth time series : ts_indata_Sales_EU_Consumer")
    lines(fitted(Smoothn_EU_Sales_exp)[, 1],
          col = "blue",
          lwd = 2)
    legend(
      "bottomright",
      col = c("blue", "black"),
      lwd = c(1, 1, 1, 1),
      c("width=0.5", "orignal"),
      cex = 0.5,
      ncol = 2
    )
    
    Smoothn_EU_Sales_exp_ts <-
      fitted(Smoothn_EU_Sales_exp)[, 1]
    plot(decompose(Smoothn_EU_Sales_exp_ts))
    
    ## conclusion : clearly the value for the Tread, seasonality and noise
    # can be seen in the given time series.
    
    ## STEP 2. Build the model with trend and seasonality.
    
    EU_Sales_lmfit_exp <-
      tslm(Smoothn_EU_Sales_exp_ts ~ trend + season, data = Smoothn_EU_Sales_exp_ts)
    
    ## STEP 3: Predict the global values for the time series using the above model.
    
    EU_Sales_exp_predict_ts <-
      ts(
        predict(EU_Sales_lmfit_exp),
        start = c(2011, 1),
        end = c(2014, 6),
        frequency = 12
      )
    
    plot(ts_indata_Sales_EU_Consumer)
    lines(Smoothn_EU_Sales_exp_ts,
          col = "blue",
          lwd = 2)
    lines(EU_Sales_exp_predict_ts,
          col = "red",
          lwd = 2)
    legend(
      "bottomright",
      col = c("blue", "red", "black"),
      lwd = c(1, 1, 1, 1),
      c("smoothSeries", "predicted", "orignal"),
      cex = 0.4,
      ncol = 2
    )
    
    ## Generated model fits closely to the actual series.
    
    ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
    ## get the residual series.
    
    residue_EU_Sales_exp <-
      ts_indata_Sales_EU_Consumer - EU_Sales_exp_predict_ts
    
    ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
    
    acf(residue_EU_Sales_exp)
    acf(residue_EU_Sales_exp,type="partial")
    
    # As all the values in the acf and pacf values are within the margins the residual series is stationary.
    
    ## Step 6: Run the auto arima on the residual series.
    
    armafit <- auto.arima(residue_EU_Sales_exp)
    armafit
    
    resi <- residue_EU_Sales_exp - fitted(armafit)
    
    ## Step 7: Check the stationarity of the series.
    adf.test(resi, alternative = "stationary")
    kpss.test(resi)
    # Both hypothesis testing results are in line to show that the residual is stationary
    
    # Step 8: Check the MAPE value for the series.
    Sales_forcast_EU_exp <- forecast(EU_Sales_lmfit_exp, h = 6)
    
    plot(ts(
      Sales_Demand_EU_Consumer$Sales,
      start = c(2011, 1),
      end = c(2014, 12),
      frequency = 12
    ))
    lines(Sales_forcast_EU_exp$mean, col = "blue", lwd = 2)
    
    
    mape_exp <- accuracy(Sales_forcast_EU_exp$mean, outdata_Sales_Demand_EU_Consumer[, 4])[5]
    mape_exp #34.52873
  
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##      EXPONENTIAL SMOOTHING TECHNIQUE + Polynomial and Sinosoidal 
  ##      model in regression Additional modeling was done to evaluate
  ##      the MAPE value
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
    ## STEP 1. Smoothen and Decompose the series to check the various components,
    ##         using variables from the previous series 
    ## STEP 2. Build the model with treand and seasonality.
    
    ## Convert the into data frame the time series.
    
    EU_Sales_df <-
      as.data.frame(cbind(c(1:42), as.vector(
        fitted(Smoothn_EU_Sales_exp)[, 1]
      )))
    colnames(EU_Sales_df) <- c('Month', 'Sales')
    
    EU_Sales_lm_exp <-
      lm(Sales ~ sin(0.5 * Month) * poly(Month, 6) + Month, data = EU_Sales_df)
    
    ## STEP 3: Predict the global values for the time series using the above model.
    
    EU_Sales_lm_exp_predict <-
      ts(
        predict(EU_Sales_lm_exp),
        start = c(2011, 1),
        end = c(2014, 6),
        frequency = 12
      )
    
    plot(ts_indata_Sales_EU_Consumer)
    
    lines(Smoothn_EU_Sales_exp_ts,
          col = "blue",
          lwd = 2)
    lines(EU_Sales_lm_exp_predict,
          col = "red",
          lwd = 2)
    legend(
      "bottomright",
      col = c("blue", "red", "black"),
      lwd = c(1, 1, 1, 1),
      c("smoothSeries", "predicted", "orignal"),
      cex = 0.4,
      ncol = 2
    )
    
    ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
    ## get the residual series.
    
    EU_Sales_exp_lm_residue <-
      ts_indata_Sales_EU_Consumer - EU_Sales_lm_exp_predict
    
    ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
    
    acf(EU_Sales_exp_lm_residue)
    acf(EU_Sales_exp_lm_residue, type = "partial")
    # As all the values in the acf and pacf values are within the margins the residual series is stationary.
    
    ## Step 6: Run the auto arima on the residual series.
    
    armafit <- auto.arima(EU_Sales_exp_lm_residue)
    armafit
    
    resi <- EU_Sales_exp_lm_residue - fitted(armafit)
    
    ## Step 7: Check the stationarity of the series.
    
    adf.test(resi, alternative = "stationary")
    kpss.test(resi)
    # Both hypothesis testing results are in line to show that the residual is stationary
    
    # Step 8: Check the MAPE value for the series.
    
    Sales_forcast_EU <- predict(EU_Sales_lm_exp, n.ahead = 6)
    mape <- accuracy(Sales_forcast_EU, outdata_Sales_Demand_EU_Consumer[, 4])[5]
    mape #77.83339
    
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##                          MODEL 4-  AUTO ARIMA
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
    
    # ARIMA model
    autoarima_in_Sales_EU_Consumer <-
      auto.arima(ts_indata_Sales_EU_Consumer, allowdrift = FALSE)
    autoarima_in_Sales_EU_Consumer
    
    fcast_auto_arima_Sales_EU_Consumer <-
      forecast(autoarima_in_Sales_EU_Consumer, h = 6)
    
    plot(ts(
      Sales_Demand_EU_Consumer$Sales,
      start = c(2011, 1),
      end = c(2014, 12),
      frequency = 12
    ))
    lines(fcast_auto_arima_Sales_EU_Consumer$mean,
          col = "blue",
          lwd = 2)
    
    autoplot(fcast_auto_arima_Sales_EU_Consumer)
    
    MAPE_auto_arima_Sales_EU_Consumer <-
      accuracy(fcast_auto_arima_Sales_EU_Consumer$mean, outdata_Sales_Demand_EU_Consumer[, 4])[5]
    
    MAPE_auto_arima_Sales_EU_Consumer   #19.23495
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ## Compare the MAPE Value to find the best model to be used for forecasting.
  
  # mape for Moving Average 22.02904
  # mape for exponenetial average 34.33288
  # mape for exponenetial average with Liner regression 77.83339
  # mape for auto arima 19.23495
  
  ## Best Mape is the lowest value of the comparison so choosing the Auto Arima
    
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
    
  # Step 9 Forcasting the values using the Auto Arima being the best model for the given data set.
  
  EU_Sales_forecast <- forecast(autoarima_in_Sales_EU_Consumer, h = 6)
  
  plot(ts(
    Sales_Demand_EU_Consumer$Sales,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(EU_Sales_forecast$mean,
        col = "red",
        lwd = 2)
  

##################################################################################
#                     Model Building - EU CONSUMER DEMAND
##################################################################################
# ----------------------------------------------------------------------

  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  #       MODEL 1 - Classical Decomposition for the Model Building + 
  #                 Time Series Regression + MA Smoothening
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###


  ## STEP 1. Smoothen and Decompose the series to check the various components,
  
  # Time Series smoothing : ts_indata_Demand_EU_Consumer
  plot(ts_indata_Demand_EU_Consumer)
  title("smooth time series : ts_indata_Demand_EU_Consumer")
  lines(smoothedseries(ts_indata_Demand_EU_Consumer, 0.2),
        col = "blue",
        lwd = 2)
  lines(smoothedseries(ts_indata_Demand_EU_Consumer, 0.5),
        col = "red",
        lwd = 2)
  lines(smoothedseries(ts_indata_Demand_EU_Consumer, 0.8),
        col = "green",
        lwd = 2)
  lines(smoothedseries(ts_indata_Demand_EU_Consumer, 2),
        col = "yellow",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "green", "yellow", "black"),
    lwd = c(1, 1, 1, 1),
    c("width=0.2", "width=0.5" , "width=0.8","width=2", "orignal"),
    cex = 0.25,
    ncol = 2
  )
  
  ## checking the smoothening values we can see 0.5 is the ideal value to be used.
  
  smooth_ts_indata_Demand_EU_Consumer <- smoothedseries(ts_indata_Demand_EU_Consumer, 0.5)
  plot(decompose(smooth_ts_indata_Demand_EU_Consumer))
  
  ## conclusion : clearly the value for the Tread, seasonality and noise
  # can be seen in the given time series.
  
  ## STEP 2. Build the model with treand and seasonality.
  
  EU_Demand_lmfit <-
    tslm(smooth_ts_indata_Demand_EU_Consumer ~ trend + season, data = smooth_ts_indata_Demand_EU_Consumer)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  EU_Demand_lmfit_Predict <-
    ts(
      predict(EU_Demand_lmfit),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  plot(ts_indata_Demand_EU_Consumer)
  lines(smooth_ts_indata_Demand_EU_Consumer,
        col = "blue",
        lwd = 2)
  lines(EU_Demand_lmfit_Predict,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Generated model fits closely to the actual series.
  
  ## Step 4: Remove the Trend and Seasonality component from the series.
  ## get the residual series.
  
  EU_Demand_lmfit_Residue <-
    ts_indata_Demand_EU_Consumer - EU_Demand_lmfit_Predict
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(EU_Demand_lmfit_Residue)
  acf(EU_Demand_lmfit_Residue, type = "partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  EU_Demand_armafit <- auto.arima(EU_Demand_lmfit_Residue)
  
  #tsdiag(EU_Demand_armafit)
  EU_Demand_armafit
  resi <- EU_Demand_lmfit_Residue - fitted(EU_Demand_armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  Demand_forcast_EU <- forecast(EU_Demand_lmfit, h = 6)
  plot(ts(
    Sales_Demand_EU_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(Demand_forcast_EU$mean, col = "blue", lwd = 2)
  mape <- accuracy(Demand_forcast_EU$mean, outdata_Sales_Demand_EU_Consumer[, 5])[5]
  mape #24.83302
  
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##        MODEL 2 - EXPONENTIAL SMOOTHING TECHNIQUE
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
  ## STEP 1. Smoothen  and Decompose the series to check the various components,
  
  Smoothn_EU_Demand_exp <-
    HoltWinters(
      ts_indata_Demand_EU_Consumer,
      alpha = 0.7,
      beta = FALSE,
      gamma = FALSE
    )
  
  # Time Series smoothing : ts_indata_Demand_EU_Consumer
  plot(ts_indata_Demand_EU_Consumer)
  title("smooth time series : ts_indata_Demand_EU_Consumer")
  lines(fitted(Smoothn_EU_Demand_exp)[, 1],
        col = "blue",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "black"),
    lwd = c(1, 1, 1, 1),
    c("width=0.5", "orignal"),
    cex = 0.5,
    ncol = 2
  )
  
  
  Smoothn_EU_Demand_exp_ts <-
    fitted(Smoothn_EU_Demand_exp)[, 1]
  plot(decompose(Smoothn_EU_Demand_exp_ts))
  
  ## conclusion : clearly the value for the Tread, seasonality and noise
  # can be seen in the given time series.
  
  ## STEP 2. Build the model with treand and seasonality.
  
  EU_Demand_lmfit_exp <- tslm(Smoothn_EU_Demand_exp_ts ~ trend + season, data = Smoothn_EU_Demand_exp_ts)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  EU_Demand_exp_predict_ts <-
    ts(
      predict(EU_Demand_lmfit_exp),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  plot(ts_indata_Demand_EU_Consumer)
  
  lines(Smoothn_EU_Demand_exp_ts,
        col = "blue",
        lwd = 2)
  lines(EU_Demand_exp_predict_ts,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Generated model fits closely to the actual series.
  
  ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
  ## get the residual series.
  
  residue_EU_Demand_exp <- ts_indata_Demand_EU_Consumer - EU_Demand_exp_predict_ts
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(residue_EU_Demand_exp)
  acf(residue_EU_Demand_exp,type="partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  armafit <- auto.arima(residue_EU_Demand_exp)
  armafit
  
  resi <- residue_EU_Demand_exp - fitted(armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  
  Demand_forcast_EU_exp <- forecast(EU_Demand_lmfit_exp, h = 6)
  
  plot(ts(
    Sales_Demand_EU_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(Demand_forcast_EU_exp$mean, col = "blue", lwd = 2)
  mape_exp <- accuracy(Demand_forcast_EU_exp$mean, outdata_Sales_Demand_EU_Consumer[, 5])[5]
  mape_exp #36.57106
  
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##        EXPONENTIAL SMOOTHING TECHNIQUE + Polynomial and Sinosoidal model in regression
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
  ## STEP 1. Smoothen and Decompose the series to check the various components,
  # using variables from the previous series 
  ## STEP 2. Build the model with treand and seasonality.
  
  ## Convert the into data frame the time series.
  
  EU_Demand_df <-
    as.data.frame(cbind(c(1:42), as.vector(
      fitted(Smoothn_EU_Demand_exp)[, 1]
    )))
  colnames(EU_Demand_df) <- c('Month', 'Demand')
  
  
  EU_Demand_lm_exp <-
    lm(Demand ~ sin(0.5 * Month) * poly(Month, 6) + Month, data = EU_Demand_df)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  EU_Demand_lm_exp_predict <-
    ts(
      predict(EU_Demand_lm_exp),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Demand_EU_Consumer)
  lines(Smoothn_EU_Demand_exp_ts,
        col = "blue",
        lwd = 2)
  lines(EU_Demand_lm_exp_predict,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
  ## get the residual series.
  
  EU_Demand_exp_lm_residue <-
    ts_indata_Demand_EU_Consumer - EU_Demand_lm_exp_predict
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(EU_Demand_exp_lm_residue)
  acf(EU_Demand_exp_lm_residue, type = "partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  armafit <- auto.arima(EU_Demand_exp_lm_residue)
  armafit
  resi <- EU_Demand_exp_lm_residue - fitted(armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  
  Demand_forcast_EU <- predict(EU_Demand_lm_exp, n.ahead = 6)
  mape <- accuracy(Demand_forcast_EU, outdata_Sales_Demand_EU_Consumer[, 4])[5]
  mape #99.63567
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##                        MODEL 4-  AUTO ARIMA
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  # ARIMA model
  autoarima_in_Demand_EU_Consumer <-
    auto.arima(ts_indata_Demand_EU_Consumer, allowdrift = FALSE)
  autoarima_in_Demand_EU_Consumer
  
  fcast_auto_arima_Demand_EU_Consumer <-
    forecast(autoarima_in_Demand_EU_Consumer, h = 6)
  
  plot(ts(
    Sales_Demand_EU_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(fcast_auto_arima_Demand_EU_Consumer$mean, col = "blue", lwd = 2)
  
  MAPE_auto_arima_Demand_EU_Consumer <-
    accuracy(fcast_auto_arima_Demand_EU_Consumer$mean,
             outdata_Sales_Demand_EU_Consumer[, 5])[5]
  MAPE_auto_arima_Demand_EU_Consumer   #35.53743
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ## Compare the MAPE Value to find the best model to be used for forecasting.
  
  
  # mape for Moving Average 24.83302
  # mape for exponenetial average 36.57106
  # mape for exponenetial average with Liner regression 99.63567
  # mape for auto arima 35.53743
  
  ## Best Mape is the lowest value of the comparison so choosing the Moving Average
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  # Step 9 Forcasting the values using the MOving Average being the best model for the given data set.
  
  EU_Demand_forecast <- forecast(EU_Demand_lmfit, h = 6)
  
  plot(ts(
    Sales_Demand_EU_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(EU_Demand_forecast$mean,
        col = "red",
        lwd = 2)
  


##################################################################################
#                     Model Building - APAC CONSUMER SALES - Yet to start
##################################################################################
# ----------------------------------------------------------------------

  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  #       MODEL 1 - Classical Decomposition for the Model Building + 
  #                 Time Series Regression + MA Smoothening
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###


  ## STEP 1. Smoothen and Decompose the series to check the various components,
  
  # Time Series smoothing : ts_indata_Sales_APAC_Consumer
  plot(ts_indata_Sales_APAC_Consumer)
  title("smooth time series : ts_indata_Sales_APAC_Consumer")
  lines(smoothedseries(ts_indata_Sales_APAC_Consumer, 0.2),
        col = "blue",
        lwd = 2)
  lines(smoothedseries(ts_indata_Sales_APAC_Consumer, 0.5),
        col = "red",
        lwd = 2)
  lines(smoothedseries(ts_indata_Sales_APAC_Consumer, 0.8),
        col = "green",
        lwd = 2)
  lines(smoothedseries(ts_indata_Sales_APAC_Consumer, 2),
        col = "yellow",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "green", "yellow", "black"),
    lwd = c(1, 1, 1, 1),
    c("width=0.2", "width=0.5" , "width=0.8","width=2", "orignal"),
    cex = 0.3,
    ncol = 2
  )
  
  ## checking the smoothening values we can see 0.5 is the ideal value to be used.
  
  smooth_ts_indata_Sales_APAC_Consumer <-
    smoothedseries(ts_indata_Sales_APAC_Consumer, 0.5)
  
  plot(decompose(smooth_ts_indata_Sales_APAC_Consumer))
  
  ## conclusion : clearly the value for the Tread, seasonality and noise
  # can be seen in the given time series.
  
  ## STEP 2. Build the model with treand and seasonality.
  
  APAC_Sales_lmfit <-
    tslm(smooth_ts_indata_Sales_APAC_Consumer ~ trend + season, data = smooth_ts_indata_Sales_APAC_Consumer)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  APAC_Sales_lmfit_Predict <-
    ts(
      predict(APAC_Sales_lmfit),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Sales_APAC_Consumer)
  lines(smooth_ts_indata_Sales_APAC_Consumer,
        col = "blue",
        lwd = 2)
  lines(APAC_Sales_lmfit_Predict,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Generated model fits closely to the actual series.
  
  ## Step 4: Remove the Trend and Seasonality component from the series.
  ## get the residual series.
  
  APAC_Sales_lmfit_Residue <-
    ts_indata_Sales_APAC_Consumer - APAC_Sales_lmfit_Predict
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(APAC_Sales_lmfit_Residue)
  acf(APAC_Sales_lmfit_Residue, type = "partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  APAC_Sales_armafit <- auto.arima(APAC_Sales_lmfit_Residue)
  
  #tsdiag(APAC_Sales_armafit)
  APAC_Sales_armafit
  resi <- APAC_Sales_lmfit_Residue - fitted(APAC_Sales_armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  Sales_forcast_APAC <- forecast(APAC_Sales_lmfit, h = 6)
  plot(ts(
    Sales_Demand_APAC_Consumer$Sales,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(Sales_forcast_APAC$mean, col = "blue", lwd = 2)
  
  mape <-
    accuracy(Sales_forcast_APAC$mean, outdata_Sales_Demand_APAC_Consumer[, 4])[5]
  mape #20.36833


  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##        MODEL 2 - EXPONENTIAL SMOOTHING TECHNIQUE
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
  ## STEP 1. Soomthen  and Decompose the series to check the various components,
  
  Smoothn_APAC_Sales_exp <-
    HoltWinters(
      ts_indata_Sales_APAC_Consumer,
      alpha = 0.7,
      beta = FALSE,
      gamma = FALSE
    )
  
  # Time Series smoothing : ts_indata_Sales_APAC_Consumer
  plot(ts_indata_Sales_APAC_Consumer)
  title("smooth time series : ts_indata_Sales_APAC_Consumer")
  lines(fitted(Smoothn_APAC_Sales_exp)[, 1],
        col = "blue",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "black"),
    lwd = c(1, 1, 1, 1),
    c("width=0.5", "orignal"),
    cex = 0.5,
    ncol = 2
  )
  
  
  Smoothn_APAC_Sales_exp_ts <-
    fitted(Smoothn_APAC_Sales_exp)[, 1]
  plot(decompose(Smoothn_APAC_Sales_exp_ts))
  
  ## conclusion : clearly the value for the Tread, seasonality and noise
  # can be seen in the given time series.
  
  ## STEP 2. Build the model with treand and seasonality.
  
  APAC_Sales_lmfit_exp <-
    tslm(Smoothn_APAC_Sales_exp_ts ~ trend + season, data = Smoothn_APAC_Sales_exp_ts)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  APAC_Sales_exp_predict_ts <-
    ts(
      predict(APAC_Sales_lmfit_exp),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Sales_APAC_Consumer)
  lines(Smoothn_APAC_Sales_exp_ts,
        col = "blue",
        lwd = 2)
  lines(APAC_Sales_exp_predict_ts,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Generated model fits closely to the actual series.
  
  ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
  ## get the residual series.
  
  residue_APAC_Sales_exp <-
    ts_indata_Sales_APAC_Consumer - APAC_Sales_exp_predict_ts
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(residue_APAC_Sales_exp)
  acf(residue_APAC_Sales_exp,type="partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  armafit <- auto.arima(residue_APAC_Sales_exp)
  armafit
  resi <- residue_APAC_Sales_exp - fitted(armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  
  Sales_forcast_APAC_exp <- forecast(APAC_Sales_lmfit_exp, h = 6)
  plot(ts(
    Sales_Demand_APAC_Consumer$Sales,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(Sales_forcast_APAC_exp$mean, col = "blue", lwd = 2)
  
  mape_exp <-
    accuracy(Sales_forcast_APAC_exp$mean, outdata_Sales_Demand_APAC_Consumer[, 4])[5]
  mape_exp #27.4543
  
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##        EXPONENTIAL SMOOTHING TECHNIQUE + Polynomial and Sinosoidal model in regression
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
  ## STEP 1. Smoothen and Decompose the series to check the various components,
  # using variables from the previous series 
  ## STEP 2. Build the model with treand and seasonality.
  
  ## Convert the into data frame the time series.
  
  APAC_Sales_df <-
    as.data.frame(cbind(c(1:42), as.vector(
      fitted(Smoothn_APAC_Sales_exp)[, 1]
    )))
  colnames(APAC_Sales_df) <- c('Month', 'Sales')
  
  APAC_Sales_lm_exp <-
    lm(Sales ~ sin(0.5 * Month) * poly(Month, 6) + Month, data = APAC_Sales_df)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  APAC_Sales_lm_exp_predict <-
    ts(
      predict(APAC_Sales_lm_exp),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Sales_APAC_Consumer)
  lines(Smoothn_APAC_Sales_exp_ts,
        col = "blue",
        lwd = 2)
  lines(APAC_Sales_lm_exp_predict,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
  ## get the residual series.
  
  APAC_Sales_exp_lm_residue <-
    ts_indata_Sales_APAC_Consumer - APAC_Sales_lm_exp_predict
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(APAC_Sales_exp_lm_residue)
  
  acf(APAC_Sales_exp_lm_residue, type = "partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  armafit <- auto.arima(APAC_Sales_exp_lm_residue)
  armafit
  resi <- APAC_Sales_exp_lm_residue - fitted(armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  
  Sales_forcast_APAC <- predict(APAC_Sales_lm_exp, n.ahead = 6)
  
  mape <- accuracy(Sales_forcast_APAC, outdata_Sales_Demand_APAC_Consumer[, 4])[5]
  mape # 69.11246
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##                                  MODEL 4-  AUTO ARIMA
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  # ARIMA model
  autoarima_in_Sales_APAC_Consumer <-
    auto.arima(ts_indata_Sales_APAC_Consumer, allowdrift = FALSE)
  autoarima_in_Sales_APAC_Consumer
  
  fcast_auto_arima_Sales_APAC_Consumer <-
    forecast(autoarima_in_Sales_APAC_Consumer, h = 6)
  
  plot(ts(
    Sales_Demand_APAC_Consumer$Sales,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(fcast_auto_arima_Sales_APAC_Consumer$mean,
        col = "blue",
        lwd = 2)
  
  MAPE_auto_arima_Sales_APAC_Consumer <-
    accuracy(fcast_auto_arima_Sales_APAC_Consumer$mean,
             outdata_Sales_Demand_APAC_Consumer[, 4])[5]
  MAPE_auto_arima_Sales_APAC_Consumer   #24.30024
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ## Compare the MAPE Value to find the best model to be used for forecasting.
  
  
  # mape for Moving Average 20.36833
  # mape for exponenetial average 27.4543
  # mape for exponenetial average with Liner regression 69.11246
  # mape for auto arima 24.30024
  
  ## Best Mape is the lowest value of the comparison so choosing the Moving Average
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  # Step 9 Forcasting the values using the MOving Average being the best model for the given data set.
  
  APAC_Sales_forecast <- forecast(APAC_Sales_lmfit, h = 6)
  
  plot(ts(
    Sales_Demand_APAC_Consumer$Sales,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(APAC_Sales_forecast$mean,
        col = "red",
        lwd = 2)


##################################################################################
#                     Model Building - APAC CONSUMER DEMAND
##################################################################################
# ----------------------------------------------------------------------

  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  #       MODEL 1 - Classical Decomposition for the Model Building + 
  #                 Time Series Regression + MA Smoothening
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###



  ## STEP 1. Smoothen and Decompose the series to check the various components,
  
  # Time Series smoothing : ts_indata_Demand_APAC_Consumer
  plot(ts_indata_Demand_APAC_Consumer)
  title("smooth time series : ts_indata_Demand_APAC_Consumer")
  lines(smoothedseries(ts_indata_Demand_APAC_Consumer, 0.2),
        col = "blue",
        lwd = 2)
  lines(smoothedseries(ts_indata_Demand_APAC_Consumer, 0.5),
        col = "red",
        lwd = 2)
  lines(smoothedseries(ts_indata_Demand_APAC_Consumer, 0.8),
        col = "green",
        lwd = 2)
  lines(smoothedseries(ts_indata_Demand_APAC_Consumer, 2),
        col = "yellow",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue","red","green","yellow", "black"),
    lwd = c(1, 1, 1, 1),
    c("width=0.2", "width=0.5", "width=0.8","width=2","orignal"),
    cex = 0.3,
    ncol = 2
  )
  
  ## checking the smoothening values we can see 0.5 is the ideal value to be used.
  
  smooth_ts_indata_Demand_APAC_Consumer <-
    smoothedseries(ts_indata_Demand_APAC_Consumer, 0.5)
  
  plot(decompose(smooth_ts_indata_Demand_APAC_Consumer))
  
  ## conclusion : clearly the value for the Tread, seasonality and noise
  # can be seen in the given time series.
  
  ## STEP 2. Build the model with treand and seasonality.
  
  APAC_Demand_lmfit <-
    tslm(smooth_ts_indata_Demand_APAC_Consumer ~ trend + season, data = smooth_ts_indata_Demand_APAC_Consumer)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  APAC_Demand_lmfit_Predict <-
    ts(
      predict(APAC_Demand_lmfit),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Demand_APAC_Consumer)
  lines(smooth_ts_indata_Demand_APAC_Consumer,
        col = "blue",
        lwd = 2)
  lines(APAC_Demand_lmfit_Predict,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Generated model fits closely to the actual series.
  
  ## Step 4: Remove the Trend and Seasonality component from the series.
  ## get the residual series.
  
  APAC_Demand_lmfit_Residue <-
    ts_indata_Demand_APAC_Consumer - APAC_Demand_lmfit_Predict
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(APAC_Demand_lmfit_Residue)
  acf(APAC_Demand_lmfit_Residue, type = "partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  APAC_Demand_armafit <- auto.arima(APAC_Demand_lmfit_Residue)
  APAC_Demand_armafit
  
  resi <- APAC_Demand_lmfit_Residue - fitted(APAC_Demand_armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  Demand_forcast_APAC <- forecast(APAC_Demand_lmfit, h = 6)
  plot(ts(
    Sales_Demand_APAC_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(Demand_forcast_APAC$mean, col = "blue", lwd = 2)
  
  mape <-
    accuracy(Demand_forcast_APAC$mean, outdata_Sales_Demand_APAC_Consumer[, 5])[5]
  mape #19.67828
  
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##                    MODEL 2 - EXPONENTIAL SMOOTHING TECHNIQUE
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
  ## STEP 1. Soomthen  and Decompose the series to check the various components,
  
  Smoothn_APAC_Demand_exp <-
    HoltWinters(
      ts_indata_Demand_APAC_Consumer,
      alpha = 0.7,
      beta = FALSE,
      gamma = FALSE
    )
  
  # Time Series smoothing : ts_indata_Demand_APAC_Consumer
  plot(ts_indata_Demand_APAC_Consumer)
  title("smooth time series : ts_indata_Demand_APAC_Consumer")
  lines(fitted(Smoothn_APAC_Demand_exp)[, 1],
        col = "blue",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "black"),
    lwd = c(1, 1, 1, 1),
    c("width=0.5", "orignal"),
    cex = 0.5,
    ncol = 2
  )
  
  
  Smoothn_APAC_Demand_exp_ts <-
    fitted(Smoothn_APAC_Demand_exp)[, 1]
  plot(decompose(Smoothn_APAC_Demand_exp_ts))
  
  ## conclusion : clearly the value for the Tread, seasonality and noise
  # can be seen in the given time series.
  
  ## STEP 2. Build the model with treand and seasonality.
  
  APAC_Demand_lmfit_exp <-
    tslm(Smoothn_APAC_Demand_exp_ts ~ trend + season, data = Smoothn_APAC_Demand_exp_ts)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  APAC_Demand_exp_predict_ts <-
    ts(
      predict(APAC_Demand_lmfit_exp),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Demand_APAC_Consumer)
  lines(Smoothn_APAC_Demand_exp_ts,
        col = "blue",
        lwd = 2)
  lines(APAC_Demand_exp_predict_ts,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Generated model fits closely to the actual series.
  
  ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
  ## get the residual series.
  
  residue_APAC_Demand_exp <-
    ts_indata_Demand_APAC_Consumer - APAC_Demand_exp_predict_ts
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(residue_APAC_Demand_exp)
  acf(residue_APAC_Demand_exp,type="partial")
  
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  armafit <- auto.arima(residue_APAC_Demand_exp)
  armafit
  
  resi <- residue_APAC_Demand_exp - fitted(armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  
  Demand_forcast_APAC_exp <- forecast(APAC_Demand_lmfit_exp, h = 6)
  plot(ts(
    Sales_Demand_APAC_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12))
  lines(Demand_forcast_APAC_exp$mean, col = "blue", lwd = 2)
  mape_exp <-
    accuracy(Demand_forcast_APAC_exp$mean, outdata_Sales_Demand_APAC_Consumer[, 5])[5]
  mape_exp #30.40038
  
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##        EXPONENTIAL SMOOTHING TECHNIQUE + Polynomial and Sinosoidal model in regression
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  
  ## STEP 1. Smoothen and Decompose the series to check the various components,
  # using variables from the previous series 
  ## STEP 2. Build the model with treand and seasonality.
  
  ## Convert the into data frame the time series.
  
  APAC_Demand_df <-
    as.data.frame(cbind(c(1:42), as.vector(
      fitted(Smoothn_APAC_Demand_exp)[, 1]
    )))
  colnames(APAC_Demand_df) <- c('Month', 'Demand')
  
  
  APAC_Demand_lm_exp <-
    lm(Demand ~ sin(0.5 * Month) * poly(Month, 6) + Month, data = APAC_Demand_df)
  
  ## STEP 3: Predict the global values for the time series using the above model.
  
  APAC_Demand_lm_exp_predict <-
    ts(
      predict(APAC_Demand_lm_exp),
      start = c(2011, 1),
      end = c(2014, 6),
      frequency = 12
    )
  
  plot(ts_indata_Demand_APAC_Consumer)
  lines(Smoothn_APAC_Demand_exp_ts,
        col = "blue",
        lwd = 2)
  lines(APAC_Demand_lm_exp_predict,
        col = "red",
        lwd = 2)
  legend(
    "bottomright",
    col = c("blue", "red", "black"),
    lwd = c(1, 1, 1, 1),
    c("smoothSeries", "predicted", "orignal"),
    cex = 0.4,
    ncol = 2
  )
  
  ## Step 4: Remove the Trend and Seasonality component from the smoothened series.
  ## get the residual series.
  
  APAC_Demand_exp_lm_residue <-
    ts_indata_Demand_APAC_Consumer - APAC_Demand_lm_exp_predict
  
  ## Step 5: Check the Stationarity of the residual Series. using acf and pacf test.
  
  acf(APAC_Demand_exp_lm_residue)
  acf(APAC_Demand_exp_lm_residue, type = "partial")
  # As all the values in the acf and pacf values are within the margins the residual series is stationary.
  
  ## Step 6: Run the auto arima on the residual series.
  
  armafit <- auto.arima(APAC_Demand_exp_lm_residue)
  armafit
  
  resi <- APAC_Demand_exp_lm_residue - fitted(armafit)
  
  ## Step 7: Check the stationarity of the series.
  adf.test(resi, alternative = "stationary")
  kpss.test(resi)
  # Both hypothesis testing results are in line to show that the residual is stationary
  
  # Step 8: Check the MAPE value for the series.
  
  Demand_forcast_APAC <- predict(APAC_Demand_lm_exp, n.ahead = 6)
  
  mape <-
    accuracy(Demand_forcast_APAC, outdata_Sales_Demand_APAC_Consumer[, 4])[5]
  mape #99.61992
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ##                                      MODEL 4-  AUTO ARIMA
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  # ARIMA model
  autoarima_in_Demand_APAC_Consumer <-
    auto.arima(ts_indata_Demand_APAC_Consumer, allowdrift = FALSE)
  autoarima_in_Demand_APAC_Consumer
  
  fcast_auto_arima_Demand_APAC_Consumer <-
    forecast(autoarima_in_Demand_APAC_Consumer, h = 6)
  
  plot(ts(
    Sales_Demand_APAC_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12))
  lines(fcast_auto_arima_Demand_APAC_Consumer$mean, col = "blue", lwd = 2)
  
  MAPE_auto_arima_Demand_APAC_Consumer <-
    accuracy(fcast_auto_arima_Demand_APAC_Consumer$mean,
             outdata_Sales_Demand_APAC_Consumer[, 5])[5]
  MAPE_auto_arima_Demand_APAC_Consumer   #22.15233
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  ## Compare the MAPE Value to find the best model to be used for forecasting.
  
  
  # mape for Moving Average 19.67828
  # mape for exponenetial average 30.40038
  # mape for exponenetial average with Liner regression 99.61992
  # mape for auto arima 22.15233
  
  ## Best Mape is the lowest value of the comparison so choosing the Moving Average
  
  ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###
  
  # Step 9 Forcasting the values using the MOving Average being the best model for the given data set.
  
  APAC_Demand_forecast <- forecast(APAC_Demand_lmfit, h = 6)
  
  plot(ts(
    Sales_Demand_APAC_Consumer$Quantity,
    start = c(2011, 1),
    end = c(2014, 12),
    frequency = 12
  ))
  lines(APAC_Demand_forecast$mean,
        col = "red",
        lwd = 2)
#Based on the Model chosen for the forecasts below is the approximate forecasted Sales & Demand for EU & APAC.
  # EU Consumer		
  # Month	Sales	Demand
  # Jul	~ 32,580	~ 554
  # Aug	~ 38,294	~ 711
  # Sep	~ 39,908	~ 517
  # Oct	~ 27,094	~ 500
  # Nov	~ 48,598	~ 647
  # Dec	~ 42,853	~ 566
  
  # APAC Consumer		
  # Month	Sales	Demand
  # Jul	~ 45,602	~ 538
  # Aug	~ 54,851	~ 660
  # Sep	~ 60,010	~ 680
  # Oct	~ 63,874	~ 730
  # Nov	~ 62,248	~ 745
  # Dec	~ 56,086	~ 622
  




