# ---- Functions ----

residual_return <- function(data, market, info=NULL, window=NULL) {

    # Make sure that window is a sequence
    if (length(window) == 2)
        window <- window[1]:window[2]


    # Use all observation if window is NULL
    if (is.null(window))
        window <- 1:dim(data)[2]


    # Create residual matrix
    res <- array(dim=dim(data))

    for (mk in names(market)) {
        for (i in which(info == mk)) {
            lmfit <- lm(data[i, ] ~ market[[mk]])
            res[i, window] <- lmfit$residuals
        }
    }

    res
}


# ----

# Create residual matrix
#   - rows are each stock
#   - columns are each time point
res <- residual_return(data = logret,
                       market = list(kospi=kospi, kosdaq=kosdaq),
                       info = stock_info$market,
                       window = NULL)
