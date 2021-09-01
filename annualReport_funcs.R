library(tidyverse)

# adapted from NADA package
censtats <- function(object){
  x = object
  s = x@survfit
  summaryVec = function(x) {
    s = x@survfit
    n = s$n
    cen = s$n - sum(s$n.event)
    median = median(x)
    mean = mean(x)[1]
    sd = sd(x)
    return(c(n, cen, median, mean, sd))
  }
  ret = NULL
  tag = c("n", "n.cen", "median", "mean", 
          "sd")
  if (is.null(s$strata)) {
    ret = summaryVec(x)
    names(ret) = tag
  }
  else {
    ret = summaryVec(x[1])
    for (i in 2:length(s$strata)) {
      ret = rbind(ret, summaryVec(x[i]))
    }
    colnames(ret) = tag
    rownames(ret) = names(s$strata)
  }

    df_ret = as.data.frame(ret)
  df_ret <- cbind(region = rownames(df_ret), df_ret)
  rownames(df_ret) <- 1:nrow(df_ret)

  df_ret <- df_ret %>%
    separate(Region, c('region', 'date'), ' - ') %>%
    separate(Region, c('rm', 'region'), '=')
  
  df_ret <- subset(df_ret, select = -c(rm))
  
  return(df_ret)
}

blank_theme <- theme_bw() + theme(#panel.border = element_blank(),
  # panel.grid.major = element_blank(),
  panel.grid.major.x = element_blank() ,
  panel.grid.minor = element_blank(),
  axis.line = element_blank(),#line(color = 'black'),
  axis.text = element_text(color='black', size=9, family='sans'),
  axis.text.x = element_text(angle = 90, vjust = 0.5, margin = margin(t = 1)),
  axis.title.x = element_blank(),#text(size = 14, family = 'sans'),
  axis.title.y = element_blank(),#text(size = 14, family = 'sans'),
  plot.title = element_text(size=12, hjust=0.5),
  legend.position = 'top',#c(0.5,0.94),
  # legend.margin = c(0,0,0,0),
  # legend.background = element_rect(color = 'black'),
  legend.title = element_blank(),
  # legend.margin = margin(0,0,0,0),
  legend.box.margin=margin(-10,-10,-10,-10),
  legend.text = element_text(size=10))