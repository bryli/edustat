library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
#library(ggiraphExtra)

fulldata <- fread('md_report_card.csv', na.strings="--")
names(fulldata) <- replace(names(fulldata), 1, 'School Name')
fulldata <- cbind(fulldata %>% select('School Name', 'School County', 'School Type'),
                  fulldata %>% select(-c('School Name', 'School County', 'School Type')) %>%
                    mutate_all(funs(as.character)) %>%
                    mutate_all(funs(parse_number)))
slctdata <- fulldata %>%
  select(NAME='School Name', CNTY='School County', TYPE='School Type',
         PTS='Points %', FARMS='FARMS Rate', STD_FARMS='FARMS Standardized', ASN_INDX='Asian Index', HSP_INDX="hispanic index", SEGR_INDX='Segregation Index', STD_SEGR='Standardized')
typesp.data <- setNames(slctdata %>% group_split(TYPE, keep=FALSE), slctdata %>% group_keys(TYPE) %>% pull(1))
get_models <- function(xval, yval, data=typesp.data){
  typesp.models <<- lapply(data, function(typed){lm(paste(yval, "~", xval), data=typed)})
}
get_models("SEGR_INDX", "PTS")

get_mult_models <- function(xvals, yval, data=typesp.data){
  typesp.models <<- lapply(data, function(typed){lm(paste0(yval, "~", paste(xvals, collapse="*")), data=typed)})
}

parsemodel <- function(level, xval, yval, tdata=typesp.data, tmodels=typesp.models){
  plot <- (ggplot(tdata[[level]], aes_string(x=xval, y=yval)) +
             geom_point(color='#0e5ba9') +
             geom_smooth(method=lm, se=FALSE, color='red') +
             scale_x_continuous() +
             scale_y_continuous() + 
             labs(title = paste(yval, "vs.", xval, "for", level),
                  x = xval, y = yval))
  show(plot)
  tmodels <- get_models(xval, yval)
  summary(tmodels[[level]])
}

parsemodel('Elementary', "SEGR_INDX", "PTS")

fullsummary <- function(level, xval, yval, tdata=typesp.data, tmodels=typesp.models){
  tmodels <- get_models(xval, yval)
  tsmry <- summary(tmodels[[level]])
  list("R-squared"=tsmry$r.squared, "Coefficients of Variance"=tsmry$cov.unscaled,
             "F-statistic"=tsmry$fstatistic, )
}
get_mult_models(c("STD_SEGR", "STD_FARMS"), "PTS")
summary(typesp.models[['Elementary']])
summary(typesp.models[['Middle']])
summary(typesp.models[['High School']])
residlist <- setNames(unlist(sapply(typesp.models, resid)),
                      unlist(sapply(c('Elementary', 'High School', 'Middle'),
                          function(x){
                            slctdata %>% filter(TYPE==x) %>% pull(NAME)
                          })))
residlist_mag <- residlist[order(abs(residlist), decreasing=TRUE)]
residlist_neg <- sort(residlist[residlist <= 0])
residlist_pos <- sort(residlist[residlist > 0], decreasing=TRUE)

resid_sep <- mapply(X=c('Elementary', 'High School', 'Middle'), Y=typesp.models, function(X, Y){
  setNames(resid(Y), slctdata %>% filter(TYPE==X) %>% pull(NAME))
})
resid_smag <- lapply(resid_sep, function(x){x[order(abs(x), decreasing=TRUE)]})
resid_sneg <- lapply(resid_sep, function(x){sort(x[x <= 0])})
resid_spos <- lapply(resid_sep, function(x){sort(x[x > 0], decreasing=TRUE)})

# for (county in names(cntydata)){
#   cntymodels[[county]] = lm(PTS ~ FARMS, data=cntydata[[county]])
# }