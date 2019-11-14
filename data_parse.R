library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)

fulldata <- fread('md_report_card.csv', na.strings="--")
names(fulldata) <- replace(names(fulldata), 1, 'School Name')
fulldata <- cbind(fulldata %>% select('School Name', 'School County', 'School Type'),
                  fulldata %>% select(-c('School Name', 'School County', 'School Type')) %>%
                    mutate_all(funs(as.character)) %>%
                    mutate_all(funs(parse_number)))
slctdata <- fulldata %>%
  select(NAME='School Name', CNTY='School County', TYPE='School Type',
         PTS='Points %', FARMS='FARMS Rate', SEGR_INDX='Segregation Index')
typesp.data <- setNames(slctdata %>% group_split(TYPE, keep=FALSE), slctdata %>% group_keys(TYPE) %>% pull(1))
get_models <- function(xval, yval, data=typesp.data){
  typesp.models <<- lapply(data, function(typed){lm(paste(yval, "~", xval), data=typed)})
}
get_models("SEGR_INDX", "PTS")

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
# for (county in names(cntydata)){
#   cntymodels[[county]] = lm(PTS ~ FARMS, data=cntydata[[county]])
# }