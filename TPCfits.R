# load packages
library(ggplot2)
library(data.table)
library(dplyr)
library(reshape2)
library(viridis)

library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

#toggle between desktop (y) and laptop (n)
desktop<- "n"

#other TPCs
#larval TPC
#growth rate
#Figure 2, mean short term mass-specific growth rate, from kingsolver 2000, 
#time period: July 25-30, Aug 12-19, Aug 21-26
#g/g/hr
temps= c(11,17,23,29,35,40,41)
mgr= c(.010, .018, .0435, .0494, .0726, .0388, .0165)
gr= as.data.frame(cbind(temps, mgr))

colnames(gr)=c("temp","rate")
d=gr

#--------------------
#load constant temperature data 

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/My Drive/Buckley/Work/WARP/projects/TPCconstant/out/")
if(desktop=="n") setwd("/Users/lbuckley/Library/CloudStorage/GoogleDrive-lbuckley@uw.edu/My Drive/Buckley/Work/WARP/projects/TPCconstant/out/")

tpc<- read.csv("PastPresentFilteredConstantTpc2024.csv")

#restrict to 6hrs and 4th instar, past
tpc1<- tpc[which(tpc$time.per=="past" & tpc$active=="y" & tpc$instar==4 & tpc$time>0.5),]
#restrict to 6 hrs
tpc1<- tpc1[which(tpc1$time>5 & tpc1$time<10),]

#estimates means across temperatures
tpc.agg.f2 <- tpc1 %>%
  group_by(temp) %>% 
  dplyr::summarise(
    mean = mean(rgrlog, na.rm = TRUE),
    n= length(rgrlog),
    se = sd(rgrlog, na.rm = TRUE) / sqrt(n)
  )

#format data for analysis
d= tpc1[,c("temp","rgrlog")]
colnames(d)<- c("temp", "rate")

# write function to label ggplot2 panels
label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

d_fits <- nest(d, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)),
         gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         weibull = map(data, ~nls_multstart(rate~weibull_1995(temp = temp, a,topt,b,c),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)))

# stack models
d_stack <- dplyr::select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', beta:weibull)

# get parameters using tidy
params <- d_stack %>%
  mutate(., est = map(fit, tidy)) %>%
  dplyr::select(-fit) %>%
  unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(d$temp), max(d$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  dplyr::select(-fit) %>%
  unnest(preds)

# plot fits
ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), d) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 5) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)

#extract model
mod= d_fits$beta[[1]]

# get predictions
preds <- data.frame(temp = seq(min(d$temp), max(d$temp), length.out = 100))
preds <- broom::augment(mod, newdata = preds)

#extract coefficients
tpc.beta= coef(mod)
#beta_2012(temp, a, b, c, d, e)

# write function to label ggplot2 panels
label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

#plot TPC
tpc.c<- summary(mod)$coefficients
beta.params<- tpc.c[,1]

plot(1:50, beta_2012(temp = 1:50, a=beta.params[1], b=beta.params[2], c=beta.params[3], d=beta.params[4], e=beta.params[5]))