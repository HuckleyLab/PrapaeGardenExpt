# load packages
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

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

#----------------------------
##P. rapae 1999 selection data
# rgr at each temperature
# survival to pupation, time to pupation, pupal mass
#survive to eclosion, time to eclosion, adult mass
#number of eggs laid
#Relate to selection estimates: determine Topt and plot against fitness components, only pupal mass was significant?

#compare to empirical data
if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/My Drive/Buckley/Work/Proposals/NSF_ORCC/historical/")
if(desktop=="n") setwd("/Users/lbuckley/Library/CloudStorage/GoogleDrive-lbuckley@uw.edu/My Drive/Buckley/Work/Proposals/NSF_ORCC/historical/")

pr= read.csv("PrapaeUW.Seln2.1999.Combineddata.OPUS2021.csv")

#plot TPCs
pr1= pr[,c("Mom","UniID", "Mi", "RGR11", "RGR17", "RGR23", "RGR29", "RGR35")]
pr1= melt(pr1, id.vars=c("Mom","UniID", "Mi"), variable.name="temp", value.name="rgr")           
pr1$temperature= gsub('RGR', '', pr1$temp)

ggplot(pr1, aes(x=temperature, y=rgr, color=UniID, group=UniID))+geom_line()+geom_point()+
  facet_wrap(~Mom)

#fit TPC
#load FitGaussian

fitG =
  function(x,y,mu,sig,scale){
    
    f = function(p){
      d = p[3]*dnorm(x,mean=p[1],sd=p[2])
      sum((d-y)^2)
    }
    
    optim(c(mu,sig,scale),f)
  }

ids= unique(pr1$UniID)
#by mom
#ids= unique(pr1$Mom)

tpc.p= matrix(NA, nrow=length(ids), ncol=3)

for(id.k in 1:length(ids)){
  
  gr= pr1[pr1$UniID==ids[id.k],c("temperature","rgr")]
  colnames(gr)=c("temp","rate")
  gr$temp= as.numeric(gr$temp)
  gr= na.omit(gr)
  
  tryCatch({ gr.fit<- fit.tpcs(gr) 
  tpc.p[id.k,]<-coef(gr.fit) },
  error=function(e){})
  
}
#fix so Topt can be >35

#plot TPCs
pr$Topt= tpc.p[match(pr$UniID,ids), 2]

#plot selection functions
ggplot(pr, aes(x=Topt, y=Time.to.Pupation, color=UniID, group=UniID))+geom_point()
ggplot(pr, aes(x=Topt, y=Pupa.wt, color=UniID, group=UniID))+geom_point()
ggplot(pr, aes(x=Topt, y=Butt..Wt, color=UniID, group=UniID))+geom_point()
ggplot(pr, aes(x=Topt, y=Fecundity, color=UniID, group=UniID))+geom_point()

#============================

