library(tidyverse) 
library(readr) 
library(kableExtra) 
library(cluster)    
library(factoextra) 
library(GGally) 
library(NbClust) 
library(caret)

cc_data <- read.csv("CC_GENERAL.csv", header=TRUE)

str(cc_data)

head(cc_data)

transformed_variables <- c("BALANCE", "PURCHASES", "ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES", "CASH_ADVANCE", "CASH_ADVANCE_TRX", "PURCHASES_TRX", "CREDIT_LIMIT", "PAYMENTS", "MINIMUM_PAYMENTS" ) # log transformationvariables


#remove data that has less than 12 month of history and remove mising values and getting ready for log transformation.  
clustering_data <- cc_data %>%                                  
  filter(TENURE == 12) %>%                                      
  .[,-c(18)] %>%                                                
  na.omit() %>%                                                 
  mutate_at(vars(transformed_variables), funs(log(1 + .))) %>%  
  mutate_at(c(2:17), funs(c(scale(.))))     


#plot density plot and shirnk to size for each variable pairs. 
plots <- as.data.frame(clustering_data[,-c(1)]) %>%
  gather() %>%                             
  ggplot(aes(value)) +                    
  facet_wrap(~ key, scales = "free") +  
  geom_density() +                      
  theme(strip.text = element_text(size=5))

plots  


 # ggally to plot correlation and shrink to size
corr_plots <- ggpairs(as.data.frame(clustering_data[,-c(1)]),                        
                      lower = list(continuous = wrap("points", 
                                                     alpha = 0.3, size=0.15),
                                   combo = wrap("dot", alpha = 0.4,size=0.2)
                      )
)

corr_plots     

# calculate correlation values between variable 
corr_values <- cor(clustering_data[,-1]) 

corr_values %>%                          
  as.data.frame() %>%
  kable(digits = 3) %>%
  kable_styling(font_size = 9) 


#reduce variables based on similar correlation 0.6 was arbitrarily selected
above_cutoff <- findCorrelation(corr_values,   
                                names = TRUE, 
                                cutoff= 0.6) 

reduced_data <- clustering_data %>%   # make dataset keeping uncorrelated variables, only
  select(-one_of(above_cutoff))  # remove one of each from variable pairs > cutoff
reduced_data$CUST_ID<-NULL


set.seed(123)

#create cluster using basic kmeans algo with NbClust library
nc_reduced <- NbClust(reduced_data, min.nc=2, max.nc=10, method="kmeans")

  table(nc_reduced$Best.n[1,])


# estimates best number of clusters 
nc_reduced$All.index 


barplot(table(nc_reduced$Best.n[1,]),
        xlab="Number of Clusters Reduced", ylab="Number of Criteria",
        main="Number of Clusters Reduced Chosen by Criteria")


# add cluster assignment as variable to original dataset
clustered_data <- cc_data %>%  
  merge(as.data.frame(nc_reduced$Best.partition) %>%
          rownames_to_column(),
        by.x = "CUST_ID", by.y = "rowname", all = TRUE)

set.seed(123) 

# calculate gap statistic for clusters (K) = 1 to 10 against 500 simulated datasets that meet the null hypothesis
gap_stat_reduced <- clusGap(as.data.frame(reduced_data), 
                            FUN = kmeans, nstart = 25,         
                            K.max = 10, B = 500, d.power = 2) 


 # visualize the gap statistic to see where it plateaus to identify the most likely K
fviz_gap_stat(gap_stat_reduced) 

# don't need customer id
clustered_data[,-1] %>%   
  na.omit() %>% 
  group_by(`nc_reduced$Best.partition`) %>% 
  summarise_all(funs(mean = mean, median = median, sd = sd)) %>% 
  as.data.frame() %>%
  kable() %>%
  kable_styling(font_size = 9) 

#Cluster 1. spend a low amount, take low to no cash advances, and carry a high (compared to cluster 2) balance

#Cluster 2. spend a low amount, take low to no cash advances, and carry a low (compared to cluster 1) balance

#Cluster 3. spend a low amount, take large cash advances, and carry a high balance

#Cluster 4. spend a high amount, take low to no cash advances, and carry a low balance
