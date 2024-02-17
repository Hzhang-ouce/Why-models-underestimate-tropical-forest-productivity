# Huanyuan - Xiongjie  FAPAR & GPP & Time Series

standard_error_calc <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

library(tidyverse)
# so xiongjie helps to download fapar from MODIS using GEE, now I just need to plot it with R
setwd('H:\\Oxford\\Chapter_three\\github_version/2_Figure_four_timeseries_of_fapar/')
Raw_csv<-read_csv('Plot Points Data  from GEE Xiongjie.csv')
std.error <- function(x) sd(x)/sqrt(length(x))
Processed_csv<-Raw_csv%>%
  rowwise()%>%
  mutate( FparQC_binary=paste(rev(as.integer(intToBits(FparLai_QC))), collapse=""),
          # convert to binary
          FparQC_binary = stringi::stri_sub(FparQC_binary,-8,-1),
          # only last 8 digits are meaningful, remove other 0 at the start of the string
          CLOUDSTATE = as.numeric(substr(FparQC_binary,4,5)),
          # extract cloud state, 00 denote no cloud
          month = substr(date,6,7),
          # extract month from the 6th and 7th digits
          site = substr(name,1,3))%>%
          # extract site from the 1st to 3rd digits
  group_by(month, site)%>%
  mutate(number_measured = n(),
         #number of measurements under a month and plot
         number_no_cloud = sum(CLOUDSTATE==0,na.rm=T),
         # number of no clound measurements
         percentage_of_no_clound = number_no_cloud/number_measured)%>%
  group_by(month, site)%>%
  summarise(Fpar_500m_mean = mean(Fpar_500m, na.rm=T),
            Fpar_500m_se = std.error(Fpar_500m),
            percentage_of_no_clound=mean(percentage_of_no_clound))

FAPAR<-read.csv('LAI_monthly_Caneye_illastik_20211208.csv')%>%
  mutate(site= substr(plot_code,1,3))%>%
  group_by(month,site)%>%
  summarise(    FAPAR_field_se = standard_error_calc(final_FAPAR),
                FAPAR_field = mean (final_FAPAR)*100,
            n=n())

# Units of FAPAR: %

# Plot the result
library(ggplot2)

Processed_csv %>%
  mutate(month = as.numeric(month)) %>%
  ggplot() +
   geom_rect(aes(xmin = month - 0.5, xmax = month + 0.5, ymin = 0, ymax = 100, fill = 1 - percentage_of_no_clound)) +
  geom_line(aes(x = month, y = Fpar_500m_mean,col= 'MODIS')) +
  geom_point(aes(x = month, y = Fpar_500m_mean,col= 'MODIS')) +
  geom_line(aes(x = month, y = FAPAR_field,col = 'FAPAR'), data = FAPAR) +
  geom_point(aes(x = month, y = FAPAR_field,col = 'FAPAR'), data = FAPAR) +
  facet_wrap(~site, nrow = 3, ncol = 1) +
  scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ylab("FAPAR mean (%)") +
  scale_fill_gradient(low = "white", high = "black", name = "Percentage of \n data with cloud") + # Set gradient colors from blue to red
  theme_bw() +
  scale_color_manual(values =c('#FFB000', 'cyan'),labels = c('Measured' , 'MODIS'), name= 'Source')+
  #scale_shape_manual(values =c(16, 17),labels = c('Measured' , 'MODIS'), name= 'Source')+
  theme(panel.grid = element_blank()) 

ggsave(filename = 'Fapar_time_series_Ghana_20230218.jpg',width=5.37 , height=4.84 )



# Now we commute fapar mean with and without cloud for each site


fapar_no_cloud<-Raw_csv%>%
  rowwise()%>%
  mutate( FparQC_binary=paste(rev(as.integer(intToBits(FparLai_QC))), collapse=""),
          # convert to binary
          FparQC_binary = stringi::stri_sub(FparQC_binary,-8,-1),
          # only last 8 digits are meaningful, remove other 0 at the start of the string
          CLOUDSTATE = as.numeric(substr(FparQC_binary,4,5)),
          # extract cloud state, 00 denote no cloud
          month = substr(date,6,7),
          # extract month from the 6th and 7th digits
          site = substr(name,1,3))%>%
  filter(CLOUDSTATE=='0' | CLOUDSTATE=='10')%>% # no clound, mixed cloud is allowed, otherwise there are too little data
  # extract site from the 1st to 3rd digits
  group_by(month, site)%>%
  mutate(number_measured = n(),
         #number of measurements under a month and plot
         number_no_cloud = sum(CLOUDSTATE==0,na.rm=T),
         # number of no clound measurements
         percentage_of_no_clound = number_no_cloud/number_measured)%>%
  group_by(site)%>%
  summarise(Fpar_500m_mean = mean(Fpar_500m, na.rm=T),
            Fpar_500m_se = std.error(Fpar_500m),
            percentage_of_no_clound=mean(percentage_of_no_clound))%>%
  rename(Group.1=site,
         fapar_no_cloud=Fpar_500m_mean)




fapar_all_cloud_status<-Raw_csv%>%
  rowwise()%>%
  mutate( FparQC_binary=paste(rev(as.integer(intToBits(FparLai_QC))), collapse=""),
          # convert to binary
          FparQC_binary = stringi::stri_sub(FparQC_binary,-8,-1),
          # only last 8 digits are meaningful, remove other 0 at the start of the string
          CLOUDSTATE = as.numeric(substr(FparQC_binary,4,5)),
          # extract cloud state, 00 denote no cloud
          month = substr(date,6,7),
          # extract month from the 6th and 7th digits
          site = substr(name,1,3))%>%
  
  # extract site from the 1st to 3rd digits
  group_by(month, site)%>%
  mutate(number_measured = n(),
         #number of measurements under a month and plot
         number_no_cloud = sum(CLOUDSTATE==0,na.rm=T),
         # number of no clound measurements
         percentage_of_no_clound = number_no_cloud/number_measured)%>%
  group_by(site)%>%
  summarise(Fpar_500m_mean = mean(Fpar_500m, na.rm=T),
            Fpar_500m_se = std.error(Fpar_500m),
            percentage_of_no_clound=mean(percentage_of_no_clound))%>%
  rename(Group.1=site,
         fapar_all_cloud_status=Fpar_500m_mean)



load("../1_prepare_climate_data/organized_climate_withppfd.rda")
result<-result%>%
  left_join(fapar_all_cloud_status[,c('Group.1','fapar_all_cloud_status')])%>%
 left_join(fapar_no_cloud[,c('Group.1','fapar_no_cloud')])
save(result, file = 'organized_climate_withppfd_two_fapar.rda')

