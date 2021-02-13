library(viridis)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(tidyverse)

set.seed(1234)
load("C:/Users/Alessandra/Desktop/sds/HW3/hw3_data.RData")

# Data Exploration: ASD subjects -------------------------------------------


#rename the name of the subjects for convenience 
names(asd_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))

#build a dataframe of min and max values for all the region
# for all the 12 subjects

range_dataframe <- function(data_frame) {
  #given a df of m columns and n rows return a df with 2 columns and
  #m columns, containing the min and max values of each column
  return (data.frame(min = sapply(data_frame, min), max = sapply(data_frame, max)))
}

range_all_subjects <- lapply(asd_sel, range_dataframe)
range_all_subjects_df <- data.frame(range_all_subjects)

#view all the ranges 
range_all_subjects_df

#sample 20 regions to print out, for tidiness
indeces.samples <- sample(nrow(range_all_subjects_df), size = 20, replace = F)
range_all_subjects_df[indeces.samples, ]

#extract the ROI n°1 from all the subjects
get_roi <-function(dataframe, roi_id) {
  return(dataframe[, roi_id])
}

#dataframe that contains the only ROI NUMBER 1 of all the 
#subject. 12 columns x 145 obs
roi1_all_subjects <- data.frame(sapply(asd_sel, get_roi, 1))

#add the times column, a numeric column of values from 1 to 145
roi1_all_subjects <- cbind(times = seq(1:145), roi1_all_subjects)

#pivoting the dataframe to make a plot
roi1_all_subjects_long <- roi1_all_subjects %>% 
  pivot_longer(names(roi1_all_subjects)[-1], names_to = "subject_ID", values_to = "values")

#spaghetti plot of the ROI 2001 of 12 subjects
roi1_all_subjects_long %>%
  ggplot( aes(x=times, y=values, group=subject_ID, color=subject_ID)) +
  geom_line(size = 0.8) +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("A spaghetti plot of Region of Interest '2001' of the 12 ASD") +
  theme_ipsum()

#plot of the ROI "2001" of 12 subjects with individual subject highlighted
tmp <- roi1_all_subjects_long %>%
  mutate(name2=subject_ID)

tmp %>%
  ggplot( aes(x=times, y=values)) +
  geom_line( data=tmp %>% dplyr::select(-subject_ID), aes(group=name2), color="grey", size=1, alpha=0.5) +
  geom_line( aes(color=subject_ID), color="#69b3a2", size= 1.1)+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  labs(title = "Region of Interest 2001 of the 12 ASD subjects",
       subtitle = "the individual subject is highlighted",
       caption = "" ) +
  facet_wrap(~subject_ID)

#Concusions: subject1 is an outlyer


# Data Exploration: TD subjects -------------------------------------------

#Let's see that in the TD dataset the first and sencond subjects are outliers

#rename the name of the subjects for convenience 
names(td_sel) <- c(paste("subj", seq(1,9), sep = '_0'), 
                    paste("subj", seq(10,12), sep = '_'))

#min e max value in all the ROI of all the TD subjects
range_all_subjects_TD <- lapply(td_sel, range_dataframe)
range_all_subjects_TD_df <- data.frame(range_all_subjects_TD)

#sample 20 regions to print out, for tidiness
range_all_subjects_TD_df[sample(nrow(range_all_subjects_df),20, replace = F), ]


#function that merge all the columns (ROI's) in a sigle column
flat_df <- function(data_frame, name) {
  return (data.frame(name = c(t(data_frame)), stringsAsFactors=FALSE))
}

#flat all the TD subjects
m <- mapply(flat_df, td_sel, names(td_sel))
all_ROI_per_subjects <- data.frame(m)

#rename the subjects for convenience
names(all_ROI_per_subjects) <- names(td_sel)

#pivot just for plot with ggplot
subjects_ROI <- all_ROI_per_subjects %>%
  pivot_longer(names(all_ROI_per_subjects), names_to = "subjects", values_to = "ROI_values")

#Boxplot

subjects_ROI %>%
  ggplot( aes(x=subjects, y=ROI_values, fill=subjects)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=16)
  ) +
  labs(title = "What is the range of values of each subject's time series",
       caption = "Boxplot built considering all the ROIs of the subject \n 
       for a total of 16820 values per subject.") +
  xlab("")

# Pull togheter the subjects: Mean Value -------------------------------------


