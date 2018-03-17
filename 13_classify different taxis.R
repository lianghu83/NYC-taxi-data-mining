######################################################################
## This file builds classification model to classify different taxis
## 200 mi, 300 mi, or 300+ mile



rm(list = ls())

df <- read.csv("U:\\Desktop\\By vehicle_F8_Review.csv")

#old infrastructure
df$Complete_200 <- 0
df$Complete_200[df$Feasibility_200_Review >= 0.99] <- 1
summary(df$Feasibility_200_Review)

df$Complete_300 <- 0
df$Complete_300[df$Feasibility_300_Review >= 0.99] <- 1
summary(df$Feasibility_300_Review)

df$Complete <- 0
df$Complete[df$Complete_200==0 & df$Complete_300==0] <- '300-infeasible'
df$Complete[df$Complete_200==0 & df$Complete_300==1] <- '300-feasible'
df$Complete[df$Complete_200==1] <- '200-feasible'
df$Complete <- as.factor(df$Complete)
summary(df$Complete)

#new infrastructure
df$Complete_200X <- 0
df$Complete_200X[df$Feasibility_200X_Review >= 0.99] <- 1
summary(df$Feasibility_200X_Review)

df$Complete_300X <- 0
df$Complete_300X[df$Feasibility_300X_Review >= 0.99] <- 1
summary(df$Feasibility_300X_Review)

df$CompleteX <- 0
df$CompleteX[df$Complete_200X==0 & df$Complete_300X==0] <- '300-infeasible'
df$CompleteX[df$Complete_200X==0 & df$Complete_300X==1] <- '300-feasible'
df$CompleteX[df$Complete_200X==1] <- '200-feasible'
#Here it should be 'other', but only 1 car, for convenience, it is converted to '200 feasible'
df$CompleteX <- as.factor(df$CompleteX)
summary(df$CompleteX)

# Shift. Infeasible shift to feasible
# old infrastructure to new
df$Label <- paste(as.character(df$Complete), 'to', as.character(df$CompleteX))
df$Label[df$Label=='300-feasible to 300-infeasible'] <- '300-feasible to 300-feasible'
#Here 2 taxis are '300 feasible to 300 infeasible', for convenience, convert to '300 feasible to 300 feasible'
df$Label <- as.factor(df$Label)
summary(df$Label)



#Plot travel pattern variables
library(ggplot2)
library(scales)
library(Rmisc)

#Boxplot one by one, from X1 to X10
#new infrastructure
#X1 
X1 <- summarySE(df, measurevar="ave_shift_new_Review", groupvars=c("Complete"))
ggplot(X1, aes(x=Complete, y=ave_shift_new, fill=Complete)) + 
  scale_y_continuous(limits = c(0, 3.5)) + 
  geom_bar(position="dodge", stat="identity", width=0.3) +
  geom_errorbar(aes(ymin=ave_shift_new-sd, ymax=ave_shift_new+sd),
                width=0.1, position=position_dodge(.9)) + 
  guides(fill=F) + 
  ylab("V1: Mean of Daily Shifts") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

ggplot(df, aes(x=CompleteX, y=ave_shift_new_Review, color=CompleteX)) + 
  scale_y_continuous(limits = c(1, 3.5)) +
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X1: Mean of the Number of Daily Shifts") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))
  
#X2
X2 <- summarySE(df, measurevar="mode_shift_new", groupvars=c("Complete"))
ggplot(X2, aes(x=Complete, y=mode_shift_new, fill=Complete)) + 
  scale_y_continuous(limits = c(0, 3.5)) + 
  geom_bar(position="dodge", stat="identity", width=0.3) +
  geom_errorbar(aes(ymin=mode_shift_new-sd, ymax=mode_shift_new+sd),
                width=0.1, position=position_dodge(.9)) + 
  guides(fill=F) + 
  ylab("X2: Mode of the Number of Daily Shifts") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

ggplot(df, aes(x=CompleteX, y=mode_shift_new_Review, color=CompleteX)) + 
  scale_y_continuous(limits = c(1, 3.5)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X2: Mode of the Number of Daily Shifts") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X3 
X3 <- summarySE(df, measurevar="num_driver", groupvars=c("Complete"))
ggplot(X3, aes(x=Complete, y=num_driver, fill=Complete)) + 
  #scale_y_continuous(limits = c(0, 3.5)) + 
  geom_bar(position="dodge", stat="identity", width=0.3) +
  geom_errorbar(aes(ymin=num_driver-sd, ymax=num_driver+sd),
                width=0.1, position=position_dodge(.9)) + 
  guides(fill=F) + 
  ylab("V3: Number of Drivers") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

ggplot(df, aes(x=CompleteX, y=num_driver, color=CompleteX)) + 
#  scale_y_continuous(limits = c(0, 200)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X3: Number of Drivers") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X4 
ggplot(df, aes(x=CompleteX, y=ave_shift_driver_new, color=CompleteX)) + 
  #  scale_y_continuous(limits = c(0, 200)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X4: Number of Shifts per Driver in a Year") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X5 
ggplot(df, aes(x=CompleteX, y=ave_trip_distance, color=CompleteX)) + 
  scale_y_continuous(limits = c(0, 5)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X5: Mean of Occupied Trip Length (mile)") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X6
ggplot(df, aes(x=CompleteX, y=DVMT_Review, color=CompleteX)) + 
  scale_y_continuous(limits = c(0, 300)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X6: Mean of DVMT (mile)") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X7
ggplot(df, aes(x=CompleteX, y=between_distance_X, color=CompleteX)) + 
  scale_y_continuous(limits = c(0, 85)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X7: Mean of In-between Travel Distance (mile)") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X8
ggplot(df, aes(x=CompleteX, y=daily_dwell_Review, color=CompleteX)) + 
  scale_y_continuous(limits = c(0, 6)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X8: Mean of the Number of Daily Dwells") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X9
hist(df$ave_dwell_time, main=NA, xlab='Mean of dwell length--For all taxis',
     xlim=c(0, 3000), breaks=25)
hist(df$ave_dwell_time_1)
hist(df$ave_dwell_time_3, main=NA, xlab='Mean of dwell length--For all taxis',
     xlim=c(0, 1400), breaks=20)
hist(df$ave_dwell_time_3)
ggplot(df, aes(x=CompleteX, y=ave_dwell_time_3, color=CompleteX)) + 
  scale_y_continuous(limits = c(0, 800)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X9: Mean of Dwell Length (minute)") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X10 %_Manhattan
ggplot(df, aes(x=CompleteX, y=100*Manhattan, color=CompleteX)) + 
  scale_y_continuous(limits = c(80, 100)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X10: Percentage of Dwells in Manhattan (%)") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#X11 ave_shift_duration
ggplot(df, aes(x=CompleteX, y=ave_shift_new_duration, color=CompleteX)) + 
  #scale_y_continuous(limits = c(80, 100)) + 
  geom_boxplot(width=0.65, aes(color=CompleteX), outlier.shape=NA) + 
  stat_summary(fun.y="mean", geom="point", shape=18, size=3) +
  ylab("X11: Mean of shift time duration (minute)") +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color='black'),
        axis.text.x=element_text(size=13, color='black'),
        axis.text.y=element_text(size=13, color='black'))

#statistics: mean, std
X1 <- summarySE(df, measurevar="ave_shift_new", groupvars=c("CompleteX"))
X2 <- summarySE(df, measurevar="mode_shift_new", groupvars=c("CompleteX"))
X3 <- summarySE(df, measurevar="num_driver", groupvars=c("CompleteX"))
X4 <- summarySE(df, measurevar="ave_shift_driver_new", groupvars=c("CompleteX"))
X5 <- summarySE(df, measurevar="ave_trip_distance", groupvars=c("CompleteX"))
X6 <- summarySE(df, measurevar="DVMT", groupvars=c("CompleteX"))
X7 <- summarySE(df, measurevar="between_distance_X", groupvars=c("CompleteX"))
X8 <- summarySE(df, measurevar="daily_dwell_Review", groupvars=c("CompleteX"))
X9 <- summarySE(df, measurevar="ave_dwell_time_2", groupvars=c("CompleteX"))
X10 <- summarySE(df, measurevar="Manhattan", groupvars=c("CompleteX"))



#shift from infeasible to feasible
aa <- subset(df, df$Complete=='300-infeasible')
summary(aa$Label)
aa$Label <- as.character(aa$Label)
aa$Label[aa$Label=='300-infeasible to 200-feasible' | aa$Label=='300-infeasible to 300-feasible'] <- 'feasible'
aa$Label[aa$Label=='300-infeasible to 300-infeasible'] <- 'infeasible'
aa$Label <- as.factor(aa$Label)
summary(aa$Label)

X1 <- summarySE(aa, measurevar="ave_shift_new", groupvars=c("Label"))
by(aa$ave_shift_new, aa$Label, summary)
X2 <- summarySE(aa, measurevar="mode_shift_new", groupvars=c("Label"))
by(aa$mode_shift_new, aa$Label, summary)
X3 <- summarySE(aa, measurevar="num_driver", groupvars=c("Label"))
by(aa$num_driver, aa$Label, summary)
X4 <- summarySE(aa, measurevar="ave_shift_driver_new", groupvars=c("Label"))
by(aa$ave_shift_driver_new, aa$Label, summary)
X5 <- summarySE(aa, measurevar="ave_trip_distance", groupvars=c("Label"))
by(aa$ave_trip_distance, aa$Label, summary)
X6 <- summarySE(aa, measurevar="DVMT", groupvars=c("Label"))
by(aa$DVMT, aa$Label, summary)
X7 <- summarySE(aa, measurevar="between_distance_X", groupvars=c("Label"))
by(aa$between_distance, aa$Label, summary)
X8 <- summarySE(aa, measurevar="daily_dwell", groupvars=c("Label"))
by(aa$daily_dwell, aa$Label, summary)
X9 <- summarySE(aa, measurevar="ave_dwell_time_3", groupvars=c("Label"))
by(aa$ave_dwell_time, aa$Label, summary)
X10 <- summarySE(aa, measurevar="Manhattan", groupvars=c("Label"))
by(aa$Manhattan, aa$Label, summary)



# Scatterplots
library(GGally)
library(ggplot2)
ggpairs(df, columns=c(13,14,10,17,6,7,22,9,23,2), mapping=aes(color=CompleteX, alpha=0.5))
ggpairs(df, columns=c(13,7,22,9), mapping=aes(color=CompleteX, shape=CompleteX, alpha=0.5))
ggplot(df, aes(x=DVMT, y=daily_dwell, color=CompleteX, shape=CompleteX)) + geom_point(size=1)+ stat_smooth(method=lm, se=F)
# shape=Label,
ggpairs(aa, columns=c(7,22,9), mapping=aes(color=Label, alpha=0.5))
ggplot(aa, aes(x=DVMT, y=daily_dwell, color=Label, shape=Label)) + geom_point(size=1)+ stat_smooth(method=lm, se=F)
ggplot(df, aes(x=DVMT, y=between_distance_X, color=CompleteX, shape=CompleteX)) + geom_point(size=1)+ stat_smooth(method=lm, se=F)



# Quadratic Discriminant Analysis for df
library(MASS)
aa.qda <- lda(Label ~ ave_shift_new
                        +mode_shift_new
                        +num_driver
                        +ave_shift_driver_new
                        +ave_trip_distance
                        +DVMT
                        +between_distance_X
                        +daily_dwell
                        +ave_dwell_time_2 #8,23,24
                        +Manhattan 
                        ,data=aa)
aa.qda.predicted <- predict(aa.qda, aa[, c(13,14,10,17,6,7,22,9,24,2)])
ct <- table(aa$Label, aa.qda.predicted$class)
diag(prop.table(ct, 1))
sum(diag(prop.table(ct)))
summary(aa.qda)



# Logistic regression analysis for aa
# p is the probablity that y=1, which is feasible
aa$Label_LR[aa$Label=='infeasible'] = 0
aa$Label_LR[aa$Label=='feasible'] = 1
#aa$ave_shift_driver_new_Review <- aa$num_day_Review * aa$ave_shift_new_Review / aa$num_driver
#split, 70% train, 30% test
set.seed(1991) 
sample <- sample.int(n = nrow(aa), size = floor(.7*nrow(aa)), replace = F)
aa_train <- aa[sample, ]
aa_test  <- aa[-sample, ]

aa.fit <- glm(Label_LR ~ ave_shift_new #13, 32
                         +mode_shift_new #14, 33
                         +num_driver
                         +ave_shift_driver_new #17, 42
                         +ave_trip_distance
                         +DVMT
                         +between_distance_X
                         +daily_dwell_Review #9, 31
                         +ave_dwell_time_3 #8,23,24, 28
                         +Manhattan
              ,family=binomial, data=aa_train)
summary(aa.fit)
exp(aa.fit$coefficients)

aa.fitted_results <- 
  predict(aa.fit, newdata=aa_test[,c(13,14,10,17,6,7,22,31,28,2)], type = 'response')
aa_test$prob <- aa.fitted_results
aa.fitted_results <- ifelse(aa.fitted_results > 0.5,1,0)
aa_test$class_fitted <- aa.fitted_results
misClasificError <- mean(aa.fitted_results != aa_test$Label_LR)
print(paste('Accuracy',1-misClasificError))

aa.fitted_results <- 
  predict(aa.fit, newdata=aa_train[,c(13,14,10,17,6,7,22,31,28,2)], type = 'response')
aa_train$prob <- aa.fitted_results
aa.fitted_results <- ifelse(aa.fitted_results > 0.5,1,0)
aa_train$class_fitted <- aa.fitted_results
misClasificError <- mean(aa.fitted_results != aa_train$Label_LR)
print(paste('Accuracy',1-misClasificError))

#Wald test
library(survey)
regTermTest(aa.fit, "ave_shift_new")
regTermTest(aa.fit, "mode_shift_new")
regTermTest(aa.fit, "num_driver")
regTermTest(aa.fit, "ave_shift_driver_new")
regTermTest(aa.fit, "ave_trip_distance")
regTermTest(aa.fit, "DVMT")
regTermTest(aa.fit, "between_distance_X")
regTermTest(aa.fit, "daily_dwell_Review")
regTermTest(aa.fit, "ave_dwell_time_3")
regTermTest(aa.fit, "Manhattan")
#McFadden R2, Cox Snell, NagelkerkeR2 R2
library(pscl)
pR2(aa.fit)



# multinomial logistic regression analysis for df
# Use CompleteX as label
library(nnet)
bb <- df
bb$CompleteX <- as.numeric(bb$CompleteX) - 1 #200-feasible: 0; 300-feasible: 1
bb.fit <- multinom(CompleteX ~ ave_shift_new
              +mode_shift_new
              +num_driver
              +ave_shift_driver_new
              +ave_trip_distance
              +DVMT
              +between_distance
              +daily_dwell
              +ave_dwell_time
              +Manhattan
              ,family=binomial, data=bb)
summary(bb.fit)
#exp(bb.fit$coefficients)
bb.fitted_results <- 
  predict(bb.fit, newdata=bb[,c(13,14,10,17,6,7,15,9,8,2)], type = 'probs')
bb$class_fitted <- as.numeric(colnames(bb.fitted_results)[apply(bb.fitted_results, 1, which.max)])
misClasificError <- mean(bb$class_fitted != bb$CompleteX)
print(paste('Accuracy', 1-misClasificError))


