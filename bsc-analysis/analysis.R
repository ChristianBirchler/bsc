library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggbiplot)
library(MASS)
library(partykit)
library(randomForest)
library(FSelector)
library(gbm)
library(JOUSBoost)
library(xgboost)
library(adabag)
library(Matrix)
library(naivebayes)
library(pROC)

#set.seed(123)

########## READ DATA ##########
setwd("~/Desktop/bsc-analysis")
dd.orig <- fread("vm9-no-gc-merged.csv")
idflakies <- fread("idflakies-projects.csv")
###############################


######### CLEAN DATA ##########
dd <- dd.orig[,-c('V1')]

# convert all collumns with numbers as numeric data type
# collect indices of NA entries -> those are corrupted data entries
dd$deadlock.count <- as.numeric( dd$deadlock.count) 
na.indeces <- which( is.na( dd$deadlock.count))

dd$loaded <- as.numeric( dd$loaded) 
na.indeces <- unique( c(na.indeces, which( is.na(dd$loaded))))

dd$pools.Metaspace.used <- as.numeric( dd$pools.Metaspace.used)
na.indeces <- unique( c(na.indeces, which( is.na(dd$pools.Metaspace.used))))

dd$`pools.PS-Eden-Space.committed` <- as.numeric(dd$`pools.PS-Eden-Space.committed`)
na.indeces <- unique( c(na.indeces, which( is.na(dd$`pools.PS-Eden-Space.committed`))))

dd$`pools.PS-Eden-Space.init` <- as.numeric(dd$`pools.PS-Eden-Space.init`)
na.indeces <- unique( c(na.indeces, which( is.na(dd$`pools.PS-Eden-Space.init`))))

dd$`pools.PS-Eden-Space.max` <- as.numeric(dd$`pools.PS-Eden-Space.max`)
na.indeces <- unique( c(na.indeces, which( is.na(dd$`pools.PS-Eden-Space.max`))))

dd$`pools.PS-Eden-Space.used` <- as.numeric(dd$`pools.PS-Eden-Space.used`)
na.indeces <- unique( c(na.indeces, which( is.na(dd$`pools.PS-Eden-Space.used`))))

dd$`pools.PS-Survivor-Space.committed` <- as.numeric(dd$`pools.PS-Survivor-Space.committed`)
na.indeces <- unique( c(na.indeces, which( is.na(dd$`pools.PS-Survivor-Space.committed`))))

dd$terminated.count <- as.numeric(dd$terminated.count)
na.indeces <- unique( c(na.indeces, which( is.na(dd$terminated.count))))

# proportion of corrupted data entries -> ~1%
print(length( na.indeces)/nrow(dd))


# omit corrupted data entries
dd <- dd[-na.indeces,]
########################################




############# FLAKINESS ##############
unique_tests <- unique(dd[, c('TestCase','ProjectName','CommitHash')])

# count the different outcomes per test case (group by TestCase,ProjectName and CommitHash)
nr_different_outcomes <- dd[  , list(`pass/fail` = `pass/fail`, Count = uniqueN(`pass/fail`))
                              , by=c('TestCase','ProjectName','CommitHash') ]

# more than one different outcomes (PASSED,FAILED,ERROR,NOT FOUND) NOT FOUND -> no test report although test was run
different_outcomes <- nr_different_outcomes[ Count > 1 ,  ]

## in case we do not consider missing reports (NOT NEEDED FOR DATA FROM VM9 and VM11!!!)
#different_outcomes <- merge(different_outcomes[`pass/fail`=='PASSED',], different_outcomes[ `pass/fail`=='FAILED' | `pass/fail`=='ERROR',], by=c('TestCase','ProjectName','CommitHash'), all=FALSE)

# less than 20 entries -> build failure -> flaky?
nr_measurements_per_test <- dd[  , list(Count = .N)
                                 , by=c('TestCase','ProjectName','CommitHash') ]

# there are only even number of test measurements of a single test case
# --> there can only build failures if there are less than 20 measurements
less_than_20_entries <- nr_measurements_per_test[ Count < 20 ,  ] # flaky?

# merge (union) different outcomes with tests which have not 20 measurements
different_outcomes_or_less_20_entries <- merge(different_outcomes, less_than_20_entries
                                               , by=c('TestCase','ProjectName','CommitHash')
                                               , all=TRUE)

#flaky.own <- different_outcomes_or_less_20_entries
flaky.own <- unique( different_outcomes[, c('TestCase','ProjectName','CommitHash')] )

# percentage of flaky tests in own measurements
flakiness_rate <- nrow(flaky.own)/nrow(unique_tests)

flakiness_rate

#########################################




############ COMPARISON WITH IDFLAKIES ############
# set merge with idflakies data set (intersection over 'TestCase','CommitHash')
flaky.intersect <- merge(flaky.own, idflakies, by.x=c('TestCase','CommitHash')
                         , by.y=c('Test Name','SHA'), all=FALSE)

nrow(flaky.intersect)

###################################################




########### CREATE LABELED DATA SET ##############
# label data set (flaky/not flaky)
flaky.own$isFlaky <- TRUE
dd.labeled <- merge(dd, flaky.own, by=c('TestCase','ProjectName','CommitHash'), all=TRUE)
dd.labeled[ is.na(isFlaky) , "isFlaky"] <- FALSE

##################################################
##################################################
##################################################



############## CORRELATION BASED FEATURE SUBSET SELECTION #####################################

cfs.dd.labeled <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor')]
cfs.dd.labeled$isFlaky <- as.numeric(cfs.dd.labeled$isFlaky)

# check if columns are constant (remove them)
non_const_cols <- c()
const_cols <- c()
for (col in colnames(cfs.dd.labeled)) {
  if(max(cfs.dd.labeled[,..col])!=min(cfs.dd.labeled[,..col])) {
    non_const_cols <- c(non_const_cols, col)
  } else {
    const_cols <- c(const_cols, col)
  }
}

cfs.dd.labeled <- cfs.dd.labeled[,..non_const_cols]

cor.matrix <- cor( cfs.dd.labeled, method='pearson')

heuristic <- function(v) {
  for (var in v) {
    #if(grepl( '.init', var, fixed=TRUE) || grepl( '.max', var, fixed=TRUE)) {
    # return(0)
    #}
  }
  k <- length(v)
  sub.matrix <- cor.matrix[v,v]
  rtt <- mean( na.omit( abs( sub.matrix[lower.tri( sub.matrix)]))) # average feature intercorrelation
  rct <- mean( na.omit( abs( cor.matrix['isFlaky',v]))) # average feature to class variable correlation
  
  g <- k*rct/sqrt(k+k*(k-1)*rtt)
  #print(g)
  if(is.na(g)) {
    g <- 0
  }
  return(g)
}


vars <- colnames(cfs.dd.labeled)
size <- length( vars)

# BestFirst search with correlation heuristic ( Mark A. Hall et. al. )
selected_features <- best.first.search( vars[1:size-1], eval.fun=heuristic, max.backtracks=15)

#######################################################


############## CREATE FORMULA BASED ON CFS ##############
f <- 'isFlaky~'
for (i in 1:length(selected_features)) {
  f <- paste(f,'`',selected_features[i],'`', sep='')
  if(i<length(selected_features)){
    f <- paste(f,'+',sep='')
  }
}
cfs_formula <- as.formula(f)
print(cfs_formula)
#########################################################




#######################################################################################
#######################################################################################
#                                       END SETUP                                     #
#######################################################################################
#######################################################################################



########### SUMMARY OF ALL PROJECTS ##############

# unique tests cases (original data set has before/after entries)
labeled_tests <- unique( dd.labeled[, c('TestCase','ProjectName','CommitHash','isFlaky')] )

flaky_tests_per_project <- labeled_tests[ isFlaky == TRUE , list(NrFlaky = .N)
                                          , by=c('ProjectName','CommitHash') ]
non_flaky_tests_per_project <- labeled_tests[ isFlaky == FALSE , list(NrNotFlaky = .N)
                                              , by=c('ProjectName','CommitHash') ]

projects_overview <- merge(flaky_tests_per_project, non_flaky_tests_per_project,
                           by=c('ProjectName','CommitHash'), all=TRUE)

projects_overview[,-'CommitHash']

####################################################



########## PROJECTS OF INTEREST ###############
projects_overview[c(4,25,34,39,55), -'CommitHash']
###############################################







###################### PCA STRUTS ########################
struts <- dd.labeled[ProjectName == 'hadoop',]

ignore <- c('V1','pass/fail','iteration','TestCase','CommitHash','before/after','ProjectName','deadlocks','name','vendor','waiting.count','pools.PS-Survivor-Space.init','pools.PS-Survivor-Space.max','heap.init','heap.max','non-heap.init','non-heap.max','pools.Compressed-Class-Space.max','pools.Code-Cache.init','pools.Code-Cache.max','pools.Metaspace.init','pools.PS-Eden-Space.init','pools.PS-Old-Gen.init','pools.PS-Old-Gen.max','total.max','total.used','pools.PS-Eden-Space.usage','pools.PS-Survivor-Space.committed','isFlaky')
select_columns <- c()

# for-loop cannot handle data.table :-(
struts <- as.data.frame(struts)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(struts)){
  x <- struts[3,i]
  if(all(struts[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
struts.pca <- prcomp(struts[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(struts$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(struts.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE)

################################################################





###################### PCA WILDFLY ##############################
wildfly <- dd.labeled[ProjectName == 'wildfly',]

select_columns <- c()

# for-loop cannot handle data.table :-(
wildfly <- as.data.frame(wildfly)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(wildfly)){
  x <- wildfly[3,i]
  if(all(wildfly[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
wildfly.pca <- prcomp(wildfly[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(wildfly$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(wildfly.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE)

#####################################################################





######################## PCA ORYX ################################3
oryx <- dd.labeled[ProjectName == 'oryx',]

select_columns <- c()

# for-loop cannot handle data.table :-(
oryx <- as.data.frame(oryx)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(oryx)){
  x <- oryx[3,i]
  if(all(oryx[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
oryx.pca <- prcomp(oryx[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(oryx$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(oryx.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE, circle=TRUE)

###################################################################################





####################### PCA JHIPSTER-REGISTRY #################################
jhipsterregistry <- dd.labeled[ProjectName == 'jhipster-registry',]

select_columns <- c()

# for-loop cannot handle data.table :-(
jhipsterregistry <- as.data.frame(jhipsterregistry)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(jhipsterregistry)){
  x <- jhipsterregistry[3,i]
  if(all(jhipsterregistry[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
jhipsterregistry.pca <- prcomp(jhipsterregistry[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(jhipsterregistry$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(jhipsterregistry.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE, circle=TRUE)

##############################################################################



################### PCA OF ALL MEASUREMENTS ###################################
# convert all collumns with numbers as numeric data type
# collect indices of NA entries -> those are corrupted data entries
all_together <- dd.labeled

select_columns <- c()

# for-loop cannot handle data.table :-(
all_together <- as.data.frame(all_together)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(all_together)){
  x <- all_together[3,i]
  if(all(all_together[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
all_together.pca <- prcomp(all_together[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(all_together$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(all_together.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE, alpha=0.1)



# down sampling
nr_non_flaky <- 120
nr_flaky <- 40

non_flaky_sample <- dd.labeled[sample( which( !dd.labeled$isFlaky), nr_non_flaky), ]
flaky_sample <- dd.labeled[sample( which( dd.labeled$isFlaky), nr_flaky), ]

all_together.downsample <- rbind(non_flaky_sample, flaky_sample)


select_columns <- c()

# for-loop cannot handle data.table :-(
all_together.downsample <- as.data.frame(all_together.downsample)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(all_together.downsample)){
  x <- all_together.downsample[3,i]
  if(all(all_together.downsample[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
all_together.downsample.pca <- prcomp(all_together.downsample[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(all_together.downsample$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(all_together.downsample.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE)

######################################################################################




###################### PCA OF IDFLAKIES TESTS ###################################
dd.idflakies <- merge( dd, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=FALSE)[, 1:73] # keep only columns of dd

nr_measure_idflakies_entries <- nrow( unique( dd.idflakies[, c('TestCase','CommitHash')]))
# proportion of measured idlfakies entries
nr_measure_idflakies_entries/nrow(idflakies)


# label idflakies data set with isFlaky
dd.idflakies$isFlaky <- TRUE

# merege dd.idflakies with all non flaky tests
dd.labeled <- as.data.frame( dd.labeled)
dd.idflakies <- as.data.frame( dd.idflakies)
tmp <-  merge( dd.labeled[ which( dd.labeled$isFlaky == FALSE),], dd.idflakies[, c('TestCase','CommitHash','isFlaky')], 
               by=c('TestCase','CommitHash'), all=TRUE, allow.cartesian=FALSE)

tmp <- unique( as.data.table(tmp))

tmp[isFlaky.y == TRUE, 'isFlaky.x'] <- TRUE
tmp$isFlaky.y <- NULL
tmp <- tmp[,1:74]
#rename variable
colnames(tmp)[length(colnames(tmp))] <- 'isFlaky'

# data set of interest (only the tests of the data set IdFlakies are marked as flaky)
idflakies_only <- na.omit(tmp)
rm(tmp)


select_columns <- c()

# for-loop cannot handle data.table :-(
idflakies_only <- as.data.frame(idflakies_only)
# PCA cannot be performed if there are vectors if uniform entries
for (i in colnames(idflakies_only)){
  x <- idflakies_only[3,i]
  if(all(idflakies_only[,i] == x) || i %in% ignore){
    next
  }
  select_columns <- c(select_columns, i)
}

# perform PCA with scaling and center (avoid missleading eigenvectors)
idflakies_only.pca <- prcomp(idflakies_only[,select_columns], center=TRUE, scale.=TRUE )

groups <- as.factor(idflakies_only$isFlaky)
levels(groups) <- c("non-flaky","flaky")
ggbiplot(idflakies_only.pca, groups=groups, ellipse=TRUE, obs.scale=TRUE, var.scale=TRUE, alpha=0.1)

##############################################################################################


###################### WITHIN PROJECTS IDFLAKIES ONLY #############################

idflakies_only <- as.data.table( idflakies_only)
tests_only <- unique( idflakies_only[, c('ProjectName','CommitHash','isFlaky')])
idflakies_only[, list(flakyMeasurements=sum(isFlaky), notflakyMeasurements=sum(!isFlaky)),
               by=c('ProjectName','CommitHash')][,c('ProjectName','flakyMeasurements','notflakyMeasurements')]

# TODO!!!!!

###################################################################################





################################################################################
#                             CLASSIFICATION MODELS                            #
################################################################################

######################## LOGISTIC REGRESSION STRUTS ################################
# STRUTS (ignore non-numeric variables)
struts <- dd.labeled[ProjectName == 'Struts', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( struts$isFlaky)
non_flaky_ind <- sample( which( !struts$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

struts_training <- struts[ union( training_ind_flaky, training_ind_non_flaky),]
struts_test <- struts[ union( test_ind_flaky, test_ind_non_flaky),]


glm.struts <- glm( isFlaky~., family=binomial, data=struts_training)
summary( glm.struts)

glm.prob <- predict( glm.struts, new=struts_test, type='response')

glm.pred <- ifelse( glm.prob>0.5, TRUE, FALSE)

# proportion of correct predictions in the test set
length( which( struts_test$isFlaky==glm.pred))/nrow( struts_test)
# proportion of wrong predictions
length( which( struts_test$isFlaky!=glm.pred))/nrow( struts_test)

#############################################################################




######################## LOGISTIC REGRESSION WILDFLY ################################
# WILDFLY (ignore non-numeric variables)
wildfly <- dd.labeled[ProjectName == 'wildfly', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( wildfly$isFlaky)
non_flaky_ind <- sample( which( !wildfly$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

wildfly_training <- wildfly[ union( training_ind_flaky, training_ind_non_flaky),]
wildfly_test <- wildfly[ union( test_ind_flaky, test_ind_non_flaky),]


glm.wildfly <- glm( isFlaky~., family=binomial, data=wildfly_training)
summary( glm.wildfly)

glm.prob <- predict( glm.wildfly, new=wildfly_test, type='response')

glm.pred <- ifelse( glm.prob>0.5, TRUE, FALSE)

# proportion of correct predictions in the test set
length( which( wildfly_test$isFlaky==glm.pred))/nrow( wildfly_test)
# proportion of wrong predictions
length( which( wildfly_test$isFlaky!=glm.pred))/nrow( wildfly_test)

#############################################################################




######################## LOGISTIC REGRESSION ORYX ################################
# ORYX (ignore non-numeric variables)
oryx <- dd.labeled[ProjectName == 'oryx', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( oryx$isFlaky)
non_flaky_ind <- sample( which( !oryx$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

oryx_training <- oryx[ union( training_ind_flaky, training_ind_non_flaky),]
oryx_test <- oryx[ union( test_ind_flaky, test_ind_non_flaky),]


glm.oryx <- glm( isFlaky~., family=binomial, data=oryx_training)
summary( glm.oryx)

glm.prob <- predict( glm.oryx, new=oryx_test, type='response')

glm.pred <- ifelse( glm.prob>0.5, TRUE, FALSE)

# proportion of correct predictions in the test set
length( which( oryx_test$isFlaky==glm.pred))/nrow( oryx_test)
# proportion of wrong predictions
length( which( oryx_test$isFlaky!=glm.pred))/nrow( oryx_test)

#############################################################################




######################## LOGISTIC REGRESSION JHIPSTER-REGISTRY ################################
# JHIPSTER-REGISTRY (ignore non-numeric variables)
jhipster <- dd.labeled[ProjectName == 'jhipster-registry', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( jhipster$isFlaky)
non_flaky_ind <- sample( which( !jhipster$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

jhipster_training <- jhipster[ union( training_ind_flaky, training_ind_non_flaky),]
jhipster_test <- jhipster[ union( test_ind_flaky, test_ind_non_flaky),]


glm.jhipster <- glm( isFlaky~., family=binomial, data=jhipster_training)
summary( glm.jhipster)

glm.prob <- predict( glm.jhipster, new=jhipster_test, type='response')

glm.pred <- ifelse( glm.prob>0.5, TRUE, FALSE)

# proportion of correct predictions in the test set
length( which( jhipster_test$isFlaky==glm.pred))/nrow( jhipster_test)
# proportion of wrong predictions
length( which( jhipster_test$isFlaky!=glm.pred))/nrow( jhipster_test)

#############################################################################





######################## LOGISTIC REGRESSION OVERALL ################################
overall <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( overall$isFlaky)
non_flaky_ind <- sample( which( !overall$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

overall_training <- overall[ union( training_ind_flaky, training_ind_non_flaky),]
overall_test <- overall[ union( test_ind_flaky, test_ind_non_flaky),]


glm.overall <- glm( isFlaky~., family=binomial, data=overall_training)
summary( glm.overall)

glm.prob <- predict( glm.overall, new=overall_test, type='response')

glm.pred <- ifelse( glm.prob>0.5, TRUE, FALSE)

# proportion of correct predictions in the test set
length( which( overall_test$isFlaky==glm.pred))/nrow( overall_test)
# proportion of wrong predictions
length( which( overall_test$isFlaky!=glm.pred))/nrow( overall_test)

#############################################################################






######################## LOGISTIC REGRESSION IDFLAKIES ################################
# set data set where only idflakies entries are labeled as flaky
idflakies.labeled <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all.x=TRUE)
idflakies.labeled$isFlaky <- FALSE
idflakies.labeled[ !is.na(URL),]$isFlaky <- TRUE
idflakies.labeled <- idflakies.labeled[, 1:74]

overall.idflakies <- idflakies.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( overall.idflakies$isFlaky)
non_flaky_ind <- sample( which( !overall.idflakies$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

overall.idflakies_training <- overall.idflakies[ union( training_ind_flaky, training_ind_non_flaky),]
overall.idflakies_test <- overall.idflakies[ union( test_ind_flaky, test_ind_non_flaky),]


glm.overall.idflakies <- glm( isFlaky~., family=binomial, data=overall.idflakies_training)
summary( glm.overall.idflakies)

glm.prob <- predict( glm.overall.idflakies, new=overall.idflakies_test, type='response')

glm.pred <- ifelse( glm.prob>0.5, TRUE, FALSE)

# proportion of correct predictions in the test set
length( which( overall.idflakies_test$isFlaky==glm.pred))/nrow( overall.idflakies_test)
# proportion of wrong predictions
length( which( overall.idflakies_test$isFlaky!=glm.pred))/nrow( overall.idflakies_test)

#############################################################################







######################## DISCRIMINATION OVERALL ################################
overall <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( overall$isFlaky)
non_flaky_ind <- sample( which( !overall$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

overall_training <- overall[ union( training_ind_flaky, training_ind_non_flaky),]
overall_test <- overall[ union( test_ind_flaky, test_ind_non_flaky),]

# lda and ignore constant columns and 'isFlaky'
overall.lda <- lda( overall_training[,-c(10,17,19,24,25,29,38,40,57,65)], grouping=as.factor(overall_training$isFlaky))

overall.pred <- predict( overall.lda, overall_test[,-c(10,17,19,24,25,29,38,40,57,65)])$class

# proportion of correct predictions in the test set
length( which( overall_test$isFlaky==overall.pred))/nrow( overall_test)
# proportion of wrong predictions
length( which( overall_test$isFlaky!=overall.pred))/nrow( overall_test)

#############################################################################








######################## DISCRIMINATION IDFLAKIES ################################
# set data set where only idflakies entries are labeled as flaky
idflakies.labeled <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all.x=TRUE)
idflakies.labeled$isFlaky <- FALSE
idflakies.labeled[ !is.na(URL),]$isFlaky <- TRUE
idflakies.labeled <- idflakies.labeled[, 1:74]

overall.idflakies <- idflakies.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( overall.idflakies$isFlaky)
non_flaky_ind <- sample( which( !overall.idflakies$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

overall.idflakies_training <- overall.idflakies[ union( training_ind_flaky, training_ind_non_flaky),]
overall.idflakies_test <- overall.idflakies[ union( test_ind_flaky, test_ind_non_flaky),]

# lda and ignore constant columns and 'isFlaky'
overall.idflakies.lda <- lda( overall.idflakies_training[,-c(10,17,19,24,25,29,38,40,57,65)], grouping=as.factor(overall.idflakies_training$isFlaky))

overall.idflakies.pred <- predict( overall.idflakies.lda, overall.idflakies_test[,-c(10,17,19,24,25,29,38,40,57,65)])$class

# proportion of correct predictions in the test set
length( which( overall.idflakies_test$isFlaky==overall.idflakies.pred))/nrow( overall.idflakies_test)
# proportion of wrong predictions
length( which( overall.idflakies_test$isFlaky!=overall.idflakies.pred))/nrow( overall.idflakies_test)

#############################################################################







######################## DISCRIMINATION STRUTS ################################
struts <- dd.labeled[ ProjectName == 'Struts', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( struts$isFlaky)
non_flaky_ind <- sample( which( !struts$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

struts_training <- struts[ union( training_ind_flaky, training_ind_non_flaky),]
struts_test <- struts[ union( test_ind_flaky, test_ind_non_flaky),]

# lda and ignore constant columns and 'isFlaky'
struts.lda <- lda( struts_training[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)], grouping=as.factor(struts_training$isFlaky))

struts.pred <- predict( struts.lda, struts_test[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)])$class

# proportion of correct predictions in the test set
length( which( struts_test$isFlaky==struts.pred))/nrow( struts_test)
# proportion of wrong predictions
length( which( struts_test$isFlaky!=struts.pred))/nrow( struts_test)

#############################################################################







######################## DISCRIMINATION WILDFLY ################################
wildfly <- dd.labeled[ ProjectName == 'wildfly', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( wildfly$isFlaky)
non_flaky_ind <- sample( which( !wildfly$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

wildfly_training <- wildfly[ union( training_ind_flaky, training_ind_non_flaky),]
wildfly_test <- wildfly[ union( test_ind_flaky, test_ind_non_flaky),]

# lda and ignore constant columns and 'isFlaky'
wildfly.lda <- lda( wildfly_training[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)], grouping=as.factor(wildfly_training$isFlaky))

wildfly.pred <- predict( wildfly.lda, wildfly_test[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)])$class

# proportion of correct predictions in the test set
length( which( wildfly_test$isFlaky==wildfly.pred))/nrow( wildfly_test)
# proportion of wrong predictions
length( which( wildfly_test$isFlaky!=wildfly.pred))/nrow( wildfly_test)

#############################################################################





######################## DISCRIMINATION ORYX ################################
oryx <- dd.labeled[ ProjectName == 'oryx', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( oryx$isFlaky)
non_flaky_ind <- sample( which( !oryx$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

oryx_training <- oryx[ union( training_ind_flaky, training_ind_non_flaky),]
oryx_test <- oryx[ union( test_ind_flaky, test_ind_non_flaky),]

# lda and ignore constant columns and 'isFlaky'
oryx.lda <- lda( oryx_training[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)], grouping=as.factor(oryx_training$isFlaky))

oryx.pred <- predict( oryx.lda, oryx_test[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)])$class

# proportion of correct predictions in the test set
length( which( oryx_test$isFlaky==oryx.pred))/nrow( oryx_test)
# proportion of wrong predictions
length( which( oryx_test$isFlaky!=oryx.pred))/nrow( oryx_test)

#############################################################################






######################## DISCRIMINATION JHIPSTER-REGISTRY ################################
jhipster <- dd.labeled[ ProjectName == 'jhipster-registry', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( jhipster$isFlaky)
non_flaky_ind <- sample( which( !jhipster$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

jhipster_training <- jhipster[ union( training_ind_flaky, training_ind_non_flaky),]
jhipster_test <- jhipster[ union( test_ind_flaky, test_ind_non_flaky),]

# lda and ignore constant columns and 'isFlaky'
jhipster.lda <- lda( jhipster_training[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)], grouping=as.factor(jhipster_training$isFlaky))

jhipster.pred <- predict( jhipster.lda, jhipster_test[,-c(7,10,12,13,17,19,20,24,25,29,30,34,35,38,40,41,45,46,51,52,57,60,61,63,65)])$class

# proportion of correct predictions in the test set
length( which( jhipster_test$isFlaky==jhipster.pred))/nrow( jhipster_test)
# proportion of wrong predictions
length( which( jhipster_test$isFlaky!=jhipster.pred))/nrow( jhipster_test)

#############################################################################











######################## REGRESSION TREE OVERALL ################################
overall <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( overall$isFlaky)
non_flaky_ind <- sample( which( !overall$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

overall_training <- overall[ union( training_ind_flaky, training_ind_non_flaky),]
overall_test <- overall[ union( test_ind_flaky, test_ind_non_flaky),]

overall_training$isFlaky <- as.factor( overall_training$isFlaky)
overall_test$isFlaky <- as.factor( overall_test$isFlaky)

overall.ctree <- ctree( isFlaky~. ,data=overall_training)

overall.pred <- predict( overall.ctree, overall_test)




# proportion of correct predictions in the test set
length( which( overall_test$isFlaky==overall.pred))/nrow( overall_test)
# proportion of wrong predictions
length( which( overall_test$isFlaky!=overall.pred))/nrow( overall_test)

#############################################################################







######################## REGRESSION TREE STRUTS ################################
struts <- dd.labeled[ProjectName=='Struts', -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]

# proportion of the training data
training_prop <- 0.8

# ensure the training and test data are equally 'balanced'
flaky_ind <- which( struts$isFlaky)
non_flaky_ind <- sample( which( !struts$isFlaky), length( flaky_ind))

# create training and test data sets
training_ind_flaky <- sample( flaky_ind, training_prop*length(flaky_ind))
training_ind_non_flaky <- sample( non_flaky_ind, training_prop*length(non_flaky_ind))
test_ind_flaky <- setdiff( flaky_ind, training_ind_flaky)
test_ind_non_flaky <- setdiff( non_flaky_ind, training_ind_non_flaky)

struts_training <- struts[ union( training_ind_flaky, training_ind_non_flaky),]
struts_test <- struts[ union( test_ind_flaky, test_ind_non_flaky),]

struts_training$isFlaky <- as.factor( struts_training$isFlaky)
struts_test$isFlaky <- as.factor( struts_test$isFlaky)

struts.ctree <- ctree( isFlaky~. ,data=struts_training)

struts.pred <- predict( struts.ctree, struts_test)




# proportion of correct predictions in the test set
length( which( struts_test$isFlaky==struts.pred))/nrow( struts_test)
# proportion of wrong predictions
length( which( struts_test$isFlaky!=struts.pred))/nrow( struts_test)

#############################################################################




######################################################################
#       CLASSIFICATION WITH FEATURE SELECTION AND CROSS-VALIDATION
######################################################################

########### MATHEWS CORRELATION COEFFICIENT ###################
get_mcc <- function(test, pred) {
  # intputs are logical arrays (isFlaky -> binary classification)
  # must be declared as numeric to avoid integer overflow (32bit)
  tp <- as.numeric(sum( pred[which(test)])) # true positives (=hits)
  tn <- as.numeric(sum( pred[which(test == FALSE)] == FALSE)) # true negatives (=correct rejections)
  fp <- as.numeric(sum( pred[which(test == FALSE)]))
  fn <- as.numeric(sum( pred[which(test)] == FALSE))
  
  mcc <- (tp*tn - fp*fn) / sqrt( (tp+fp) * (tp+fn) * (tn+fp) * (tn+fn) )
  return(mcc)
}
################################################################



######################## REGRESSION TREE OVERALL ################################
ctree_cv <- function(data, nr_fold, balance){
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  #overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  #overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  #overall <- dd.labeled[, ..cols]
  
  
  
  overall <- data
  
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    ct.mod <- ctree(isFlaky~., data=overall[train,])
    ct.pred <- predict(ct.mod, overall[test,])
    
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(ct.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(ct.pred))/sum(as.logical(ct.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(ct.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(ct.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(ct.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('ctree',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################



######################## RANDOM FOREST OVERALL ################################
random_forest_cv <- function(data, nr_fold, balance){
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  overall <- data
  
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
      cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    rf.mod <- randomForest(isFlaky~., data=overall[train,], ntree=100)
    rf.pred <- predict(rf.mod, overall[test,])
  
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(rf.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(rf.pred))/sum(as.logical(rf.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(rf.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(rf.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(rf.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('randomForest',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################






######################## GRADIENT BOOSTING OVERALL ################################
gbm_cv <- function(data, nr_fold, balance){
    
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  
  overall <- data  
    
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  #overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    gbm.mod <- gbm(isFlaky~., data=overall[train,], distribution='bernoulli', n.trees=100)
    gbm.pred <- predict(gbm.mod, overall[test,], n.trees=100, type='response')
    gbm.pred <- gbm.pred > 0.5
    
    
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(gbm.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(gbm.pred))/sum(as.logical(gbm.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(gbm.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(gbm.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(gbm.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('gbm',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################








######################## ADA BOOSTING OVERALL ################################
ada_boosting_cv <- function(data, nr_fold, balance){
  
#cols <- c(selected_features, 'isFlaky')

## overall only own identified flaky tests
#overall <- dd.labeled[, ..cols]

## overall only idflieks flaky tests
# overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
# overall[,'isFlaky'] <- FALSE
# overall[!is.na('SHA'), 'isFlaky'] <- TRUE
# overall <- dd.labeled[, ..cols]

## overall union (own identified flaky tests and flaky tests from idflakies)
# overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
# overall[!is.na('SHA'), 'isFlaky'] <- TRUE
# overall <- dd.labeled[, ..cols]

overall <- data
  
# change illegal attribute names
cols <- c()
for (var in colnames(overall)) {
  cols <- c(cols, gsub('-','.',var))
}
colnames(overall) <- cols

# shuffle data
overall <- overall[ sample(nrow(overall)),]

# balance data
if(balance) {
  flaky_ind <- which(overall$isFlaky)
  non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
  overall <- overall[c(flaky_ind,non_flaky_ind),]
}

# shuffle data
overall <- overall[ sample(nrow(overall)),]

# class label as factor
overall$isFlaky <- as.factor(overall$isFlaky)

# k-fold cross-validation
k <- nr_fold
test_size <- round(nrow(overall)/k)
cv <- lapply(1:k, function(i){
  if(i==k){
    test <- ((i-1)*test_size+1):nrow(overall)
  } else {
    test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
  }
  train <- setdiff(1:nrow(overall), test)
  
  ada.mod <- boosting(isFlaky~., data=overall[train,])
  ada.pred <- predict(ada.mod, overall[test,])$class
  
  acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(ada.pred))/nrow(overall[test,])
  prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(ada.pred))/sum(as.logical(ada.pred))
  reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(ada.pred))/sum(as.logical(overall[test,]$isFlaky))
  f1 <- 2*prec*reca/(prec+reca)
  mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(ada.pred))
  auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(ada.pred))))
  
  res <- c(acc,prec,reca,f1,mcc,auc)
  names(res) <- c('acc','prec','reca','f1','mcc','auc')
  return(res)
})
avg_acc <- mean(unlist(cv)[seq(1,60,6)])
avg_prec <- mean(unlist(cv)[seq(2,60,6)])
avg_reca <- mean(unlist(cv)[seq(3,60,6)])
avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
avg_auc <- mean(unlist(cv)[seq(6,60,6)])

print(paste('Avg. Accuracy: ',avg_acc))
print(paste('Avg. Precision: ',avg_prec))
print(paste('Avg. Recall: ',avg_reca))
print(paste('Avg. F1: ',avg_f1))
print(paste('Avg. MCC: ',avg_mcc))
print(paste('Avg. AUC: ',avg_auc))

classification_results <- cbind('adaBoost',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
return(classification_results)
}
#############################################################################









######################## EXTREME GRADIENT BOOSTING OVERALL ################################
xg_boosting_cv <- function(data, nr_fold, balance){
    
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  # change illegal attribute names
  
  overall <- data
    
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as numeric
  overall$isFlaky <- as.numeric(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    xgb.mod <- xgboost(data=data.matrix(overall[train, -'isFlaky']), label=overall[train,]$isFlaky, nrounds=100, objective='binary:logistic')
    xgb.pred <- predict(xgb.mod, data.matrix(overall[test, -'isFlaky'])) > 0.5
    
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(xgb.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(xgb.pred))/sum(as.logical(xgb.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(xgb.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(xgb.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(xgb.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('xgBoost',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################









######################## NAIVE BAYES OVERALL ################################
naive_bayes_cv <- function(data, nr_fold, balance){
    
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  overall <- data
    
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    nb.mod <- naive_bayes(isFlaky~., data=overall[train,], usekernel=TRUE)
    nb.pred <- predict(nb.mod, overall[test,-'isFlaky'])
    
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(nb.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(nb.pred))/sum(as.logical(nb.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(nb.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(nb.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(nb.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('naiveBayes',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################








######################## LOGISTIC REGRESSION OVERALL ################################
logistic_regression_cv <- function(data, nr_fold, balance){
  
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  # Logistic regression needs much more features than those proposed from the CFS algorithm !!!!!
  #overall <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  overall <- data
    
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    lr.mod <- glm(isFlaky~., data=overall[train,], family='binomial')
    lr.pred <- predict(lr.mod, overall[test,], type='response')
    lr.pred <- ifelse( lr.pred>0.5, TRUE, FALSE)
    
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(lr.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(lr.pred))/sum(as.logical(lr.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(lr.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(lr.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(lr.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('logReg',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################




######################## LDA OVERALL ################################
lda_cv <- function(data, nr_fold, balance){
  
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  # Logistic regression needs much more features than those proposed from the CFS algorithm !!!!!
  #overall <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]
  #overall <- overall[,-c(10,17,19,24,25,29,38,40,57,65)] # remove more features to get better performance; CFS is not sufficient!!!!
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  overall <- data
    
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    lda.mod <- lda(overall[train,-'isFlaky'], grouping=overall[train,]$isFlaky)
    lda.pred <- predict(lda.mod, overall[test,-'isFlaky'])$class
  
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(lda.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(lda.pred))/sum(as.logical(lda.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(lda.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(lda.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(lda.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('lda',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################




######################## QDA OVERALL ################################
qda_cv <- function(data, nr_fold, balance){
  
  #cols <- c(selected_features, 'isFlaky')
  
  ## overall only own identified flaky tests
  # Logistic regression needs much more features than those proposed from the CFS algorithm !!!!!
  #overall <- dd.labeled[, -c('TestCase','ProjectName','CommitHash','pass/fail','before/after','deadlocks','name','vendor','waiting.count')]
  #overall <- overall[,-c(10,17,19,24,25,29,38,40,57,65)] # remove more features to get better performance; CFS is not sufficient!!!!
  #overall <- dd.labeled[, ..cols]
  
  ## overall only idflieks flaky tests
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[,'isFlaky'] <- FALSE
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  ## overall union (own identified flaky tests and flaky tests from idflakies)
  # overall <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all=TRUE)
  # overall[!is.na('SHA'), 'isFlaky'] <- TRUE
  # overall <- dd.labeled[, ..cols]
  
  overall <- data
    
  # change illegal attribute names
  cols <- c()
  for (var in colnames(overall)) {
    cols <- c(cols, gsub('-','.',var))
  }
  colnames(overall) <- cols
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # balance data
  if(balance) {
    flaky_ind <- which(overall$isFlaky)
    non_flaky_ind <- sample( setdiff(1:nrow(overall), flaky_ind), length(flaky_ind))
    overall <- overall[c(flaky_ind,non_flaky_ind),]
  }
  
  # shuffle data
  overall <- overall[ sample(nrow(overall)),]
  
  # class label as factor
  overall$isFlaky <- as.factor(overall$isFlaky)
  
  # k-fold cross-validation
  k <- nr_fold
  test_size <- round(nrow(overall)/k)
  cv <- lapply(1:k, function(i){
    if(i==k){
      test <- ((i-1)*test_size+1):nrow(overall)
    } else {
      test <- ((i-1)*test_size+1):((i-1)*test_size+test_size)
    }
    train <- setdiff(1:nrow(overall), test)
    
    qda.mod <- qda(overall[train,-'isFlaky'], grouping=overall[train,]$isFlaky)
    qda.pred <- predict(qda.mod, overall[test,-'isFlaky'])$class
    
    acc <- sum(as.logical(overall[test,]$isFlaky)==as.logical(qda.pred))/nrow(overall[test,])
    prec <- sum(as.logical(overall[test,]$isFlaky) & as.logical(qda.pred))/sum(as.logical(qda.pred))
    reca <- sum(as.logical(overall[test,]$isFlaky) & as.logical(qda.pred))/sum(as.logical(overall[test,]$isFlaky))
    f1 <- 2*prec*reca/(prec+reca)
    mcc <- get_mcc( as.logical(overall[test,]$isFlaky), as.logical(qda.pred))
    auc <- pROC::auc( roc( as.numeric(as.logical(overall[test,]$isFlaky)), as.numeric(as.logical(qda.pred))))
    
    res <- c(acc,prec,reca,f1,mcc,auc)
    names(res) <- c('acc','prec','reca','f1','mcc','auc')
    return(res)
  })
  avg_acc <- mean(unlist(cv)[seq(1,60,6)])
  avg_prec <- mean(unlist(cv)[seq(2,60,6)])
  avg_reca <- mean(unlist(cv)[seq(3,60,6)])
  avg_f1 <- mean(unlist(cv)[seq(4,60,6)])
  avg_mcc <- mean(unlist(cv)[seq(5,60,6)])
  avg_auc <- mean(unlist(cv)[seq(6,60,6)])
  
  print(paste('Avg. Accuracy: ',avg_acc))
  print(paste('Avg. Precision: ',avg_prec))
  print(paste('Avg. Recall: ',avg_reca))
  print(paste('Avg. F1: ',avg_f1))
  print(paste('Avg. MCC: ',avg_mcc))
  print(paste('Avg. AUC: ',avg_auc))
  
  classification_results <- cbind('qda',avg_acc,avg_prec,avg_reca,avg_f1,avg_mcc,avg_auc)
  colnames(classification_results) <- c('model','acc','prec','reca','f1','mcc','auc')
  return(classification_results)
}
#############################################################################


get_numeric_cols <- function(data) {
  cl <- sapply(data, class)
  nums <- which( cl == 'numeric' | cl == 'integer' | cl == 'logical')
  return(nums)
}


######## CREATE 3 MAJOR DATA SETS ##############################
cfs <- c(selected_features, 'isFlaky')
nums <- get_numeric_cols(dd.labeled) # all numeric columns

overall.own <- dd.labeled


overall.idflakies <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all.x=TRUE)
flaky_ind <- which(!is.na(overall.idflakies$Version))
non_flaky_ind <- setdiff(1:nrow(overall.idflakies), flaky_ind)
#non_flaky_ind <- sample(non_flaky_ind, 60*length(flaky_ind))
overall.idflakies[flaky_ind,]$isFlaky <- TRUE
overall.idflakies[-flaky_ind,]$isFlaky <- FALSE
overall.idflakies <- overall.idflakies[union(flaky_ind,non_flaky_ind),1:73]
overall.idflakies <- na.omit(overall.idflakies)


overall.union <- merge( dd.labeled, idflakies, by.x=c('TestCase','CommitHash'), by.y=c('Test Name','SHA'), all.x=TRUE)
flaky_ind <- which(!is.na(overall.union$Version))
overall.union[flaky_ind,]$isFlaky <- TRUE
overall.union <- overall.union[,1:73]
overall.union <- na.omit(overall.union)
################################################################


# output file
prefix <- 'vm-9'
output_folder <- '~/Desktop/analysis-results'


########## overall.own CFS and BALANED ####################
res <- ctree_cv(overall.own[,..cfs], 10, TRUE)
res <- rbind(res, random_forest_cv(overall.own[,..cfs], 10, TRUE))
res <- rbind(res, gbm_cv(overall.own[,..cfs], 10, TRUE))
#res <- rbind(res, ada_boosting_cv(overall.own[,..cfs], 10, TRUE))
res <- rbind(res, xg_boosting_cv(overall.own[,..cfs], 10, TRUE))
res <- rbind(res, naive_bayes_cv(overall.own[,..cfs], 10, TRUE))
res <- rbind(res, logistic_regression_cv(overall.own[,..cfs], 10, TRUE))
res <- rbind(res, lda_cv(overall.own[,..cfs], 10, TRUE))
res <- rbind(res, qda_cv(overall.own[,..cfs], 10, TRUE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-cfs-balanced-overall-own.csv',sep = ''))


########## overall.own CFS and NOT BALANED ####################
res <- ctree_cv(overall.own[,..cfs], 10, FALSE)
#res <- rbind(res, random_forest_cv(overall.own[,..cfs], 10, FALSE))
res <- rbind(res, gbm_cv(overall.own[,..cfs], 10, FALSE))
#res <- rbind(res, ada_boosting_cv(overall.own[,..cfs], 10, FALSE))
res <- rbind(res, xg_boosting_cv(overall.own[,..cfs], 10, FALSE))
res <- rbind(res, naive_bayes_cv(overall.own[,..cfs], 10, FALSE))
res <- rbind(res, logistic_regression_cv(overall.own[,..cfs], 10, FALSE))
res <- rbind(res, lda_cv(overall.own[,..cfs], 10, FALSE))
res <- rbind(res, qda_cv(overall.own[,..cfs], 10, FALSE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-cfs-not_balanced-overall-own.csv',sep = ''))


########## overall.own NO CFS and BALANED ####################
res <- ctree_cv(overall.own[,..nums], 10, TRUE)
res <- rbind(res, random_forest_cv(overall.own[,..nums], 10, TRUE))
res <- rbind(res, gbm_cv(overall.own[,..nums], 10, TRUE))
#res <- rbind(res, ada_boosting_cv(overall.own[,..nums], 10, TRUE))
res <- rbind(res, xg_boosting_cv(overall.own[,..nums], 10, TRUE))
res <- rbind(res, naive_bayes_cv(overall.own[,..nums], 10, TRUE))
res <- rbind(res, logistic_regression_cv(overall.own[,..nums], 10, TRUE))
res <- rbind(res, lda_cv(overall.own[,..nums], 10, TRUE))
res <- rbind(res, qda_cv(overall.own[,..nums], 10, TRUE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-no_cfs-balanced-overall-own.csv',sep = ''))


########## overall.own NO CFS and NOT BALANED ####################
#res <- ctree_cv(overall.own[,..nums], 10, FALSE)
#res <- rbind(res, random_forest_cv(overall.own[,..nums], 10, FALSE))
res <- gbm_cv(overall.own[,..nums], 10, FALSE)
#res <- rbind(res, ada_boosting_cv(overall.own[,..nums], 10, FALSE))
res <- rbind(res, xg_boosting_cv(overall.own[,..nums], 10, FALSE))
res <- rbind(res, naive_bayes_cv(overall.own[,..nums], 10, FALSE))
#res <- rbind(res, logistic_regression_cv(overall.own[,..nums], 10, FALSE))
res <- rbind(res, lda_cv(overall.own[,..nums], 10, FALSE))
res <- rbind(res, qda_cv(overall.own[,..nums], 10, FALSE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-no_cfs-not_balanced-overall-own.csv',sep = ''))






#############################################################
#############################################################






########## overall.idflakies CFS and BALANED ####################
res <- ctree_cv(overall.idflakies[,..cfs], 10, TRUE)
res <- rbind(res, random_forest_cv(overall.idflakies[,..cfs], 10, TRUE))
res <- rbind(res, gbm_cv(overall.idflakies[,..cfs], 10, TRUE))
#res <- rbind(res, ada_boosting_cv(overall.idflakies[,..cfs], 10, TRUE))
res <- rbind(res, xg_boosting_cv(overall.idflakies[,..cfs], 10, TRUE))
res <- rbind(res, naive_bayes_cv(overall.idflakies[,..cfs], 10, TRUE))
res <- rbind(res, logistic_regression_cv(overall.idflakies[,..cfs], 10, TRUE))
res <- rbind(res, lda_cv(overall.idflakies[,..cfs], 10, TRUE))
res <- rbind(res, qda_cv(overall.idflakies[,..cfs], 10, TRUE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-cfs-balanced-overall-idflakies.csv',sep = ''))


########## overall.idflakies CFS and NOT BALANED ####################
res <- ctree_cv(overall.idflakies[,..cfs], 10, FALSE)
#res <- rbind(res, random_forest_cv(overall.idflakies[,..cfs], 10, FALSE))
res <- rbind(res, gbm_cv(overall.idflakies[,..cfs], 10, FALSE))
#res <- rbind(res, ada_boosting_cv(overall.idflakies[,..cfs], 10, FALSE))
res <- rbind(res, xg_boosting_cv(overall.idflakies[,..cfs], 10, FALSE))
res <- rbind(res, naive_bayes_cv(overall.idflakies[,..cfs], 10, FALSE))
res <- rbind(res, logistic_regression_cv(overall.idflakies[,..cfs], 10, FALSE))
res <- rbind(res, lda_cv(overall.idflakies[,..cfs], 10, FALSE))
res <- rbind(res, qda_cv(overall.idflakies[,..cfs], 10, FALSE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-cfs-not_balanced-overall-idflakies.csv',sep = ''))


########## overall.idflakies NO CFS and BALANED ####################
non_const <- c(6:13,15:20,22:25,27:29,31:69,71:72)
lda_qda <- setdiff(non_const, c(6,7,8,11,13,14,15,16,17,19,21,25,29,31,36,44,47,53))
res <- ctree_cv(overall.idflakies[,..non_const], 10, TRUE)
res <- rbind(res, random_forest_cv(overall.idflakies[,..non_const], 10, TRUE))
res <- rbind(res, gbm_cv(overall.idflakies[,..non_const], 10, TRUE))
#res <- rbind(res, ada_boosting_cv(overall.idflakies[,..nums], 10, TRUE))
res <- rbind(res, xg_boosting_cv(overall.idflakies[,..non_const], 10, TRUE))
res <- rbind(res, naive_bayes_cv(overall.idflakies[,..non_const], 10, TRUE))
res <- rbind(res, logistic_regression_cv(overall.idflakies[,..non_const], 10, TRUE))
#res <- rbind(res, lda_cv(overall.idflakies[,..lda_qda], 10, TRUE))
#res <- rbind(res, qda_cv(overall.idflakies[,..non_const], 10, TRUE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-no_cfs-balanced-overall-idflakies.csv',sep = ''))


########## overall.idflakies NO CFS and NOT BALANED ####################
res <- ctree_cv(overall.idflakies[,..non_const], 10, FALSE)
#res <- rbind(res, random_forest_cv(overall.idflakies[,..nums], 10, FALSE))
#res <- rbind(res, gbm_cv(overall.idflakies[,..nums], 10, FALSE))
#res <- rbind(res, ada_boosting_cv(overall.idflakies[,..nums], 10, FALSE))
res <- rbind(res, xg_boosting_cv(overall.idflakies[,..nums], 10, FALSE))
res <- rbind(res, naive_bayes_cv(overall.idflakies[,..nums], 10, FALSE))
#res <- rbind(res, logistic_regression_cv(overall.idflakies[,..nums], 10, FALSE))
#res <- rbind(res, lda_cv(overall.idflakies[,..nums], 10, FALSE))
#res <- rbind(res, qda_cv(overall.idflakies[,..nums], 10, FALSE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-no_cfs-not_balanced-overall-idflakies.csv',sep = ''))



#############################################################
#############################################################





########## overall.union CFS and BALANED ####################
res <- ctree_cv(overall.union[,..cfs], 10, TRUE)
res <- rbind(res, random_forest_cv(overall.union[,..cfs], 10, TRUE))
res <- rbind(res, gbm_cv(overall.union[,..cfs], 10, TRUE))
#res <- rbind(res, ada_boosting_cv(overall.union[,..cfs], 10, TRUE))
res <- rbind(res, xg_boosting_cv(overall.union[,..cfs], 10, TRUE))
res <- rbind(res, naive_bayes_cv(overall.union[,..cfs], 10, TRUE))
res <- rbind(res, logistic_regression_cv(overall.union[,..cfs], 10, TRUE))
res <- rbind(res, lda_cv(overall.union[,..cfs], 10, TRUE))
res <- rbind(res, qda_cv(overall.union[,..cfs], 10, TRUE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-cfs-balanced-overall-union.csv',sep = ''))


########## overall.union CFS and NOT BALANED ####################
res <- ctree_cv(overall.union[,..cfs], 10, FALSE)
#res <- rbind(res, random_forest_cv(overall.union[,..cfs], 10, FALSE))
res <- rbind(res, gbm_cv(overall.union[,..cfs], 10, FALSE))
#res <- rbind(res, ada_boosting_cv(overall.union[,..cfs], 10, FALSE))
res <- rbind(res, xg_boosting_cv(overall.union[,..cfs], 10, FALSE))
res <- rbind(res, naive_bayes_cv(overall.union[,..cfs], 10, FALSE))
res <- rbind(res, logistic_regression_cv(overall.union[,..cfs], 10, FALSE))
res <- rbind(res, lda_cv(overall.union[,..cfs], 10, FALSE))
res <- rbind(res, qda_cv(overall.union[,..cfs], 10, FALSE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-cfs-not_balanced-overall-union.csv',sep = ''))


########## overall.union NO CFS and BALANED ####################
res <- ctree_cv(overall.union[,..nums], 10, TRUE)
res <- rbind(res, random_forest_cv(overall.union[,..nums], 10, TRUE))
res <- rbind(res, gbm_cv(overall.union[,..nums], 10, TRUE))
#res <- rbind(res, ada_boosting_cv(overall.union[,..nums], 10, TRUE))
res <- rbind(res, xg_boosting_cv(overall.union[,..nums], 10, TRUE))
res <- rbind(res, naive_bayes_cv(overall.union[,..nums], 10, TRUE))
res <- rbind(res, logistic_regression_cv(overall.union[,..nums], 10, TRUE))
res <- rbind(res, lda_cv(overall.union[,..nums], 10, TRUE))
res <- rbind(res, qda_cv(overall.union[,..nums], 10, TRUE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-no_cfs-balanced-overall-union.csv',sep = ''))


########## overall.union NO CFS and NOT BALANED ####################
res <- ctree_cv(overall.union[,..nums], 10, FALSE)
#res <- rbind(res, random_forest_cv(overall.union[,..nums], 10, FALSE))
#res <- rbind(res, gbm_cv(overall.union[,..nums], 10, FALSE))
#res <- rbind(res, ada_boosting_cv(overall.union[,..nums], 10, FALSE))
res <- rbind(res, xg_boosting_cv(overall.union[,..nums], 10, FALSE))
res <- rbind(res, naive_bayes_cv(overall.union[,..nums], 10, FALSE))
#res <- rbind(res, logistic_regression_cv(overall.union[,..nums], 10, FALSE))
res <- rbind(res, lda_cv(overall.union[,..nums], 10, FALSE))
res <- rbind(res, qda_cv(overall.union[,..nums], 10, FALSE))
###########################################################

write.csv(res, paste(output_folder,'/',prefix,'-no_cfs-not_balanced-overall-union.csv',sep = ''))


