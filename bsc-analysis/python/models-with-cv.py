import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import accuracy_score, roc_auc_score, recall_score, matthews_corrcoef, f1_score, precision_score
from sklearn.naive_bayes import GaussianNB
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis

###############################################################################

def add_scores_to_dict(scores_dict, y_true, y_pred): 
    y_true = np.array(y_true).astype(np.int)
    y_pred = np.array(y_pred).astype(np.int)
    
    scores_dict['acc'].append(accuracy_score(y_true, y_pred))
    scores_dict['prec'].append(precision_score(y_true, y_pred, pos_label=1, labels=[1, 0], zero_division=1))
    scores_dict['reca'].append(recall_score(y_true, y_pred, pos_label=1, labels=[1, 0], zero_division=1))
    scores_dict['f1'].append(f1_score(y_true, y_pred, pos_label=1, labels=[1, 0], zero_division=1))
    scores_dict['mcc'].append(matthews_corrcoef(y_true, y_pred))
    scores_dict['auc'].append(roc_auc_score(y_true, y_pred, labels=[1, 0]))


def xgboost_cv_scores(X, y):
    print("XG Boost")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        xg_classifier = xgb.XGBClassifier(nthread=NR_THREADS)
        xg_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = xg_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores
    
    
def ctree_cv_scores(X, y):
    print("Classification tree")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        ctree_classifier = DecisionTreeClassifier()
        ctree_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = ctree_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores
    

def rforest_cv_scores(X, y):
    print("Random forest")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        rforest_classifier = RandomForestClassifier()
        rforest_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = rforest_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def logreg_cv_scores(X, y):
    print("Logistic Regression")
    scaler = MinMaxScaler()
    X_scaled = scaler.fit_transform(X)
    X = pd.DataFrame(X_scaled)

    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        logreg_classifier = LogisticRegression(max_iter=1000)
        logreg_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = logreg_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def adaboost_cv_scores(X, y):
    print("Ada Boost")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        adaboost_classifier = AdaBoostClassifier()
        adaboost_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = adaboost_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def gnbayes_cv_scores(X, y):
    print("Gaussian Naive Bayes")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        gnb_classifier = GaussianNB()
        gnb_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = gnb_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def gboost_cv_scores(X, y):
    print("Gradient Boost")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        gboost_classifier = GradientBoostingClassifier()
        gboost_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = gboost_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def lda_cv_scores(X, y):
    print("LDA")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        lda_classifier = LinearDiscriminantAnalysis()
        lda_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = lda_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def qda_cv_scores(X, y):
    print("QDA")
    scores = {'acc': [], 'prec': [], 'reca': [], 'f1': [], 'mcc': [], 'auc': []}
    kf = StratifiedKFold(n_splits=10)
    fold_nr = 0
    for train, test in kf.split(X, y):
        fold_nr += 1
        print("k="+str(fold_nr))
        
        # split data
        X_train, X_test, y_train, y_test = X.iloc[train], X.iloc[test], y.iloc[train], y.iloc[test]
    
        # fit model
        qda_classifier = QuadraticDiscriminantAnalysis()
        qda_classifier.fit(X_train, y_train)
        
        # make prediction
        y_pred = qda_classifier.predict(X_test)
        
        # add scores
        add_scores_to_dict(scores, y_test, y_pred)
        
    return scores


def create_summary_df(lda_scores, qda_scores, gnbayes_scores, logreg_scores,
                               ctree_scores, rforest_scores, adaboost_scores, xgboost_scores):
    
    d = {}
    
    acc = []
    acc.append(np.mean(lda_scores['acc']))
    acc.append(np.mean(qda_scores['acc']))
    acc.append(np.mean(gnbayes_scores['acc']))
    acc.append(np.mean(logreg_scores['acc']))
    acc.append(np.mean(ctree_scores['acc']))
    acc.append(np.mean(rforest_scores['acc']))
    #acc.append(np.mean(gboost_scores['acc']))
    acc.append(np.mean(adaboost_scores['acc']))
    acc.append(np.mean(xgboost_scores['acc']))
    
    d['acc'] = acc
    
    prec = []
    prec.append(np.mean(lda_scores['prec']))
    prec.append(np.mean(qda_scores['prec']))
    prec.append(np.mean(gnbayes_scores['prec']))
    prec.append(np.mean(logreg_scores['prec']))
    prec.append(np.mean(ctree_scores['prec']))
    prec.append(np.mean(rforest_scores['prec']))
   # prec.append(np.mean(gboost_scores['prec']))
    prec.append(np.mean(adaboost_scores['prec']))
    prec.append(np.mean(xgboost_scores['prec']))
    
    d['prec'] = prec
    
    reca = []
    reca.append(np.mean(lda_scores['reca']))
    reca.append(np.mean(qda_scores['reca']))
    reca.append(np.mean(gnbayes_scores['reca']))
    reca.append(np.mean(logreg_scores['reca']))
    reca.append(np.mean(ctree_scores['reca']))
    reca.append(np.mean(rforest_scores['reca']))
    #reca.append(np.mean(gboost_scores['reca']))
    reca.append(np.mean(adaboost_scores['reca']))
    reca.append(np.mean(xgboost_scores['reca']))
    
    d['reca'] = reca
    
    f1 = []
    f1.append(np.mean(lda_scores['f1']))
    f1.append(np.mean(qda_scores['f1']))
    f1.append(np.mean(gnbayes_scores['f1']))
    f1.append(np.mean(logreg_scores['f1']))
    f1.append(np.mean(ctree_scores['f1']))
    f1.append(np.mean(rforest_scores['f1']))
    #f1.append(np.mean(gboost_scores['f1']))
    f1.append(np.mean(adaboost_scores['f1']))
    f1.append(np.mean(xgboost_scores['f1']))
    
    d['f1'] = f1
    
    mcc = []
    mcc.append(np.mean(lda_scores['mcc']))
    mcc.append(np.mean(qda_scores['mcc']))
    mcc.append(np.mean(gnbayes_scores['mcc']))
    mcc.append(np.mean(logreg_scores['mcc']))
    mcc.append(np.mean(ctree_scores['mcc']))
    mcc.append(np.mean(rforest_scores['mcc']))
    #mcc.append(np.mean(gboost_scores['mcc']))
    mcc.append(np.mean(adaboost_scores['mcc']))
    mcc.append(np.mean(xgboost_scores['mcc']))
    
    d['mcc'] = mcc
    
    auc = []
    auc.append(np.mean(lda_scores['auc']))
    auc.append(np.mean(qda_scores['auc']))
    auc.append(np.mean(gnbayes_scores['auc']))
    auc.append(np.mean(logreg_scores['auc']))
    auc.append(np.mean(ctree_scores['auc']))
    auc.append(np.mean(rforest_scores['auc']))
    #auc.append(np.mean(gboost_scores['auc']))
    auc.append(np.mean(adaboost_scores['auc']))
    auc.append(np.mean(xgboost_scores['auc']))
    
    d['auc'] = auc
    
    summary_df = pd.DataFrame(data=d, index=['lda','qda','gnbayes','logreg','ctree','rforest','adaboost','xgboost'])
    
    return summary_df


def perform_cv_on_several_classifiers(X, y, out_file=None):
    # parametric models
    lda_scores = lda_cv_scores(X, y)
    qda_scores = qda_cv_scores(X, y)
    gnbayes_scores = gnbayes_cv_scores(X, y)
    logreg_scores = logreg_cv_scores(X, y)
    
    # non-parametric models
    ctree_scores = ctree_cv_scores(X, y)
    rforest_scores = rforest_cv_scores(X, y)
    #gboost_scores = gboost_cv_scores(X, y)
    adaboost_scores = adaboost_cv_scores(X, y)
    xgboost_scores = xgboost_cv_scores(X, y)
    
    # keep the order in the parameter list!!!
    df_summary = create_summary_df(lda_scores, qda_scores, gnbayes_scores, logreg_scores,
                                   ctree_scores, rforest_scores, adaboost_scores, xgboost_scores)
    
    if out_file != None:
        df_summary.to_csv(out_file)

    print(df_summary)

###############################################################################

# VM = "vm9"
# DATADIR = "~/Desktop/DIFF/"+VM+"/"
# OUTDIR="~/Desktop/DIFF/"+VM+"/"

# FILE_OWN = "overall_own_iter_diff.csv"
# FILE_IDFLAKIES = "overall_idflakies_iter_diff.csv"
# FILE_UNION = "overall_union_iter_diff.csv"

# FILES = [FILE_OWN, FILE_IDFLAKIES, FILE_UNION]
# PREFIXS = ['own_diff', 'idflakies_diff', 'union_diff']

# cfs_diff_vm_nine = [['PS-MarkSweep.time_diff','timed_waiting.count_diff','heap.max','pools.PS-Old-Gen.max','pools.PS-Survivor-Space.max','total.max'],
#                     ['PS-MarkSweep.time_diff','waiting.count_diff','heap.max','pools.PS-Eden-Space.usage','pools.PS-Survivor-Space.max','total.max'],
#                     ['PS-MarkSweep.time_diff','count_diff','heap.max','pools.PS-Eden-Space.usage','pools.PS-Survivor-Space.max','total.max']]

# cfs_diff_vm_eleven = [['PS-MarkSweep.time_diff','pools.PS-Survivor-Space.committed_diff','pools.PS-Survivor-Space.used-after-gc_diff','PS-MarkSweep.count','pools.PS-Old-Gen.max'],
#                       ['PS-MarkSweep.time_diff','daemon.count_diff','heap.used_diff','pools.PS-Eden-Space.max_diff','waiting.count_diff','non-heap.max','pools.Metaspace.max','pools.PS-Old-Gen.max','pools.PS-Survivor-Space.init'],
#                       ['PS-MarkSweep.time_diff','daemon.count_diff','heap.used_diff','pools.PS-Eden-Space.max_diff','waiting.count_diff','non-heap.max','pools.Metaspace.max','pools.PS-Old-Gen.max','pools.PS-Survivor-Space.init']]




# NR_THREADS = 1



# for i in range(len(FILES)):

#     dd = pd.read_csv(DATADIR+FILES[i])
#     try:
#         dd = dd.drop(columns=["Unnamed: 0", 'iteration'])
#     except:
#         pass
#     tmp = dd['isFlaky']
#     dd = dd.iloc[:,:-1].select_dtypes(['number'])
#     dd['isFlaky'] = tmp

#     # shuffle
#     dd = dd.sample(frac=1).reset_index(drop=True)
    
#     nr_flak_own = len(dd.loc[ dd["isFlaky"] == True, "isFlaky" ])
#     nr_non_flak_own = len(dd.loc[ dd["isFlaky"] == False, "isFlaky" ])
    
    
#     flaky_model_ind = np.array(dd[ dd["isFlaky"] == True ].index)
#     non_flaky_model_ind = np.array(dd[ dd["isFlaky"] == False ].index)
    
#     # down sampling of non-flaky data
#     non_flaky_model_ind_balanced = np.random.choice(non_flaky_model_ind, size=nr_flak_own, replace=False)
    
#     # data indices for model fitting (balanced data and non-balanced data)
#     model_ind = np.concatenate((flaky_model_ind, non_flaky_model_ind))
#     model_ind_balanced = np.concatenate((flaky_model_ind, non_flaky_model_ind_balanced))
    
    
#     ###############################################################################
    
#     # CFS was done within R since there are no appropriate api for python
#     # VM9
#     #cfs_col = ["PS-MarkSweep.count", "pools.PS-Survivor-Space.max", "runnable.count", "timed_waiting.count"]
#     cfs_col = cfs_diff_vm_nine[i]

#     # VM11
#     #cfs_col = ["PS-MarkSweep.count", "heap.committed", "heap.used", "pools.PS-Eden-Space.committed"]
#     #cfs_col = cfs_diff_vm_eleven[i]

#     # non-balanced data
#     X, y = dd.iloc[model_ind,:-1], dd.iloc[model_ind,-1]
    
#     # balanced data
#     X_bal, y_bal = dd.iloc[model_ind_balanced,:-1], dd.iloc[model_ind_balanced,-1]
    
#     # cfs non-balanced data
#     X_cfs, y_cfs = dd.loc[model_ind, cfs_col], dd.iloc[model_ind,-1]
    
#     # cfs balanced data
#     X_cfs_bal, y_cfs_bal = dd.loc[model_ind_balanced, cfs_col], dd.iloc[model_ind_balanced,-1]
    
#     ###############################################################################
    
#     # no cfs and no balancing
#     perform_cv_on_several_classifiers(X, y, out_file=OUTDIR+VM+"-"+PREFIXS[i]+"-no_cfs-no_bal.csv")
    
#     # no cfs with balancing
#     perform_cv_on_several_classifiers(X_bal, y_bal, out_file=OUTDIR+VM+"-"+PREFIXS[i]+"-no_cfs-bal.csv")
    
#     # cfs no balancing
#     perform_cv_on_several_classifiers(X_cfs, y_cfs, out_file=OUTDIR+VM+"-"+PREFIXS[i]+"-cfs-no_bal.csv")
    
#     # cfs balancing
#     perform_cv_on_several_classifiers(X_cfs_bal, y_cfs_bal, out_file=OUTDIR+VM+"-"+PREFIXS[i]+"-cfs-bal.csv")









# ###############################################################################
# ###############################################################################
# # THIS BLOCK IS THE CONCATONATED ANALYSIS OF VM9 (WEAK) AND VM11 (STRONG)
# ###############################################################################
# ###############################################################################

OUTDIR = "~/Desktop/out"
NR_THREADS = 1

# load data of vm 9 and 11 with iDFlakies information
df_weak = pd.read_csv('~/Desktop/bsc-analysis/python/labeled-data-sets/vm9/overall_union_iter_diff.csv')
df_strong = pd.read_csv('~/Desktop/bsc-analysis/python/labeled-data-sets/vm11/overall_union_iter_diff.csv')

try:
    df_weak = df_weak.drop(columns=["Unnamed: 0"])
    df_strong = df_strong.drop(columns=["Unnamed: 0"])
except:
    pass
df_concat = pd.concat([df_weak, df_strong], ignore_index=True, sort=False)
df_concat = df_concat.dropna(axis=1)

# common flaky test cases (almost identical!)
df_flaky_weak = df_weak.loc[ df_weak['isFlaky'] == True, ['TestCase','CommitHash','ProjectName'] ].drop_duplicates(ignore_index=True)
df_flaky_strong = df_strong.loc[ df_strong['isFlaky'] == True, ['TestCase','CommitHash','ProjectName'] ].drop_duplicates(ignore_index=True)
df_flaky_concat = pd.concat([df_flaky_weak, df_flaky_strong], ignore_index=True, sort=False).drop_duplicates(ignore_index=True)

df_flaky_concat['dummy'] = True

# measurements regarded as flaky now
df_concat = pd.merge(df_concat, df_flaky_concat, how='left', on=['TestCase','CommitHash','ProjectName'], sort=False)
df_concat.loc[ df_concat['dummy'] == True, 'isFlaky' ] = True
df_concat = df_concat.drop('dummy', axis=1)


# CFS of the df_concat calculate in R
cfs_col = ['PS-MarkSweep.time_diff','blocked.count_diff','daemon.count_diff','heap.used_diff','pools.Code-Cache.committed_diff','waiting.count_diff','non-heap.max','pools.PS-Old-Gen.max']


# CFS of the df_concat calculate in R (for non diff data sets)
#cfs_col = ['PS-MarkSweep.count','blocked.count','heap.committed','heap.max','heap.used','pools.PS-Eden-Space.committed','pools.PS-Old-Gen.committed','pools.PS-Old-Gen.max','pools.PS-Survivor-Space.max','total.max']


# take only numeric columns and the response (isFlaky)
tmp = df_concat['isFlaky']
df_concat = df_concat.iloc[:,4:-1].select_dtypes(['number'])
df_concat['isFlaky'] = tmp

# shuffle
df_concat = df_concat.sample(frac=1).reset_index(drop=True)

nr_flak_own = len(df_concat.loc[ df_concat["isFlaky"] == True, "isFlaky" ])
nr_non_flak_own = len(df_concat.loc[ df_concat["isFlaky"] == False, "isFlaky" ])


flaky_model_ind = np.array(df_concat[ df_concat["isFlaky"] == True ].index)
non_flaky_model_ind = np.array(df_concat[ df_concat["isFlaky"] == False ].index)

# down sampling of non-flaky data
non_flaky_model_ind_balanced = np.random.choice(non_flaky_model_ind, size=nr_flak_own, replace=False)

# data indices for model fitting (balanced data and non-balanced data)
model_ind = np.concatenate((flaky_model_ind, non_flaky_model_ind))
model_ind_balanced = np.concatenate((flaky_model_ind, non_flaky_model_ind_balanced))

# set up the data
# non-balanced data
X, y = df_concat.iloc[model_ind,:-1], df_concat.iloc[model_ind,-1]

# balanced data
X_bal, y_bal = df_concat.iloc[model_ind_balanced,:-1], df_concat.iloc[model_ind_balanced,-1]

# cfs non-balanced data
X_cfs, y_cfs = df_concat.loc[model_ind, cfs_col], df_concat.iloc[model_ind,-1]

# cfs balanced data
X_cfs_bal, y_cfs_bal = df_concat.loc[model_ind_balanced, cfs_col], df_concat.iloc[model_ind_balanced,-1]

###############################################################################

# no cfs and no balancing
perform_cv_on_several_classifiers(X, y, out_file=OUTDIR+"special_diff_union"+"-no_cfs-no_bal.csv")

# no cfs with balancing
perform_cv_on_several_classifiers(X_bal, y_bal, out_file=OUTDIR+"special_diff_union"+"-no_cfs-bal.csv")

# cfs no balancing
perform_cv_on_several_classifiers(X_cfs, y_cfs, out_file=OUTDIR+"special_diff_union"+"-cfs-no_bal.csv")

# cfs balancing
perform_cv_on_several_classifiers(X_cfs_bal, y_cfs_bal, out_file=OUTDIR+"special_diff_union"+"-cfs-bal.csv")




###############################################################################
###############################################################################
###############################################################################


