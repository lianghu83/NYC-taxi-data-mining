"""
This file builds more classification models to classify different types of taxis.

Use sklearn module
"""

#import module
import pandas as pd
import numpy as np
from sklearn import linear_model, metrics, svm, naive_bayes
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split

#read file
df = pd.read_csv("U:\\Desktop\\By vehicle_F9_Review_aa.csv")

X = df.iloc[:, [12, 13, 9, 16, 5, 6, 21, 30, 27, 1]]
Y = df[['Label']]

#70% training set
X_train, X_test, Y_train, Y_test = train_test_split(
        X, Y, test_size=0.3)

#define confusion matrix
def perf_measure(y_actual, y_hat):
    TP = 0
    FP = 0
    TN = 0
    FN = 0
    for i in range(len(y_hat)): 
        if y_actual[i]==y_hat[i]=='feasible':
           TP += 1
    for i in range(len(y_hat)): 
        if y_hat[i]=='feasible' and y_actual[i]!=y_hat[i]:
           FP += 1
    for i in range(len(y_hat)): 
        if y_actual[i]==y_hat[i]=='infeasible':
           TN += 1
    for i in range(len(y_hat)): 
        if y_hat[i]=='infeasible' and y_actual[i]!=y_hat[i]:
           FN += 1
    return(TP, FP, TN, FN)



#logistic classification
logreg = linear_model.LogisticRegression(C=1e5)
logreg.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(logreg.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) #82

Y_test_pred = pd.DataFrame(logreg.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #82
perf_measure(np.array(Y_test), np.array(Y_test_pred))



#KNN
from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(n_neighbors=100)
knn.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(knn.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) #

Y_test_pred = pd.DataFrame(knn.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #



#stochastic gradient descent classification
sgd = linear_model.SGDClassifier()
sgd.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(sgd.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) #

Y_test_pred = pd.DataFrame(sgd.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #



#SVM
svm = svm.SVC(kernel="linear")
svm.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(svm.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) 

Y_test_pred = pd.DataFrame(svm.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) 



#linearSVC
lin_svm = svm.LinearSVC()
lin_svm.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(lin_svm.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) 

Y_test_pred = pd.DataFrame(lin_svm.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) 



#decision tree
from sklearn import tree
tree = tree.DecisionTreeClassifier()
tree.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(tree.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) #85

Y_test_pred = pd.DataFrame(tree.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #73



#random forest
from sklearn.ensemble import RandomForestClassifier
forest = RandomForestClassifier(n_estimators=10)
forest.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(forest.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) #98

Y_test_pred = pd.DataFrame(forest.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #75



#Bayes clasification
bayes = naive_bayes.GaussianNB()
bayes.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(bayes.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) #76

Y_test_pred = pd.DataFrame(bayes.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #75



#neural network
#backpropagation
nn = MLPClassifier()
nn.fit(X, Y)

Y_pred = pd.DataFrame(nn.predict(X))
metrics.accuracy_score(Y, Y_pred) #73

Y_test_pred = pd.DataFrame(nn.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) #74



#linear and quadratic discriminant
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis

lqda = LinearDiscriminantAnalysis()
lqda = QuadraticDiscriminantAnalysis()
lqda.fit(X_train, Y_train)

Y_train_pred = pd.DataFrame(lqda.predict(X_train))
metrics.accuracy_score(Y_train, Y_train_pred) 

Y_test_pred = pd.DataFrame(lqda.predict(X_test))
metrics.accuracy_score(Y_test, Y_test_pred) 
perf_measure(np.array(Y_test), np.array(Y_test_pred))



### Model accuracy

#30% test set
sum_train = 0
sum_test = 0
iteration = 50
for i in range(iteration):
    X_train, X_test, Y_train, Y_test = train_test_split(
        X, Y, test_size=0.3)
    
    model = KNeighborsClassifier(n_neighbors=100)
    
    model.fit(X_train, Y_train)
    
    Y_train_pred = pd.DataFrame(model.predict(X_train))
    sum_train += metrics.accuracy_score(Y_train, Y_train_pred) 

    Y_test_pred = pd.DataFrame(model.predict(X_test))
    sum_test += metrics.accuracy_score(Y_test, Y_test_pred) 

sum_train/iteration
sum_test/iteration



#10-fold CV of logistic regression
from sklearn.model_selection import cross_val_score, KFold
seed = 7
kfold = KFold(n_splits=10, random_state=seed)
#model = naive_bayes.GaussianNB()
model = linear_model.LogisticRegression(C=1e5)
results = cross_val_score(model, X, Y, cv=kfold)
results.mean()



#10-fold CV of KNN
seed = 7
kfold = KFold(n_splits=10, random_state=seed)
test_accuracy = []
for K in range(1, 101):
    model = KNeighborsClassifier(n_neighbors=K)
    results = cross_val_score(model, X, Y, cv=kfold)
    test_accuracy.append(results.mean())
test_accuracy = np.array(test_accuracy)
K_list = np.arange(1, 101)
test_accuracy.max()
K_max = test_accuracy.argmax(axis=0) + 1

import matplotlib.pyplot as plt
plt.scatter(x=K_list, y=test_accuracy)
plt.show()


