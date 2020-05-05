"""
Luke Kurlandski
CSC275: Intro to Data Science 
Final Project
April 2020
Description: the ML aspect of the project to compute survival chances.
"""

import pandas as pd
from sklearn.linear_model import SGDClassifier, LinearRegression
from sklearn.model_selection import cross_val_score

class Ticket:
    def __init__(self, embarked=None, pclass=None, sex=None, age=None, fare=None, parch=None, sibsp=None):
        
        # User's ticket in the form of a data frame.
        self.datas = pd.DataFrame(
            {"Pclass" : pclass,
             "Sex" : 1 if sex=="male" or sex=="Male" else 0,
             "Embarked" : 1 if embarked=="C" else (2 if embarked=="Q" else 3),
             "Alone" : 0 if (sibsp is not None and sibsp > 0) or (parch is not None and parch > 0) else 1,
             "Survived" : "?"},
            index = [1])
        
        # Drop columns that contain an NA value.
        self.datas = self.datas.dropna(axis="columns")

def wrangle_data(df):
    """
    """
    
    # Convert Male/Female into 1/0 respectively.
    if "Sex" in df.columns:
        df.Sex = df.Sex.cat.codes
    # C, Q, S to 1, 2, 3 respectively.
    if "Embarked" in df.columns:
        df.Embarked = df.Embarked.cat.codes
    # Alone to 1, Not Alone to 0
    if "Alone" in df.columns:
        df.loc[df['Alone'] == "Alone", 'Alone'] = 1
        df.loc[df['Alone'] == "Not Alone", 'Alone'] = 0
    
    return df
    
def caller_from_R(ML_algorithm=None, embarked=None, pclass=None, sex=None, age=None, 
                  fare=None, parch=None, sibsp=None, num_folds=3):
    """
    Reciever from R function to call various ML algorithms.
    """
    
    #return "Heres what I got: " + str(pclass) + str(sex) + str(age) + str(fare) + str(parch) + str(sibsp)
    
    # Wrap user's ticket purchase into an object.
    ticket = Ticket(embarked, pclass, sex, age, fare, parch, sibsp)
    
    # Saving/reading from saved dataframe file.
    #df.to_pickle("pickledDF.pkl", protocol = 4)
    DF = pd.read_pickle("pickledDF.pkl")
    
    # Extract the X training set, containing selected features.
    X_train = DF[ticket.datas.columns]
    X_train = X_train.dropna()
    X_train = wrangle_data(X_train)
    
    # Extract the Y training set, those who survived.
    Y_train = X_train["Survived"]
    Y_train_survived = (Y_train == 1)
    
    # Now drop the Survived Column from the X_train
    X_train = X_train.drop(columns="Survived")
    
    if (ML_algorithm == "binary_classifier"):
        return binary_classifier(X_train, Y_train_survived, ticket, num_folds)
    else:
        return "Error" 

def binary_classifier(X_train, Y_train_survived, ticket, num_folds):
    """
    Perform binary classification, live or die prediction.
    
    Return the survival prediction and 3 cross validation scores.
    """
    
    sgd_clf = SGDClassifier(random_state = 42)
    sgd_clf.fit(X_train, Y_train_survived)
    p = sgd_clf.predict(ticket.datas.drop(columns="Survived"))
    
    scores = cross_val_score(sgd_clf, X_train, Y_train_survived, cv = int(num_folds), scoring = "accuracy")
    
    return p, scores

def tester_from_python():
    """
    """
    
    p = caller_from_R("binary_classifier",  "C", 1, "Female", 9, 100, 0, 0)
    print(p)
    
#tester_from_python()