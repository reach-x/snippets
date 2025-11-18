"""
scikit-learn - Machine learning library for Python
Install: pip install scikit-learn

Comprehensive machine learning library providing tools for classification,
regression, clustering, dimensionality reduction, model selection, and preprocessing.
"""

import numpy as np
from sklearn import datasets
from sklearn.model_selection import train_test_split, cross_val_score, cross_validate
from sklearn.preprocessing import StandardScaler, LabelEncoder, OneHotEncoder
from sklearn.linear_model import LogisticRegression, LinearRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.cluster import KMeans
from sklearn.metrics import (
    accuracy_score, confusion_matrix, classification_report,
    mean_squared_error, r2_score, silhouette_score
)
from sklearn.pipeline import Pipeline
from sklearn.feature_selection import SelectKBest, f_classif


def data_preprocessing():
    """Demonstrate data preprocessing and normalization"""
    print("--- Data Preprocessing ---")

    # Load sample dataset
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    print(f"Original data shape: {X.shape}")
    print(f"First 3 samples (original):\n{X[:3]}")
    print(f"Original feature range - Min: {X.min()}, Max: {X.max()}")

    # Standardization (zero mean, unit variance)
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)
    print(f"\nAfter StandardScaler:")
    print(f"First 3 samples (scaled):\n{X_scaled[:3]}")
    print(f"Scaled feature range - Min: {X_scaled.min():.3f}, Max: {X_scaled.max():.3f}")
    print(f"Mean: {X_scaled.mean():.6f}, Std Dev: {X_scaled.std():.6f}")

    # Label encoding for categorical data
    print("\n--- Label Encoding ---")
    categories = np.array(['cat', 'dog', 'bird', 'cat', 'dog'])
    le = LabelEncoder()
    encoded = le.fit_transform(categories)
    print(f"Original categories: {categories}")
    print(f"Encoded: {encoded}")
    print(f"Classes: {le.classes_}")

    # One-hot encoding
    print("\n--- One-Hot Encoding ---")
    color_data = np.array(['red', 'blue', 'green', 'red']).reshape(-1, 1)
    ohe = OneHotEncoder(sparse_output=False)
    one_hot_encoded = ohe.fit_transform(color_data)
    print(f"Original: {color_data.flatten()}")
    print(f"One-hot encoded:\n{one_hot_encoded}")
    print(f"Feature names: {ohe.get_feature_names_out(['color'])}")


def train_test_split_example():
    """Demonstrate train-test split"""
    print("--- Train-Test Split ---")

    # Load dataset
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    # Split into train (80%) and test (20%) sets
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42, stratify=y
    )

    print(f"Total samples: {len(X)}")
    print(f"Training set size: {len(X_train)} ({len(X_train)/len(X)*100:.1f}%)")
    print(f"Test set size: {len(X_test)} ({len(X_test)/len(X)*100:.1f}%)")
    print(f"Class distribution in training set: {np.bincount(y_train)}")
    print(f"Class distribution in test set: {np.bincount(y_test)}")


def classification_logistic_regression():
    """Demonstrate Logistic Regression classification"""
    print("--- Logistic Regression Classification ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    # Only use 2 classes for clarity (binary classification)
    mask = y != 2
    X = X[mask]
    y = y[mask]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Scale features
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    # Train model
    log_reg = LogisticRegression(random_state=42, max_iter=200)
    log_reg.fit(X_train_scaled, y_train)

    # Make predictions
    y_pred = log_reg.predict(X_test_scaled)
    y_pred_proba = log_reg.predict_proba(X_test_scaled)

    # Evaluate
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Accuracy: {accuracy:.4f}")
    print(f"Coefficients: {log_reg.coef_[0]}")
    print(f"Intercept: {log_reg.intercept_[0]:.4f}")
    print(f"\nPredicted probabilities (first 5 samples):\n{y_pred_proba[:5]}")
    print(f"Predictions (first 10): {y_pred[:10]}")
    print(f"Actual (first 10): {y_test[:10]}")


def classification_decision_tree():
    """Demonstrate Decision Tree classification"""
    print("--- Decision Tree Classification ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Train decision tree
    dt = DecisionTreeClassifier(max_depth=4, random_state=42)
    dt.fit(X_train, y_train)

    # Make predictions
    y_pred = dt.predict(X_test)

    # Evaluate
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Accuracy: {accuracy:.4f}")
    print(f"Tree depth: {dt.get_depth()}")
    print(f"Number of leaves: {dt.get_n_leaves()}")
    print(f"Feature importances: {dt.feature_importances_}")
    print(f"Feature importance names: {iris.feature_names}")


def classification_random_forest():
    """Demonstrate Random Forest classification"""
    print("--- Random Forest Classification ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Train random forest
    rf = RandomForestClassifier(n_estimators=10, max_depth=5, random_state=42)
    rf.fit(X_train, y_train)

    # Make predictions
    y_pred = rf.predict(X_test)

    # Evaluate
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Accuracy: {accuracy:.4f}")
    print(f"Number of trees: {rf.n_estimators}")
    print(f"Feature importances:\n{dict(zip(iris.feature_names, rf.feature_importances_))}")


def model_evaluation():
    """Demonstrate model evaluation metrics"""
    print("--- Model Evaluation ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Train classifier
    clf = DecisionTreeClassifier(random_state=42)
    clf.fit(X_train, y_train)
    y_pred = clf.predict(X_test)

    # Accuracy
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Accuracy: {accuracy:.4f}")

    # Confusion Matrix
    conf_matrix = confusion_matrix(y_test, y_pred)
    print(f"\nConfusion Matrix:\n{conf_matrix}")

    # Classification Report
    print("\nClassification Report:")
    print(classification_report(y_test, y_pred, target_names=iris.target_names))


def regression_example():
    """Demonstrate Linear Regression"""
    print("--- Linear Regression ---")

    # Create sample regression data
    X = np.array([[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]])
    y = np.array([2, 4, 5, 4, 5, 7, 8, 8, 9, 10])

    # Split data
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Train model
    lin_reg = LinearRegression()
    lin_reg.fit(X_train, y_train)

    # Make predictions
    y_pred = lin_reg.predict(X_test)

    # Evaluate
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_test, y_pred)

    print(f"Coefficient: {lin_reg.coef_[0]:.4f}")
    print(f"Intercept: {lin_reg.intercept_:.4f}")
    print(f"Mean Squared Error: {mse:.4f}")
    print(f"Root Mean Squared Error: {rmse:.4f}")
    print(f"R² Score: {r2:.4f}")
    print(f"Predictions (test set): {y_pred}")
    print(f"Actual (test set): {y_test}")


def clustering_kmeans():
    """Demonstrate K-Means clustering"""
    print("--- K-Means Clustering ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data

    # Scale data
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)

    # Fit K-Means with 3 clusters
    kmeans = KMeans(n_clusters=3, random_state=42, n_init=10)
    clusters = kmeans.fit_predict(X_scaled)

    # Evaluate clustering
    inertia = kmeans.inertia_  # Sum of squared distances to cluster centers
    silhouette = silhouette_score(X_scaled, clusters)

    print(f"Cluster assignments (first 10): {clusters[:10]}")
    print(f"Number of clusters: {kmeans.n_clusters}")
    print(f"Cluster centers shape: {kmeans.cluster_centers_.shape}")
    print(f"Inertia (sum of squared distances): {inertia:.4f}")
    print(f"Silhouette Score: {silhouette:.4f}")
    print(f"Cluster sizes: {np.bincount(clusters)}")


def cross_validation_example():
    """Demonstrate cross-validation"""
    print("--- Cross-Validation ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    # Create classifier
    clf = LogisticRegression(max_iter=200, random_state=42)

    # Perform k-fold cross-validation (k=5)
    cv_scores = cross_val_score(clf, X, y, cv=5, scoring='accuracy')
    print(f"Cross-validation scores (5-fold): {cv_scores}")
    print(f"Mean CV accuracy: {cv_scores.mean():.4f}")
    print(f"Std Dev: {cv_scores.std():.4f}")

    # Detailed cross-validation with multiple metrics
    scoring = {'accuracy': 'accuracy', 'precision': 'precision_weighted', 'recall': 'recall_weighted'}
    cv_results = cross_validate(clf, X, y, cv=5, scoring=scoring)

    print(f"\nDetailed CV results:")
    for metric, scores in cv_results.items():
        if metric.startswith('test_'):
            metric_name = metric.replace('test_', '')
            print(f"  {metric_name}: {scores.mean():.4f} (+/- {scores.std():.4f})")


def pipeline_example():
    """Demonstrate Pipeline for preprocessing and modeling"""
    print("--- Pipeline Example ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Create pipeline: Scale -> Classify
    pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('classifier', LogisticRegression(max_iter=200, random_state=42))
    ])

    # Fit pipeline
    pipeline.fit(X_train, y_train)

    # Make predictions
    y_pred = pipeline.predict(X_test)

    # Evaluate
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Pipeline accuracy: {accuracy:.4f}")
    print(f"Pipeline steps: {[name for name, _ in pipeline.steps]}")

    # Access individual steps
    scaler_step = pipeline.named_steps['scaler']
    classifier_step = pipeline.named_steps['classifier']
    print(f"Scaler type: {type(scaler_step).__name__}")
    print(f"Classifier type: {type(classifier_step).__name__}")


def feature_selection_example():
    """Demonstrate feature selection"""
    print("--- Feature Selection ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    # Select k-best features using f_classif
    k_best = SelectKBest(f_classif, k=2)
    X_selected = k_best.fit_transform(X, y)

    # Get selected feature indices and scores
    selected_indices = k_best.get_support(indices=True)
    feature_scores = k_best.scores_

    print(f"Original feature count: {X.shape[1]}")
    print(f"Selected feature count: {X_selected.shape[1]}")
    print(f"Selected feature indices: {selected_indices}")
    print(f"Selected features: {[iris.feature_names[i] for i in selected_indices]}")

    # Feature importance scores
    print(f"\nFeature scores (f_classif):")
    for feature_name, score in zip(iris.feature_names, feature_scores):
        print(f"  {feature_name}: {score:.4f}")

    # Compare model performance with all features vs selected features
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    clf_all = LogisticRegression(max_iter=200, random_state=42)
    clf_all.fit(X_train, y_train)
    accuracy_all = accuracy_score(y_test, clf_all.predict(X_test))

    X_train_selected = k_best.transform(X_train)
    X_test_selected = k_best.transform(X_test)

    clf_selected = LogisticRegression(max_iter=200, random_state=42)
    clf_selected.fit(X_train_selected, y_train)
    accuracy_selected = accuracy_score(y_test, clf_selected.predict(X_test_selected))

    print(f"\nModel comparison:")
    print(f"  With all 4 features: {accuracy_all:.4f}")
    print(f"  With selected 2 features: {accuracy_selected:.4f}")


def combined_pipeline_with_feature_selection():
    """Demonstrate pipeline with feature selection and classification"""
    print("--- Pipeline with Feature Selection ---")

    # Load and prepare data
    iris = datasets.load_iris()
    X = iris.data
    y = iris.target

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Create pipeline: Scale -> Select Features -> Classify
    pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('feature_selection', SelectKBest(f_classif, k=2)),
        ('classifier', LogisticRegression(max_iter=200, random_state=42))
    ])

    # Fit pipeline
    pipeline.fit(X_train, y_train)

    # Make predictions
    y_pred = pipeline.predict(X_test)

    # Evaluate
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Pipeline accuracy: {accuracy:.4f}")

    # Access feature selection step to see which features were selected
    feature_selection_step = pipeline.named_steps['feature_selection']
    selected_indices = feature_selection_step.get_support(indices=True)
    print(f"Selected features: {[iris.feature_names[i] for i in selected_indices]}")


if __name__ == "__main__":
    print("=== scikit-learn Examples ===\n")

    print("1. Data Preprocessing")
    data_preprocessing()

    print("\n" + "="*60 + "\n")
    print("2. Train-Test Split")
    train_test_split_example()

    print("\n" + "="*60 + "\n")
    print("3. Logistic Regression Classification")
    classification_logistic_regression()

    print("\n" + "="*60 + "\n")
    print("4. Decision Tree Classification")
    classification_decision_tree()

    print("\n" + "="*60 + "\n")
    print("5. Random Forest Classification")
    classification_random_forest()

    print("\n" + "="*60 + "\n")
    print("6. Model Evaluation")
    model_evaluation()

    print("\n" + "="*60 + "\n")
    print("7. Linear Regression")
    regression_example()

    print("\n" + "="*60 + "\n")
    print("8. K-Means Clustering")
    clustering_kmeans()

    print("\n" + "="*60 + "\n")
    print("9. Cross-Validation")
    cross_validation_example()

    print("\n" + "="*60 + "\n")
    print("10. Pipeline Example")
    pipeline_example()

    print("\n" + "="*60 + "\n")
    print("11. Feature Selection")
    feature_selection_example()

    print("\n" + "="*60 + "\n")
    print("12. Combined Pipeline with Feature Selection")
    combined_pipeline_with_feature_selection()
