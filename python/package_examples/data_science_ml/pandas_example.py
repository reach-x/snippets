"""
pandas - Data manipulation and analysis
Install: pip install pandas

Powerful data structures and analysis tools for working with structured data
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import os
import tempfile


def dataframe_creation():
    """Create DataFrames in various ways"""
    # From dictionary
    df_dict = pd.DataFrame({
        'name': ['Alice', 'Bob', 'Charlie', 'Diana'],
        'age': [25, 30, 35, 28],
        'salary': [50000, 60000, 75000, 55000],
        'department': ['Sales', 'IT', 'HR', 'Sales']
    })

    # From list of dictionaries
    df_list_dict = pd.DataFrame([
        {'product': 'Laptop', 'price': 1000, 'quantity': 5},
        {'product': 'Mouse', 'price': 25, 'quantity': 50},
        {'product': 'Keyboard', 'price': 75, 'quantity': 30}
    ])

    # From list of lists (with column names)
    df_list = pd.DataFrame(
        [[1, 'John', 85.5], [2, 'Jane', 92.0], [3, 'Jack', 78.5]],
        columns=['ID', 'Name', 'Score']
    )

    # From numpy array
    df_numpy = pd.DataFrame(
        np.random.randn(3, 4),
        columns=['A', 'B', 'C', 'D']
    )

    # With custom index
    df_custom_index = pd.DataFrame(
        {'value': [10, 20, 30]},
        index=['row1', 'row2', 'row3']
    )

    print("DataFrame from Dictionary:")
    print(df_dict)
    print("\nDataFrame from List of Dicts:")
    print(df_list_dict)
    print("\nDataFrame with Custom Index:")
    print(df_custom_index)


def reading_writing_data():
    """Read and write data in various formats"""
    # Create sample DataFrame
    df = pd.DataFrame({
        'name': ['Alice', 'Bob', 'Charlie'],
        'age': [25, 30, 35],
        'score': [85.5, 92.0, 78.5]
    })

    # Use temporary directory for file operations
    with tempfile.TemporaryDirectory() as tmpdir:
        # CSV operations
        csv_path = os.path.join(tmpdir, 'data.csv')
        df.to_csv(csv_path, index=False)
        df_from_csv = pd.read_csv(csv_path)
        print("Read from CSV:")
        print(df_from_csv)

        # Excel operations
        excel_path = os.path.join(tmpdir, 'data.xlsx')
        df.to_excel(excel_path, index=False, sheet_name='Sheet1')
        df_from_excel = pd.read_excel(excel_path, sheet_name='Sheet1')
        print("\nRead from Excel:")
        print(df_from_excel)

        # JSON operations
        json_path = os.path.join(tmpdir, 'data.json')
        df.to_json(json_path, orient='records')
        df_from_json = pd.read_json(json_path)
        print("\nRead from JSON:")
        print(df_from_json)

        # TSV/delimiter operations
        tsv_path = os.path.join(tmpdir, 'data.tsv')
        df.to_csv(tsv_path, sep='\t', index=False)
        df_from_tsv = pd.read_csv(tsv_path, sep='\t')
        print("\nRead from TSV:")
        print(df_from_tsv)


def data_selection_filtering():
    """Select and filter data using loc, iloc, and boolean indexing"""
    df = pd.DataFrame({
        'name': ['Alice', 'Bob', 'Charlie', 'Diana', 'Eve'],
        'age': [25, 30, 35, 28, 26],
        'salary': [50000, 60000, 75000, 55000, 52000],
        'department': ['Sales', 'IT', 'HR', 'Sales', 'IT']
    })

    # Selection by column
    names = df['name']
    print("Select single column:")
    print(names)

    # Selection by multiple columns
    subset = df[['name', 'salary']]
    print("\nSelect multiple columns:")
    print(subset)

    # iloc - integer location based
    row_1 = df.iloc[1]  # Second row
    rows_0_2 = df.iloc[0:3]  # First 3 rows
    element = df.iloc[1, 2]  # Row 1, Column 2
    print("\nUsing iloc:")
    print(f"Row 1: {row_1.to_dict()}")
    print(f"Element at [1,2]: {element}")

    # loc - label based
    row_by_label = df.loc[2]  # Row with index 2
    age_and_salary = df.loc[2, ['age', 'salary']]
    print("\nUsing loc:")
    print(f"Row 2: {row_by_label.to_dict()}")

    # Boolean indexing
    over_30 = df[df['age'] > 30]
    print("\nBoolean indexing (age > 30):")
    print(over_30)

    # Multiple conditions
    it_and_high_salary = df[(df['department'] == 'IT') & (df['salary'] > 55000)]
    print("\nIT department AND salary > 55000:")
    print(it_and_high_salary)

    # Using isin for membership
    sales_or_it = df[df['department'].isin(['Sales', 'IT'])]
    print("\nSales or IT department:")
    print(sales_or_it)


def data_manipulation():
    """GroupBy, merge, join, and concat operations"""
    # Create sample data
    df1 = pd.DataFrame({
        'employee_id': [1, 2, 3, 4],
        'name': ['Alice', 'Bob', 'Charlie', 'Diana'],
        'department': ['Sales', 'IT', 'HR', 'Sales'],
        'salary': [50000, 60000, 75000, 55000]
    })

    df2 = pd.DataFrame({
        'employee_id': [1, 2, 3, 4],
        'bonus': [5000, 8000, 10000, 5500]
    })

    # GroupBy operations
    print("GroupBy department with aggregation:")
    grouped = df1.groupby('department').agg({
        'salary': ['mean', 'max', 'count'],
        'name': 'count'
    })
    print(grouped)

    # Merge (SQL-like join)
    merged = pd.merge(df1, df2, on='employee_id', how='inner')
    print("\nMerged DataFrames:")
    print(merged)

    # Join (index-based)
    df1_indexed = df1.set_index('employee_id')
    df2_indexed = df2.set_index('employee_id')
    joined = df1_indexed.join(df2_indexed)
    print("\nJoined DataFrames:")
    print(joined)

    # Concatenation
    df3 = pd.DataFrame({
        'name': ['Frank'],
        'age': [40],
        'salary': [70000],
        'department': ['IT']
    })

    df_simple = pd.DataFrame({
        'name': ['Alice', 'Bob'],
        'age': [25, 30],
        'salary': [50000, 60000],
        'department': ['Sales', 'IT']
    })

    concatenated = pd.concat([df_simple, df3], ignore_index=True)
    print("\nConcatenated DataFrames:")
    print(concatenated)

    # Pivot table
    sales_data = pd.DataFrame({
        'month': ['Jan', 'Jan', 'Feb', 'Feb'],
        'product': ['A', 'B', 'A', 'B'],
        'sales': [100, 200, 150, 180]
    })

    pivot = sales_data.pivot_table(values='sales', index='month', columns='product')
    print("\nPivot Table:")
    print(pivot)


def handling_missing_data():
    """Handle missing and null values"""
    df = pd.DataFrame({
        'name': ['Alice', 'Bob', None, 'Diana'],
        'age': [25, np.nan, 35, 28],
        'salary': [50000, 60000, None, 55000],
        'score': [85.5, None, 78.5, 92.0]
    })

    print("DataFrame with missing values:")
    print(df)

    # Check for missing values
    print("\nNull value count:")
    print(df.isnull().sum())

    # Drop rows with missing values
    dropped_rows = df.dropna()
    print("\nAfter dropping rows with NaN:")
    print(dropped_rows)

    # Drop columns with missing values
    dropped_cols = df.dropna(axis=1)
    print("\nAfter dropping columns with NaN:")
    print(dropped_cols)

    # Fill missing values with constant
    filled_const = df.fillna(0)
    print("\nAfter filling with 0:")
    print(filled_const)

    # Fill with mean
    filled_mean = df.copy()
    filled_mean['age'] = filled_mean['age'].fillna(filled_mean['age'].mean())
    print("\nAfter filling age with mean:")
    print(filled_mean)

    # Forward fill (propagate previous value)
    filled_forward = df.fillna(method='ffill')
    print("\nAfter forward fill:")
    print(filled_forward)

    # Interpolate
    df_numeric = pd.DataFrame({'value': [1, np.nan, np.nan, 4, 5]})
    interpolated = df_numeric.interpolate()
    print("\nAfter interpolation:")
    print(interpolated)


def data_aggregation():
    """Data aggregation and summary statistics"""
    df = pd.DataFrame({
        'category': ['A', 'B', 'A', 'B', 'A', 'B'],
        'values': [10, 20, 15, 25, 12, 22],
        'scores': [85, 90, 88, 92, 87, 91]
    })

    # Summary statistics
    print("Summary statistics:")
    print(df.describe())

    # Group by with multiple aggregations
    print("\nGroupBy with multiple aggregations:")
    agg_result = df.groupby('category').agg({
        'values': ['sum', 'mean', 'std', 'min', 'max'],
        'scores': ['mean', 'count']
    })
    print(agg_result)

    # Custom aggregation with lambda
    print("\nCustom aggregation (range = max - min):")
    custom_agg = df.groupby('category')['values'].agg(
        sum_val=('sum'),
        range_val=lambda x: x.max() - x.min()
    )
    print(custom_agg)

    # Multiple grouping levels
    df_multi = pd.DataFrame({
        'country': ['US', 'US', 'UK', 'UK', 'US', 'UK'],
        'city': ['NYC', 'LA', 'London', 'Manchester', 'NYC', 'London'],
        'sales': [1000, 1500, 800, 900, 1200, 950]
    })

    print("\nMulti-level groupby:")
    multi_group = df_multi.groupby(['country', 'city'])['sales'].sum()
    print(multi_group)


def time_series_operations():
    """Time series data operations"""
    # Create time series data
    dates = pd.date_range('2024-01-01', periods=10, freq='D')
    ts_df = pd.DataFrame({
        'date': dates,
        'value': np.random.randn(10).cumsum() + 100
    })

    # Set date as index
    ts_df.set_index('date', inplace=True)

    print("Time Series DataFrame:")
    print(ts_df.head())

    # Resampling (downsampling)
    print("\nResampled to weekly (mean):")
    weekly = ts_df.resample('W').mean()
    print(weekly)

    # Resampling (upsampling with interpolation)
    print("\nResampled to 6-hour intervals:")
    upsampled = ts_df.resample('6H').interpolate()
    print(upsampled.head())

    # Rolling window operations
    print("\nRolling mean (window=3):")
    rolling_mean = ts_df['value'].rolling(window=3).mean()
    print(rolling_mean.head(8))

    # Expanding window
    print("\nExpanding mean:")
    expanding_mean = ts_df['value'].expanding().mean()
    print(expanding_mean.head())

    # Shift operations (lag)
    ts_df['value_lag_1'] = ts_df['value'].shift(1)
    print("\nWith lag-1 column:")
    print(ts_df.head())

    # Date components
    ts_df['year'] = ts_df.index.year
    ts_df['month'] = ts_df.index.month
    ts_df['day'] = ts_df.index.day
    print("\nWith date components:")
    print(ts_df.head())


def apply_functions():
    """Apply custom functions to data"""
    df = pd.DataFrame({
        'name': ['Alice', 'Bob', 'Charlie'],
        'age': [25, 30, 35],
        'score': [85.5, 92.0, 78.5]
    })

    # Apply to single column
    print("Original DataFrame:")
    print(df)

    print("\nApply lambda to score column (add 5):")
    df['score_adjusted'] = df['score'].apply(lambda x: x + 5)
    print(df)

    # Apply custom function
    def grade_assignment(score):
        if score >= 90:
            return 'A'
        elif score >= 80:
            return 'B'
        elif score >= 70:
            return 'C'
        else:
            return 'F'

    df['grade'] = df['score'].apply(grade_assignment)
    print("\nWith grade assignment:")
    print(df)

    # Apply to multiple columns (row-wise)
    print("\nApply across row (sum age and score):")
    df['total'] = df[['age', 'score']].apply(lambda row: row['age'] + row['score'], axis=1)
    print(df)

    # Apply to entire DataFrame
    print("\nApply to entire DataFrame (multiply by 2):")
    df_numeric = df[['age', 'score']].applymap(lambda x: x * 2)
    print(df_numeric)

    # Apply with external function
    def categorize_age(age):
        if age < 30:
            return 'Young'
        else:
            return 'Senior'

    df['age_category'] = df['age'].apply(categorize_age)
    print("\nWith age categorization:")
    print(df)


def string_operations():
    """String operations on text data"""
    df = pd.DataFrame({
        'name': ['alice johnson', 'bob smith', 'charlie brown'],
        'email': ['alice@example.com', 'bob@example.com', 'charlie@example.com'],
        'address': ['123 Main St', '456 Oak Ave', '789 Pine Rd']
    })

    print("Original DataFrame:")
    print(df)

    # String methods
    print("\nUppercase names:")
    print(df['name'].str.upper())

    print("\nCapitalized names:")
    print(df['name'].str.capitalize())

    print("\nLength of names:")
    print(df['name'].str.len())

    # String slicing
    print("\nFirst 5 characters of email:")
    print(df['email'].str[:5])

    # String contains
    print("\nEmails containing 'example':")
    print(df[df['email'].str.contains('example')])

    # String split
    print("\nSplit names into first and last:")
    name_split = df['name'].str.split(' ', expand=True)
    name_split.columns = ['first_name', 'last_name']
    print(name_split)

    # String replace
    print("\nReplace 'St' with 'Street':")
    print(df['address'].str.replace('St', 'Street'))

    # Extract using regex
    print("\nExtract domain from email:")
    df['domain'] = df['email'].str.extract(r'@(.+)')
    print(df)

    # Check if starts/ends with
    print("\nNames starting with 'c':")
    print(df[df['name'].str.startswith('c')])


def practical_example_sales_analysis():
    """Practical example: Sales data analysis"""
    # Create sample sales data
    np.random.seed(42)
    dates = pd.date_range('2024-01-01', periods=30, freq='D')
    sales_df = pd.DataFrame({
        'date': np.repeat(dates, 3),
        'product': np.tile(['Product A', 'Product B', 'Product C'], 30),
        'region': np.tile(['North', 'South', 'East'], 30),
        'quantity': np.random.randint(5, 50, 90),
        'price': np.random.uniform(10, 100, 90)
    })

    sales_df['revenue'] = sales_df['quantity'] * sales_df['price']

    print("Sales Data Sample:")
    print(sales_df.head(10))

    # Sales by product
    print("\nTotal revenue by product:")
    product_sales = sales_df.groupby('product')['revenue'].agg(['sum', 'mean', 'count'])
    print(product_sales)

    # Sales by region
    print("\nTotal revenue by region:")
    region_sales = sales_df.groupby('region')['revenue'].sum().sort_values(ascending=False)
    print(region_sales)

    # Daily revenue
    sales_df['date_only'] = sales_df['date'].dt.date
    print("\nDaily total revenue:")
    daily_revenue = sales_df.groupby('date_only')['revenue'].sum()
    print(daily_revenue.head())

    # Product and region combination
    print("\nRevenue by product and region:")
    cross_tab = sales_df.pivot_table(values='revenue', index='product', columns='region', aggfunc='sum')
    print(cross_tab)

    # Top selling products
    print("\nTop 3 products by total quantity:")
    top_products = sales_df.groupby('product')['quantity'].sum().nlargest(3)
    print(top_products)

    # Average price by product
    print("\nAverage price by product:")
    avg_price = sales_df.groupby('product')['price'].mean()
    print(avg_price)


def dataframe_info_inspection():
    """Inspect and get information about DataFrames"""
    df = pd.DataFrame({
        'name': ['Alice', 'Bob', 'Charlie'],
        'age': [25, 30, 35],
        'salary': [50000.50, 60000.75, 75000.25],
        'hire_date': pd.date_range('2020-01-01', periods=3, freq='Y')
    })

    print("DataFrame shape:", df.shape)
    print("DataFrame columns:", df.columns.tolist())
    print("DataFrame dtypes:")
    print(df.dtypes)

    print("\nDataFrame info:")
    df.info()

    print("\nFirst few rows:")
    print(df.head(2))

    print("\nLast few rows:")
    print(df.tail(1))

    print("\nBasic statistics:")
    print(df.describe())

    print("\nValue counts for name column:")
    print(df['name'].value_counts())


def sorting_and_ranking():
    """Sort and rank data"""
    df = pd.DataFrame({
        'name': ['Alice', 'Bob', 'Charlie', 'Diana'],
        'score': [85, 92, 78, 92],
        'attempts': [1, 2, 1, 3]
    })

    print("Original DataFrame:")
    print(df)

    # Sort by single column
    print("\nSorted by score (ascending):")
    print(df.sort_values('score'))

    print("\nSorted by score (descending):")
    print(df.sort_values('score', ascending=False))

    # Sort by multiple columns
    print("\nSorted by score desc, then attempts asc:")
    print(df.sort_values(['score', 'attempts'], ascending=[False, True]))

    # Rank
    print("\nRank by score:")
    df['rank'] = df['score'].rank(ascending=False)
    print(df)

    # Dense rank (no gaps)
    print("\nDense rank by score:")
    df['dense_rank'] = df['score'].rank(method='dense', ascending=False)
    print(df)


if __name__ == "__main__":
    print("=== Pandas Examples ===\n")

    print("1. DataFrame Creation")
    dataframe_creation()

    print("\n" + "="*50)
    print("2. Reading and Writing Data")
    reading_writing_data()

    print("\n" + "="*50)
    print("3. Data Selection and Filtering")
    data_selection_filtering()

    print("\n" + "="*50)
    print("4. Data Manipulation (GroupBy, Merge, Join, Concat)")
    data_manipulation()

    print("\n" + "="*50)
    print("5. Handling Missing Data")
    handling_missing_data()

    print("\n" + "="*50)
    print("6. Data Aggregation")
    data_aggregation()

    print("\n" + "="*50)
    print("7. Time Series Operations")
    time_series_operations()

    print("\n" + "="*50)
    print("8. Apply Functions")
    apply_functions()

    print("\n" + "="*50)
    print("9. String Operations")
    string_operations()

    print("\n" + "="*50)
    print("10. DataFrame Information and Inspection")
    dataframe_info_inspection()

    print("\n" + "="*50)
    print("11. Sorting and Ranking")
    sorting_and_ranking()

    print("\n" + "="*50)
    print("12. Practical Example: Sales Data Analysis")
    practical_example_sales_analysis()
