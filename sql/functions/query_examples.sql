-- SQL Query Examples
-- SELECT statements with various clauses and techniques

USE sample_db;

-- ====================
-- Basic SELECT
-- ====================

-- Select all columns
SELECT * FROM customers;

-- Select specific columns
SELECT first_name, last_name, email
FROM customers;

-- Select with alias
SELECT
    first_name AS fname,
    last_name AS lname,
    email AS email_address
FROM customers;

-- ====================
-- WHERE Clause
-- ====================

-- Simple where
SELECT * FROM customers
WHERE last_name = 'Doe';

-- Multiple conditions with AND
SELECT * FROM orders
WHERE status = 'delivered'
AND total_amount > 100;

-- Multiple conditions with OR
SELECT * FROM orders
WHERE status = 'pending'
OR status = 'processing';

-- IN operator
SELECT * FROM orders
WHERE status IN ('pending', 'processing', 'shipped');

-- NOT IN operator
SELECT * FROM customers
WHERE customer_id NOT IN (1, 2, 3);

-- BETWEEN operator
SELECT * FROM orders
WHERE total_amount BETWEEN 100 AND 500;

-- LIKE operator (pattern matching)
SELECT * FROM customers
WHERE email LIKE '%@example.com';

-- Wildcards
SELECT * FROM customers
WHERE first_name LIKE 'J%';  -- Starts with J

SELECT * FROM customers
WHERE last_name LIKE '%son';  -- Ends with son

SELECT * FROM customers
WHERE email LIKE '%@%.%';  -- Contains @ and .

-- IS NULL / IS NOT NULL
SELECT * FROM customers
WHERE phone IS NULL;

SELECT * FROM customers
WHERE phone IS NOT NULL;

-- ====================
-- ORDER BY
-- ====================

-- Order ascending
SELECT * FROM customers
ORDER BY last_name ASC;

-- Order descending
SELECT * FROM orders
ORDER BY total_amount DESC;

-- Multiple columns
SELECT * FROM customers
ORDER BY last_name ASC, first_name ASC;

-- Order by computed column
SELECT
    first_name,
    last_name,
    CONCAT(first_name, ' ', last_name) AS full_name
FROM customers
ORDER BY full_name;

-- ====================
-- LIMIT and OFFSET
-- ====================

-- Limit results
SELECT * FROM orders
ORDER BY order_date DESC
LIMIT 10;

-- Pagination with offset
SELECT * FROM orders
ORDER BY order_id
LIMIT 10 OFFSET 20;  -- Skip first 20, get next 10

-- Alternative pagination syntax
SELECT * FROM orders
ORDER BY order_id
LIMIT 20, 10;  -- Skip 20, get 10

-- ====================
-- DISTINCT
-- ====================

-- Get unique values
SELECT DISTINCT status FROM orders;

-- Distinct on multiple columns
SELECT DISTINCT customer_id, status
FROM orders;

-- Count distinct
SELECT COUNT(DISTINCT customer_id) AS unique_customers
FROM orders;

-- ====================
-- Aggregate Functions
-- ====================

-- COUNT
SELECT COUNT(*) AS total_customers FROM customers;
SELECT COUNT(phone) AS customers_with_phone FROM customers;

-- SUM
SELECT SUM(total_amount) AS total_revenue FROM orders;

-- AVG
SELECT AVG(total_amount) AS average_order FROM orders;

-- MIN and MAX
SELECT
    MIN(total_amount) AS smallest_order,
    MAX(total_amount) AS largest_order
FROM orders;

-- Multiple aggregates
SELECT
    COUNT(*) AS order_count,
    SUM(total_amount) AS total_revenue,
    AVG(total_amount) AS average_order,
    MIN(total_amount) AS min_order,
    MAX(total_amount) AS max_order
FROM orders;

-- ====================
-- GROUP BY
-- ====================

-- Group by single column
SELECT
    status,
    COUNT(*) AS count
FROM orders
GROUP BY status;

-- Group by with aggregate
SELECT
    customer_id,
    COUNT(*) AS order_count,
    SUM(total_amount) AS total_spent
FROM orders
GROUP BY customer_id;

-- Group by multiple columns
SELECT
    customer_id,
    status,
    COUNT(*) AS count
FROM orders
GROUP BY customer_id, status;

-- ====================
-- HAVING Clause
-- ====================

-- Filter after grouping
SELECT
    customer_id,
    COUNT(*) AS order_count,
    SUM(total_amount) AS total_spent
FROM orders
GROUP BY customer_id
HAVING COUNT(*) >= 2;

-- Having with multiple conditions
SELECT
    customer_id,
    COUNT(*) AS order_count,
    SUM(total_amount) AS total_spent
FROM orders
GROUP BY customer_id
HAVING COUNT(*) >= 2
AND SUM(total_amount) > 500;

-- ====================
-- JOIN Operations
-- ====================

-- INNER JOIN
SELECT
    c.customer_id,
    c.first_name,
    c.last_name,
    o.order_id,
    o.order_date,
    o.total_amount
FROM customers c
INNER JOIN orders o ON c.customer_id = o.customer_id;

-- LEFT JOIN
SELECT
    c.customer_id,
    c.first_name,
    c.last_name,
    o.order_id,
    o.total_amount
FROM customers c
LEFT JOIN orders o ON c.customer_id = o.customer_id;

-- RIGHT JOIN
SELECT
    c.customer_id,
    c.first_name,
    o.order_id,
    o.total_amount
FROM orders o
RIGHT JOIN customers c ON o.customer_id = c.customer_id;

-- Multiple joins
SELECT
    c.first_name,
    c.last_name,
    o.order_id,
    o.order_date,
    oi.product_name,
    oi.quantity,
    oi.price
FROM customers c
INNER JOIN orders o ON c.customer_id = o.customer_id
INNER JOIN order_items oi ON o.order_id = oi.order_id;

-- Self join
SELECT
    c1.first_name AS customer1,
    c2.first_name AS customer2
FROM customers c1
INNER JOIN customers c2 ON c1.last_name = c2.last_name
WHERE c1.customer_id < c2.customer_id;

-- ====================
-- Subqueries
-- ====================

-- Subquery in WHERE
SELECT * FROM customers
WHERE customer_id IN (
    SELECT DISTINCT customer_id
    FROM orders
    WHERE total_amount > 200
);

-- Subquery in SELECT
SELECT
    first_name,
    last_name,
    (SELECT COUNT(*) FROM orders o WHERE o.customer_id = c.customer_id) AS order_count
FROM customers c;

-- Subquery in FROM
SELECT
    avg_orders.status,
    avg_orders.avg_amount
FROM (
    SELECT
        status,
        AVG(total_amount) AS avg_amount
    FROM orders
    GROUP BY status
) AS avg_orders
WHERE avg_orders.avg_amount > 100;

-- Correlated subquery
SELECT c.first_name, c.last_name
FROM customers c
WHERE EXISTS (
    SELECT 1 FROM orders o
    WHERE o.customer_id = c.customer_id
    AND o.total_amount > 200
);

-- ====================
-- UNION
-- ====================

-- Combine results from multiple queries
SELECT customer_id, first_name, 'Customer' AS type
FROM customers
WHERE customer_id <= 2
UNION
SELECT customer_id, first_name, 'Premium' AS type
FROM customers
WHERE customer_id > 2;

-- UNION ALL (includes duplicates)
SELECT first_name FROM customers
UNION ALL
SELECT last_name FROM customers;

-- ====================
-- CASE Expressions
-- ====================

-- Simple case
SELECT
    order_id,
    total_amount,
    CASE
        WHEN total_amount < 100 THEN 'Small'
        WHEN total_amount < 300 THEN 'Medium'
        ELSE 'Large'
    END AS order_size
FROM orders;

-- Case in aggregate
SELECT
    status,
    COUNT(*) AS total,
    SUM(CASE WHEN total_amount > 200 THEN 1 ELSE 0 END) AS large_orders
FROM orders
GROUP BY status;

-- ====================
-- String Functions
-- ====================

-- Concatenation
SELECT CONCAT(first_name, ' ', last_name) AS full_name
FROM customers;

-- Length
SELECT first_name, LENGTH(first_name) AS name_length
FROM customers;

-- Upper/Lower case
SELECT
    UPPER(first_name) AS upper_name,
    LOWER(last_name) AS lower_name
FROM customers;

-- Substring
SELECT
    email,
    SUBSTRING(email, 1, LOCATE('@', email) - 1) AS username
FROM customers;

-- ====================
-- Date Functions
-- ====================

-- Current date/time
SELECT NOW(), CURDATE(), CURTIME();

-- Date formatting
SELECT
    order_date,
    DATE_FORMAT(order_date, '%Y-%m-%d') AS formatted_date,
    DATE_FORMAT(order_date, '%M %d, %Y') AS long_format
FROM orders;

-- Date arithmetic
SELECT
    order_date,
    DATE_ADD(order_date, INTERVAL 30 DAY) AS due_date,
    DATEDIFF(NOW(), order_date) AS days_ago
FROM orders;

-- Extract date parts
SELECT
    order_date,
    YEAR(order_date) AS year,
    MONTH(order_date) AS month,
    DAY(order_date) AS day,
    DAYNAME(order_date) AS day_name
FROM orders;

-- ====================
-- Window Functions (MySQL 8+)
-- ====================

-- Row number
SELECT
    customer_id,
    order_id,
    total_amount,
    ROW_NUMBER() OVER (PARTITION BY customer_id ORDER BY order_date) AS order_number
FROM orders;

-- Rank
SELECT
    customer_id,
    total_amount,
    RANK() OVER (ORDER BY total_amount DESC) AS amount_rank
FROM orders;

-- Running total
SELECT
    order_id,
    order_date,
    total_amount,
    SUM(total_amount) OVER (ORDER BY order_date) AS running_total
FROM orders;

-- ====================
-- Common Table Expressions (CTE)
-- ====================

WITH customer_summary AS (
    SELECT
        c.customer_id,
        c.first_name,
        c.last_name,
        COUNT(o.order_id) AS order_count,
        COALESCE(SUM(o.total_amount), 0) AS total_spent
    FROM customers c
    LEFT JOIN orders o ON c.customer_id = o.customer_id
    GROUP BY c.customer_id, c.first_name, c.last_name
)
SELECT *
FROM customer_summary
WHERE total_spent > 100
ORDER BY total_spent DESC;
