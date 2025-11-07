-- DML (Data Manipulation Language) Examples
-- INSERT, UPDATE, DELETE operations

USE sample_db;

-- ====================
-- INSERT Operations
-- ====================

-- Simple insert
INSERT INTO customers (first_name, last_name, email, phone)
VALUES ('John', 'Doe', 'john.doe@example.com', '555-0100');

-- Insert multiple rows
INSERT INTO customers (first_name, last_name, email, phone) VALUES
('Jane', 'Smith', 'jane.smith@example.com', '555-0101'),
('Bob', 'Johnson', 'bob.johnson@example.com', '555-0102'),
('Alice', 'Williams', 'alice.williams@example.com', '555-0103');

-- Insert with select
INSERT INTO categories (category, description)
SELECT DISTINCT 'General', 'General category'
WHERE NOT EXISTS (SELECT 1 FROM categories WHERE category = 'General');

-- Insert ignore (skip duplicates)
INSERT IGNORE INTO customers (first_name, last_name, email, phone)
VALUES ('John', 'Doe', 'john.doe@example.com', '555-0100');

-- Insert on duplicate key update
INSERT INTO customers (first_name, last_name, email, phone)
VALUES ('John', 'Doe', 'john.doe@example.com', '555-0199')
ON DUPLICATE KEY UPDATE phone = VALUES(phone);

-- Get last inserted ID
-- SELECT LAST_INSERT_ID();

-- ====================
-- UPDATE Operations
-- ====================

-- Simple update
UPDATE customers
SET phone = '555-9999'
WHERE customer_id = 1;

-- Update multiple columns
UPDATE customers
SET
    first_name = 'Jonathan',
    last_name = 'Doe',
    phone = '555-8888'
WHERE customer_id = 1;

-- Update with calculation
UPDATE orders
SET total_amount = total_amount * 1.10
WHERE order_date < '2024-01-01';

-- Update with join
UPDATE orders o
INNER JOIN customers c ON o.customer_id = c.customer_id
SET o.status = 'processing'
WHERE c.email LIKE '%@example.com'
AND o.status = 'pending';

-- Update with case statement
UPDATE orders
SET status = CASE
    WHEN DATEDIFF(NOW(), order_date) > 30 THEN 'delivered'
    WHEN DATEDIFF(NOW(), order_date) > 7 THEN 'shipped'
    ELSE status
END
WHERE status IN ('pending', 'processing');

-- Update all rows (be careful!)
-- UPDATE customers SET updated_at = NOW();

-- ====================
-- DELETE Operations
-- ====================

-- Simple delete
DELETE FROM customers
WHERE customer_id = 999;

-- Delete with condition
DELETE FROM orders
WHERE status = 'cancelled'
AND order_date < DATE_SUB(NOW(), INTERVAL 1 YEAR);

-- Delete with join
DELETE o
FROM orders o
INNER JOIN customers c ON o.customer_id = c.customer_id
WHERE c.email LIKE '%spam%';

-- Delete with subquery
DELETE FROM customers
WHERE customer_id IN (
    SELECT customer_id
    FROM (
        SELECT c.customer_id
        FROM customers c
        LEFT JOIN orders o ON c.customer_id = o.customer_id
        WHERE o.order_id IS NULL
        AND c.created_at < DATE_SUB(NOW(), INTERVAL 2 YEAR)
    ) AS temp
);

-- Delete all rows (be very careful!)
-- DELETE FROM temp_calculations;

-- ====================
-- REPLACE Operations
-- ====================

-- Replace (delete and insert)
REPLACE INTO categories (category_id, category, description)
VALUES (1, 'Electronics', 'Electronic products');

-- ====================
-- Bulk Operations
-- ====================

-- Insert sample orders
INSERT INTO orders (customer_id, order_date, total_amount, status) VALUES
(1, '2024-01-15', 199.99, 'delivered'),
(1, '2024-02-20', 299.50, 'delivered'),
(2, '2024-03-10', 49.99, 'delivered'),
(2, '2024-04-05', 399.00, 'shipped'),
(3, '2024-05-01', 89.95, 'processing'),
(3, '2024-05-15', 159.99, 'pending'),
(4, '2024-06-01', 249.00, 'pending');

-- Insert sample order items
INSERT INTO order_items (order_id, item_number, product_name, quantity, price) VALUES
(1, 1, 'Widget A', 2, 99.99),
(1, 2, 'Widget B', 1, 0.01),
(2, 1, 'Gadget X', 1, 299.50),
(3, 1, 'Tool Y', 3, 16.66),
(4, 1, 'Device Z', 1, 399.00);

-- ====================
-- Transaction Example
-- ====================

START TRANSACTION;

-- Insert new customer
INSERT INTO customers (first_name, last_name, email)
VALUES ('Test', 'User', 'test@example.com');

SET @new_customer_id = LAST_INSERT_ID();

-- Create order for new customer
INSERT INTO orders (customer_id, order_date, total_amount, status)
VALUES (@new_customer_id, CURDATE(), 100.00, 'pending');

SET @new_order_id = LAST_INSERT_ID();

-- Add order items
INSERT INTO order_items (order_id, item_number, product_name, quantity, price)
VALUES (@new_order_id, 1, 'Sample Product', 1, 100.00);

-- Commit or rollback
COMMIT;
-- ROLLBACK;

-- ====================
-- Conditional Updates
-- ====================

-- Update only if condition met
UPDATE orders
SET status = 'shipped'
WHERE order_id = 1
AND status = 'processing'
AND total_amount > 0;

-- Affected rows check
-- SELECT ROW_COUNT();
