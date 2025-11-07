-- DDL (Data Definition Language) Examples
-- Creating, altering, and dropping database objects

-- Create database
CREATE DATABASE IF NOT EXISTS sample_db;
USE sample_db;

-- Create simple table
CREATE TABLE IF NOT EXISTS customers (
    customer_id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    phone VARCHAR(20),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_email (email),
    INDEX idx_last_name (last_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Create table with foreign key
CREATE TABLE IF NOT EXISTS orders (
    order_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    customer_id INT UNSIGNED NOT NULL,
    order_date DATE NOT NULL,
    total_amount DECIMAL(12,2) DEFAULT 0.00,
    status ENUM('pending', 'processing', 'shipped', 'delivered', 'cancelled') DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id) ON DELETE CASCADE,
    INDEX idx_customer (customer_id),
    INDEX idx_order_date (order_date),
    INDEX idx_status (status)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Create table with composite primary key
CREATE TABLE IF NOT EXISTS order_items (
    order_id BIGINT UNSIGNED NOT NULL,
    item_number INT UNSIGNED NOT NULL,
    product_name VARCHAR(100) NOT NULL,
    quantity INT UNSIGNED DEFAULT 1,
    price DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (order_id, item_number),
    FOREIGN KEY (order_id) REFERENCES orders(order_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Create lookup table
CREATE TABLE IF NOT EXISTS categories (
    category_id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    category VARCHAR(50) UNIQUE NOT NULL,
    description TEXT,
    active BOOLEAN DEFAULT TRUE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Alter table - add column
ALTER TABLE customers
ADD COLUMN date_of_birth DATE AFTER last_name;

-- Alter table - modify column
ALTER TABLE customers
MODIFY COLUMN phone VARCHAR(25);

-- Alter table - add index
ALTER TABLE customers
ADD INDEX idx_created_at (created_at);

-- Alter table - add foreign key
ALTER TABLE customers
ADD COLUMN category_id INT UNSIGNED,
ADD FOREIGN KEY (category_id) REFERENCES categories(category_id);

-- Create view
CREATE OR REPLACE VIEW customer_orders AS
SELECT
    c.customer_id,
    c.first_name,
    c.last_name,
    c.email,
    COUNT(o.order_id) AS order_count,
    SUM(o.total_amount) AS total_spent
FROM customers c
LEFT JOIN orders o ON c.customer_id = o.customer_id
GROUP BY c.customer_id, c.first_name, c.last_name, c.email;

-- Create temporary table
CREATE TEMPORARY TABLE IF NOT EXISTS temp_calculations (
    id INT AUTO_INCREMENT PRIMARY KEY,
    value DECIMAL(10,2),
    calculation_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create index
CREATE INDEX idx_customer_name ON customers(last_name, first_name);

-- Drop index
-- ALTER TABLE customers DROP INDEX idx_customer_name;

-- Rename table
-- RENAME TABLE customers TO clients;

-- Drop table
-- DROP TABLE IF EXISTS temp_calculations;

-- Truncate table (remove all rows but keep structure)
-- TRUNCATE TABLE temp_calculations;

-- Show table structure
DESCRIBE customers;
SHOW CREATE TABLE customers;

-- Show all tables
SHOW TABLES;

-- Show indexes
SHOW INDEXES FROM customers;
