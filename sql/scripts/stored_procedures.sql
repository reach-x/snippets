-- Stored Procedures and Functions Examples

USE sample_db;

-- ====================
-- Simple Stored Procedure
-- ====================

DELIMITER //

CREATE PROCEDURE get_all_customers()
BEGIN
    SELECT * FROM customers;
END //

DELIMITER ;

-- Call procedure
-- CALL get_all_customers();

-- ====================
-- Procedure with Parameters
-- ====================

DELIMITER //

CREATE PROCEDURE get_customer_by_id(IN p_customer_id INT)
BEGIN
    SELECT *
    FROM customers
    WHERE customer_id = p_customer_id;
END //

DELIMITER ;

-- Call with parameter
-- CALL get_customer_by_id(1);

-- ====================
-- Procedure with OUT Parameter
-- ====================

DELIMITER //

CREATE PROCEDURE get_customer_order_count(
    IN p_customer_id INT,
    OUT p_order_count INT
)
BEGIN
    SELECT COUNT(*)
    INTO p_order_count
    FROM orders
    WHERE customer_id = p_customer_id;
END //

DELIMITER ;

-- Call with OUT parameter
-- CALL get_customer_order_count(1, @count);
-- SELECT @count;

-- ====================
-- Procedure with INOUT Parameter
-- ====================

DELIMITER //

CREATE PROCEDURE apply_discount(
    INOUT p_amount DECIMAL(10,2),
    IN p_discount_percent INT
)
BEGIN
    SET p_amount = p_amount * (1 - p_discount_percent / 100);
END //

DELIMITER ;

-- Call with INOUT
-- SET @price = 100.00;
-- CALL apply_discount(@price, 10);
-- SELECT @price;

-- ====================
-- Procedure with Conditional Logic
-- ====================

DELIMITER //

CREATE PROCEDURE categorize_order(
    IN p_amount DECIMAL(10,2),
    OUT p_category VARCHAR(20)
)
BEGIN
    IF p_amount < 100 THEN
        SET p_category = 'Small';
    ELSEIF p_amount < 500 THEN
        SET p_category = 'Medium';
    ELSE
        SET p_category = 'Large';
    END IF;
END //

DELIMITER ;

-- ====================
-- Procedure with Loop
-- ====================

DELIMITER //

CREATE PROCEDURE insert_test_data(IN p_count INT)
BEGIN
    DECLARE v_counter INT DEFAULT 1;

    WHILE v_counter <= p_count DO
        INSERT INTO categories (category, description)
        VALUES (
            CONCAT('Category ', v_counter),
            CONCAT('Description for category ', v_counter)
        );

        SET v_counter = v_counter + 1;
    END WHILE;
END //

DELIMITER ;

-- ====================
-- Procedure with CASE
-- ====================

DELIMITER //

CREATE PROCEDURE get_order_status_description(
    IN p_status VARCHAR(20),
    OUT p_description TEXT
)
BEGIN
    CASE p_status
        WHEN 'pending' THEN
            SET p_description = 'Order is awaiting processing';
        WHEN 'processing' THEN
            SET p_description = 'Order is being prepared';
        WHEN 'shipped' THEN
            SET p_description = 'Order has been shipped';
        WHEN 'delivered' THEN
            SET p_description = 'Order has been delivered';
        WHEN 'cancelled' THEN
            SET p_description = 'Order has been cancelled';
        ELSE
            SET p_description = 'Unknown status';
    END CASE;
END //

DELIMITER ;

-- ====================
-- Procedure with Cursor
-- ====================

DELIMITER //

CREATE PROCEDURE calculate_customer_totals()
BEGIN
    DECLARE done INT DEFAULT FALSE;
    DECLARE v_customer_id INT;
    DECLARE v_total DECIMAL(12,2);
    DECLARE cur CURSOR FOR
        SELECT customer_id, SUM(total_amount)
        FROM orders
        GROUP BY customer_id;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

    -- Create temp table for results
    DROP TEMPORARY TABLE IF EXISTS customer_totals;
    CREATE TEMPORARY TABLE customer_totals (
        customer_id INT,
        total_spent DECIMAL(12,2)
    );

    OPEN cur;

    read_loop: LOOP
        FETCH cur INTO v_customer_id, v_total;
        IF done THEN
            LEAVE read_loop;
        END IF;

        INSERT INTO customer_totals VALUES (v_customer_id, v_total);
    END LOOP;

    CLOSE cur;

    -- Return results
    SELECT * FROM customer_totals;
END //

DELIMITER ;

-- ====================
-- Procedure with Error Handling
-- ====================

DELIMITER //

CREATE PROCEDURE create_order_safe(
    IN p_customer_id INT,
    IN p_total_amount DECIMAL(12,2),
    OUT p_order_id BIGINT,
    OUT p_error_message VARCHAR(255)
)
BEGIN
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
        SET p_error_message = 'Error creating order';
        SET p_order_id = 0;
        ROLLBACK;
    END;

    START TRANSACTION;

    -- Validate customer exists
    IF NOT EXISTS (SELECT 1 FROM customers WHERE customer_id = p_customer_id) THEN
        SET p_error_message = 'Customer not found';
        SET p_order_id = 0;
        ROLLBACK;
    ELSE
        -- Insert order
        INSERT INTO orders (customer_id, order_date, total_amount, status)
        VALUES (p_customer_id, CURDATE(), p_total_amount, 'pending');

        SET p_order_id = LAST_INSERT_ID();
        SET p_error_message = NULL;

        COMMIT;
    END IF;
END //

DELIMITER ;

-- ====================
-- Stored Function
-- ====================

DELIMITER //

CREATE FUNCTION calculate_tax(p_amount DECIMAL(10,2), p_rate DECIMAL(5,2))
RETURNS DECIMAL(10,2)
DETERMINISTIC
BEGIN
    RETURN p_amount * (p_rate / 100);
END //

DELIMITER ;

-- Use function
-- SELECT calculate_tax(100.00, 8.5);

-- ====================
-- Function that Returns Customer Status
-- ====================

DELIMITER //

CREATE FUNCTION get_customer_status(p_customer_id INT)
RETURNS VARCHAR(20)
READS SQL DATA
BEGIN
    DECLARE v_order_count INT;
    DECLARE v_total_spent DECIMAL(12,2);

    SELECT
        COUNT(*),
        COALESCE(SUM(total_amount), 0)
    INTO v_order_count, v_total_spent
    FROM orders
    WHERE customer_id = p_customer_id;

    IF v_order_count = 0 THEN
        RETURN 'New';
    ELSEIF v_total_spent > 1000 THEN
        RETURN 'Premium';
    ELSEIF v_total_spent > 500 THEN
        RETURN 'Gold';
    ELSE
        RETURN 'Standard';
    END IF;
END //

DELIMITER ;

-- Use function
-- SELECT customer_id, first_name, get_customer_status(customer_id) AS status
-- FROM customers;

-- ====================
-- Trigger Example
-- ====================

DELIMITER //

CREATE TRIGGER update_order_total
BEFORE INSERT ON order_items
FOR EACH ROW
BEGIN
    DECLARE v_total DECIMAL(12,2);

    -- Calculate new order total
    SELECT COALESCE(SUM(quantity * price), 0) + (NEW.quantity * NEW.price)
    INTO v_total
    FROM order_items
    WHERE order_id = NEW.order_id;

    -- Update order total
    UPDATE orders
    SET total_amount = v_total
    WHERE order_id = NEW.order_id;
END //

DELIMITER ;

-- ====================
-- Drop Procedures/Functions
-- ====================

-- DROP PROCEDURE IF EXISTS get_all_customers;
-- DROP PROCEDURE IF EXISTS get_customer_by_id;
-- DROP PROCEDURE IF EXISTS get_customer_order_count;
-- DROP FUNCTION IF EXISTS calculate_tax;
-- DROP TRIGGER IF EXISTS update_order_total;

-- ====================
-- Show Procedures/Functions
-- ====================

-- SHOW PROCEDURE STATUS WHERE db = 'sample_db';
-- SHOW FUNCTION STATUS WHERE db = 'sample_db';
-- SHOW CREATE PROCEDURE get_all_customers;
-- SHOW CREATE FUNCTION calculate_tax;
