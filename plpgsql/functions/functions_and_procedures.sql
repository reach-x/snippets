-- PL/pgSQL Examples - PostgreSQL
-- Functions, procedures, and advanced features

-- Basic function
CREATE OR REPLACE FUNCTION get_user_by_id(p_user_id INTEGER)
RETURNS TABLE(user_id INTEGER, name VARCHAR, email VARCHAR, created_at TIMESTAMP)
AS $$
BEGIN
    RETURN QUERY
    SELECT u.user_id, u.name, u.email, u.created_at
    FROM users u
    WHERE u.user_id = p_user_id;
END;
$$ LANGUAGE plpgsql;

-- Function with OUT parameter
CREATE OR REPLACE FUNCTION create_user(
    p_name VARCHAR,
    p_email VARCHAR,
    OUT p_new_user_id INTEGER
)
AS $$
BEGIN
    INSERT INTO users (name, email, created_at)
    VALUES (p_name, p_email, NOW())
    RETURNING user_id INTO p_new_user_id;
END;
$$ LANGUAGE plpgsql;

-- Function with exception handling
CREATE OR REPLACE FUNCTION update_user_email(
    p_user_id INTEGER,
    p_new_email VARCHAR
) RETURNS VOID
AS $$
DECLARE
    v_row_count INTEGER;
BEGIN
    UPDATE users
    SET email = p_new_email, updated_at = NOW()
    WHERE user_id = p_user_id;

    GET DIAGNOSTICS v_row_count = ROW_COUNT;

    IF v_row_count = 0 THEN
        RAISE EXCEPTION 'User not found with ID: %', p_user_id;
    END IF;
EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error: %', SQLERRM;
        RAISE;
END;
$$ LANGUAGE plpgsql;

-- Scalar function
CREATE OR REPLACE FUNCTION calculate_age(p_birth_date DATE)
RETURNS INTEGER
AS $$
BEGIN
    RETURN EXTRACT(YEAR FROM AGE(NOW(), p_birth_date));
END;
$$ LANGUAGE plpgsql;

-- Function returning custom type
CREATE TYPE user_order AS (
    order_id INTEGER,
    order_date TIMESTAMP,
    total_amount NUMERIC
);

CREATE OR REPLACE FUNCTION get_user_orders(p_user_id INTEGER)
RETURNS SETOF user_order
AS $$
BEGIN
    RETURN QUERY
    SELECT o.order_id, o.order_date, o.total_amount
    FROM orders o
    WHERE o.user_id = p_user_id;
END;
$$ LANGUAGE plpgsql;

-- Procedure (PostgreSQL 11+)
CREATE OR REPLACE PROCEDURE add_user(
    p_name VARCHAR,
    p_email VARCHAR,
    INOUT p_user_id INTEGER DEFAULT NULL
)
AS $$
BEGIN
    INSERT INTO users (name, email, created_at)
    VALUES (p_name, p_email, NOW())
    RETURNING user_id INTO p_user_id;

    COMMIT;
END;
$$ LANGUAGE plpgsql;

-- Trigger function
CREATE OR REPLACE FUNCTION trg_users_audit()
RETURNS TRIGGER
AS $$
DECLARE
    v_action VARCHAR(10);
BEGIN
    IF TG_OP = 'INSERT' THEN
        v_action := 'INSERT';
        INSERT INTO audit_log (table_name, action, user_id, changed_at)
        VALUES (TG_TABLE_NAME, v_action, NEW.user_id, NOW());
    ELSIF TG_OP = 'UPDATE' THEN
        v_action := 'UPDATE';
        INSERT INTO audit_log (table_name, action, user_id, changed_at)
        VALUES (TG_TABLE_NAME, v_action, NEW.user_id, NOW());
    ELSIF TG_OP = 'DELETE' THEN
        v_action := 'DELETE';
        INSERT INTO audit_log (table_name, action, user_id, changed_at)
        VALUES (TG_TABLE_NAME, v_action, OLD.user_id, NOW());
    END IF;

    RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

-- Create trigger
CREATE TRIGGER users_audit_trigger
AFTER INSERT OR UPDATE OR DELETE ON users
FOR EACH ROW
EXECUTE FUNCTION trg_users_audit();

-- Loop examples
CREATE OR REPLACE FUNCTION loop_examples()
RETURNS VOID
AS $$
DECLARE
    v_counter INTEGER := 0;
    v_user RECORD;
BEGIN
    -- Simple loop
    LOOP
        v_counter := v_counter + 1;
        EXIT WHEN v_counter > 5;
        RAISE NOTICE 'Counter: %', v_counter;
    END LOOP;

    -- While loop
    v_counter := 0;
    WHILE v_counter < 5 LOOP
        v_counter := v_counter + 1;
        RAISE NOTICE 'While counter: %', v_counter;
    END LOOP;

    -- For loop
    FOR i IN 1..5 LOOP
        RAISE NOTICE 'For loop: %', i;
    END LOOP;

    -- For loop with step
    FOR i IN REVERSE 10..1 BY 2 LOOP
        RAISE NOTICE 'Reverse: %', i;
    END LOOP;

    -- Loop through query results
    FOR v_user IN SELECT user_id, name FROM users WHERE active = true LOOP
        RAISE NOTICE 'User: % - %', v_user.user_id, v_user.name;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- Conditional statements
CREATE OR REPLACE FUNCTION conditional_examples(p_age INTEGER)
RETURNS VARCHAR
AS $$
DECLARE
    v_result VARCHAR;
BEGIN
    -- IF-THEN-ELSE
    IF p_age >= 18 THEN
        v_result := 'Adult';
    ELSIF p_age >= 13 THEN
        v_result := 'Teen';
    ELSE
        v_result := 'Child';
    END IF;

    -- CASE statement
    v_result := CASE
        WHEN p_age >= 65 THEN 'Senior'
        WHEN p_age >= 18 THEN 'Adult'
        WHEN p_age >= 13 THEN 'Teen'
        ELSE 'Child'
    END;

    RETURN v_result;
END;
$$ LANGUAGE plpgsql;

-- Dynamic SQL
CREATE OR REPLACE FUNCTION dynamic_sql_example(p_table_name VARCHAR)
RETURNS BIGINT
AS $$
DECLARE
    v_count BIGINT;
    v_sql TEXT;
BEGIN
    v_sql := format('SELECT COUNT(*) FROM %I', p_table_name);
    EXECUTE v_sql INTO v_count;
    RETURN v_count;
END;
$$ LANGUAGE plpgsql;

-- Dynamic SQL with parameters
CREATE OR REPLACE FUNCTION dynamic_query_with_params(
    p_table_name VARCHAR,
    p_user_id INTEGER
)
RETURNS TABLE(name VARCHAR, email VARCHAR)
AS $$
BEGIN
    RETURN QUERY EXECUTE
        format('SELECT name, email FROM %I WHERE user_id = $1', p_table_name)
        USING p_user_id;
END;
$$ LANGUAGE plpgsql;

-- Exception handling
CREATE OR REPLACE FUNCTION exception_examples(p_user_id INTEGER)
RETURNS VARCHAR
AS $$
DECLARE
    v_name VARCHAR;
BEGIN
    SELECT name INTO STRICT v_name
    FROM users
    WHERE user_id = p_user_id;

    RETURN v_name;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RAISE NOTICE 'User not found';
        RETURN NULL;
    WHEN TOO_MANY_ROWS THEN
        RAISE NOTICE 'Multiple users found';
        RETURN NULL;
    WHEN OTHERS THEN
        RAISE NOTICE 'Error: %', SQLERRM;
        RAISE;
END;
$$ LANGUAGE plpgsql;

-- Custom exception
CREATE OR REPLACE FUNCTION custom_exception_example(p_user_id INTEGER)
RETURNS VOID
AS $$
DECLARE
    v_active BOOLEAN;
BEGIN
    SELECT active INTO v_active FROM users WHERE user_id = p_user_id;

    IF NOT v_active THEN
        RAISE EXCEPTION 'user_not_active'
            USING HINT = 'User must be activated first',
                  ERRCODE = 'P0001';
    END IF;
EXCEPTION
    WHEN SQLSTATE 'P0001' THEN
        RAISE NOTICE 'Custom exception caught: %', SQLERRM;
END;
$$ LANGUAGE plpgsql;

-- Arrays
CREATE OR REPLACE FUNCTION array_examples()
RETURNS VOID
AS $$
DECLARE
    v_numbers INTEGER[] := ARRAY[1, 2, 3, 4, 5];
    v_names VARCHAR[] := ARRAY['Alice', 'Bob', 'Charlie'];
    v_element INTEGER;
BEGIN
    -- Array access
    RAISE NOTICE 'First element: %', v_numbers[1];
    RAISE NOTICE 'Array length: %', array_length(v_numbers, 1);

    -- Array append
    v_numbers := array_append(v_numbers, 6);

    -- Array prepend
    v_numbers := array_prepend(0, v_numbers);

    -- Loop through array
    FOREACH v_element IN ARRAY v_numbers LOOP
        RAISE NOTICE 'Element: %', v_element;
    END LOOP;

    -- Array contains
    IF 3 = ANY(v_numbers) THEN
        RAISE NOTICE '3 is in the array';
    END IF;
END;
$$ LANGUAGE plpgsql;

-- Common Table Expression (CTE) in function
CREATE OR REPLACE FUNCTION cte_example()
RETURNS TABLE(user_id INTEGER, name VARCHAR, order_count BIGINT, total_spent NUMERIC)
AS $$
BEGIN
    RETURN QUERY
    WITH user_stats AS (
        SELECT
            o.user_id,
            COUNT(*) AS order_count,
            SUM(o.total_amount) AS total_spent
        FROM orders o
        GROUP BY o.user_id
    )
    SELECT
        u.user_id,
        u.name,
        COALESCE(us.order_count, 0) AS order_count,
        COALESCE(us.total_spent, 0) AS total_spent
    FROM users u
    LEFT JOIN user_stats us ON u.user_id = us.user_id;
END;
$$ LANGUAGE plpgsql;

-- Recursive CTE
CREATE OR REPLACE FUNCTION org_hierarchy()
RETURNS TABLE(employee_id INTEGER, name VARCHAR, manager_id INTEGER, level INTEGER)
AS $$
BEGIN
    RETURN QUERY
    WITH RECURSIVE hierarchy AS (
        -- Anchor member
        SELECT e.employee_id, e.name, e.manager_id, 0 AS level
        FROM employees e
        WHERE e.manager_id IS NULL

        UNION ALL

        -- Recursive member
        SELECT e.employee_id, e.name, e.manager_id, h.level + 1
        FROM employees e
        INNER JOIN hierarchy h ON e.manager_id = h.employee_id
    )
    SELECT * FROM hierarchy;
END;
$$ LANGUAGE plpgsql;

-- Window functions
CREATE OR REPLACE FUNCTION window_function_examples()
RETURNS TABLE(
    user_id INTEGER,
    order_id INTEGER,
    order_date TIMESTAMP,
    total_amount NUMERIC,
    order_number BIGINT,
    amount_rank BIGINT,
    user_total NUMERIC,
    prev_amount NUMERIC,
    next_amount NUMERIC
)
AS $$
BEGIN
    RETURN QUERY
    SELECT
        o.user_id,
        o.order_id,
        o.order_date,
        o.total_amount,
        ROW_NUMBER() OVER (PARTITION BY o.user_id ORDER BY o.order_date) AS order_number,
        RANK() OVER (ORDER BY o.total_amount DESC) AS amount_rank,
        SUM(o.total_amount) OVER (PARTITION BY o.user_id) AS user_total,
        LAG(o.total_amount) OVER (PARTITION BY o.user_id ORDER BY o.order_date) AS prev_amount,
        LEAD(o.total_amount) OVER (PARTITION BY o.user_id ORDER BY o.order_date) AS next_amount
    FROM orders o;
END;
$$ LANGUAGE plpgsql;

-- UPSERT with ON CONFLICT
CREATE OR REPLACE FUNCTION upsert_user(
    p_user_id INTEGER,
    p_name VARCHAR,
    p_email VARCHAR
) RETURNS VOID
AS $$
BEGIN
    INSERT INTO users (user_id, name, email, created_at)
    VALUES (p_user_id, p_name, p_email, NOW())
    ON CONFLICT (user_id) DO UPDATE
    SET name = EXCLUDED.name,
        email = EXCLUDED.email,
        updated_at = NOW();
END;
$$ LANGUAGE plpgsql;

-- JSON operations
CREATE OR REPLACE FUNCTION json_examples()
RETURNS JSONB
AS $$
DECLARE
    v_json JSONB;
BEGIN
    -- Build JSON
    v_json := jsonb_build_object(
        'name', 'Alice',
        'email', 'alice@example.com',
        'age', 30
    );

    -- Access JSON field
    RAISE NOTICE 'Name: %', v_json->>'name';

    -- Modify JSON
    v_json := jsonb_set(v_json, '{age}', '31'::jsonb);

    -- Array of JSON objects
    v_json := jsonb_agg(jsonb_build_object('id', user_id, 'name', name))
              FROM users LIMIT 5;

    RETURN v_json;
END;
$$ LANGUAGE plpgsql;

-- Advisory locks
CREATE OR REPLACE FUNCTION use_advisory_lock(p_lock_key BIGINT)
RETURNS BOOLEAN
AS $$
BEGIN
    -- Try to acquire lock
    IF pg_try_advisory_lock(p_lock_key) THEN
        RAISE NOTICE 'Lock acquired';

        -- Do some work
        PERFORM pg_sleep(1);

        -- Release lock
        PERFORM pg_advisory_unlock(p_lock_key);
        RETURN TRUE;
    ELSE
        RAISE NOTICE 'Could not acquire lock';
        RETURN FALSE;
    END IF;
END;
$$ LANGUAGE plpgsql;
