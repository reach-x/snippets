-- PL/SQL Examples - Oracle Database
-- Procedures, functions, packages, and advanced features

-- Basic procedure
CREATE OR REPLACE PROCEDURE get_user_by_id (
    p_user_id IN NUMBER
) AS
BEGIN
    FOR rec IN (
        SELECT user_id, name, email, created_at
        FROM users
        WHERE user_id = p_user_id
    ) LOOP
        DBMS_OUTPUT.PUT_LINE('User: ' || rec.name || ', Email: ' || rec.email);
    END LOOP;
END;
/

-- Procedure with OUT parameter
CREATE OR REPLACE PROCEDURE create_user (
    p_name IN VARCHAR2,
    p_email IN VARCHAR2,
    p_new_user_id OUT NUMBER
) AS
BEGIN
    INSERT INTO users (user_id, name, email, created_at)
    VALUES (user_seq.NEXTVAL, p_name, p_email, SYSDATE)
    RETURNING user_id INTO p_new_user_id;

    COMMIT;
END;
/

-- Procedure with exception handling
CREATE OR REPLACE PROCEDURE update_user_email (
    p_user_id IN NUMBER,
    p_new_email IN VARCHAR2
) AS
    v_count NUMBER;
BEGIN
    UPDATE users
    SET email = p_new_email, updated_at = SYSDATE
    WHERE user_id = p_user_id;

    v_count := SQL%ROWCOUNT;

    IF v_count = 0 THEN
        RAISE_APPLICATION_ERROR(-20001, 'User not found');
    END IF;

    COMMIT;
EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        DBMS_OUTPUT.PUT_LINE('Error: ' || SQLERRM);
        RAISE;
END;
/

-- Function (scalar)
CREATE OR REPLACE FUNCTION calculate_age (
    p_birth_date IN DATE
) RETURN NUMBER IS
    v_age NUMBER;
BEGIN
    v_age := TRUNC(MONTHS_BETWEEN(SYSDATE, p_birth_date) / 12);
    RETURN v_age;
END;
/

-- Function with table return type
CREATE OR REPLACE TYPE order_rec AS OBJECT (
    order_id NUMBER,
    order_date DATE,
    total_amount NUMBER
);
/

CREATE OR REPLACE TYPE order_table AS TABLE OF order_rec;
/

CREATE OR REPLACE FUNCTION get_user_orders (
    p_user_id IN NUMBER
) RETURN order_table PIPELINED IS
BEGIN
    FOR rec IN (
        SELECT order_id, order_date, total_amount
        FROM orders
        WHERE user_id = p_user_id
    ) LOOP
        PIPE ROW(order_rec(rec.order_id, rec.order_date, rec.total_amount));
    END LOOP;
    RETURN;
END;
/

-- Package specification
CREATE OR REPLACE PACKAGE user_pkg AS
    -- Public type declarations
    TYPE user_rec IS RECORD (
        user_id NUMBER,
        name VARCHAR2(100),
        email VARCHAR2(255)
    );

    TYPE user_table IS TABLE OF user_rec INDEX BY PLS_INTEGER;

    -- Public procedure and function declarations
    PROCEDURE get_all_users (
        p_users OUT user_table
    );

    FUNCTION get_user_count RETURN NUMBER;

    PROCEDURE add_user (
        p_name IN VARCHAR2,
        p_email IN VARCHAR2,
        p_user_id OUT NUMBER
    );
END user_pkg;
/

-- Package body
CREATE OR REPLACE PACKAGE BODY user_pkg AS
    -- Private variables
    g_user_count NUMBER := 0;

    -- Private procedure
    PROCEDURE log_action (
        p_action IN VARCHAR2
    ) IS
    BEGIN
        INSERT INTO audit_log (action, logged_at)
        VALUES (p_action, SYSDATE);
    END;

    -- Public procedure implementation
    PROCEDURE get_all_users (
        p_users OUT user_table
    ) IS
        v_index PLS_INTEGER := 1;
    BEGIN
        FOR rec IN (SELECT user_id, name, email FROM users) LOOP
            p_users(v_index).user_id := rec.user_id;
            p_users(v_index).name := rec.name;
            p_users(v_index).email := rec.email;
            v_index := v_index + 1;
        END LOOP;
    END;

    -- Public function implementation
    FUNCTION get_user_count RETURN NUMBER IS
    BEGIN
        SELECT COUNT(*) INTO g_user_count FROM users;
        RETURN g_user_count;
    END;

    -- Public procedure implementation
    PROCEDURE add_user (
        p_name IN VARCHAR2,
        p_email IN VARCHAR2,
        p_user_id OUT NUMBER
    ) IS
    BEGIN
        INSERT INTO users (user_id, name, email, created_at)
        VALUES (user_seq.NEXTVAL, p_name, p_email, SYSDATE)
        RETURNING user_id INTO p_user_id;

        log_action('User added: ' || p_name);
        COMMIT;
    END;
END user_pkg;
/

-- Trigger
CREATE OR REPLACE TRIGGER trg_users_audit
AFTER INSERT OR UPDATE OR DELETE ON users
FOR EACH ROW
DECLARE
    v_action VARCHAR2(10);
BEGIN
    IF INSERTING THEN
        v_action := 'INSERT';
    ELSIF UPDATING THEN
        v_action := 'UPDATE';
    ELSE
        v_action := 'DELETE';
    END IF;

    INSERT INTO audit_log (table_name, action, user_id, changed_at)
    VALUES ('users', v_action, NVL(:NEW.user_id, :OLD.user_id), SYSDATE);
END;
/

-- Cursor with FOR loop
DECLARE
    CURSOR user_cursor IS
        SELECT user_id, name, email
        FROM users
        WHERE active = 1;
BEGIN
    FOR rec IN user_cursor LOOP
        DBMS_OUTPUT.PUT_LINE('User: ' || rec.name);
    END LOOP;
END;
/

-- Explicit cursor
DECLARE
    CURSOR user_cursor IS SELECT user_id, name FROM users;
    v_user_id users.user_id%TYPE;
    v_name users.name%TYPE;
BEGIN
    OPEN user_cursor;
    LOOP
        FETCH user_cursor INTO v_user_id, v_name;
        EXIT WHEN user_cursor%NOTFOUND;
        DBMS_OUTPUT.PUT_LINE(v_name);
    END LOOP;
    CLOSE user_cursor;
END;
/

-- Cursor with parameters
DECLARE
    CURSOR user_cursor (p_min_id NUMBER) IS
        SELECT user_id, name
        FROM users
        WHERE user_id >= p_min_id;
BEGIN
    FOR rec IN user_cursor(100) LOOP
        DBMS_OUTPUT.PUT_LINE(rec.name);
    END LOOP;
END;
/

-- Bulk collect and FORALL
DECLARE
    TYPE user_id_table IS TABLE OF users.user_id%TYPE;
    TYPE name_table IS TABLE OF users.name%TYPE;

    v_user_ids user_id_table;
    v_names name_table;
BEGIN
    -- Bulk collect
    SELECT user_id, name
    BULK COLLECT INTO v_user_ids, v_names
    FROM users
    WHERE active = 1;

    -- Process in bulk
    FORALL i IN 1..v_user_ids.COUNT
        UPDATE user_stats
        SET last_processed = SYSDATE
        WHERE user_id = v_user_ids(i);

    COMMIT;
END;
/

-- Dynamic SQL
DECLARE
    v_table_name VARCHAR2(30) := 'users';
    v_sql VARCHAR2(1000);
    v_count NUMBER;
BEGIN
    v_sql := 'SELECT COUNT(*) FROM ' || DBMS_ASSERT.SIMPLE_SQL_NAME(v_table_name);
    EXECUTE IMMEDIATE v_sql INTO v_count;
    DBMS_OUTPUT.PUT_LINE('Count: ' || v_count);
END;
/

-- Dynamic SQL with bind variables
DECLARE
    v_sql VARCHAR2(1000);
    v_user_id NUMBER := 123;
    v_name VARCHAR2(100);
BEGIN
    v_sql := 'SELECT name FROM users WHERE user_id = :user_id';
    EXECUTE IMMEDIATE v_sql INTO v_name USING v_user_id;
    DBMS_OUTPUT.PUT_LINE('Name: ' || v_name);
END;
/

-- Exception handling
DECLARE
    v_user_id NUMBER := 999;
    v_name VARCHAR2(100);
BEGIN
    SELECT name INTO v_name
    FROM users
    WHERE user_id = v_user_id;

    DBMS_OUTPUT.PUT_LINE('User: ' || v_name);
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE('User not found');
    WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Multiple users found');
    WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error: ' || SQLERRM);
        RAISE;
END;
/

-- Custom exception
DECLARE
    e_user_not_active EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_user_not_active, -20002);

    v_active NUMBER;
BEGIN
    SELECT active INTO v_active FROM users WHERE user_id = 123;

    IF v_active = 0 THEN
        RAISE_APPLICATION_ERROR(-20002, 'User is not active');
    END IF;
EXCEPTION
    WHEN e_user_not_active THEN
        DBMS_OUTPUT.PUT_LINE('User not active');
END;
/

-- Collections
DECLARE
    TYPE number_table IS TABLE OF NUMBER;
    v_numbers number_table := number_table(1, 2, 3, 4, 5);
BEGIN
    v_numbers.EXTEND;
    v_numbers(6) := 6;

    FOR i IN 1..v_numbers.COUNT LOOP
        DBMS_OUTPUT.PUT_LINE('Number: ' || v_numbers(i));
    END LOOP;

    v_numbers.DELETE(3); -- Delete element at index 3
    v_numbers.TRIM(2);   -- Remove last 2 elements
END;
/

-- Associative array (index-by table)
DECLARE
    TYPE user_map IS TABLE OF VARCHAR2(100) INDEX BY VARCHAR2(50);
    v_users user_map;
    v_key VARCHAR2(50);
BEGIN
    v_users('alice') := 'Alice Smith';
    v_users('bob') := 'Bob Jones';

    v_key := v_users.FIRST;
    WHILE v_key IS NOT NULL LOOP
        DBMS_OUTPUT.PUT_LINE(v_key || ' = ' || v_users(v_key));
        v_key := v_users.NEXT(v_key);
    END LOOP;
END;
/

-- Hierarchical query (CONNECT BY)
SELECT
    employee_id,
    name,
    manager_id,
    LEVEL AS hierarchy_level,
    SYS_CONNECT_BY_PATH(name, ' > ') AS path
FROM employees
START WITH manager_id IS NULL
CONNECT BY PRIOR employee_id = manager_id
ORDER SIBLINGS BY name;

-- Analytic functions
SELECT
    user_id,
    order_id,
    order_date,
    total_amount,
    ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY order_date) AS order_number,
    RANK() OVER (ORDER BY total_amount DESC) AS amount_rank,
    SUM(total_amount) OVER (PARTITION BY user_id) AS user_total,
    AVG(total_amount) OVER (PARTITION BY user_id) AS user_avg,
    LAG(total_amount) OVER (PARTITION BY user_id ORDER BY order_date) AS prev_amount,
    LEAD(total_amount) OVER (PARTITION BY user_id ORDER BY order_date) AS next_amount
FROM orders;

-- MERGE statement
MERGE INTO users target
USING (SELECT 123 AS user_id, 'Alice' AS name, 'alice@example.com' AS email FROM dual) source
ON (target.user_id = source.user_id)
WHEN MATCHED THEN
    UPDATE SET target.name = source.name, target.email = source.email, target.updated_at = SYSDATE
WHEN NOT MATCHED THEN
    INSERT (user_id, name, email, created_at)
    VALUES (source.user_id, source.name, source.email, SYSDATE);
