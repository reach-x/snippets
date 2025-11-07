-- T-SQL (Transact-SQL) Examples - Microsoft SQL Server
-- Stored procedures, functions, and advanced features

-- Basic stored procedure
CREATE PROCEDURE GetUserById
    @UserId INT
AS
BEGIN
    SET NOCOUNT ON;

    SELECT user_id, name, email, created_at
    FROM users
    WHERE user_id = @UserId;
END;
GO

-- Stored procedure with output parameter
CREATE PROCEDURE CreateUser
    @Name NVARCHAR(100),
    @Email NVARCHAR(255),
    @NewUserId INT OUTPUT
AS
BEGIN
    SET NOCOUNT ON;

    INSERT INTO users (name, email, created_at)
    VALUES (@Name, @Email, GETDATE());

    SET @NewUserId = SCOPE_IDENTITY();
END;
GO

-- Stored procedure with error handling
CREATE PROCEDURE UpdateUserEmail
    @UserId INT,
    @NewEmail NVARCHAR(255)
AS
BEGIN
    SET NOCOUNT ON;
    BEGIN TRY
        BEGIN TRANSACTION;

        UPDATE users
        SET email = @NewEmail, updated_at = GETDATE()
        WHERE user_id = @UserId;

        IF @@ROWCOUNT = 0
            THROW 50001, 'User not found', 1;

        COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION;

        DECLARE @ErrorMessage NVARCHAR(4000) = ERROR_MESSAGE();
        DECLARE @ErrorSeverity INT = ERROR_SEVERITY();
        DECLARE @ErrorState INT = ERROR_STATE();

        RAISERROR(@ErrorMessage, @ErrorSeverity, @ErrorState);
    END CATCH;
END;
GO

-- Scalar function
CREATE FUNCTION dbo.CalculateAge(@BirthDate DATE)
RETURNS INT
AS
BEGIN
    DECLARE @Age INT;
    SET @Age = DATEDIFF(YEAR, @BirthDate, GETDATE()) -
               CASE WHEN DATEADD(YEAR, DATEDIFF(YEAR, @BirthDate, GETDATE()), @BirthDate) > GETDATE()
                    THEN 1 ELSE 0 END;
    RETURN @Age;
END;
GO

-- Table-valued function
CREATE FUNCTION dbo.GetUserOrders(@UserId INT)
RETURNS TABLE
AS
RETURN
(
    SELECT order_id, order_date, total_amount
    FROM orders
    WHERE user_id = @UserId
);
GO

-- Multi-statement table-valued function
CREATE FUNCTION dbo.GetTopProducts(@CategoryId INT, @TopN INT)
RETURNS @Results TABLE
(
    product_id INT,
    product_name NVARCHAR(100),
    total_sales DECIMAL(18,2)
)
AS
BEGIN
    INSERT INTO @Results
    SELECT TOP (@TopN)
        p.product_id,
        p.product_name,
        SUM(oi.quantity * oi.price) AS total_sales
    FROM products p
    INNER JOIN order_items oi ON p.product_id = oi.product_id
    WHERE p.category_id = @CategoryId
    GROUP BY p.product_id, p.product_name
    ORDER BY total_sales DESC;

    RETURN;
END;
GO

-- Trigger
CREATE TRIGGER trg_users_audit
ON users
AFTER INSERT, UPDATE, DELETE
AS
BEGIN
    SET NOCOUNT ON;

    IF EXISTS(SELECT * FROM inserted)
    BEGIN
        IF EXISTS(SELECT * FROM deleted)
        BEGIN
            -- UPDATE
            INSERT INTO audit_log (table_name, action, user_id, changed_at)
            SELECT 'users', 'UPDATE', user_id, GETDATE()
            FROM inserted;
        END
        ELSE
        BEGIN
            -- INSERT
            INSERT INTO audit_log (table_name, action, user_id, changed_at)
            SELECT 'users', 'INSERT', user_id, GETDATE()
            FROM inserted;
        END
    END
    ELSE
    BEGIN
        -- DELETE
        INSERT INTO audit_log (table_name, action, user_id, changed_at)
        SELECT 'users', 'DELETE', user_id, GETDATE()
        FROM deleted;
    END
END;
GO

-- Common Table Expression (CTE)
WITH UserStats AS (
    SELECT
        user_id,
        COUNT(*) AS order_count,
        SUM(total_amount) AS total_spent
    FROM orders
    GROUP BY user_id
)
SELECT
    u.name,
    u.email,
    us.order_count,
    us.total_spent
FROM users u
LEFT JOIN UserStats us ON u.user_id = us.user_id;

-- Recursive CTE (organizational hierarchy)
WITH OrgHierarchy AS (
    -- Anchor member
    SELECT employee_id, name, manager_id, 0 AS level
    FROM employees
    WHERE manager_id IS NULL

    UNION ALL

    -- Recursive member
    SELECT e.employee_id, e.name, e.manager_id, oh.level + 1
    FROM employees e
    INNER JOIN OrgHierarchy oh ON e.manager_id = oh.employee_id
)
SELECT * FROM OrgHierarchy;

-- Window functions
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

-- MERGE statement (upsert)
MERGE INTO users AS target
USING (SELECT @UserId AS user_id, @Name AS name, @Email AS email) AS source
ON target.user_id = source.user_id
WHEN MATCHED THEN
    UPDATE SET name = source.name, email = source.email, updated_at = GETDATE()
WHEN NOT MATCHED THEN
    INSERT (name, email, created_at)
    VALUES (source.name, source.email, GETDATE());

-- Dynamic SQL with parameters
DECLARE @TableName NVARCHAR(128) = N'users';
DECLARE @SQL NVARCHAR(MAX);

SET @SQL = N'SELECT * FROM ' + QUOTENAME(@TableName) + N' WHERE user_id = @UserId';

EXEC sp_executesql @SQL, N'@UserId INT', @UserId = 123;

-- Cursor (avoid when possible, use set-based operations)
DECLARE @UserId INT, @UserName NVARCHAR(100);

DECLARE user_cursor CURSOR FOR
    SELECT user_id, name FROM users WHERE active = 1;

OPEN user_cursor;
FETCH NEXT FROM user_cursor INTO @UserId, @UserName;

WHILE @@FETCH_STATUS = 0
BEGIN
    PRINT @UserName;
    FETCH NEXT FROM user_cursor INTO @UserId, @UserName;
END;

CLOSE user_cursor;
DEALLOCATE user_cursor;

-- Temporary tables
CREATE TABLE #TempUsers (
    user_id INT,
    name NVARCHAR(100),
    email NVARCHAR(255)
);

INSERT INTO #TempUsers SELECT user_id, name, email FROM users WHERE active = 1;

SELECT * FROM #TempUsers;

DROP TABLE #TempUsers;

-- Table variables
DECLARE @UserTable TABLE (
    user_id INT,
    name NVARCHAR(100)
);

INSERT INTO @UserTable VALUES (1, 'Alice'), (2, 'Bob');

SELECT * FROM @UserTable;

-- JSON operations (SQL Server 2016+)
SELECT
    user_id,
    name,
    email,
    (
        SELECT order_id, order_date, total_amount
        FROM orders o
        WHERE o.user_id = u.user_id
        FOR JSON PATH
    ) AS orders_json
FROM users u
FOR JSON PATH;

-- Parse JSON
DECLARE @json NVARCHAR(MAX) = N'{"name":"Alice","email":"alice@example.com"}';

SELECT
    JSON_VALUE(@json, '$.name') AS name,
    JSON_VALUE(@json, '$.email') AS email;

-- OPENJSON
SELECT *
FROM OPENJSON(@json)
WITH (
    name NVARCHAR(100) '$.name',
    email NVARCHAR(255) '$.email'
);
