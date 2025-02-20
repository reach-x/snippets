local http = require("socket.http")
local env = require "luasql.mysql".mysql()


-- MySQL credentials
local DB_HOST = "pm-prod-database.cjcyp4tyx7zk.us-east-1.rds.amazonaws.com"
local DB_USER = "haproxy"
local DB_PASS = "AKZxXuo7KBd0QPDn"
local DB_NAME = "haproxy"
local DB_PORT = 3306


-- Function to establish a MySQL connection
local function get_db_connection()
    local conn = env:connect(DB_NAME, DB_USER, DB_PASS, DB_HOST, DB_PORT)
    if not conn then
        core.Alert("Database connection failed!")
        return nil
    end
    return conn
end

-- Function to sanitize SQL inputs (prevents SQL injection)
local function escape_sql(value)
    if not value then return "NULL" end
    return "'" .. value:gsub("'", "''") .. "'"
end


core.register_action("log_request", {"http-req"}, function(txn)
    local conn = get_db_connection()
    if not conn then return end  -- Stop if DB connection fails

    local client_ip = txn.sf:req_fhdr("X-Forwarded-For") or txn.f:src() or "UNKNOWN"
    local method = txn.sf:method() or "UNKNOWN"
    local path = txn.sf:path() or "/"
    local hostname = txn.sf:req_fhdr("Host") or "UNKNOWN"

    -- Prevent SQL Injection
    local sql = string.format(
        [[INSERT INTO request_log (client_ip, method, path, hostname) VALUES (%s, %s, %s, %s)]],
        escape_sql(client_ip), escape_sql(method), escape_sql(path), escape_sql(hostname)
    )

    local res, err = conn:execute(sql)
    if not res then
        core.Alert("Database Error (log_request): " .. err)
    end

    conn:close()  -- Close the DB connection after use
end)


core.register_action("log_response", {"http-res"}, function(txn)
    local conn = get_db_connection()
    if not conn then return end  -- Stop if DB connection fails

    local status_code = txn.sf:status() or 0
    local path = txn.sf:path() or "/"
    local hostname = txn.sf:req_fhdr("Host") or "UNKNOWN"

    -- Prevent SQL Injection
    local sql = string.format(
        [[INSERT INTO response_log (status_code, hostname, path) VALUES (%d, %s, %s)]],
        status_code, escape_sql(hostname), escape_sql(path)
    )

    local res, err = conn:execute(sql)
    if not res then
        core.Alert("Database Error (log_response): " .. err)
    end

    conn:close()  -- Close the DB connection after use
end)

