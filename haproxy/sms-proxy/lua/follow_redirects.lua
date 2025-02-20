http = require("socket.http")
ltn12 = require("ltn12")
json = require("cjson.safe")
socket = require("socket")
https = require("ssl.https")

local function trim(s)
    return s:match("^%s*(.-)%s*$") -- Removes leading and trailing whitespace
end

function filter_empty_lines(t)
    if type(t) ~= "table" then
        core.log(core.err, "filter_empty_lines received a non-table value: " .. tostring(t))
        return {} -- Return an empty table instead of failing
    end

    local filtered = {}
    for _, line in ipairs(t) do
        if line ~= "" then
            table.insert(filtered, line)
        end
    end
    return filtered
end

function log(message)
    core.log(core.info, message)
end

-- Remove leading `""` and any whitespace/newlines
function clean_json_string(s)
    s = s:gsub('^%s*""%s*', '')
    return s
end

core.register_service("follow_redirects", "http", function(applet)
    local start_time = socket.gettime()  
    local debug = false
    local max_redirects = 10
    local user_id = applet.path:sub(3)
    local url = "https://clickserver.smsmanagerpro.com/advertorialdata?click_id=" .. user_id
--  local url = "https://170.39.213.118/advertorialdata?click_id=" .. user_id
    
    local headers = {}
    local original_headers = {}

    -- Get original request headers
    local req_headers = applet.headers or {}

    for name, values in pairs(req_headers) do
        if type(values) == "table" then
            headers[name] = values[1] or ""
        else
            headers[name] = values or ""
        end
    end

    -- Set required headers
    original_headers["X-Forwarded-For"] = applet.sf:src() or ""
    original_headers["User-Agent"] = headers["user-agent"] or "SMS Manager Pro"

    local bot_score = headers["bot-score"] or "0"
    local verified_bot =  headers["verified-bot"] or "false"

    if bot_score ~= "0" and bot_score ~= "" then
        log("LUA: bot score: " .. bot_score)
        log("LUA: verified bot: " .. verified_bot)
    end

    local count = 0
    local response = {}

    core.log(core.info, "LUA: calling " .. url)
    local start_clickserver_request_time = socket.gettime()

    local result, status_code, res_headers, status_line = https.request {
        url = url,
        headers = original_headers,
        redirect = false,
        sink = ltn12.sink.table(response),
        verify = "none",   -- Disable SSL certificate validation
        options = "all"    -- Allow all SSL options
    }

    local clickserver_execution_time = math.floor(((socket.gettime() - start_clickserver_request_time ) * 1000))

    -- Ensure response is valid
    if type(response) ~= "table" or #response == 0 then
        core.log(core.err, "LUA: API response is empty or not a table")
        applet:set_status(500)
        applet:add_header("Content-Type", "text/plain")
        applet:start_response()
        applet:send("Error: Empty or invalid response from click server")
        return
    end

    local response_body = table.concat(response)  -- Join table elements into a single string

    local after_clickserver_request_time = socket.gettime()

    -- Ensure response headers are always a table
    if type(res_headers) ~= "table" then
        res_headers = {}
    end

    -- Handle failed request
    if not result then
        applet:set_status(500)
        applet:add_header("Content-Type", "text/plain")
        applet:start_response()
        applet:send("Request failed for URL: " .. url)
        return
    end

    -- Ensure response_body is reconstructed correctly
    local clean_response = filter_empty_lines(response)  -- Correct
    local response_body = table.concat(clean_response)  -- Join table elements into a single string

    -- Debugging: Log response before parsing
    -- core.log(core.info, "LUA: Raw JSON Response: " .. (response_body or "EMPTY RESPONSE"))

    -- Ensure response_body is not nil or empty
    if not response_body or response_body == "" then
        applet:set_status(500)
        applet:add_header("Content-Type", "text/plain")
        applet:start_response()
        applet:send("Error: Empty response from server")
        return
    end

    -- Attempt to decode JSON
    local data, json_error = json.decode(response_body)

    if json_error then
        -- Log raw JSON for debugging
        core.log(core.err, "LUA ERROR: JSON Parse Error: " .. json_error)
        core.log(core.err, "LUA ERROR: JSON RAW: " .. response_body)
        applet:set_status(500)
        applet:add_header("Content-Type", "text/plain")
        applet:start_response()
        applet:send("JSON Parse Error: " .. json_error)
        return
    end

    -- Ensure required field exists
    if not data.destination_url then
        applet:set_status(500)
        applet:add_header("Content-Type", "text/plain")
        applet:start_response()
        applet:send("Error: Missing 'destination url' from click server")
        return
    end

    url = data.destination_url  -- Safe to access after validation

    -- Redirect loop
    while count < max_redirects do
        local now = socket.gettime()
        if now - start_time > 5 then  -- Stop execution after 5 seconds
            core.log(core.info, "LUA: Breaking Redirect for user_id: " .. user_id)
            break
        end

        local response = {}

        local result, status_code, res_headers, status_line = https.request {
            url = url,
            headers = original_headers,
            redirect = false,
            sink = ltn12.sink.table(response),
            verify = "none",   -- Disable SSL certificate validation
            options = "all"    -- Allow all SSL options
        }

        -- Ensure response headers are always a table
        if type(res_headers) ~= "table" then
            res_headers = {}
        end

        -- Log HTTP request failure
        if not result then
            core.log(core.err, "LUA: HTTP request failed for URL: " .. url)
            applet:set_status(500)
            applet:add_header("Content-Type", "text/plain")
            applet:start_response()
            applet:send("Error processing dataset")
            return
        end

        -- If not a redirect, break the loop
        if status_code ~= 301 and status_code ~= 302 then
            break
        end

        -- Updating URL for the next request
        if res_headers["location"] then
            url = res_headers["location"]
        else
            break  -- Stop redirect loop if no Location header
        end

        count = count + 1
    end

    -- Responding with a redirect to the final URL
    applet:set_status(302)
    applet:add_header("Location", url)

    for k, v in pairs(res_headers) do
        if k ~= "location" and k ~= "content-length" and k ~= "x-powered-by" then
            applet:add_header(k, v)
        end
    end

    -- Logging execution time
    local end_time = socket.gettime()
    local execution_time = (end_time - start_time) * 1000  -- Convert to ms
    local after_clickserver_execution_time = (socket.gettime() - after_clickserver_request_time) * 1000

    core.log(core.info, "User: " .. (data.fname or "") .. " " .. (data.lname or "") .. " Phone: " .. (data.phone or "") .. " Email: " .. (data.email or ""))
    core.log(core.info, "LUA: Followed " .. count .. " redirect(s). Clickserver execution time " .. math.floor(clickserver_execution_time) .. " ms Total Execution time: " .. math.floor(execution_time) .. " ms.")

    applet:start_response()
end)
