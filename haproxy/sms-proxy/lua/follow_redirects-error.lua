http = require("socket.http")
ltn12 = require("ltn12")
json = require("cjson.safe")

function printTable(tbl, indent)
    if not indent then
        indent = 0
    end
    for k, v in pairs(tbl) do
        formatting = string.rep("  ", indent) .. k .. ": "
        if type(v) == "table" then
            print(formatting)
            printTable(v, indent + 1)
        else
            print(formatting .. tostring(v))
        end
    end
end

function filter_empty_lines(t)
    local filtered = {}
    for _, line in ipairs(t) do
        if line ~= "" then
            table.insert(filtered, line)
        end
    end
    return filtered
end


function clean_json_string(s)
    -- Remove leading `""` and any whitespace/newlines
    s = s:gsub('^%s*""%s*', '')
    return s
end

core.register_service("follow_redirects", "http", function(applet)
    local debug = false
     --  local debug = true
    local max_redirects = 10
    local user_id = applet.path:sub(3)
    local url = "http://170.39.213.118/advertorialdata?click_id=" .. user_id
    local headers = {}
    local original_headers = {}

    if debug == true then
        print(" >>>>>>>>>>>>>>>>>>  Headers   <<<<<<<<<<<<<<<<<<")
        printTable(applet.headers, 0)
        print(" >>>>>>>>>>>>>>>>>>  Headers   <<<<<<<<<<<<<<<<<<")
    end

    local req_headers = applet.headers

    for name, values in pairs(req_headers) do
        if type(values) == "table" then
            headers[name] = values[0] or ""
        else
            headers[name] = values or ""
        end
    end

    if debug == true then
        print("========= headers =========\n")
        printTable(headers, 0)
        print("========= original_headers =========\n")
        printTable(original_headers, 0)
        print("========= end headers =========\n")
    end

    original_headers["X-Forwarded-For"] = applet.sf:src() 
    original_headers["User-Agent"] = headers["user-agent"] or "SMS Manager Pro"

    local count = 0
    local response = {}

    -- this adds the user to clicksync
-- local result, status_code, headers, status_line = http.request {
-- 	headers = original_headers,
-- 	url = "https://clicksync.popularllc.com/endpoint/" .. user_id,
-- 	redirect = false,
-- 	sink = ltn12.sink.table(response)
--   }

    local result, status_code, headers, status_line = http.request {
	headers = original_headers,
        url = url,
        redirect = false,
        sink = ltn12.sink.table(response)
    }

    applet:add_header("X-SMP-Redirect-Code", status_code)
    applet:add_header("X-SMP-destination",url)

    if debug == true then
        print("URL Called : ", url)
        print("result: ", result)
    end

    -- Parse the first response and extract 'destination_url'
    -- this needs to have error handling around it.
    clean_response = filter_empty_lines(response)
    local first_body = table.concat(clean_response)
    clean_body = clean_json_string(first_body)
    local data, json_error = json.decode(clean_body)
    
	if json_error then
		applet:set_status(512)
		applet:add_header("Content-Type", "text/plain")
		applet:start_response()
		applet:send(first_body .. "\n" .. json_error)
		return
	end

    local url = data.destination_url

    while count < max_redirects do
    	-- applet:add_header("X-original-headers", original_headers)
	local user_agent = headers["user-agent"] or "No User Agent"
        applet:add_header("X-user-agent", user_agent)

        -- Sending the request
        local result, status_code, headers, status_line = http.request {
            headers = request_headers,
            url = url,
            redirect = false,
            sink = ltn12.sink.table(response)
        }

        applet:add_header("X-last-url", url)
        applet:add_header("X-status-line", status_line or "no status line") 
        applet:add_header("X-status-code", status_code or "no status code")

        if debug == true then
            print("Completed-------------------------------------------------")
            print("Result:", result)
            print("Status Code:", status_code)
            print("Status Line:", status_line)
            print("/Completed-------------------------------------------------")
        end

        if not result then
		-- Responding with a redirect to the final URL
		applet:set_status(302)
		applet:add_header("Location", url)
		break
	end

    applet:start_response()

    if #response > 0 then
        applet:send(table.concat(response))
    end


        --    applet:set_status(511)
        --    applet:add_header("Content-Type", "text/plain")
        --    applet:start_response()
	--    applet:send("Error from URL: " .. url)
        --    applet:send("Request failed: " .. status_line or "no status line")
        --    return

        -- If the response is not a redirect, break the loop
        if status_code ~= 301 and status_code ~= 302 then
            break
        end

        -- Updating URL for the next request
        url = headers["location"]

        if debug == true then
        	for k, v in pairs(headers) do
            		print("HEADER:(" .. k .. "): " .. v)
        	end
	end

        if debug == true then
            print("NEW URL: ------------------------------")
            print("URL: " .. url)
            print("NEW URL: ------------------------------")
        end

        count = count + 1
        response = {}
    end

    -- Responding with a redirect to the final URL
    applet:set_status(302)
    applet:add_header("Location", url)

    for k, v in pairs(headers) do
        if k ~= "location" and k~= "content-length"then
            applet:add_header(k, v)
        end
    end

    applet:start_response()

    if #response > 0 then
        applet:send(table.concat(response))
    end

end)
