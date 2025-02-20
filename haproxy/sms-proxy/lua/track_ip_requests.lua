local ip_tracker = {}  -- Table to track IP addresses per token

-- Function to handle and track IP requests for tokens
core.register_action("track_ip_requests", {"http-req"}, function(txn)
    local client_ip = txn.sf:req_fhdr("X-Forwarded-For") or txn.f:src()  -- Get client IP
    local path = txn.sf:path()  -- Get the requested path
    local referer = txn.sf:req_fhdr("Referer") or "None"  -- Get the Referer header, default to "None"
    local token = path:match("/(%w+)$")  -- Extract the token from the URL

    if token then
        -- Initialize the token entry if it doesn't exist
        if not ip_tracker[token] then
            ip_tracker[token] = { ips = {}, referers = {} }
        end

        -- Track unique IP addresses for the token
        if not ip_tracker[token].ips[client_ip] then
            ip_tracker[token].ips[client_ip] = true  -- Mark IP as seen for this token
        end

        -- Track unique Referer headers for the token
        if not ip_tracker[token].referers[referer] then
            ip_tracker[token].referers[referer] = true  -- Mark Referer as seen for this token
        end
    end
end)

-- Function to print or log the tracked IP counts and Referer headers
core.register_service("log_ip_counts", "http", function(applet)
    local counts = {}
    for token, data in pairs(ip_tracker) do
        -- Count unique IPs
        local unique_ips = 0
        for _ in pairs(data.ips) do
            unique_ips = unique_ips + 1
        end

        -- Collect unique Referers
        local referers_list = {}
        for referer, _ in pairs(data.referers) do
            table.insert(referers_list, referer)
        end

        table.insert(counts, string.format("Token: %s, Unique IPs: %d, Referers: %s", token, unique_ips, table.concat(referers_list, ", ")))
    end

    -- Respond with the counts and referers
    applet:set_status(200)
    applet:add_header("Content-Type", "text/plain")
    applet:start_response()
    applet:send(table.concat(counts, "\n"))
end)

