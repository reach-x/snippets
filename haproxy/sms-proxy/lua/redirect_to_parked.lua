-- Function to extract the base domain from the Host header
function extract_base_domain(host)
    -- Use a pattern to remove subdomains, leaving only the base domain
    local base_domain = host:match("([^%.]+%.[^%.]+)$")
    return base_domain or host  -- Fallback to the full host if no match
end

-- Register the function as an HTTP request action
core.register_service("redirect_to_parked", {"http-req"}, function(txn)
    local host = txn.sf:req_fhdr("Host") or ""
    local base_domain = extract_base_domain(host)
    local redirect_url = "https://parked-domain-manager.com/?domain=" .. base_domain

    -- Perform the redirection
    txn.res:set_status(302)
    txn.res:add_header("Location", redirect_url)
    txn:done()
end)

