
core.register_fetches("forensiq", function(txn)

-- load up the libraries
   local http_util = require "http.util" -- for dict_to_query
--   local http = require "socket"

local io = require("io")
local http = require("socket.http")
local ltn12 = require("ltn12")

--
-- Connect to Forensiq fraud detection web service
-- https://2pth.com/check?query_string
-- parameters

-- local variables
-- local api_root = "https://2pth.com"
    -- local api_root = "https://admin.populardatamanagement.com/public/fraud/index"
    local api_root = "https://admin.populardatamanagement.com/public/fraud"
    local headers = ""
    local parameters = {}

--  ck = client key
    parameters["ck"] = "clientkey"

--  rt = request type (display|click|action)
    parameters["rt"] = "click"

--  ip = ip address (Dot-decimal notation)
    local clientip = txn.f:src()
    parameters["ip"] = clientip

--  s = session id
    parameters["s"] = ""

--  p = publisher id
    parameters["p"] = ""

--  cvtm = Conversion Time
    local now = os.date("*t")
    parameters["ctvm"] = string.format("%d-%02d-%02d %02d:%02d:%02d",now["year"],now["month"],now["day"],now["hour"],now["min"],now["sec"])

--  ctm = Click Time
    parameters["ctm"] = string.format("%d-%02d-%02d %02d:%02d:%02d",now["year"],now["month"],now["day"],now["hour"],now["min"],now["sec"])

--  itm = Impression Time
    parameters["itm"] = string.format("%d-%02d-%02d %02d:%02d:%02d",now["year"],now["month"],now["day"],now["hour"],now["min"],now["sec"])

--  art = Request Type of the Attributed Event (click|impression)
    parameters["art"] = "click"

--  a = Sub Source
    parameters["a"] = ""

--  cmp = Campaign ID
    parameters["cmp"] = ""

--  stId = Sale Transaction ID -
    parameters["stId"] = ""

--  rf = Referring Source
    parameters["rf"] = txn.sf:req_fhdr("host")

--  ua = User Agent
    parameters["ua"] = txn.sf:req_fhdr("host")

--  tm = Date Time Value
    parameters["tm"] = ""

--  output = format (JSON|XML)
    parameters["output"] = "JSON"

--  pfm = The platform
    parameters["pfm"] = ""

--  c1,c2...c5 = optional variables for client tracking
-- parameters["c1"] = ""
-- parameters["c2"] = ""
-- parameters["c3"] = ""
-- parameters["c4"] = ""
-- parameters["c5"] = ""
--
--    for k,v in parameters(t) do print(k,v) end

    local api_call = api_root .. "?" .. http_util.dict_to_query(parameters)

    print("API CALL: ", api_call)

    result, statuscode, content = http.request(api_call)

    -- result = http.request(api_call)
    --
    -- for i,v in pairs(http) do
    --    print("\t",i, v)
    -- end
    --

	print("result: ",result);
	print("statusCode: ",statusCode);
	print("content: ",content);

	return 1

end)