http = require("socket.http")
ltn12 = require("ltn12")
json = require("cjson.safe")

env = require "luasql.mysql".mysql()
conn = env:connect("haproxy", "haproxy", "AKZxXuo7KBd0QPDn", "pm-prod-database.cjcyp4tyx7zk.us-east-1.rds.amazonaws.com", 3306) -- env:connect("dbname","user","password","host","port")


core.register_service("report_errors", "http", function(applet)

	local req_headers = applet.headers
	local response = {}
	local status_code = applet.status

	if status_code ~= nil then
		if status_code == 404 or status_code == 500 then
			local sql = string.format([[ INSERT INTO haproxy.bad_requests (request_url) VALUES ('%s') ]], applet.path)
			cursor, errorString = assert(conn:execute(sql))
			row = cursor:fetch({}, "a")
		end
	end

	return

end)
