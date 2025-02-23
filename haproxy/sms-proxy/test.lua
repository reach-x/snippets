local https = require 'ssl.https'
local r, c, h, s = https.request{
    url = "https://my-server:443/example.php",
    sink = ltn12.sink.table(resp),
    protocol = "tlsv1"
}
