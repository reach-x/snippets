<?php


$data = "HTTP/1.1 200 OK\r\nDate: Wed, 17 Aug 2016 19:34:14 GMT\r\nServer: Apache\r\nContent-Length: 126\r\nX-RateLimit-Limit: 1000\r\nX-RateLimit-Remaining: 981\r\nX-RateLimit-Reset: 1471462964\r\nConnection: close\r\nContent-Type: application/json; charset=UTF-8\r\n\r\n{meta:{api_version:1.1,offset:0,limit:0,total_results:1},links:[http://abc-marketplace.info/ea676e8ee6078b000/]}
[Wed Aug 17 12:34:14 2016] [error] [client 174.56.185.253] Array\n(\n    [hitpath_url] => http://www.lmcaligirl1992.com/rd/r.php?sid=4478&pub=270249&c1=886&c2=3250709384%7E1700167625%7E1%7E1%7E1%7E2933588&c3=2016-8-17&topica_list_id=1700167625&manager_post_id=3250709384&first_name=Helen&last_name=Martin&email_address=Alloascot%40yahoo.com&zip_code=31322&city=POOLER&state=GA\n)\n";


printf("%s", $data);



// $contents = file_get_contents(test.txt);
// $contents = iconv(UTF-8, ISO-8859-1//TRANSLIT, $contents);
// printf(%s,$contents);

