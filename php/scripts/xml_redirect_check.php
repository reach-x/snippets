#!/usr/bin/php


<?php

$xml_code=<<<EOX
<response>
    <result>success</result>
    <lead_id>80HDCOXZ</lead_id>
    <price>37.55</price>
    <redirect_url>https%3A%2F%2Fwww.networx.com%2Flead-confirmation%3Fid%3D28219602%26aff_token%3D1139766f2af9cd4268768140644%26utm_source%3D11397%26utm_medium%3Daffiliate</redirect_url>
    <msg>Lead Accepted</msg>
</response>
EOX;

$xml = simplexml_load_string($xml_code);
$redirect_url = urldecode($xml->redirect_url);

printf("%s\n",$redirect_url);
