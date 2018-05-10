<?php

$xml = <<<EOX
<OrderCardRequest version="1.6">
   <credentials> 
      <username>wax_popl</username> 
      <password>aP3eS2Fw</password> 
      <distributor>netspend_onlin</distributor>
   </credentials> 
   <affiliateInfo> 
      <AID>POPL</AID>
      <siteId>GETPAIDTOTRY</siteId>
      <sub></sub>
   </affiliateInfo> 
   <accountInfo> 
      <firstName>latunya</firstName>
      <middleInitial></middleInitial>
      <lastName>powell</lastName>
      <emailAddress>tbopp_09@yahoo.com</emailAddress>
      <ssn/>
      <phoneNumber></phoneNumber>
      <dob></dob>
      <address>
        <line1>2355 N State Highway 360</line1>
        <aptOrSuite/>
        <zip/>75050-8720</zip>
      </address>
      <shipToAddress>
          <line1></line1>
          <aptOrSuite></aptOrSuite>
          <zip></zip>
      </shipToAddress>
      <ipAddress><ip_address></ipAddress>
   </accountInfo> 
   <callerReferenceNumber></callerReferenceNumber> 
   <imageReferenceId></imageReferenceId> 
</OrderCardRequest>
EOX;

function remove_unused_xml_template_variables ($xml) {

    $matches = array();

    preg_match_all("/(\%\%.*\%\%)/", $xml, $matches);

    foreach ($matches as $match) {
        $xml = str_replace($match, '', $xml);
    }

    return $xml;
}

printf("%s\n", remove_unused_xml_template_variables($xml));

//printf("%s\n", $xml);
