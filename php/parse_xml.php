<?php

$xml = <<<XML
<?xml version="1.0" encoding="UTF-8"?>
<root>
     <Status>OK</Status>
     <BidID>149796928</BidID>
     <TransactionID>125528749</TransactionID>
     <Payout>1</Payout>
     <Log>Ping successful - OK to send lead. Estimated payout: $1.00</Log>
</root>
XML;

$xml_parser = xml_parser_create();

$values = array();
$index = array();

xml_parse_into_struct($xml_parser, $xml, $values, $index);

print_r(array(
//	'values' => $values,
//	'index' => $index,
	'payout index' => $index['PAYOUT'],
	'payout' => $values[$index['PAYOUT'][0]],
//	'payout' => $values[7],
	
));

xml_parser_free($xml_parser);
