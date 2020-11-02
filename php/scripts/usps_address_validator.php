<?php
//Your Username is 449POPUL4321
//Your Password is 912OQ24OJ288
//
$name = filter_input( INPUT_GET, "name", FILTER_SANITIZE_STRING );
$address = filter_input( INPUT_GET, "address", FILTER_SANITIZE_STRING );
$city = filter_input( INPUT_GET, "city", FILTER_SANITIZE_STRING );
$state = filter_input( INPUT_GET, "state", FILTER_SANITIZE_STRING );
$zip5 = filter_input( INPUT_GET, "zip5", FILTER_SANITIZE_STRING );
$zip4 = filter_input( INPUT_GET, "zip4", FILTER_SANITIZE_STRING );

$xml = <<<XML
<AddressValidateRequest USERID="449POPUL4321">
    <Address ID="0">
        <Address1>{$name}</Address1>
        <Address2>{$address}</Address2>
        <City>{$city}</City>
        <State>{$state}</State>
        <Zip5>{$zip5}</Zip5>
        <Zip4>{$zip4}</Zip4>
    </Address>
</AddressValidateRequest>
XML;

$xml = urlencode( $xml );
$url = "http://production.shippingapis.com/ShippingAPI.dll?API=Verify&XML={$xml}";

$curl_handle = curl_init();
curl_setopt( $curl_handle, CURLOPT_URL, $url );
curl_setopt( $curl_handle, CURLOPT_HTTPGET, 1 );
curl_setopt( $curl_handle, CURLOPT_FOLLOWLOCATION, 1 );
curl_setopt( $curl_handle, CURLOPT_HEADER, 0 );
curl_setopt( $curl_handle, CURLOPT_RETURNTRANSFER, 1 );
curl_setopt( $curl_handle, CURLOPT_VERBOSE, 0 );

$output = curl_exec( $curl_handle );
$xml = simplexml_load_string( $output );
$xml = json_encode( $xml );
$xml = json_decode( $xml, TRUE );

if ( array_key_exists( "Error", $xml['Address'] ) ) {
    print json_encode( array(
        'result'  => 'error',
        'message' => $xml['Address']['Error']['Description'],
    ) );
} else {

    $address = $xml['Address'];

    print json_encode( array(
        'result'   => 'success',
        'address1' => $address['Address1'],
        'address2' => $address['Address2'],
        'city'     => $address['City'],
        'state'    => $address['State'],
        'zip5'     => $address['Zip5'],
        'zip4'     => $address['Zip4'],
    ) );
}
