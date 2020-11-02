<?php

//
//symbols = "123456789BCDFGHJKLMNPQRSTWXZ";
//    OFFER_ENCODING_SALT = 853569;
//    AFFILIATE_ENCODING_SALT = 781345;
//
//encodeOffer(Long networkOfferId) {
//        return encodeLong(networkOfferId * OFFER_ENCODING_SALT);
//    }
//
//    encodeAffiliate(Long networkAffiliateId) {
//        return encodeLong(networkAffiliateId * AFFILIATE_ENCODING_SALT);
//    }
//
//    ncodeLong(long num) {
//        final int B = symbols.length();
//        StringBuilder sb = new StringBuilder();
//        while (num != 0) {
//            sb.append(symbols.charAt((int) (num % B)));
//            num /= B;
//        }
//        return sb.reverse().toString();
//    }
//

class everflow_encoder {

    public $symbols;
    public $OFFER_ENCODING_SALT = 853569;
    public $AFFILIATE_ENCODING_SALT = 781345;

    function __construct() {
        $this->symbols = str_split("123456789BCDFGHJKLMNPQRSTWXZ", 1);
    }

    function encode_offer($network_offer_id) {
        return $this->encode($network_offer_id * $this->OFFER_ENCODING_SALT);
    }

    function encode_affiliate($network_affiliate_id) {
        return $this->encode($network_affiliate_id * $this->AFFILIATE_ENCODING_SALT);
    }

    function encode($num) {

        $symbol_count = count($this->symbols);
        $encoded_string = "";
        $counter = 0;

        while ($num != 0) {
            $counter++;
            $char_at = floor($num % $symbol_count);
            $character = $this->symbols[$char_at];
            $encoded_string .= $character;
            $num = floor($num / $symbol_count);
        }

        return strrev($encoded_string);
    }
}

$encoder = new everflow_encoder();

printf("   Sample URL: https://www.clickdealing.net/JLQXQD4G/CSF1RMH/\n");
printf("Generated URL: https://www.clickdealing.net/%s/%s/\n", $encoder->encode_affiliate(270001), $encoder->encode_offer(6118));

$affiliates = range(270001, 272000);
$offers = range(0, 9000);

$file_handle = fopen("affiliate_map.txt", "w");
fprintf($file_handle, "%s\t%s\n", "encoded_affiliate", "affiliate_id");
foreach ($affiliates as $affiliate_id) {
    fprintf($file_handle, "%s\t%s\n", $encoder->encode_affiliate($affiliate_id), $affiliate_id);
}
fclose($file_handle);

$file_handle = fopen("offer_map.txt", "w");
fprintf($file_handle, "%s\t%s\n", "encoded_offer_id", "offer_id");
foreach ($offers as $offer_id) {
    fprintf($file_handle, "%s\t%s\n", $encoder->encode_offer($offer_id), $offer_id);
}
fclose($file_handle);



//printf("%s\t%s\t%s\t%s\n", "encoded_affiliate", "encoded_offer", "affiliate_id", "offer_id");
//foreach ($affiliates as $affiliate_id) {
//    foreach ($offers as $offer_id) {
//        printf("%s\t%s\t%s\t%s\n", $encoder->encode_affiliate($affiliate_id), $encoder->encode_offer($offer_id), $affiliate_id, $offer_id);
//    }
//}

