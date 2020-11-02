<?php

$string = convert_utf8('Zavala Hernɬndez');
$charset_in = "UTF-8";
$charset_out = "UTF-8";

print_r(array(
	'string'=>$string,
	'converted'=>mb_convert_encoding($string, $charset_out,$charset_in)
));


echo 'Original : ', $string, PHP_EOL;
echo 'TRANSLIT : ', iconv($charset_in,"{$charset_out}//TRANSLIT", $string), PHP_EOL;
echo 'IGNORE   : ', iconv($charset_in,"{$charset_out}//IGNORE", $string), PHP_EOL;
echo 'Plain    : ', iconv($charset_in,$charset_out, $string), PHP_EOL;
echo 'Mix      : ', iconv("UTF-8", "ASCII//IGNORE//TRANSLIT", $string), PHP_EOL;
echo 'Mix      : ', iconv("UTF-8", "UTF-8//IGNORE//TRANSLIT", $string), PHP_EOL;


function convert_utf8( $string ) {
    if ( strlen(utf8_decode($string)) == strlen($string) ) {  
        // $string is not UTF-8
        return iconv("ISO-8859-1", "UTF-8", $string);
    } else {
        // already UTF-8
        return $string;
    }
}
