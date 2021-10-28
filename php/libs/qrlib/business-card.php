<?php


include('qrlib.php');
include('qrconfig.php');

// how to build raw content - QRCode with detailed Business Card (VCard)

$tempDir = '/tmp/';
//
//// here our data
//$name = 'John Doe';
//$sortName = 'Doe;John';
//$phone = '(049)012-345-678';
//$phonePrivate = '(049)012-345-987';
//$phoneCell = '(049)888-123-123';
//$orgName = 'My Company Inc.';
//
//$email = 'john.doe@example.com';
//
//// if not used - leave blank!
//$addressLabel = 'Our Office';
//$addressPobox = '';
//$addressExt = 'Suite 123';
//$addressStreet = '7th Avenue';
//$addressTown = 'New York';
//$addressRegion = 'NY';
//$addressPostCode = '91921-1234';
//$addressCountry = 'USA';
//
//// we building raw data
//$codeContents = 'BEGIN:VCARD' . "\n";
//$codeContents .= 'VERSION:2.1' . "\n";
//$codeContents .= 'N:' . $sortName . "\n";
//$codeContents .= 'FN:' . $name . "\n";
//$codeContents .= 'ORG:' . $orgName . "\n";
//
//$codeContents .= 'TEL;WORK;VOICE:' . $phone . "\n";
//$codeContents .= 'TEL;HOME;VOICE:' . $phonePrivate . "\n";
//$codeContents .= 'TEL;TYPE=cell:' . $phoneCell . "\n";
//
//$codeContents .= 'ADR;TYPE=work;' .
//	'LABEL="' . $addressLabel . '":'
//	. $addressPobox . ';'
//	. $addressExt . ';'
//	. $addressStreet . ';'
//	. $addressTown . ';'
//	. $addressPostCode . ';'
//	. $addressCountry
//	. "\n";
//
//$codeContents .= 'EMAIL:' . $email . "\n";
//
//$codeContents .= 'END:VCARD';

$codeContents = <<<EOS
BEGIN:VCARD
VERSION:3.0
FN;CHARSET=UTF-8:John N Brahy
N;CHARSET=UTF-8:Brahy;John;N;Mr.;II
NICKNAME;CHARSET=UTF-8:JB
GENDER:M
BDAY:19720519
ANNIVERSARY:20051001
EMAIL;CHARSET=UTF-8;type=HOME,INTERNET:john@brahy.com
EMAIL;CHARSET=UTF-8;type=WORK,INTERNET:jbrahy@popularllc.com
LOGO;ENCODING=b;TYPE=PNG:IMAGEDATA..
PHOTO;ENCODING=b;TYPE=JPEG:IMAGEDATA..
TEL;TYPE=CELL:4242374565
LABEL;CHARSET=UTF-8;TYPE=HOME:Home
ADR;CHARSET=UTF-8;TYPE=HOME:;;3005 East Pacific Coast Highway;Signal Hill;CA;90755;United States of America
LABEL;CHARSET=UTF-8;TYPE=WORK:Work
ADR;CHARSET=UTF-8;TYPE=WORK:;;16000 Fontaine Ave;Austin;TX;78734;United States of America
TITLE;CHARSET=UTF-8:Chief Technology Officer
ROLE;CHARSET=UTF-8:Executive
ORG;CHARSET=UTF-8:Popular Marketing
URL;type=WORK;CHARSET=UTF-8:https://www.popularmarketing.com/
X-SOCIALPROFILE;TYPE=linkedin:https://www.linkedin.com/in/jbrahy/
REV:2021-10-21T21:27:04.068Z
END:VCARD
EOS;

// generating
QRcode::png($codeContents, $tempDir . '026.png', QR_ECLEVEL_L, 3);

// displaying
// echo '<img src="' . EXAMPLE_TMP_URLRELPATH . '026.png" />';