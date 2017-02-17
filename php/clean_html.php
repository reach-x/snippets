<?php

$query_string = <<<EOS
                  
&key=9vZbim8rK48xPhxD9kIC&user_info=[{"COUNTRY_ID":"",
"OWNER_LIST_ID":"171",
"OWNER_LIST_CATEGORY":"US Other",
"RECEIVED_AT":"2016-05-19 14:44:50",
"FIRST_NAME":"BRU&apos;CHONDA",
"LAST_NAME":"JOHNSON",
"email":"chonda38@gmail.com",
"HOME_PHONE":"",
"MOBILE_PHONE":"",
"CITY":"ALOMA",
"STATE":"FL",
"ZIP_CODE":"32792",
"BIRTH_DATE":"",
"GENDER":"",
"IP_ADDRESS":"66.192.104.12",
"GENERATED_AT":"2016-05-18 00:00:00",
"CUSTOMER_FIELD":"",
"POST_STOP":"",
"SOURCE_DOMAIN":"directeducationcenter.com",
"TIME_TO_CALL":"",
"TOBACCO":"",
"HEIGHT_FEET":"",
"HEIGHT_INCHES":"",
"WEIGHT":"",
"SUBID_1":"",
"SUBID_2":"",
"TITLE":"",
"MARITAL_STATUS":"",
"BUSINESS_PHONE":"",
"HOMEOWNER_TENANT":"",
"STREET_ADDRESS":"3304 SUMMER WIND DR.",
"AFFILIATE_ID":"",
"COUNTRY":"",
"AGE":"",
"EDUCATION_LEVEL":"",
"SUB_ID":"",
"OWNER_LIST_COUNTRY": "US"}]
EOS;

printf("%s\n\n", expand_all_entities($query_string));

function expand_all_entities($string) {

	printf("%s\n", html_entity_decode($string, ENT_QUOTES | ENT_HTML5));
}


print_r(get_html_translation_table());



/*
newrelic-php5                  6.3.0.161-1               @newrelic
newrelic-php5-common           6.3.0.161-1               @newrelic
php                            5.3.3-47.el6              @base
php-channel-ezc                1-2.el6                   @epel
php-channel-phpunit            1.3-3.el6                 @epel
php-cli                        5.3.3-47.el6              @base
php-common                     5.3.3-47.el6              @base
php-ezc-Base                   1.8-1.el6                 @epel
php-ezc-ConsoleTools           1.6.1-1.el6               @epel
php-imap                       5.3.3-47.el6              @base
php-mbstring                   5.3.3-47.el6              @base
php-mysql                      5.3.3-47.el6              @base
php-pdo                        5.3.3-47.el6              @base
php-pear                       1:1.9.4-5.el6             @base
php-pecl-http                  2.2.1-1.el6               @epel
php-pecl-memcached             1.0.0-1.el6               @epel
php-pecl-propro                1.0.2-1.el6               @epel
php-pecl-raphf                 1.0.4-1.el6.1             @epel
php-pecl-ssh2                  0.11.0-7.el6              @epel
php-pecl-xdebug                2.1.4-2.el6               @epel
php-phpunit-File-Iterator      1.3.4-1.el6               @epel
php-phpunit-PHP-CodeCoverage   1.2.13-1.el6              @epel
php-phpunit-PHP-Invoker        1.1.3-2.el6               @epel
php-phpunit-PHP-Timer          1.0.5-1.el6               @epel
php-phpunit-PHP-TokenStream    1.2.1-1.el6               @epel
php-phpunit-PHPUnit            3.7.34-3.el6              @epel
php-phpunit-PHPUnit-MockObject 1.2.3-1.el6               @epel
php-phpunit-Text-Template      1.1.4-1.el6               @epel
php-soap                       5.3.3-47.el6              @base
php-symfony-class-loader       2.3.31-1.el6              @epel
php-symfony-common             2.3.31-1.el6              @epel
php-symfony-yaml               2.3.31-1.el6              @epel
php-tidy                       5.3.3-47.el6              @base
php-xml                        5.3.3-47.el6              @base
 */

/*
newrelic-php5
newrelic-php5-common
php
php-channel-ezc
php-channel-phpunit
php-cli
php-common
php-ezc-Base
php-ezc-ConsoleTools
php-imap
php-mbstring
php-mysql
php-pdo
php-pear
php-pecl-http
php-pecl-memcached
php-pecl-propro
php-pecl-raphf
php-pecl-ssh2
php-pecl-xdebug
php-phpunit-File-Iterator
php-phpunit-PHP-CodeCoverage
php-phpunit-PHP-Invoker
php-phpunit-PHP-Timer
php-phpunit-PHP-TokenStream
php-phpunit-PHPUnit
php-phpunit-PHPUnit-MockObject
php-phpunit-Text-Template
php-soap
php-symfony-class-loader
php-symfony-common
php-symfony-yaml
php-tidy
php-xml
*/




