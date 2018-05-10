<?php

$post_data = array(
"BIRTH_DATE" => "9/6/86",
"EMAIL_ADDRESS" => "zz99@inbox.lv",
"FIRST_NAME" => "MAREKS",
"GENERATED_AT" => "2014-10-15 00:00:00",
"IP_ADDRESS" => "1.1.1.1",
"LAST_NAME" => "KNOPE",
"SOURCE_DOMAIN" => "aspiremoney.co.uk",
"variable_data" => "AFF_ID=33&CKM_CAMPAIGN_ID=583&CKM_KEY=RBr19kHA9CE&em=%%EMAIL_ADDRESS%%&fn=%%FIRST_NAME%%&ln=%%LAST_NAME%%&dob=%%BIRTH_DATE%%&pc=%%ZIP_CODE%%&mp=0%%MOBILE_PHONE%%&IP=%%IP_ADDRESS%%&HP=0%%HOME_PHONE%%&A1=%%STREET_ADDRESS%%&TO=%%CITY%%&PC=%%ZIP_CODE%%&CO=%%STATE%%&WS=%%SOURCE_DOMAIN%%&OPTIN=TRUE&PTYPE=MINI"
);

$post_data = build_templated_post_fields( $post_data ) ;
print_r($post_data);


function build_templated_post_fields( $post_data ) {

    // this is the template for the posted data
    $post_fields = $post_data['variable_data'];

    if ( isset($post_data['GENERATED_AT']) && strtotime($post_data['GENERATED_AT']) ) {
        $post_data['UNIX_GENERATED_AT'] = strtotime($post_data['GENERATED_AT']);
        $post_data['SQL_GENERATED_AT'] = date("Y-m-d H:m:s", strtotime($post_data['GENERATED_AT']));
        $post_data['YY_MM_DD_GENERATED_AT'] = date("y-m-d H:m:s", strtotime($post_data['GENERATED_AT']));
        $post_data['MM_DD_YY_GENERATED_AT'] = date("m/d/Y H:m:s", strtotime($post_data['GENERATED_AT']));
    }

    $post_data['DATE_TODAY'] = date("Y-m-d");

    foreach ( $post_data as $key => $value ) {
        // variables are formatted like this: %%VARIABLE%% so we want to search through each one 
        // and replace the placeholder with the value
        $temp_string = $post_fields;
        $post_fields = str_ireplace(sprintf("%%%%%s%%%%", strtoupper($key)), $value, $temp_string);

    }

    return remove_unused_template_variables($post_fields);
}





function remove_unused_template_variables( $post_data ) {

    $clean_variable_pairs = array();
    $variable_pairs = explode("&", $post_data);

    foreach ( $variable_pairs as $variable_pair ) {

        $matches = array();

        $field_name_and_value = explode("=", $variable_pair);

        if ( count($field_name_and_value) == 2 ) {
            $field_name = $field_name_and_value[0];
            $value = urlencode($field_name_and_value[1]);

            preg_match("/(.*)=(\%\%.*\%\%)/", $variable_pair, $matches);

            if ( count($matches) == 3 ) {
                $variable_pair = sprintf("%s=", $field_name);
            } else {
                $variable_pair = sprintf("%s=%s", $field_name, $value);
            }
            $clean_variable_pairs[] = $variable_pair;
        }
    }

    return join("&", $clean_variable_pairs);
}
