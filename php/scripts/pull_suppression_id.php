<?php

$contents = file_get_contents("/tmp/contents.html");

print_r(get_suppression_id($contents,'/no google/i'));
print "\n";


function get_suppression_id ($html, $suppression_regex = "/^All/i") {

    $tidy_html = tidy_repair_string($html, array(
        'indent' => TRUE,
        'output-html' => TRUE,
        'clean' => TRUE,
        'input-encoding' => 'utf8',
        'output-encoding' => 'utf8',
        'logical-emphasis' => FALSE,
        'bare' => TRUE,
        'force-output' => TRUE,
    ));

    $doc = new DOMDocument();

    if ($doc->loadHTML($tidy_html)) {
        //print $tidy_html;
        $xpath = new DOMXpath( $doc );
        $audience_id = 0;

        foreach ($xpath->query('//input') as $eInput) {

			$is_checkbox = 0;
			$is_add_list= 0;
			$next = FALSE;

			foreach ($eInput->attributes as $attribute){

				if ($next){
					$suppression_id = $attribute->value;
					printf("%s\n", print_r(array('suppression_id' => $suppression_id, 'is_checkbox' => $is_checkbox, 'is_add_list'=> $is_add_list, 'value' => $attribute->value, TRUE)));
					$next = FALSE;
				}

				if($attribute->value == "checkbox"){
					$is_checkbox = 1;
				}

				if ($attribute->value == "add"){
					$is_add_list= 1;
					$next=TRUE;
				}

			}
				

            if ($eInput->parentNode->tagName == "a" && $eInput->parentNode->attributes->item(1)->value == "nav2") {
                if (preg_match($suppression_regex, trim($eInput->nodeValue))) {
                    $audience_id = trim($eInput->attributes->item(0)->value);
                }
            }
        }

        return $audience_id;
    } else {
        die( "Invalid HTML\n{$tidy_html}" );
    }
}
