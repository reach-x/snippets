#!/usr/local/bin/php

<?php

/*
   [campaign_name] => PersonalLoans - Fixed CPC
                    [sid] => 4679
                    [old] => $0.45
                    [new] => $0.65
*/


$campaigns = json_decode(file_get_contents("campaigns.json"),TRUE);

foreach ($campaigns as $title => $changes){
	foreach($changes as $change){
		printf("%s: (%s) %s => %s \n", $title, $change['sid'], $change['campaign_name'], $change['old'], $change['new']);
	}
}
