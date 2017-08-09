<?php 

print_r(array('token'=>gen_token(270249,12345)));

function gen_token($affiliate_id, $campaign_group_id){

	return md5(sprintf("%s%s%s", microtime(TRUE), $affiliate_id, $campaign_group_id));
}



