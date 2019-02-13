<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 11/21/18
 * Time: 15:35
 */

$sql = <<<EOQ
SELECT tbl_comment.*,tbl_like_unlike.like_unlike 
FROM tbl_comment 
	LEFT JOIN tbl_like_unlike ON tbl_comment.comment_id = tbl_like_unlike.comment_id AND member_id = " . $memberId . " 
ORDER BY tbl_like_unlike.like_unlike DESC, tbl_comment.date DESC
EOQ;

