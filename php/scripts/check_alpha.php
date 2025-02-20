<?php

$val = "asdf😇asdf";


if (preg_match("/[\p{Emoji_Presentation}\p{Extended_Pictographic}]/u",$val)){
	printf("emoji detected");
}


