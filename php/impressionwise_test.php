<?php



							$ch = curl_init();

							$query_data = array(
								'code' => '779005',
								'pwd' => 'ReaX1',
								//'email' => 'ashli.burnette@gmail.com',
								'email' => 'Slavick23@gmail.com',
							);

							$query_string = http_build_query($query_data);
							$url = sprintf("http://post.impressionwise.com/fastfeed.aspx?%s", $query_string);

							curl_setopt($ch, CURLOPT_URL, $url);
							curl_setopt($ch, CURLOPT_POST, FALSE);
							curl_setopt($ch, CURLOPT_HTTPGET, TRUE);
							curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);

							printf("%s\n",curl_exec($ch));
