<?php

class Manager_lists_model extends ReachX_Model {

	function __construct() {

		parent::__construct();
		$this->set_database('lbc');
		$this->table_name = "manager_lists";
		$this->primary_key = "manager_list_id";
		$this->primary_value = "manager_list";
	}

	function count_downloadable_records($manager_list_id) {

		$sql = "SELECT count(*) AS total FROM manager_posts WHERE manager_list_id={$manager_list_id} AND status_id=6";
		$count_query = $this->database->query($sql);

		return $count_query->row_array();
	}

	function get_downloadable_options() {

		$sql = "SELECT  CONCAT(LPAD(manager_lists.manager_list_id, 4, '0'),': ',managers.manager,' - ',manager_lists.manager_list) AS manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id WHERE managers.status_id=1 AND manager_lists.status_id=1 AND manager_lists.delivery_type_id = 2 ";

		$sql .= " ORDER BY managers.manager,manager_lists.manager_list,manager_lists.manager_list_id";

		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[$row->manager_list_id] = $row->manager_list;
		}

		return $options;
	}

	function get_message_options() {

		$sql = "SELECT  CONCAT(LPAD(manager_lists.manager_list_id, 4, '0'),': ',managers.manager,' - ',manager_lists.manager_list) AS manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id WHERE manager_lists.esp_id IN (1,2,3,10,11,13,14,15,16,17)  AND managers.status_id=1 AND manager_lists.status_id=1 ";

		$sql .= " ORDER BY managers.manager,manager_lists.manager_list,manager_lists.manager_list_id";

		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[$row->manager_list_id] = $row->manager_list;
		}

		return $options;
	}

	function get_active_options($manager_id = 0, $delivery_type_id = 0) {

		$sql = "SELECT  CONCAT(LPAD(manager_lists.manager_list_id, 4, '0'),': ',managers.manager,' - ',manager_lists.manager_list) AS manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id
                 WHERE managers.status_id=1 AND manager_lists.status_id=1 ";
		if ($manager_id > 0) {
			$sql .= " AND managers.manager_id = {$manager_id}";
		}
		if ($delivery_type_id != 0) {
			$sql .= " AND manager_lists.delivery_type_id = {$delivery_type_id}";
		}

		$sql .= " ORDER BY managers.manager,manager_lists.manager_list,manager_lists.manager_list_id";

		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[$row->manager_list_id] = $row->manager_list;
		}

		return $options;
	}

	function get_popular_options($manager_id = 0, $delivery_type_id = 0) {

		$sql = "SELECT   manager_lists.manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id WHERE managers.status_id=1 AND manager_lists.status_id=1 AND manager_lists.manager_id=23 ";

		$sql .= " ORDER BY manager_lists.manager_list";

		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[$row->manager_list_id] = $row->manager_list;
		}

		return $options;
	}

	function get_topica_options() {

		$sql = "SELECT  manager_lists.manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id WHERE managers.status_id=1 AND manager_lists.status_id=1 AND manager_lists.manager_id=23 AND (manager_lists.manager_list LIKE 'topica%' OR manager_lists.manager_list LIKE 'iem%' OR manager_lists.manager_list LIKE 'sendgrid%')";

		$sql .= " ORDER BY manager_lists.manager_list";

		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[$row->manager_list_id] = $row->manager_list;
		}

		return $options;
	}

	function get_topica_lists($esp_list_id = 0) {

		$sql = "SELECT manager_list, list_category_id, list_category, esp_id, esp_list_id, username, `password` FROM manager_lists INNER JOIN esps USING(esp_id) INNER JOIN list_categories USING(list_category_id) WHERE manager_lists.status_id=1 AND manager_list LIKE 'Topica%' ORDER BY manager_list";

		if ($esp_list_id != 0) {
			$sql .= " AND esp_list_id=" . sprintf("%d", $esp_list_id);
		}

		$result = $this->database->query($sql);

		if ($this->database->_error_number() > 0) {

			printf("ERROR(%s):%s\nSQL:%s\n",
			       print_r($this->database->_error_number(), TRUE),
			       $this->database->error(),
			       $sql);
		}

		return $result->result_array();
	}

	function get_esp_data($manager_list_id) {

		$sql = "SELECT * FROM manager_lists INNER JOIN esps USING(esp_id) WHERE manager_list_id={$manager_list_id}";
		$query = $this->database->query($sql);

		return $query->row_array();
	}

	function get_array_by_id($manager_list_id) {

		$this->database->where('manager_list_id', $manager_list_id);
		$query = $this->database->get('manager_lists');
		$row = $query->row_array();

		if (isset($row['variable_data'])) {
			$row['variable_data'] = str_replace("&", "&amp;", $row['variable_data']);
		}

		return $row;
	}

	function build_filename($manager_list_id) {

		$sql = "SELECT concat('Export_',manager_list_id,'_',REPLACE(REPLACE(REPLACE(manager_list,' ','-'),' ','_'),'--',''),'_',date_format(now(),'%Y_%m_%d')) AS filename FROM manager_lists WHERE manager_list_id={$manager_list_id}";
		$query = $this->database->query($sql);
		$row = $query->row_array();

		return $row['filename'];
	}

	function queue_owner_post($owner_post_id, $owner_list_id, $owner_id, $manager_id, $manager_list_id, $manager_post_status) {

		$sql = "INSERT INTO manager_posts (owner_post_id, manager_list_id, scheduled_time, status_id, owner_list_id,owner_id,manager_id,response)
                            VALUES ({$owner_post_id}, {$manager_list_id}, DATE_ADD(NOW(), INTERVAL 0 SECOND), {$manager_post_status}, {$owner_list_id}, {$owner_id}, {$manager_id},0);";

		$this->database->query($sql);

		return TRUE;
	}

	function run_legacy_queue($job_info) {

		$manager_list_id = $job_info->manager_list_id;
		$sql = "SELECT owner_list_id FROM routing_rules INNER JOIN manager_lists USING(manager_list_id) WHERE manager_list_id={$manager_list_id} AND routing_rules.status_id=1 ORDER BY owner_list_id ASC";

		$routing_rules_result = $this->database->query($sql);

		foreach ($routing_rules_result->result_array() as $routing_rule) {
			$owner_list_ids[] = $routing_rule['owner_list_id'];
		}

		printf("pulling data from %s owner lists\n", count($owner_list_ids));

		$this->fill_legacy_queue($owner_list_ids, $job_info);

		$this->route_legacy_queue($job_info);

		return array(
			'query_returned' => 0,
			'total_routed' => 0,
		);
	}

	function fill_legacy_queue($owner_list_ids, $job_info) {

		$counter = 0;
		$total_lists = count($owner_list_ids);

		foreach ($owner_list_ids as $owner_list_id) {
			$counter++;
			printf("FILL: %s (%s/%s)\n", $owner_list_id, $counter, $total_lists);
			$sql = <<<EOS
            SELECT {$job_info->manager_list_id} AS manager_list_id,
                           owner_posts.email_address_id,
                           owner_posts.owner_id,
                           owner_posts.owner_list_id,
                           owner_posts.owner_post_id,
                           owner_posts.isp_domain_id,
                           owner_posts.source_domain_id,
                           owner_posts.received_at,
                           owner_posts.is_duplicate,
                           owner_posts.generated_at
                    FROM   owner_posts
                    WHERE  owner_list_id = {$owner_list_id}
                        AND isp_domain_id in (103,7149,118,122,59243)
                        AND owner_posts.post_status_id=2
                        AND owner_posts.received_at BETWEEN '{$job_info->start_date} 00:00:00' AND '{$job_info->end_date} 23:59:59'
EOS;

			printf("%s\n", $sql);
			$result = $this->database->query($sql);

			if ($this->database->_error_number() > 0) {

				printf("FILL: ERROR:%s\n", print_r($this->database->error(), TRUE));
			} else {

				foreach ($result->result_array() as $row) {
					//                    INSERT IGNORE INTO `legacy_queue` (`manager_list_id`, email_address_id, `owner_id`, `owner_list_id`, `owner_post_id`, `isp_domain_id`, `source_domain_id`,`received_at`, `is_duplicate`, `generated_at`)

				}

				printf("FILL: SUCCESS\n");
			}
		}
	}

	//@deprecated

	function route_legacy_queue($job_info) {

		$counter = 0;
		$total_posts = 0;

		$sql = <<<EOS
            SELECT legacy_queue.owner_list_id,
                   legacy_queue.email_address_id,
                   legacy_queue.owner_post_id,
                   legacy_queue.owner_id,
                   legacy_queue.isp_domain_id,
                   legacy_queue.source_domain_id,
                   legacy_queue.received_at,
                   legacy_queue.is_duplicate,
                   legacy_queue.generated_at,
                   isp_domains.isp_domain_group_id,
                   email_addresses.email_address,
                   owner_lists.owner_list,
                   owner_lists.list_key,
                   owner_lists.country_id,
                   owner_lists.primary_source_domain_id,
                   owner_lists.scrub_posts
            FROM   legacy_queue
                   INNER JOIN email_addresses USING(email_address_id)
                   INNER JOIN owner_lists USING (owner_list_id)
                   INNER JOIN isp_domains USING(isp_domain_id)
            WHERE legacy_queue.manager_list_id = {$job_info->manager_list_id}
EOS;

		if ($job_info->limit > 0 && isset($job_info->direction)) {
			$sql .= " ORDER BY owner_post_id {$job_info->direction}";
		}

		print "\n";
		print "Building\n";
		print "$sql\n";

		$legacy_queue_query = $this->database->query($sql);

		if ($this->database->_error_number() > 0) {

			printf("DRAIN: ERROR:%s\n", print_r($this->database->error(), TRUE));
		} else {
			printf("DRAIN: PROCESSING %d RECORDS\n", $legacy_queue_query->num_rows);
		}

		$total_posts += $legacy_queue_query->num_rows;

		foreach ($legacy_queue_query->result_array() as $owner_post) {

			$sql = "SELECT fields.field_name AS field_name,
                        owner_posts_data.content AS content
                    FROM owner_posts_data
                        JOIN fields ON owner_posts_data.field_id = fields.field_id
                        JOIN owner_posts ON owner_posts_data.owner_post_id = owner_posts.owner_post_id
                    WHERE owner_posts.owner_post_id = {$owner_post['owner_post_id']}";

			$fields = $this->database->query($sql);

			$column_data = array();

			foreach ($fields->result() as $field) {
				if (!empty($field->content)) {
					$column_data[strtolower($field->field_name)] = $field->content;
				}
			}

			if (count($column_data) > 0) {

				$legacy_queue_post = array_merge($owner_post, $column_data);

				// if there is a limit
				if (isset($job_info->limit) && $job_info->limit > 0) {

					// if we're below the limit
					if ($counter > $job_info->limit) {
						break;
					}
				}

				$counter += $this->route_owner_post($legacy_queue_post, $job_info->manager_list_id);
			}
		}

		return array(
			'total_posts' => $total_posts,
			'routed_posts' => $counter,
		);

	}

	function route_owner_post($owner_post, $manager_list_id = 0, $topica_list_ids = array()) {

		$route_counter = 0;
		$routing_rules = array();

		if ($owner_post['owner_list_id'] != SEED_LIST) {

			$sql = "SELECT routing_rules.routing_rule_id,
                       owners.owner_id,
                       owner_lists.owner_list_id,
                       managers.manager_id,
                       manager_lists.manager_list_id,
                       isp_domain_groups.isp_domain_group_id,
                       manager_lists.post_delay,
                       manager_lists.delivery_type_id,
                       manager_lists.allow_duplicates,
                       managers.rate_limit,
                       routing_rules.is_split,
                       routing_rules.weight
                FROM   routing_rules
                       JOIN owner_lists
                         ON routing_rules.owner_list_id = owner_lists.owner_list_id
                       JOIN owners
                         ON owner_lists.owner_id = owners.owner_id
                       JOIN manager_lists
                         ON routing_rules.manager_list_id = manager_lists.manager_list_id
                       JOIN managers
                         ON manager_lists.manager_id = managers.manager_id
                       JOIN isp_domain_groups
                         ON routing_rules.isp_domain_id = isp_domain_groups.isp_domain_group_id
                WHERE  routing_rules.status_id = 1
                   AND routing_rules.owner_list_id = {$owner_post['owner_list_id']} ";

			if ($manager_list_id > 0) {
				$sql .= " AND routing_rules.manager_list_id={$manager_list_id}";
			}

			$query = $this->database->query($sql);

			$routing_rules = $query->result_array();

		} else {
			$manager_list = $this->get_by_id($owner_post['manager_list_id']);
			$routing_rules[] = array(
				'routing_rule_id' => 1,
				'owner_id' => 2,
				'owner_list_id' => 200,
				'manager_id' => $manager_list['manager_id'],
				'manager_list_id' => $manager_list['manager_list_id'],
				'isp_domain_group_id' => 0,
				'post_delay' => 0,
				'allow_duplicates' => 1,
				'delivery_type_id' => $manager_list['delivery_type_id'],
				'rate_limit' => 0,
				'is_split' => 0,
				'weight' => 100,
			);
		}

		$is_split = FALSE;
		$weighted_routing_rules = array();
		$routing_rule_counter = 0;

		foreach ($routing_rules as $routing_rule) {

			if ($routing_rule['is_split'] == 1) {
				$is_split = TRUE;
				$weighted_routing_rules[$routing_rule_counter] = $routing_rule['weight'];
			}
			$routing_rule_counter++;
		}

		if ($is_split) {

			$selected_index = $this->get_random_weighted_element($weighted_routing_rules);
			$selected_routing_rule = $routing_rules[$selected_index];
			$new_routing_rules = array();

			foreach ($routing_rules as $routing_rule) {

				if ($routing_rule['is_split'] == 0) {
					$new_routing_rules[] = $routing_rule;
				}
			}

			$new_routing_rules[] = $selected_routing_rule;
			$routing_rules = $new_routing_rules;
		}

		foreach ($routing_rules as $routing_rule) {

			$sql = "SELECT * FROM routing_rule_filters WHERE routing_rule_id={$routing_rule['routing_rule_id']}";
			$routing_rule_filters = $this->database->query($sql);

			$num_rows = $routing_rule_filters->num_rows();

			if ($num_rows > 0) {
				$is_filtered = FALSE;
				$has_seen_filters = FALSE;

				foreach ($routing_rule_filters->result_array() as $routing_rule_filter) {

					$field_name = strtolower($routing_rule_filter['field_name']);
					$criteria = $routing_rule_filter['criteria'];

					if (isset($owner_post[$field_name])) {

						$value = $owner_post[$field_name];
						$has_seen_filters = TRUE;

						switch ($routing_rule_filter['routing_rule_filter_type_id']) {

							case ROUTING_RULE_FILTER_SEED:
								$is_filtered = FALSE;
								break;

							//	Rejects record if field_value and criteria match
							case ROUTING_RULE_FILTER_STRING_COMPARE:
								$is_filtered = (strtolower($value) == strtolower($criteria));
								break;

							//	Rejects record if field value is not greater than criteria
							case ROUTING_RULE_FILTER_GREATER_THAN:
								$is_filtered = !(intval($value) > intval($criteria));
								break;

							//	Rejects record if field value is not less than criteria
							case ROUTING_RULE_FILTER_LESS_THAN:
								$is_filtered = !(intval($value) < intval($criteria));
								break;

							//	Rejects record if field value is not within range of min and max values
							case ROUTING_RULE_FILTER_WITHIN_RANGE:
								$range = json_decode($criteria);
								$is_filtered = (intval($value) < intval($range->min) || intval($value) > intval($range->max));
								break;

							//	Rejects record if value is not the same as the value in criteria
							case ROUTING_RULE_FILTER_BOOLEAN:
								$is_filtered = !((filter_var($value, FILTER_VALIDATE_BOOLEAN)) == (filter_var($criteria,
								                                                                              FILTER_VALIDATE_BOOLEAN)));
								break;

							//	Rejects record if field_value is not a value in the array
							case ROUTING_RULE_FILTER_IN_ARRAY:
								$criteria = (array) json_decode($criteria);

								if (is_array($criteria[$field_name])) {
									$is_filtered = intval(!(in_array($value, $criteria[$field_name])));
								}

								break;

							//	Rejects record if field value does not equal criteria
							case ROUTING_RULE_FILTER_EQUAL:
								$is_filtered = ($value != $criteria);
								break;

							//	Rejects record if value matches criteria
							case ROUTING_RULE_FILTER_NOT_EQUAL:
								$is_filtered = ($value == $criteria);
								break;

							//	Rejects record if fields do not match
							case ROUTING_RULE_FILTER_FIELD_MATCH:
								$is_filtered = (strtoupper($value) != strtoupper($criteria));
								break;

							//	Rejects record if fields do match
							case ROUTING_RULE_FILTER_FIELD_NO_MATCH:
								$value_to_compare = $owner_post[$criteria];
								$is_filtered = ($value == $value_to_compare);
								break;

							//	Rejects record if field value matches regex in criteria
							case ROUTING_RULE_FILTER_STRING_REGEX_MATCH:
								$is_filtered = preg_match($criteria, $value);
								break;

							// Rejects record if seen within days in criteria
							case ROUTING_RULE_FILTER_DUPLICATE_FILTER:
								//@todo: write ROUTING_RULE_FILTER_DUPLICATE_FILTER: code
								break;

							//	Rejects record if field_value is a value in the array
							case ROUTING_RULE_FILTER_NOT_IN_ARRAY:
								$criteria = (array) json_decode($criteria);
								$is_filtered = intval((in_array($value, $criteria[$field_name])));
								break;

							//	Rejects record if date is not greater than the date in the criteria
							case ROUTING_RULE_FILTER_DATE_GREATER_THAN:
								$date_now = new DateTime($criteria);
								$other_date = $this->parse_date($value);
								$is_filtered = !($date_now > $other_date);
								break;

							//	Rejects record if date is greater than the date in the criteria
							case ROUTING_RULE_FILTER_DATE_LESS_THAN:
								$date_now = new DateTime($criteria);
								$other_date = $this->parse_date($value);
								$is_filtered = !($date_now < $other_date);
								break;

							//	Rejects record if value is null
							case ROUTING_RULE_FILTER_NOT_NULL:
								$is_filtered = empty($value);
								break;

							//	Rejects record if not within operating hours. Handles weekly schedule.
							case ROUTING_RULE_FILTER_SCHEDULE:
								break;

							//@todo: Reject record based on lead age
							//// Rejects record if not within operating hours. Handles weekly schedule.
							//case ROUTING_RULE_FILTER_LEAD_AGE:
							//  break;

							default:
								print "SCRUB_AND_POST: filter type: {$routing_rule_filter['routing_rule_filter_type_id']} not matched\n";
								break;

						}
					}

					if ($is_filtered) {
						continue 2;
					}

				}

				if ($has_seen_filters == FALSE) {
					$is_filtered = TRUE;

				}

			} else {
				$is_filtered = FALSE;
			}

			// ISP DOMAIN FILTERING
			//@todo: use the isp_domain_group_id in the owner_post object

			if ($routing_rule['isp_domain_group_id'] != 0) {
				$sql = "SELECT isp_domain_group_id FROM isp_domains WHERE isp_domain_id={$owner_post['isp_domain_id']}";
				$query = $this->database->query($sql);
				$isp_domain = $query->row_array();

				if ($routing_rule['isp_domain_group_id'] != $isp_domain['isp_domain_group_id']) {
					$is_filtered = TRUE;
				}
			}

			if (!$is_filtered) {
				$route_counter++;

				if ($routing_rule['allow_duplicates'] == 1) {
					//TODO: is this useless overhead?
					$sql = "SELECT count(*) AS row_count FROM manager_posts WHERE manager_list_id={$routing_rule['manager_list_id']} AND owner_post_id={$owner_post['owner_post_id']}";

				} else {
					// unique lists means we haven't seen the email address in the last 90 days - tbruck 3/7/2014
					$sql = "SELECT count(*) AS row_count FROM manager_posts WHERE manager_list_id={$routing_rule['manager_list_id']} AND email_address_id={$owner_post['email_address_id']} and manager_posts.created_at > (now() - interval 90 day)";
				}

				$query = $this->database->query($sql);
				$row = $query->row_array();

				if ($row['row_count'] == 0) {
					switch ($routing_rule['delivery_type_id']) {
						case MANAGER_LIST_DELIVERY_TYPE_DOWNLOAD:
						case MANAGER_LIST_DELIVERY_TYPE_MANAGED_LIST:
							$manager_post_status_id = MANAGER_POST_STATUS_DOWNLOAD_QUEUED;
							break;
						case MANAGER_LIST_DELIVERY_TYPE_SINGLE_POST:
						case MANAGER_LIST_DELIVERY_TYPE_POST:
							$manager_post_status_id = MANAGER_POST_STATUS_SENDING;
							break;
						default:
							$manager_post_status_id = MANAGER_POST_STATUS_SYSTEM_ERROR;
							break;
					}

					if (isset($owner_post['GENERATED_AT']) && strtotime($owner_post['GENERATED_AT'])) {
						$generated_at_time = intval(date("His", strtotime($owner_post['GENERATED_AT'])));

						if ($generated_at_time == 0) {
							$current_time = date("H:i:s", strtotime($owner_post['received_at']));
							$owner_post['GENERATED_AT'] = sprintf("%s %s",
							                                      date("Y-m-d", strtotime($owner_post['GENERATED_AT'])),
							                                      $current_time);
						}

						$generated_at_date = date("Y-m-d H:i:s", strtotime($owner_post['GENERATED_AT']));
					} else {
						$generated_at_date = date("Y-m-d H:i:s");
					}

					//@TODO throttling is stored in managers table but implemented per list. Need to add index to make throttling per manager possible.
					if ($routing_rule['rate_limit'] > 0) {

						$sql = "SELECT coalesce(max(scheduled_time),now()) AS scheduled_time FROM manager_posts WHERE scheduled_time > now() - INTERVAL 1 HOUR AND manager_list_id=1156 AND status_id IN (1,5);";

						$result = $this->database->query($sql);
						$row = $result->row_array();

						$scheduled_time = strtotime($row['scheduled_time']);
						$now = strtotime("now");

						if ($scheduled_time >= $now) {
							$scheduled_time = date('Y-m-d H:i:s', $scheduled_time);
						} else {
							$scheduled_time = date('Y-m-d H:i:s', $now);
						}

						$generated_at_date = date('Y-m-d H:i:s',
						                          strtotime($scheduled_time . " +" . $routing_rule['rate_limit'] . " seconds"));
					}

					$skip_insert = FALSE;

					if (isset($topica_list_ids[$routing_rule['manager_list_id']])) {

						$sql = "SELECT COUNT(*) as already_sent_to_topica FROM topica_subscribers WHERE email_address_id={$owner_post["email_address_id"]}";
						$query = $this->database->query($sql);
						$row = $query->row_array();

						if (isset($row['already_sent_to_topica']) && $row['already_sent_to_topica'] > 0) {
							$skip_insert = TRUE;
						}
					}

					if (!$skip_insert) {
						$server_id = rand(1, 25);

						$sql = "INSERT INTO manager_posts
                                (server_id,owner_post_id, manager_list_id, scheduled_time, status_id, owner_list_id,owner_id,manager_id,response_time,generated_at, email_address_id, isp_domain_group_id, isp_domain_id, source_domain_id)
                            VALUES
                                ({$server_id},'{$owner_post["owner_post_id"]}', '{$routing_rule['manager_list_id']}', DATE_ADD('{$generated_at_date}', INTERVAL {$routing_rule['post_delay']} SECOND), '{$manager_post_status_id}', '{$owner_post['owner_list_id']}', '{$owner_post['owner_id']}', '{$routing_rule['manager_id']}',0,'{$generated_at_date}',{$owner_post["email_address_id"]},{$owner_post['isp_domain_group_id']},{$owner_post['isp_domain_id']},{$owner_post['source_domain_id']});";

						$this->database->query($sql);

						if ($this->database->_error_number() > 0) {

							printf("manager_lists_model->route_owner_post: SQL (%s)\nERROR:%s\n",
							       $sql,
							       print_r($this->database->error(), TRUE));
						}
					}
				}
			}
		}

		$route_status_id = ROUTE_STATUS_ASSIGNED;

		$sql = "UPDATE owner_posts SET route_status_id={$route_status_id} WHERE owner_post_id={$owner_post['owner_post_id']}";
		$this->database->query($sql);

		if ($this->database->_error_number() > 0) {

			printf("manager_lists_model->route_owner_post: SQL (%s)\nERROR:%s\n",
			       $sql,
			       print_r($this->database->error(), TRUE));
		}

		return $route_counter;

	}

	function get_by_id($manager_list_id) {

		$this->database->where('manager_list_id', $manager_list_id);
		$query = $this->database->get('manager_lists');

		return $query->row_array();
	}

	function get_random_weighted_element($weighted_values) {

		$rand = mt_rand(1, (int) array_sum($weighted_values));

		foreach ($weighted_values as $key => $value) {

			$rand -= $value;

			if ($rand <= 0) {
				return $key;
			}
		}
	}

	function parse_date($potential_date) {

		$formats = array(
			"m.d.Y",
			"m/d/Y",
			"Ymd",
			"Y-m-d",
			"//Y",
			"Y"
		); // and so on.....
		$date = "";

		foreach ($formats as $format) {
			$date = DateTime::createFromFormat($format, $potential_date);

			if ($date != FALSE) {
				break;
			}

		}

		return $date;
	}

	function get_options($manager_id = 0, $delivery_type_id = 0) {

		$sql = "SELECT  CONCAT(LPAD(manager_lists.manager_list_id, 4, '0'),': ',managers.manager,' - ',manager_lists.manager_list) AS manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id WHERE managers.status_id=1 AND manager_lists.status_id=1 ";
		if ($manager_id > 0) {
			$sql .= " AND managers.manager_id = {$manager_id}";
		}
		if ($delivery_type_id != 0) {
			$sql .= " AND manager_lists.delivery_type_id = {$delivery_type_id}";
		}

		$sql .= " ORDER BY managers.manager,manager_lists.manager_list,manager_lists.manager_list_id";

		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[$row->manager_list_id] = $row->manager_list;
		}

		return $options;
	}

	function get_manager_list_options($selected_manager_list_id) {

		$sql = "SELECT  CONCAT(LPAD(manager_lists.manager_list_id,4,0), ': ',managers.manager,': ',manager_lists.manager_list) AS manager_list,
                        manager_lists.manager_list_id
                 FROM   manager_lists
                        INNER JOIN managers
                                ON managers.manager_id = manager_lists.manager_id
               ORDER BY managers.manager, manager_lists.manager_list,manager_lists.manager_list_id";
		$result = $this->database->query($sql);
		$options = array();

		foreach ($result->result() as $row) {
			$options[] = array(
				'manager_list_id' => $row->manager_list_id,
				'manager_list' => $row->manager_list,
				'selected' => $row->manager_list_id == $selected_manager_list_id ? 1 : 0,
			);
		}

		return $options;
	}

	function hack($job_info) {

		print_r($job_info);
		print_r($this->route_legacy_queue($job_info));

	}

	function truncate_legacy_queue() {

		$this->database->truncate("legacy_queue");

		if ($this->database->_error_number() > 0) {

			printf("TRUNCATE: ERROR:%s\n", print_r($this->database->error(), TRUE));
		} else {
			printf("TRUNCATE: SUCCESS\n");
		}
	}

	function get_topica_list_ids() {

		$topica_list_ids = array();

		$sql = "SELECT manager_list_id FROM manager_lists WHERE manager_list like 'Topica%'";
		$query = $this->database->query($sql);

		foreach ($query->result_array() as $manager_list) {
			$topica_list_ids[] = $manager_list['manager_list_id'];
		}

		return $topica_list_ids;
	}

	//@deprecated

	function get_by_manager_id($manager_id) {

		$this->database->where('manager_id', $manager_id);
		$this->database->where('status_id', 1);
		$this->database->order_by("manager_list");
		$query = $this->database->get('manager_lists');

		return $query->result_array();
	}

	function assign_owner_post($routing_rules, $owner_post, $manager_post_status_id = 0) {

		$owner_post = array_change_key_case($owner_post, CASE_LOWER);

		$counter = 0;
		$duplicate_counter = 0;

		foreach ($routing_rules as $routing_rule) {
			$sql = "SELECT * FROM routing_rule_filters WHERE routing_rule_id={$routing_rule['routing_rule_id']}";
			$routing_rule_filters = $this->database->query($sql);

			$num_rows = $routing_rule_filters->num_rows();

			if ($num_rows > 0) {
				$is_filtered = TRUE;

				foreach ($routing_rule_filters->result_array() as $routing_rule_filter) {
					$field_name = strtolower($routing_rule_filter['field_name']);

					$criteria = $routing_rule_filter['criteria'];

					if (isset($owner_post[$field_name])) {

						$value = $owner_post[$field_name];

						switch ($routing_rule_filter['routing_rule_filter_type_id']) {

							//	Rejects record if fields do not match
							case ROUTING_RULE_FILTER_SEED:
								$is_filtered = (strtoupper($value) != strtoupper($criteria));
								break;

							//	Rejects record if field_value and criteria match
							case ROUTING_RULE_FILTER_STRING_COMPARE:
								$is_filtered = (strtolower($value) == strtolower($criteria));
								break;

							//	Rejects record if field value is not greater than criteria
							case ROUTING_RULE_FILTER_GREATER_THAN:
								$is_filtered = !(intval($value) > intval($criteria));
								break;

							//	Rejects record if field value is not less than criteria
							case ROUTING_RULE_FILTER_LESS_THAN:
								$is_filtered = !(intval($value) < intval($criteria));
								break;

							//	Rejects record if field value is not within range of min and max values
							case ROUTING_RULE_FILTER_WITHIN_RANGE:
								$range = json_decode($criteria);
								$is_filtered = (intval($value) < intval($range->min) || intval($value) > intval($range->max));
								break;

							//	Rejects record if value is not the same as the value in criteria
							case ROUTING_RULE_FILTER_BOOLEAN:
								$is_filtered = !((filter_var($value, FILTER_VALIDATE_BOOLEAN)) == (filter_var($criteria,
								                                                                              FILTER_VALIDATE_BOOLEAN)));
								break;

							//	Rejects record if field_value is not a value in the array
							case ROUTING_RULE_FILTER_IN_ARRAY:

								$criteria = (array) json_decode($criteria);

								if (is_array($criteria[$field_name])) {
									$is_filtered = intval(!(in_array($value, $criteria[$field_name])));
									//print "in_array($value, $criteria[$field_name])\n";
								} else {
									print_r(array(
										        'routing_rule' => $routing_rule,
										        'routing_rule_filter' => $routing_rule_filter,
										        'criteria' => $criteria,
										        'field_name' => $field_name,
										        'owner_post' => $owner_post,
									        ));
								}
								break;

							//	Rejects record if field value does not equal criteria
							case ROUTING_RULE_FILTER_EQUAL:
								$is_filtered = ($value != $criteria);
								break;

							//	Rejects record if value matches criteria
							case ROUTING_RULE_FILTER_NOT_EQUAL:
								$is_filtered = ($value == $criteria);
								break;

							//	Rejects record if fields do not match
							case ROUTING_RULE_FILTER_FIELD_MATCH:
								$is_filtered = (strtoupper($value) != strtoupper($criteria));
								break;

							//	Rejects record if fields do match
							case ROUTING_RULE_FILTER_FIELD_NO_MATCH:
								$value_to_compare = $owner_post[$criteria];
								$is_filtered = ($value == $value_to_compare);
								break;

							//	Rejects record if field value matches regex in criteria
							case ROUTING_RULE_FILTER_STRING_REGEX_MATCH:
								$is_filtered = preg_match($criteria, $value);
								break;

							// Rejects record if seen within days in criteria
							case ROUTING_RULE_FILTER_DUPLICATE_FILTER:
								//@todo: write ROUTING_RULE_FILTER_DUPLICATE_FILTER: code
								break;

							//	Rejects record if field_value is a value in the array
							case ROUTING_RULE_FILTER_NOT_IN_ARRAY:
								$criteria = (array) json_decode($criteria);
								$is_filtered = intval((in_array($value, $criteria[$field_name])));
								break;

							//	Rejects record if date is not greater than the date in the criteria
							case ROUTING_RULE_FILTER_DATE_GREATER_THAN:
								$date_now = new DateTime($criteria);
								$other_date = $this->parse_date($value);
								$is_filtered = !($date_now > $other_date);
								break;

							//	Rejects record if date is greater than the date in the criteria
							case ROUTING_RULE_FILTER_DATE_LESS_THAN:
								$date_now = new DateTime($criteria);
								$other_date = $this->parse_date($value);
								$is_filtered = !($date_now < $other_date);
								break;

							//	Rejects record if value is null
							case ROUTING_RULE_FILTER_NOT_NULL:
								$is_filtered = empty($value);
								break;

							//	Rejects record if not within operating hours. Handles weekly schedule.
							case ROUTING_RULE_FILTER_SCHEDULE:
								break;

							//@todo: Reject record based on lead age
							//// Rejects record if not within operating hours. Handles weekly schedule.
							//case ROUTING_RULE_FILTER_LEAD_AGE:
							//  break;

							default:
								print "SCRUB_AND_POST: filter type: {$routing_rule_filter['routing_rule_filter_type_id']} not matched\n";
								break;

						}
					}

				}
			} else {
				$is_filtered = FALSE;
			}

			$routing_rule['isp_domain_ids'] = $this->expand_isp_domain_group_id($routing_rule['isp_domain_id']);

			if (!$is_filtered && $routing_rule['isp_domain_id'] == 0 || in_array($owner_post['isp_domain_id'],
			                                                                     $routing_rule['isp_domain_ids'])
			) {

				if ($manager_post_status_id != 0) {
					$manager_post_status = $manager_post_status_id;
				} else {

					switch ($routing_rule['delivery_type_id']) {
						case MANAGER_LIST_DELIVERY_TYPE_DOWNLOAD:
						case MANAGER_LIST_DELIVERY_TYPE_MANAGED_LIST:
							$manager_post_status_id = MANAGER_POST_STATUS_DOWNLOAD_QUEUED;
							break;

						case MANAGER_LIST_DELIVERY_TYPE_SINGLE_POST:
						case  MANAGER_LIST_DELIVERY_TYPE_POST:
							$manager_post_status_id = MANAGER_POST_STATUS_POST_QUEUED;
							break;

						default:
							$manager_post_status_id = MANAGER_POST_STATUS_SYSTEM_ERROR;
							break;
					}

				}

				$sql = "SELECT COUNT(*) AS existing_posts FROM manager_posts INNER JOIN manager_lists ON manager_posts.manager_list_id = manager_lists.manager_list_id WHERE owner_post_id={$owner_post['owner_post_id']} AND manager_posts.manager_list_id={$routing_rule['manager_list_id']}";
				$query = $this->database->query($sql);

				if (!empty($query)) {
					$row = $query->row();
					$existing_posts = isset($row->existing_posts) ? $row->existing_posts : 0;
				} else {
					$existing_posts = 0;
				}

				if ($existing_posts == 0) {
					$generated_at_date = date("Y-m-d H:i:s");

					if (isset($owner_post['GENERATED_AT']) && strtotime($owner_post['GENERATED_AT'])) {
						$generated_at_date = date("Y-m-d H:i:s", strtotime($owner_post['GENERATED_AT']));
					}

					$sql = "INSERT INTO manager_posts (owner_post_id, manager_list_id, scheduled_time, status_id, owner_list_id,owner_id,manager_id,response,response_time) VALUES ('{$owner_post["owner_post_id"]}', '{$routing_rule['manager_list_id']}', DATE_ADD(DATE('{$generated_at_date}'), INTERVAL {$routing_rule['post_delay']} SECOND), '{$manager_post_status}', '{$owner_post['owner_list_id']}', '{$owner_post['owner_id']}', '{$routing_rule['manager_id']}',0,0);";
					$this->database->query($sql);

					$counter++;
				} else {
					$duplicate_counter++;
				}

			}
		}

		return array(
			'counter' => $counter,
			'duplicate_counter' => $duplicate_counter
		);
	}

	function expand_isp_domain_group_id($isp_domain_group_id) {

		$sql = "SELECT isp_domain_id FROM isp_domains WHERE isp_domain_group_id={$isp_domain_group_id}";
		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function convert_download_to_post($job_info) {

		$sql = "SELECT manager_post_id
        FROM   manager_posts
        WHERE  manager_list_id = {$job_info->manager_list_id}
           AND status_id = 6
        ORDER BY manager_post_id {$job_info->direction}
        LIMIT {$job_info->limit}";

		print "executing $sql\n";

		$query = $this->database->query($sql);

		print "completed executing query\n";

		if ($query->num_rows() == 0) {
			// If there are no rows here then there was probably an error
			$error_info = array(
				'error_message' => print_r($this->database->error(), TRUE),
				'sql' => $this->database->last_query(),
			);

			$this->send_email($job_info->recipient, "LBC: Convert Downloads Error", $error_info);

			print_r(array(
				        'error' => 'No rows returned in query',
				        'error_info' => $error_info,
				        'sql' => $sql,
			        ));

		} else {
			$counter = 0;
			foreach ($query->result_array() as $post_data) {
				$counter++;

				// update status_id to be queued for post and mark the user as subscribed
				$sql = "UPDATE manager_posts SET status_id=1,subscriber_status_id=2 WHERE manager_post_id={$post_data['manager_post_id']}";
				$this->database->query($sql);

				$error_message = print_r($this->database->error(), TRUE);

				if (!empty($error_message)) {
					print "ERROR: $sql\n";
				}

				if (($counter % 5) == 0) {
					print ".";
				}

				if (($counter % 500) == 0) {
					print "\n";
				}
			}

			print "converted {$counter} posts\n";
		}

		return $counter;
	}

	function send_email($to, $subject, $data) {

		$message = sprintf("<HTML><BODY><PRE>%s</PRE></BODY></HTML>", print_r($data, TRUE));
		$headers = 'MIME-Version: 1.0' . "\r\n";
		$headers .= 'Content-type: text/html; charset=iso-8859-1' . "\r\n";
		$headers .= 'From: Reach X - LBC <lbc-stats@popularllc.com>' . "\r\n";
		$headers .= 'Reply-To: lbc-stats@popularllc.com' . "\r\n";
		$headers .= 'X-Mailer: PHP/' . phpversion();

		return mail($to, $subject, $message, $headers);
	}

	function convert_download_to_topica_job($job_info, $status_id = 6) {

		$owner_posts = array();

		$sql = "SELECT manager_post_id,owner_post_id
                FROM   manager_posts
                WHERE  manager_list_id = {$job_info->manager_list_id}
                   AND status_id = {$status_id}
                ORDER BY manager_post_id {$job_info->direction}
                LIMIT {$job_info->limit}";

		print "executing $sql\n";

		$query = $this->database->query($sql);

		print "completed executing query\n";

		if ($query->num_rows() == 0) {
			// If there are no rows here then there was probably an error
			$error_info = array(
				'error_message' => print_r($this->database->error(), TRUE),
				'sql' => $this->database->last_query(),
			);

			$this->send_email($job_info->recipient, "LBC: Convert Downloads Error", $error_info);

			print_r(array(
				        'error' => 'No rows returned in query',
				        'error_info' => $error_info,
				        'sql' => $sql,
			        ));

		} else {
			$counter = 0;

			foreach ($query->result_array() as $post_data) {
				$counter++;

				$owner_post = $this->build_post($post_data['owner_post_id']);
				$owner_post['MANAGER_POST_ID'] = $post_data['manager_post_id'];
				$owner_post['OWNER_POST_ID'] = $post_data['owner_post_id'];

				//print_r($owner_post);

				$owner_posts[] = array(
					//<column order="1" field="Email Address"  create-if-new="false"/>
					$owner_post['EMAIL_ADDRESS'],
					//<column order="2" field="FIRST_NAME" create-if-new="true" data-type="text"/>
					isset($owner_post['FIRST_NAME']) ? $owner_post['FIRST_NAME'] : '',
					//<column order="3" field="LAST_NAME" create-if-new="true" data-type="text"/>
					isset($owner_post['LAST_NAME']) ? $owner_post['LAST_NAME'] : '',
					//<column order="4" field="STATE" create-if-new="true" data-type="text"/>
					isset($owner_post['STATE']) ? $owner_post['STATE'] : '',
					//<column order="5" field="ZIP_CODE" create-if-new="false" data-type="text"/>
					isset($owner_post['ZIP_CODE']) ? $owner_post['ZIP_CODE'] : '',
					//<column order="6" field="OWNER_LIST_ID" create-if-new="false" data-type="number"/>
					$owner_post['OWNER_LIST_ID'],
					//<column order="7" field="MANAGER_POST_ID" create-if-new="false" data-type="number"/>
					$owner_post['MANAGER_POST_ID'],
					//<column order="8" field="SOURCE_DOMAIN" create-if-new="false" data-type="text"/>
					isset($owner_post['SOURCE_DOMAIN']) ? $owner_post['SOURCE_DOMAIN'] : "",
					//<column order="9" field="ISP_DOMAIN_GROUP_ID" create-if-new="false" data-type="number"/>
					$owner_post['ISP_DOMAIN_GROUP_ID'],
				);

				// update status_id to posted successfully and mark the user as subscribed
				$sql = "UPDATE manager_posts SET status_id=7,subscriber_status_id=2 WHERE manager_post_id={$post_data['manager_post_id']}";
				$this->database->query($sql);

				$error_message = print_r($this->database->error(), TRUE);

				if (!empty($error_message)) {
					print "ERROR: $sql\n";
				}

				if (($counter % 5) == 0) {
					print ".";
				}

				if (($counter % 500) == 0) {
					print "\n";
				}
			}

			print "built {$counter} posts\n";

			$owner_posts[] = array(
				"campaigns@superemailaccounts.com",
				"Topica",
				"Campaigns",
				"CA",
				"90502",
				200,
				0,
				"popularmarketing.com",
				10,
			);
		}

		return $owner_posts;
	}

	function build_post($owner_post_id) {

		$sql = "SELECT owner_posts.owner_list_id,
                       owner_posts.email_address_id,
                       owner_posts.owner_post_id,
                       owner_posts.owner_id,
                       owner_posts.isp_domain_id,
                       isp_domains.isp_domain_group_id,
                       owner_posts.source_domain_id,
                       owner_posts.scrub_status_id,
                       owner_posts.post_status_id,
                       owner_posts.route_status_id,
                       '' as response,
                       owner_posts.received_at,
                       '' as error_string,
                       owner_posts.mx_status_id,
                       0 as assigned_count,
                       owner_posts.is_duplicate,
                       owner_posts.server_id,
                       owner_posts.generated_at,
                       email_addresses.email_address,
                       email_addresses.username,
                       email_addresses.domain,
                       email_addresses.last_scrubbed,
                       email_addresses.scrub_status_id,
                       owner_lists.owner_id,
                       owner_lists.owner_list,
                       owner_lists.list_key,
                       owner_lists.country_id,
                       owner_lists.max_broker_count,
                       owner_lists.primary_source_domain_id,
                       owner_lists.sms_authorized,
                       owner_lists.telemarket_authorized,
                       owner_lists.postal_authorized,
                       owner_lists.email_authorized,
                       owner_lists.scrub_posts
                FROM   owner_posts
                       INNER JOIN email_addresses USING(email_address_id)
                       INNER JOIN owner_lists USING (owner_list_id)
                       INNER JOIN isp_domains USING(isp_domain_id)
                WHERE owner_post_id={$owner_post_id}";

		$owner_post_data = $this->database->query($sql);
		$post_data = array();

		foreach ($owner_post_data->row_array() as $key => $value) {
			$post_data[strtoupper($key)] = $value;
		}

		$sql = "SELECT fields.field_name AS field_name,
                        trim(owner_posts_data.content) AS content
                    FROM owner_posts_data
                        JOIN fields ON owner_posts_data.field_id = fields.field_id
                        JOIN owner_posts ON owner_posts_data.owner_post_id = owner_posts.owner_post_id
                    WHERE owner_posts.owner_post_id = {$owner_post_id}";

		$fields = $this->database->query($sql);;

		foreach ($fields->result() as $field) {
			if (!empty($field->content)) {
				$post_data[strtoupper($field->field_name)] = $field->content;
			}
		}

		return $post_data;
	}

	function expand_isp_domain_groups($compressed_routing_rules) {

		$expanded_routing_rules = array();

		foreach ($compressed_routing_rules as $routing_rule) {

			//@todo: need to refactor routing rules table to have isp_domain_group_id and not isp_domain_id
			// even though the names don't match this statement is correct
			$isp_domain_group_id = $routing_rule['isp_domain_id'];
			$isp_domain_ids = array();

			foreach ($this->expand_isp_domain_group_id($isp_domain_group_id) as $isp_domain) {

				$isp_domain_ids[] = $isp_domain['isp_domain_id'];
			}

			$routing_rule['isp_domain_ids'] = $isp_domain_ids;
			$expanded_routing_rules[] = $routing_rule;
		}

		return $expanded_routing_rules;
	}

	function get_lists_by_server($hostname) {

		$sql = "SELECT *
                    FROM   manager_lists
                           INNER JOIN managers USING(manager_id)
                    WHERE  manager_lists.status_id = 1
                       AND managers.status_id = 1
                       AND manager_list_id NOT IN (SELECT manager_list_id
                                                   FROM   manager_posts
                                                   WHERE  status_id = 11
                                                      AND posted_at > ( Now() - INTERVAL 1 HOUR
                                                                      )
                                                   GROUP  BY manager_list_id)
                    ORDER  BY Rand()";
		$read_db = $this->load->database('read', TRUE);
		$query = $read_db->query($sql);

		return $query->result_array();
	}

	function get_unassigned_lists() {

		$sql = "SELECT * FROM manager_lists INNER JOIN managers USING(manager_id) WHERE manager_lists.status_id=1 AND managers.status_id=1 AND (SERVER IS NULL OR server = '') ORDER BY rand();  ";
		$read_db = $this->load->database('read', TRUE);
		$query = $read_db->query($sql);

		return $query->result_array();
	}

	function get_assigned_lists() {

		$sql = "SELECT * FROM manager_lists INNER JOIN managers USING(manager_id) WHERE manager_lists.status_id=1 AND managers.status_id=1 AND SERVER IS NOT NULL ORDER BY rand() LIMIT 1;  ";
		$read_db = $this->load->database('read', TRUE);
		$query = $read_db->query($sql);

		return $query->result_array();
	}

	function get_admin_listing() {

		$sql = "select * from managers";
		$query = $this->database->query($sql);
		$managers = array();

		foreach ($query->result_array() as $manager) {
			$managers[$manager['manager_id']] = $manager['manager'];
		}

		$sql = "select * from manager_lists";
		$query = $this->database->query($sql);
		$manager_lists = array();
		$manager_list_categories = array();
		$manager_list_managers = array();

		foreach ($query->result_array() as $manager_list) {
			$manager_lists[$manager_list['manager_list_id']] = $manager_list['manager_list'];
			$manager_list_categories[$manager_list['manager_list_id']] = $manager_list['list_category_id'];
			$manager_list_managers[$manager_list['manager_list_id']] = $manager_list['manager_id'];
		}

		$sql = <<<EOQ
SELECT
	manager_list_id,
	MAX(last_report_date) as last_report_date,
	COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW()),olap.report_manager_post_count_by_date.gross_count,0))) AS today,
	COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW() - INTERVAL 1 DAY),olap.report_manager_post_count_by_date.gross_count,0))) AS yesterday ,
	COALESCE(ROUND(SUM(IF(olap.report_manager_post_count_by_date.report_date > DATE(NOW() - INTERVAL 7 DAY),olap.report_manager_post_count_by_date.gross_count,0)/7),0)) AS daily_average,
	COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW()) AND (olap.report_manager_post_count_by_date.status_id IN (2,3,100,11,12,9)),olap.report_manager_post_count_by_date.gross_count,0))) AS errors_today ,
	COALESCE(round(COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW()) AND (olap.report_manager_post_count_by_date.status_id IN (2,3,100,11,12,9)),olap.report_manager_post_count_by_date.gross_count,0))) / COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW()),olap.report_manager_post_count_by_date.gross_count,0)))*100,1),0) AS error_rate,
	COALESCE(SUM(olap.report_manager_post_count_by_date.gross_count),0) AS gross
FROM olap.report_manager_post_count_by_date
LEFT OUTER JOIN (SELECT manager_list_id, max(report_date) last_report_date FROM olap.report_manager_post_count_by_date WHERE status_id IN (4,7) GROUP BY 1 ) last_posts using(manager_list_id)
GROUP BY manager_list_id
ORDER BY manager_list_id DESC
EOQ;

		$query = $this->database->query($sql);
		$manager_list_stats = array();

		foreach ($query->result_array() as $manager_list_stat) {
			$manager_list_stats[$manager_list_stat['manager_list_id']] = array(
				'today' => $manager_list_stat['today'],
				'yesterday' => $manager_list_stat['yesterday'],
				'daily_average' => $manager_list_stat['daily_average'],
				'errors_today' => $manager_list_stat['errors_today'],
				'error_rate' => $manager_list_stat['error_rate'],
				'gross' => $manager_list_stat['gross'],
				'last_report_date' => $manager_list_stat['last_report_date'],
			);
		}

		$manager_list_results = array();

		foreach ($manager_lists as $manager_list_id => $manager_list) {

			$manager_list_stat = isset($manager_list_stats[$manager_list_id]) ? $manager_list_stats[$manager_list_id] : array(
				'today' => 0,
				'yesterday' => 0,
				'daily_average' => 0,
				'errors_today' => 0,
				'error_rate' => 0,
				'gross' => 0,
				'last_report_date' => 0,
			);

			$manager_id = $manager_list_managers[$manager_list_id];

			$manager_list_results[] = array(
				'ID' => $manager_list_id,
				'manager_list_id' => $manager_list_id,
				'manager' => $managers[$manager_id],
				'manager_list' => $manager_list,
				'today' => $manager_list_stat['today'],
				'yesterday' => $manager_list_stat['yesterday'],
				'daily_average' => $manager_list_stat['daily_average'],
				'errors_today' => $manager_list_stat['errors_today'],
				'error_rate' => $manager_list_stat['error_rate'],
				'gross' => $manager_list_stat['gross'],
				'last_report_date' => $manager_list_stat['last_report_date'],
			);
		}

		return $manager_list_results;
	}

	function get_window_listing() {

		$sql = "SELECT * FROM (
                    SELECT managers.manager_id,
                           managers.manager,
                           manager_lists.manager_list_id AS ID,
                           manager_lists.manager_list,
                           Coalesce(Sum(IF(olap.report_manager_post_count_by_date.report_date = DATE(Now()), olap.report_manager_post_count_by_date.gross_count, 0))) AS today,
                           Coalesce(Sum(IF(olap.report_manager_post_count_by_date.report_date = DATE(Now() - INTERVAL 1 DAY), olap.report_manager_post_count_by_date.gross_count, 0))) AS yesterday,
                           Coalesce(Round(Sum(IF(olap.report_manager_post_count_by_date.report_date > DATE(Now() - INTERVAL 7 DAY), olap.report_manager_post_count_by_date.gross_count, 0) / 7), 0)) AS daily_average,
                           Coalesce(Sum(IF(olap.report_manager_post_count_by_date.report_date = DATE(Now())AND ( olap.report_manager_post_count_by_date.status_id IN ( 2, 3, 100, 11, 12, 9 ) ), olap.report_manager_post_count_by_date.gross_count, 0))) AS errors_today,
                           Coalesce(Round(Coalesce(Sum(IF(olap.report_manager_post_count_by_date.report_date = DATE(Now()) AND ( olap.report_manager_post_count_by_date.status_id IN ( 2, 3, 100, 11, 12, 9 ) ), olap.report_manager_post_count_by_date.gross_count, 0))) / Coalesce(Sum(IF(olap.report_manager_post_count_by_date.report_date = DATE(Now()), olap.report_manager_post_count_by_date.gross_count, 0))) * 100, 1), 0) AS error_rate,
                           Coalesce(Sum(olap.report_manager_post_count_by_date.gross_count), 0) AS gross
                    FROM   manager_lists
                           LEFT OUTER JOIN managers
                                        ON manager_lists.manager_id = managers.manager_id
                           LEFT OUTER JOIN olap.report_manager_post_count_by_date
                                        ON olap.report_manager_post_count_by_date.manager_list_id = manager_lists.manager_list_id
                    GROUP  BY manager_lists.manager_list_id,
                              manager_list,
                              managers.manager_id,
                              managers.manager
                              ) manager_lists
                WHERE daily_average > 0
                ORDER BY manager, manager_list";
		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function get_admin_routes($manager_list_id) {

		$sql = "SELECT  routing_rules.owner_list_id,
                        owners.owner,
                        owner_lists.owner_list,
                        isp_domain_groups.isp_domain_group,
                        routing_rules.status_id
                FROM routing_rules
                JOIN owner_lists USING(owner_list_id)
                JOIN owners USING(owner_id)
                JOIN isp_domain_groups ON routing_rules.isp_domain_id=isp_domain_groups.isp_domain_group_id
                WHERE manager_list_id={$manager_list_id}";

		$sql = "SELECT 	routing_rules.routing_rule_id,
                        owners.owner_id,
                        owners.owner,
                        owner_lists.owner_list_id,
                        owner_lists.owner_list,
                        list_categories.list_category,
                        COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW()),olap.report_manager_post_count_by_date.gross_count,0))) AS today,
                        COALESCE(SUM(IF(olap.report_manager_post_count_by_date.report_date = DATE(NOW() - INTERVAL 1 DAY),olap.report_manager_post_count_by_date.gross_count,0))) AS yesterday ,
                        COALESCE(ROUND(SUM(IF(olap.report_manager_post_count_by_date.report_date > DATE(NOW() - INTERVAL 7 DAY),olap.report_manager_post_count_by_date.gross_count,0)/7),0)) AS daily_average,
                        COALESCE(SUM(olap.report_manager_post_count_by_date.gross_count),0) AS gross,
                        isp_domains.isp_domain_id,
                        isp_domains.isp_domain,
                        routing_rules.status_id AS routing_rule_status_id,
                        owners.status_id AS owner_status_id,
                        owner_lists.status_id AS owner_list_status_id

                FROM   routing_rules
                    LEFT OUTER JOIN olap.report_manager_post_count_by_date  ON routing_rules.manager_list_id = olap.report_manager_post_count_by_date.manager_list_id AND routing_rules.owner_list_id=olap.report_manager_post_count_by_date.owner_list_id
                    JOIN owner_lists   ON routing_rules.owner_list_id = owner_lists.owner_list_id
                    JOIN owners ON owner_lists.owner_id = owners.owner_id
                    JOIN manager_lists ON routing_rules.manager_list_id = manager_lists.manager_list_id
                    JOIN managers ON manager_lists.manager_id = managers.manager_id
                    JOIN isp_domains ON routing_rules.isp_domain_id = isp_domains.isp_domain_id
                    JOIN list_categories ON owner_lists.list_category_id=list_categories.list_category_id
                WHERE  routing_rules.manager_list_id = {$manager_list_id}
                GROUP BY routing_rules.routing_rule_id,
                    owners.owner_id,
                    owners.owner,
                    owner_lists.owner_list_id,
                    owner_lists.owner_list,
                    owner_lists.status_id,
                    isp_domains.isp_domain_id,
                    isp_domains.isp_domain,
                    manager_lists.post_delay,
                    manager_lists.delivery_type_id,routing_rules.status_id,owner_status_id,owner_list_status_id,list_categories.list_category
                    ORDER BY routing_rule_id DESC";
		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function get_admin_last_posts_errors($manager_list_id) {
		$sql = "SELECT  * FROM manager_post_logs WHERE manager_post_id=$manager_list_id AND status_id IN (3,11)";
		$query = $this->database->query($sql);

		foreach ($query->result_array() as $manager_post) {
			foreach ($manager_post as $key => $value) {
				$manager_posts[$manager_post['manager_post_id']][$key] = $value;
			}
		}

		$manager_posts_cleaned = array();

		foreach ($manager_posts as $manager_post) {
			$manager_posts_cleaned[$manager_post['manager_post_id']]["manager_post_id"] = empty($manager_post['manager_post_id']) ? 0 : $manager_post['manager_post_id'];
			$manager_posts_cleaned[$manager_post['manager_post_id']]["owner_post_id"] = empty($manager_post['owner_post_id']) ? 0 : $manager_post['owner_post_id'];
			$manager_posts_cleaned[$manager_post['manager_post_id']]["posted_at"] = empty($manager_post['posted_at']) ? 0 : $manager_post['posted_at'];
			$manager_posts_cleaned[$manager_post['manager_post_id']]["curl_error"] = isset($manager_post['curl_error']) ? $manager_post['curl_error'] : "";
			$manager_posts_cleaned[$manager_post['manager_post_id']]["curl_output"] = isset($manager_post['curl_output']) ? $manager_post['curl_output'] : "";
			$manager_posts_cleaned[$manager_post['manager_post_id']]["variable_data"] = isset($manager_post['variable_data']) ? $manager_post['variable_data'] : "";

		}

		return $manager_posts_cleaned;

	}

	function get_admin_last_posts($manager_list_id, $limit = 10) {

		$manager_posts = array();

		$sql = "SELECT manager_post_id FROM manager_posts WHERE status_id not in (1,5,6,7) AND manager_list_id = {$manager_list_id} ORDER BY manager_post_id desc LIMIT 100";

		$query = $this->database->query($sql);
		$manager_post_ids = array(0);

		foreach ($query->result_array() as $manager_post) {
			$manager_posts[$manager_post['manager_post_id']] = $manager_post;
			$manager_post_ids[] = $manager_post['manager_post_id'];
		}

		$manager_post_ids_clause = join(",", $manager_post_ids);

		$sql = "SELECT  * FROM manager_posts inner join manager_post_statuses on manager_posts.status_id = manager_post_statuses.status_id WHERE manager_post_id in ({$manager_post_ids_clause})";
		$query = $this->database->query($sql);

		foreach ($query->result_array() as $manager_post) {
			foreach ($manager_post as $key => $value) {
				$manager_posts[$manager_post['manager_post_id']][$key] = $value;
			}
		}

		$sql = "SELECT  * FROM manager_post_logs WHERE manager_post_id in ({$manager_post_ids_clause})";
		$query = $this->database->query($sql);

		foreach ($query->result_array() as $manager_post) {
			foreach ($manager_post as $key => $value) {
				$manager_posts[$manager_post['manager_post_id']][$key] = $value;
			}
		}

		$manager_posts_cleaned = array();

		foreach ($manager_posts as $manager_post) {
			$manager_posts_cleaned[$manager_post['manager_post_id']]["manager_post_id"] = empty($manager_post['manager_post_id']) ? 0 : $manager_post['manager_post_id'];
			$manager_posts_cleaned[$manager_post['manager_post_id']]["owner_post_id"] = empty($manager_post['owner_post_id']) ? 0 : $manager_post['owner_post_id'];
			$manager_posts_cleaned[$manager_post['manager_post_id']]["posted_at"] = empty($manager_post['posted_at']) ? 0 : $manager_post['posted_at'];
			$manager_posts_cleaned[$manager_post['manager_post_id']]["curl_error"] = isset($manager_post['curl_error']) ? $manager_post['curl_error'] : "";
			$manager_posts_cleaned[$manager_post['manager_post_id']]["curl_output"] = isset($manager_post['curl_output']) ? $manager_post['curl_output'] : "";
			$manager_posts_cleaned[$manager_post['manager_post_id']]["variable_data"] = isset($manager_post['variable_data']) ? $manager_post['variable_data'] : "";

		}

		return $manager_posts_cleaned;
	}

	function get_admin_last_errors($manager_list_id, $limit = 100) {

		$sql = "SELECT *
                FROM   manager_post_logs
                WHERE manager_list_id={$manager_list_id}
                AND status_id IN (3,11)
                ORDER BY manager_post_log_id DESC
                limit {$limit} ";

		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function update_archive_routes_and_lists($manager_list_id) {

		$sql = "SELECT manager_id
                FROM   manager_lists
                WHERE manager_list_id={$manager_list_id}";

		$query = $this->database->query($sql);
		$get_manager_id = $query->result_array();
		$manager_id = $get_manager_id[0]['manager_id'];
		$sql2 = "UPDATE manager_lists SET status_id=0 WHERE manager_id=$manager_id;";
		$this->database->query($sql2);

		$sql3 = "UPDATE routing_rules SET status_id=0 WHERE manager_list_id IN (SELECT manager_list_id FROM manager_lists WHERE manager_id=$manager_id);";
		$this->database->query($sql3);


	}

	function get_current_stats($manager_list_id) {

		$current_status = array();

		$sql = "SELECT status_id, count(*) AS gross
                FROM manager_posts
                WHERE manager_list_id = {$manager_list_id}
                GROUP BY status_id";

		$query = $this->database->query($sql);
		$manager_post_statuses = array(
			"1" => "Queued",
			"2" => "Sent - No Response",
			"3" => "Sent - Error",
			"4" => "Sent - Success",
			"5" => "Sending",
			"6" => "Download - Queued",
			"7" => "Downloaded - Success",
			"8" => "Sent - Duplicate",
			"9" => "We Deleted",
			"10" => "Sent - Rejected",
			"11" => "Remote Server Error",
			"12" => "LBC Missing Data",
			"42" => "Programmatic Standby",
			"100" => "Sent - Unknown Response",
		);

		foreach ($query->result_array() as $row) {
			$current_status[] = array(
				'status' => $manager_post_statuses[$row['status_id']],
				'gross' => $row['gross'],
			);
		}

		return $current_status;

	}

	function get_admin_stats($manager_list_id) {

		$sql = "SELECT report_date,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id=6,olap.report_manager_post_count_by_date.gross_count, NULL)) AS queued_for_downloaded,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id=1,olap.report_manager_post_count_by_date.gross_count, NULL)) AS queued_for_post,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id=4,olap.report_manager_post_count_by_date.gross_count, NULL)) AS posted_success,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id=7,olap.report_manager_post_count_by_date.gross_count, NULL)) AS download_success,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id IN (2,3,100,11,12,9),olap.report_manager_post_count_by_date.gross_count, NULL)) AS `errors`,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id=5,olap.report_manager_post_count_by_date.gross_count, NULL)) AS sending,
                    SUM(IF(olap.report_manager_post_count_by_date.status_id IN (10,8),olap.report_manager_post_count_by_date.gross_count, NULL)) AS rejected,
                    SUM(olap.report_manager_post_count_by_date.gross_count) AS gross_records
                FROM olap.report_manager_post_count_by_date
                WHERE manager_list_id={$manager_list_id}
                GROUP BY report_date DESC";
		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function update_manager_lists_to_exclude() {

		$manager_list_performance_review_time = intval($this->retrieve_configuration_content("manager_list_performance_review_time"));
		$manager_list_performance_pause_time = intval($this->retrieve_configuration_content("manager_list_performance_pause_time"));
		$manager_list_performance_error_rate = intval($this->retrieve_configuration_content("manager_list_performance_error_rate"));
		$manager_list_performance_threshold = intval($this->retrieve_configuration_content("manager_list_performance_threshold"));

		$sql = "DELETE FROM manager_lists_to_exclude WHERE created_at < ( now() - INTERVAL {$manager_list_performance_pause_time} MINUTE);";
		$this->database->query($sql);

		$sql = "DELETE FROM manager_list_errors WHERE happened_at < ( now() - INTERVAL {$manager_list_performance_pause_time} MINUTE);";
		$this->database->query($sql);

		$sql = "DELETE FROM manager_list_performance WHERE happened_at < ( now() - INTERVAL {$manager_list_performance_pause_time} MINUTE);";
		$this->database->query($sql);

		$sql = "INSERT INTO manager_lists_to_exclude (manager_list_id, reason)
                SELECT manager_list_id, concat('Error Threshold at ', error_rate)
                FROM (SELECT manager_list_id,
                               Count(*)                             total,
                               Round(( Sum(CASE status_id
                                             WHEN 11 THEN 1
                                             ELSE 0
                                           END) / Count(*) ) * 100) AS error_rate
                        FROM   manager_posts
                        WHERE  created_at > ( Now() - INTERVAL {$manager_list_performance_review_time} MINUTE ) AND status_id <> 5
                        GROUP  BY manager_list_id) manager_list_errors
                WHERE  error_rate > {$manager_list_performance_error_rate} AND total > 100 AND manager_list_id NOT IN(SELECT manager_list_id FROM manager_lists_to_exclude);";

		$sql = "REPLACE INTO manager_lists_to_exclude (manager_list_id, reason)
                SELECT manager_list_id,concat('Error Threshold at ',round((total/seconds_between_errors)*100,2))  AS reason
                FROM (
                SELECT manager_list_id, Count(*) total, TIME_TO_SEC(TIMEDIFF(max(happened_at), min(happened_at))) AS seconds_between_errors
                FROM   manager_list_errors
                WHERE  happened_at > ( Now() - INTERVAL {$manager_list_performance_review_time} MINUTE )
                GROUP  BY manager_list_id) rate_query";
		$this->database->query($sql);

		$paused_manager_list_count = $this->database->affected_rows();

		$sql = "REPLACE INTO manager_lists_to_exclude (manager_list_id, reason)
                SELECT manager_list_id,concat('Response Threshold at ',round(response_time,2))  AS response_time
                FROM (
                SELECT manager_list_id, AVG(response_time) AS response_time
                FROM   manager_list_performance
                WHERE  happened_at > ( Now() - INTERVAL {$manager_list_performance_review_time} MINUTE )
                GROUP  BY manager_list_id) rate_query";
		$this->database->query($sql);

		$paused_manager_list_count += $this->database->affected_rows();

		return $paused_manager_list_count;

	}

	/**
	 * @param $configuration_key
	 *
	 * @return array
	 */
	public function retrieve_configuration_content($configuration_key) {

		$sql = "SELECT `value` FROM configuration WHERE `variable` = '{$configuration_key}'";
		$query = $this->database->query($sql);

		if ($query) {
			$row = $query->row();

			return $row->value;
		} else {
			$error = print_r($this->database->error(), TRUE);
			error_log("Missing configuration key for {$configuration_key} SQL: {$sql} ERROR: {$error}");

			return FALSE;
		}
	}

	function record_import($manager_list_id, $record_count) {

		$sql = "INSERT INTO topica_imports(manager_list_id, records_uploaded) VALUES({$manager_list_id},{$record_count})";
		$this->database->query($sql);
	}

	function get_owner_list_visualization_data() {

		$sql = <<<EOSQL
SELECT manager_lists.manager_list_id AS id,
       manager_lists.esp_id AS parentid,
	   RIGHT(manager_lists.manager_list,length(manager_lists.manager_list)-8) AS `text`,
       manager_lists.active_record_count AS `value`
FROM   manager_lists
	INNER JOIN esps ON manager_lists.esp_id = esps.esp_id
WHERE  manager_lists.manager_id = 23
   AND manager_lists.manager_list LIKE 'topica%'
   AND manager_lists.status_id = 1
   AND manager_lists.esp_list_id <> 999999999
UNION
SELECT esp_id AS id,
	   NULL AS parentid,
	   esp AS `text`,
	   '' AS `value`
FROM esps
WHERE esps.esp LIKE 'topica%'

EOSQL;

		$sql = <<<EOSQL
SELECT REPLACE(Trim(RIGHT(manager_lists.manager_list,
            Length(manager_lists.manager_list) - 8)),'_',' ') AS `label`,
       active_record_count AS `value`
FROM   manager_lists
WHERE  manager_list LIKE 'topica%'
   AND status_id = 1
   AND manager_lists.esp_list_id <> 999999999
ORDER  BY label;
EOSQL;

		$sql = <<<EOSQL
SELECT list_category AS `label`,
       sum(active_record_count) AS `value`
FROM   manager_lists
INNER JOIN list_categories USING(list_category_id)
WHERE  manager_list LIKE 'topica%'
   AND status_id = 1
   AND manager_lists.esp_list_id <> 999999999
   GROUP BY list_category
ORDER  BY label;
EOSQL;

		$sql = <<<EOSQL
SELECT `label`,
        `value`,
        concat('#',SUBSTRING((lpad(hex(@curRow := @curRow + 100),6,0)),-6)) AS color
       FROM (
    SELECT list_category AS `label`,
           sum(active_record_count) AS `value`
           FROM   manager_lists
    INNER JOIN list_categories USING(list_category_id)
    INNER JOIN (SELECT @curRow := 5426175) counter
    WHERE  manager_list LIKE 'topica%'
       AND status_id = 1
       AND manager_lists.esp_list_id <> 999999999
   GROUP BY list_category
   ) categories
   UNION
  SELECT 'Unused' AS `label`,
       6000000 - sum(active_record_count) AS `value`,
        '#993F3D' AS color
       FROM   manager_lists
    WHERE  manager_list LIKE 'topica%'
       AND status_id = 1
       AND manager_lists.esp_list_id <> 999999999
ORDER  BY VALUE DESC;
EOSQL;

		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function get_manager_lists_logs($manager_list_id) {
		$sql = "SELECT manager_list_log_id, manager_list_id, `change`, notes, manager_lists_log.user_id, username, created_at
                FROM manager_lists_log, admin.users
                WHERE manager_lists_log.manager_list_id='{$manager_list_id}'
                AND manager_lists_log.user_id=users.user_id";

		$query = $this->database->query($sql);

		return $query->result_array();
	}

	function insert_manager_lists_log($manager_list_id, $differences, $note, $user_id) {
		$data = array(
			'manager_list_id' => $manager_list_id,
			'change' => $differences,
			'notes' => $note,
			'user_id' => $user_id
		);

		if (!$this->database->insert('manager_lists_log', $data)) {
			$error = $this->database->error();

			return $error;
		}
	}

	function get_options_with_manager_ids() {

		$sql = "SELECT manager_id, manager_list_id, manager_list FROM manager_lists WHERE status_id=1 ORDER BY manager_list";
		$query = $this->database->query($sql);
		$options = array();

		foreach ($query->result_array() as $row) {
			if (!isset($options[$row['manager_id']])) {
				$options[$row['manager_id']] = array();
			}
			$options[$row['manager_id']][] = $row;
		}

		return $options;
	}
}
