<link rel="stylesheet" type="text/css" href="/css/forms.css">
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">

<dialog id="window">
	<div id="pop_content"></div>
	<button id="exit">Close Dialog</button>
</dialog>
<button id="show" style="display:none">Show Dialog</button>
<div id="routing_rules_template" style="display:none">
	<div style='margin-top: 10px;' id="jqxgrid">
		Building grid.
	</div>
	<br/><br/>
	<a href="#" id="add_routing_rule_link">Add Routing Rule</a>

	<div id="add_routing_rule_form" style="display:none;">
		<img src='/js/jqwidgets/styles/images/loader.gif'/>
	</div>
</div>
<div id="manager_template" style="display:none">
	<div id="form_div" class="form-style">
		<form action="/admin/managers/update/" method="POST">
			<fieldset style="border:0px">
				<label for="manager" style="float: left;margin-right: 10px;font-size: 15px;">Manager</label>
				<input type="text" name="manager" id="manager" value="" style="position:absolute; left:200px;"/>
			</fieldset>
			<fieldset style="border:0px">
				<label for="phone" style="float: left;margin-right: 10px;font-size: 15px;">Phone</label>
				<input type="text" name="phone" id="phone" value="" style="position:absolute; left:200px;"/>
			</fieldset>
			<fieldset style="border:0px">
				<label for="manager" style="float: left;margin-right: 10px;font-size: 15px;">Email</label>
				<input type="text" name="email" id="email" value="" style="position:absolute; left:200px;"/>
			</fieldset>
			<fieldset style="border:0px">
				<label for="status_id" style="float: left;margin-right: 10px;font-size: 15px;">Status</label>
				<select name="status_id" id="status_id" style="position:absolute; left:200px;">

				</select>
			</fieldset>
			<fieldset style="border:0px">
				<input type="submit" name="submit" id="submit" value="submit"/>
			</fieldset>
		</form>
	</div>
</div>
<div id="manager_list_template" style="display:none">
	<div id="form_div" class="form-style" style="max-width:1000px">
		<form action="/admin/manager_lists/update/" method="POST" target="_self">
			<!-- TODO: need to remove these fields -->
			<input type="hidden" name="server" id="server" value=""/>
			<input type="hidden" name="batch_size" id="batch_size" value=""/>
			<input type="hidden" name="command" id="command" value=""/>
			<table>
				<tr>
					<td>
						<fieldset style="border:0px">
							<label for="manager_list" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Manager List</label>
							<input class='validate_manager_list_view' type="text" name="manager_list" id="manager_list" value=""/>
						</fieldset>
					</td>
					<td>
						<fieldset style="border:0px">
							<label for="manager_id" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Manager</label>
							<select class='validate_manager_list_view' name="manager_id" id="manager_id">

							</select>
						</fieldset>
					</td>
				</tr>
				<tr>
					<td>
						<fieldset style="border:0px">
							<label for="delivery_type_id" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Delivery Type</label>
							<select class='validate_manager_list_view' name="delivery_type_id" id="delivery_type_id">

							</select>
						</fieldset>

						<div id="ftp_fields">

							<fieldset style="border:0px">
								<label for="username" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Username</label>
								<input class='validate_manager_list_view' type="text" name="username" id="username"/>
							</fieldset>

							<fieldset style="border:0px">
								<label for="password" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Password</label>
								<input class='validate_manager_list_view' type="text" name="password" id="password"/>
							</fieldset>

							<fieldset style="border:0px">
								<label for="remote_directory" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Remote Directory</label>
								<input class='validate_manager_list_view' type="text" name="remote_directory" id="remote_directory"/>
							</fieldset>
							<fieldset style="border:0px">
								<label for="remote_port" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Remote Port</label>
								<input class='validate_manager_list_view' type="text" name="remote_port" id="remote_port"/>
							</fieldset>

						</div>

						<div id="managed_esp_fields">

							<fieldset style="border:0px">
								<label for="esp_id"style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">ESP</label>
								<select class='validate_manager_list_view' name="esp_id" id="esp_id">

								</select>
							</fieldset>

							<fieldset style="border:0px">
								<label for="esp_list_id" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">ESP List ID</label>
								<input class='validate_manager_list_view' type="text" name="esp_list_id" id="esp_list_id" value=""/>
							</fieldset>

							<fieldset id="total_capacity_fieldset" style="border:0px">
								<label for="total_capacity" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">List Capacity</label>
								<input class='validate_manager_list_view' type="text" name="total_capacity" id="total_capacity"
								       value=""/>
							</fieldset>

							<fieldset id="days_inactive_tolerance_fieldset" style="border:0px">
								<label for="days_inactive_tolerance" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Days Inactive Tolerance</label>
								<input class='validate_manager_list_view' type="text" name="days_inactive_tolerance" id="days_inactive_tolerance"
								       value=""/>
							</fieldset>

							<fieldset id="dept_name_fieldset" style="border:0px">
								<label for="dept_name" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">From Dept</label>
								<input class='validate_manager_list_view' type="text" name="dept_name" id="dept_name"
								       value=""/>
							</fieldset>

							<fieldset id="from_address_fieldset" style="border:0px">
								<label for="from_address" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">From Name</label>
								<input class='validate_manager_list_view' type="text" name="from_address" id="from_address"
								       value=""/>
							</fieldset>

						</div>
					</td>
					<td>
						<fieldset style="border:0px">
							<label for="list_category_id" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Category</label>
							<select class='validate_manager_list_view' name="list_category_id" id="list_category_id">

							</select>
						</fieldset>
					</td>
				</tr>
				<tr>
					<td class="post_fields remote_url">
						<fieldset style="border:0px">
							<label for="post_type" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">post_type</label>
							<select class='validate_manager_list_view' name="post_type" id="post_type">

							</select>
						</fieldset>
					</td>
					<td class="post_fields">
						<fieldset style="border:0px">
							<label for="post_delay" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Post Delay</label>
							<input class='validate_manager_list_view' type="text" name="post_delay" id="post_delay" value=""/>
						</fieldset>
					</td>
				</tr>
				<tr>
					<td class="remote_url">

						<fieldset style="border:0px">
							<label for="post_url" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">post_url</label>
							<input class='validate_manager_list_view' type="text" name="post_url" id="post_url" value=""/>
						</fieldset>
					</td>
					<td class="post_fields">

						<fieldset style="border:0px">
							<label for="post_timeout" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">post_timeout</label>
							<input class='validate_manager_list_view' type="text" name="post_timeout" id="post_timeout" value=""/>
						</fieldset>
					</td>
				</tr>
				<tr>
					<td colspan="2" class="post_fields remote_url">
						<fieldset style="border:0px">
							<label for="variable_data" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">variable_data</label>
            <textarea class='validate_manager_list_view' style="width: 750px; margin: 2px 0px; height: 60px;" type="text" name="variable_data"
                      id="variable_data"></textarea>
						</fieldset>
					</td>
				</tr>
				<tr class="post_response">
					<td class="post_fields">
						<fieldset style="border:0px">
							<label for="success_text" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">success_text</label>
							<input class='validate_manager_list_view' type="text" name="success_text" id="success_text" value=""/>
						</fieldset>
					</td>
					<td class="post_fields">

						<fieldset style="border:0px">
							<label for="error_text" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">error_text</label>
							<input class='validate_manager_list_view' type="text" name="error_text" id="error_text" value=""/>
						</fieldset>
					</td>
				</tr>
				<tr class="post_response">
					<td class="post_fields">
						<fieldset style="border:0px">
							<label for="duplicate_text" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">duplicate_text</label>
							<input class='validate_manager_list_view' type="text" name="duplicate_text" id="duplicate_text" value=""/>
						</fieldset>
					</td>
					<td class="post_fields">
						<fieldset style="border:0px">
							<label for="reject_text" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">reject_text</label>
							<input class='validate_manager_list_view' type="text" name="reject_text" id="reject_text" value=""/>
						</fieldset>
					</td>
				</tr>
				<tr>
					<td>
						<fieldset style="border:0px">
							<label for="purge_inactive" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">purge_inactive</label>
							<select class='validate_manager_list_view' name="purge_inactive" id="purge_inactive">

							</select>
						</fieldset>

					</td>
					<td>
						<fieldset style="border:0px">
							<label for="allow_duplicates" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">allow_duplicates</label>
							<select class='validate_manager_list_view' name="allow_duplicates" id="allow_duplicates">

							</select>
						</fieldset>

					</td>
				</tr>
				<tr>
					<td>
						<fieldset style="border:0px">
							<label for="send_mail" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">send_mail</label>
							<select class='validate_manager_list_view' name="send_mail" id="send_mail">

							</select>
						</fieldset>
					</td>
					<td>
						<fieldset style="border:0px">
							<label for="status_id" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px">Status</label>
							<select class='validate_manager_list_view' name="status_id" id="status_id">

							</select>
						</fieldset>
					</td>
				</tr>
				<tr>
					<td></td>
					<td>
						<label for="status_id" style="text-align: left;float: left;margin-right: 10px;font-size: 15px;width:150px;"></label>
						<input type="submit" name="command" id="scrub_list_button" value="Scrub List" />
						<input type="submit" name="command" id="update_button" value="Update"/>
						<input type="submit" name="command" id="add_new_button" value="Save New" />
					</td>
				</tr>
			</table>
		</form>
	</div>
</div>
<style type="text/css">
	.rowdetails_tab {
		width: 100%;
		height: 600px;
	}

	.green {
		color: black;
		background-color: #B1E111;
	}

	.yellow {
		color: black;
		background-color: yellow;
	}

	.red {
		color: black;
		background-color: #E78383;
	}

	.green:not(.jqx-grid-cell-hover):not(.jqx-grid-cell-selected), .jqx-widget .green:not(.jqx-grid-cell-hover):not(.jqx-grid-cell-selected) {
		color: black;
		background-color: #b6ff00;
	}

	.yellow:not(.jqx-grid-cell-hover):not(.jqx-grid-cell-selected), .jqx-widget .yellow:not(.jqx-grid-cell-hover):not(.jqx-grid-cell-selected) {
		color: black;
		background-color: yellow;
	}

	.red:not(.jqx-grid-cell-hover):not(.jqx-grid-cell-selected), .jqx-widget .red:not(.jqx-grid-cell-hover):not(.jqx-grid-cell-selected) {
		color: black;
		background-color: #e83636;
	}
	dialog {
		width: 500px;
		background:#e8e8e8;
		border: 1px solid #dadada;
		font-family:sans-serif;
		padding: 5px 10px 20px 20px;
		z-index: 10000;
		box-shadow: 0px 0px 40px 10px;
		position:fixed;
	}
</style>

<script type="text/javascript">
	(function() {
		var dialog = document.getElementById('window');
		document.getElementById('show').onclick = function() {
			dialog.show();
		};
		document.getElementById('exit').onclick = function() {
			dialog.close();
		};
	})();
	function get_manager_info(datarecord,tab){
		var url = "/admin/managers/edit_by_manager_list_id/"+datarecord['manager_list_id']+"/1";
		$.ajax({
			url: url
		}).done(function(data) {
			{
				data=JSON.parse(data);
				console.log(data);
				load_manager(datarecord,data,tab)
			}
		});
	}
	function load_manager(datarecord,manager,tab){
		var url="/admin/managers/edit/"+manager['manager_id']+"/1";
		$.ajax({
			url: url
			}).done(function(data) {
			{
				data=JSON.parse(data);
				console.log(data);
				var manager=data['manager'];
				var phone=data['phone'];
				var email=data['email'];
				var status=data['status'];
				var template=$("#manager_template").clone();
				template.find("#manager").val(manager);
				template.find("#phone").val(phone);
				template.find("#email").val(email);
				for(var i in data['status_options']){
					if(i==data['status_id']){
						template.find("#status_id").prepend("<option value='"+i+"' selected'>"+data['status_options'][i]+"</option>");
					}else{
						template.find("#status_id").append("<option value='"+i+"'>"+data['status_options'][i]+"</option>");
					}
				}
				template.find("form").submit(function(e) {
					var url = '/admin/managers/update/' + data.manager_id+"/1";
					var form_data=$(this).serializeArray();
					console.log(form_data);
					$.ajax({
						type: "POST",
						url: url,
						data: form_data, // serializes the form's elements.
						success: function (data2) {
							$("#pop_content").html(data2);
							var dialog = document.getElementById('window');
							dialog.show();
						}
					});
					e.preventDefault(); // avoid to execute the actual submit of the form.
				});
			}
			template.show();
			$("#rowdetails_manager").html(template);
		});
	}
	function valid_manager_list_view(form){
		var data=new Array();
		data['server']=form.find("#server").val();
		data['batch_size']=form.find("#batch_size").val();

		data['manager_list']=form.find("#manager_list").val();
		data['manager_id']=form.find("#manager_id").val();
		data['delivery_type_id']=form.find("#delivery_type_id").val();

		data['username']=form.find("#username").val();
		data['password']=form.find("#password").val();
		data['remote_directory']=form.find("#remote_directory").val();
		data['remote_port']=form.find("#remote_port").val();

		data['esp_id']=form.find("#esp_id").val();
		data['total_capacity']=form.find("#total_capacity").val();
		data['days_inactive_tolerance']=form.find("#days_inactive_tolerance").val();
		data['dept_name']=form.find("#dept_name").val();
		data['from_address']=form.find("#from_address").val();

		data['list_category_id']=form.find("#list_category_id").val();
		data['post_type']=form.find("#post_type").val();
		data['post_delay']=form.find("#post_delay").val();
		data['post_url']=form.find("#post_url").val();
		data['post_timeout']=form.find("#post_timeout").val();

		data['variable_data']=form.find("#variable_data").val();
		data['success_text']=form.find("#success_text").val();
		data['error_text']=form.find("#error_text").val();
		data['duplicate_text']=form.find("#duplicate_text").val();
		data['reject_text']=form.find("#reject_text").val();
		data['purge_inactive']=form.find("#purge_inactive").val();
		data['allow_duplicates']=form.find("#allow_duplicates").val();
		data['send_mail']=form.find("#send_mail").val();
		data['status_id']=form.find("#status_id").val();

		console.log(data);
		if(form.find("#delivery_type_id").val()==3){//Poster
			form.find("#post_delay").parent().show();
			form.find("#success_text").parent().show();
			form.find("#error_text").parent().show();
			form.find("#duplicate_text").parent().show();
			form.find("#reject_text").parent().show();
		}else{
			form.find("#post_delay").parent().hide();
			form.find("#success_text").parent().hide();
			form.find("#error_text").parent().hide();
			form.find("#duplicate_text").parent().hide();
			form.find("#reject_text").parent().hide();
		}
		if(form.find("#delivery_type_id").val()==4){//ESP List Que
			form.find("#dept_name").parent().show();
			form.find("#from_address").parent().show();
			form.find("#esp_id").parent().show();
			form.find("#esp_list_id").parent().show();
			form.find("#purge_inactive").parent().show();
			form.find("#days_inactive_tolerance").parent().show();
			form.find("#total_capacity").parent().show();
		}else{
			form.find("#dept_name").parent().hide();
			form.find("#from_address").parent().hide();
			form.find("#esp_id").parent().hide();
			form.find("#esp_list_id").parent().hide();
			form.find("#purge_inactive").parent().hide();
			form.find("#days_inactive_tolerance").parent().hide();
			form.find("#total_capacity").parent().hide();
		}
		if(form.find("#post_type").val()==10||form.find("#post_type").val()==7||form.find("#post_type").val()==8){
			form.find("#username").parent().show();
			form.find("#password").parent().show();
			if(form.find("#post_type").val()==10){
				form.find("#remote_directory").parent().show();
				form.find("#remote_port").parent().show();
			}
		}else{
			form.find("#username").parent().hide();
			form.find("#password").parent().hide();
			form.find("#remote_directory").parent().hide();
			form.find("#remote_port").parent().hide();
		}

	}
	function load_manager_list(datarecord){
		var url = '/admin/manager_lists/edit/'+datarecord['manager_list_id']+"/1";
		var template=$("#manager_list_template").clone();
		$.ajax({
			url: url
		}).done(function(data) {
			{
				data=JSON.parse(data);
				console.log(data);
				template.find("#form_div").attr("manager_list_id",datarecord['manager_list_id']);
				template.find("#server").val(data['server']);
				template.find("#batch_size").val(data['batch_size']);

				template.find("#manager_list").val(data['manager_list']);
				for(var i in data.managers){
					if(i == data.manager_id){
						template.find("#manager_id").prepend("<option value='"+i+"' selected>"+data.managers[i]+"</option>");
					}else{
						template.find("#manager_id").append("<option value='"+i+"'>"+data.managers[i]+"</option>");
					}
				}
				for(var i in data.delivery_types){
					if(i == data.delivery_type_id){
						template.find("#delivery_type_id").prepend("<option value='"+i+"' selected>"+data.delivery_types[i]+"</option>");
					}else{
						template.find("#delivery_type_id").append("<option value='"+i+"'>"+data.delivery_types[i]+"</option>");
					}
				}
				template.find("#username").val(data.username);
				template.find("#password").val(data.password);
				template.find("#remote_directory").val(data.remote_directory);
				template.find("#remote_port").val(data.remote_port);

				for(var i in data.esps){
					if(i==data.esp_id){
						template.find("#esp_id").prepend("<option value='"+i+"' selected>"+data.esps[i]+"</option>");
					}else{
						template.find("#esp_id").append("<option value='"+i+"'>"+data.esps[i]+"</option>");
					}
				}
				template.find("#esp_list_id").val(data.esp_list_id);
				for(var i in data.list_categories){
					if(i == data.list_category_id){
						template.find("#list_category_id").prepend("<option value='"+i+"' selected>"+data.list_categories[i]+"</option>");
					}else{
						template.find("#list_category_id").append("<option value='"+i+"'>"+data.list_categories[i]+"</option>");
					}
				}
				template.find("#total_capacity").val(data.total_capacity);
				template.find("#days_inactive_tolerance").val(data.days_inactive_tolerance);
				template.find("#dept_name").val(data.dept_name);
				template.find("#from_address").val(data.from_address);
				for(var i in data.post_types){
					if(data.post_type == data.post_types[i]){
						template.find("#post_type").prepend("<option value='"+data.post_types[i]+"' selected>"+data.post_types[i]+"</option>");
					}else{
						template.find("#post_type").append("<option value='"+data.post_types[i]+"'>"+data.post_types[i]+"</option>");
					}
				}
				template.find("#post_delay").val(data['post_delay']);
				template.find("#post_url").val(data['post_url']);
				template.find("#post_timeout").val(data['post_timeout']);
				var variable_data=data['variable_data'].replace(/&amp;/g,'&');
				template.find("#variable_data").val(variable_data);
				template.find("#success_text").val(data['success_text']);
				template.find("#error_text").val(data['error_text']);
				template.find("#duplicate_text").val(data['duplicate_text']);
				template.find("#reject_text").val(data['reject_text']);

				if(data.purge_inactive==0){
					template.find("#purge_inactive").prepend("<option value='0' selected> NO </option>");
					template.find("#purge_inactive").append("<option value='1'> YES </option>");
				}else{
					template.find("#purge_inactive").prepend("<option value='1' selected> YES </option>");
					template.find("#purge_inactive").append("<option value='0'> NO </option>");
				}
				if(data.allow_duplicates==0){
					template.find("#allow_duplicates").prepend("<option value='0' selected> NO </option>");
					template.find("#allow_duplicates").append("<option value='1'> YES </option>");
				}else{
					template.find("#allow_duplicates").prepend("<option value='1' selected> YES </option>");
					template.find("#allow_duplicates").append("<option value='0'> NO </option>");
				}
				if(data.send_mail==0){
					template.find("#send_mail").prepend("<option value='0' selected> NO </option>");
					template.find("#send_mail").append("<option value='1'> YES </option>");
				}else{
					template.find("#send_mail").prepend("<option value='1' selected> YES </option>");
					template.find("#send_mail").append("<option value='0'> NO </option>");
				}
				for(var i in data.status_options){
					if(i == data.status_id){
						template.find("#status_id").prepend("<option value='"+i+"' selected>"+data.status_options[i]+"</option>");
					}else{
						template.find("#status_id").append("<option value='"+i+"'>"+data.status_options[i]+"</option>");
					}
				}
				template.show();
				template.find("#update_button").one("click",function(){
					template.find("form").submit(function(e) {
						var url = '/admin/manager_lists/update/'+data.manager_list_id+"/1";
						var form_data=$(this).serializeArray();
						console.log(form_data);
						$.ajax({
							type: "POST",
							url: url,
							data: form_data,// serializes the form's elements.
							success: function (data2) {
								$("#pop_content").html(data2);
								var dialog = document.getElementById('window');
								dialog.show();
								$("#window").find("#back_button").remove();
								$("#window").find("form").submit(function(e){
									var url='/admin/manager_lists/confirm_update/'+data.manager_list_id+"/1";
									var form_data=$("#window").find("form").serializeArray();
									$("#window").find("#back_button").remove();
									$.ajax({
										type: "POST",
										url: url,
										data: form_data,// serializes the form's elements.
										success: function (data) {
											console.log(data);
										}
									});
									e.preventDefault(); // avoid to execute the actual submit of the form.
								});
							}
						});
						e.preventDefault(); // avoid to execute the actual submit of the form.
					});
				});
				template.find("#add_new_button").one("click",function(){
					$(this).closest("#form_div").find("#command").val("Save New");
					console.log("add New");
					template.find("form").submit(function(e) {
						var url = '/admin/manager_lists/update/' + data.manager_list_id+"/1";
						var form_data=$(this).serializeArray();
						console.log(form_data);
						$.ajax({
							type: "POST",
							url: url,
							data: form_data, // serializes the form's elements.
							success: function (data2) {
								console.log(data2);
								alert("It may take up to 5 minutes for new records to appear. Alternatively you can clear your browsers cache to see the record immediately.")
//								window.location="/admin/manager_lists?new="+Math.random().toString(36).substring(7);
							}
						});
						e.preventDefault(); // avoid to execute the actual submit of the form.
					});
				});

				template.find("#scrub_list_button").click(function(){
					console.log("scrub_list");
					template.find("form").submit(function(e) {

						var url = '/admin/manager_lists/scrub_manager_list/' + data.manager_list_id;
						var form_data=$(this).serializeArray();
						console.log(form_data);
						$.ajax({
							url: url,
							success: function (data2) {
								console.log(data2);
							}
						});
						e.preventDefault(); // avoid to execute the actual submit of the form.
					});
				});
				$("#rowdetails_manager_list").html(template);
				valid_manager_list_view($("#rowdetails_manager_list"));
				$(".validate_manager_list_view").on('change',function(){
					valid_manager_list_view($(this).closest("#form_div"));
				});
				$(".validate_manager_list_view").on('focusout',function(){
					valid_manager_list_view($(this).closest("#form_div"));
				});
			}
		})


	}
	function load_routing_rules(datarecord,url){
		var url = "/admin/manager_lists/routing_rules/dataset/"+datarecord['manager_list_id'];
		// prepare the data
		var source = {
			datatype: "json",
			url: url,
			datafields: [
				{name: 'routing_rule_id', type: 'int'},
				{name: 'owner_id', type: 'int'},
				{name: 'owner', type: 'string'},
				{name: 'owner_list_id', type: 'int'},
				{name: 'owner_list', type: 'string'},
				{name: 'list_category', type: 'string'},
				{name: 'today', type: 'int'},
				{name: 'yesterday', type: 'int'},
				{name: 'daily_average', type: 'int'},
				{name: 'gross', type: 'int'},
				{name: 'isp_domain_group_id', type: 'int'},
				{name: 'isp_domain_group', type: 'string'},
				{name: 'routing_rule_status', type: 'string'},
				{name: 'owner_status_id', type: 'int'},
				{name: 'owner_list_status_id', type: 'int'}
			]
		};

		var dataAdapter = new $.jqx.dataAdapter(source);
		var template=$("#routing_rules_template").clone();
		template.show();
		template.find("#jqxgrid").jqxGrid({
			selectionmode: 'multiplecellsadvanced',
			autosavestate: true,
			source: dataAdapter,
			theme: theme,
			columnsresize: true,
			autoheight: true,
			sortable: true,
			altrows: true,
			width: "100%",
			enablemousewheel: false,
			enablebrowserselection: true,
			ready: function () {
				var dataRecord = $("#jqxgrid").jqxGrid('getrowdata', 0);
			},
			columns: [
				{
					text: 'Edit', datafield: null, cellsalign: 'left', width: 40,
					cellsrenderer: function (row, column, value) {
						row_data = template.find("#jqxgrid").jqxGrid('getrowdata', row);
						routing_rule_id = row_data["routing_rule_id"];
						var html = '<a href="#" onclick="edit_routing_rule('+routing_rule_id +','+datarecord['manager_list_id']+')" style="overflow: hidden;  text-overflow: ellipsis; padding-bottom: 2px; text-align: right; margin-right: 2px; margin-left: 4px; margin-top: 4px;">Edit</a>';
						return html;
					}
				},
				{
					text: 'Filters',
					datafield: null,
					cellsalign: 'left',
					width: 40,
					cellsrenderer: function (row, column, value) {
						row_data = template.find("#jqxgrid").jqxGrid('getrowdata', row);
						routing_rule_id = row_data["routing_rule_id"];
						var html = '<a href="#" onclick="edit_routing_rule_filter('+routing_rule_id +','+datarecord['manager_list_id']+')" style="overflow: hidden;  text-overflow: ellipsis; padding-bottom: 2px; text-align: right; margin-right: 2px; margin-left: 4px; margin-top: 4px;">Edit</a>';
						return html;
					}
				},
				{text: 'RRID', datafield: 'routing_rule_id', cellsalign: 'left', width: 40},
				{text: 'RR Status', datafield: 'routing_rule_status', cellsalign: 'right', width: 80},
				{text: 'Owner', datafield: 'owner', cellsalign: 'left', width: 125},
				{text: 'Owner List', datafield: 'owner_list', cellsalign: 'right', width: 125},
				{text: 'Owner List ID', datafield: 'owner_list_id', cellsalign: 'right', width: 80},
				{text: 'List Category', datafield: 'list_category', cellsalign: 'right', width: 125},
				{text: 'Today', datafield: 'today', cellsalign: 'right', width: 80},
				{text: 'Yesterday', datafield: 'yesterday', cellsalign: 'right', width: 80},

				{text: 'D. Average', datafield: 'daily_average', cellsalign: 'right', width: 80},
				{text: 'Gross', datafield: 'gross', cellsalign: 'right', width: 80},

				{text: 'ISP Domain Group', datafield: 'isp_domain_group', cellsalign: 'right', width: 80},
				{
					text: 'Owner Status',
					datafield: null,
					cellsalign: 'left',
					width: 80,
					cellsrenderer: function (row, column, value) {
						row_data = template.find("#jqxgrid").jqxGrid('getrowdata', row);
						owner_list_id = row_data["owner_list_id"];

						if (owner_list_id == 1) {
							return "ON";
						} else {
							return "OFF";
						}
					}
				},
				{
					text: 'OL Status',
					datafield: null,
					cellsalign: 'left',
					width: 80,
					cellsrenderer: function (row, column, value) {
						row_data = template.find("#jqxgrid").jqxGrid('getrowdata', row);
						owner_list_status_id = row_data["owner_list_status_id"];

						if (owner_list_status_id == 1) {
							return "ON";
						} else {
							return "OFF";
						}
					}
				}
			]
		});
		template.find("#add_routing_rule_link").parent().append("<div id='routing_secondary_"+datarecord['manager_list_id']+"'></div>");

		template.find("#add_routing_rule_link").attr("id","add_routing_rule_link_"+datarecord['manager_list_id']);

		template.find("#add_routing_rule_link_"+datarecord['manager_list_id']).click(function (event) {
			event.preventDefault();
			$("#routing_secondary_"+datarecord['manager_list_id']).hide();
			add_routing_rule(datarecord['manager_list_id']);
		});
		$("#rowdetails_routes").html(template);
	}
	function add_routing_rule(manager_list_id){
		$("#routing_secondary_"+manager_list_id).html('<img src="/js/jqwidgets/styles/images/loader.gif">');
		var url = '/admin/manager_lists/routing_rules/add?manager_list_id='+manager_list_id;
		$.ajax({
			url: url,
			success: function (data) {
				$("#routing_secondary_" + manager_list_id).html($(data).find("#form_div"));
				$("#routing_secondary_" + manager_list_id).show();
				$("#routing_secondary_" + manager_list_id).find("form").submit(function (e) {
					var url = "/admin/manager_lists/routing_rules/update/0";
					var form_data = $(this).serializeArray();
					$.ajax({
						type: "POST",
						url: url,
						data: form_data,
						success: function (data2) {
							$("#pop_content").html("Record has been successfully added.");
							$("#routing_secondary_" + manager_list_id).html('<img src="/js/jqwidgets/styles/images/loader.gif">');
							var dialog = document.getElementById('window');
							dialog.show();
							$("#routes"+manager_list_id).trigger("click");
						}
					});
					e.preventDefault();
				});
			}
		});
	}
	function edit_routing_rule(routing_rule_id,manager_list_id){
		$("#routing_secondary_"+manager_list_id).html('<img src="/js/jqwidgets/styles/images/loader.gif">');
		var url = '/admin/manager_lists/edit_routing_rules/'+routing_rule_id;
		$.ajax({
			url: url,
			success: function (data2) {
				$("#routing_secondary_"+manager_list_id).html($(data2).find("#form_div"));
				$("#routing_secondary_"+manager_list_id).show();
				$("#routing_secondary_"+manager_list_id).find("form").submit(function(e) {

					var url = '/admin/manager_lists/routing_rules/update/'+routing_rule_id;
					var form_data=$(this).serializeArray();
					$("#routing_secondary_"+manager_list_id).html('<img src="/js/jqwidgets/styles/images/loader.gif">');
					$.ajax({
						type: "POST",
						url: url,
						data: form_data,
						success: function (data2) {
							$("#routing_secondary_"+manager_list_id).html($(data2).find("form"));
							$("#routing_secondary_"+manager_list_id).find("#back_button").click(function(e){
								e.preventDefault();
								edit_routing_rule(routing_rule_id,manager_list_id);
							});
							$("#routing_secondary_"+manager_list_id).show();
							$("#routing_secondary_"+manager_list_id).find("form").submit(function(e) {
								var url = '/admin/manager_lists/confirm_routing_rule_update/'+routing_rule_id+'/'+manager_list_id;
								var form_data = $(this).serializeArray();
								$("#routing_secondary_"+manager_list_id).html('<img src="/js/jqwidgets/styles/images/loader.gif">');
								$.ajax({
									type: "POST",
									url: url,
									data: form_data,
									success:function(data2){
										$("#routing_secondary_"+manager_list_id).html("<div></div>");
										$("#routing_secondary_"+manager_list_id).show();
										$("#pop_content").html("Record has been successfully updated.");
										var dialog = document.getElementById('window');
										dialog.show();
										$("#routes"+manager_list_id).trigger("click");
									}
								});
								e.preventDefault(); // avoid to execute the actual submit of the form.
							});
						}
					});
					e.preventDefault(); // avoid to execute the actual submit of the form.
				});
			}
		});
	}
	function edit_routing_rule_filter(routing_rule_id,managerListId){
		$("#routing_secondary_"+managerListId).html('<img src="/js/jqwidgets/styles/images/loader.gif">');
		var url = '/admin/routing_rules/'+routing_rule_id;
		$.ajax({
			url: url
		}).done(function(data){
			var entity=$(data).find("#content");
			$("#routing_secondary_"+managerListId).html(entity);

			$("#routing_secondary_"+managerListId).show();
//			$.getJSON("/admin/owner_lists/get_by_owner_id/" + $("#owner_id").val(), function (response) {
//				for (index in response.owner_lists) {
//					option = document.createElement("option");
//					option.value = response.owner_lists[index].owner_list_id;
//					option.text = response.owner_lists[index].owner_list;
//					entity.find("#owner_list_id").append(option);
//				}
//			});
			$.getJSON("/admin/manager_lists/get_by_manager_id/"+entity.find("#manager_id").val(), function (response) {
				for (index in response.manager_lists) {
					option = document.createElement("option");
					option.value = response.manager_lists[index].manager_list_id;
					option.text = response.manager_lists[index].manager_list;
					entity.find("#manager_list_id").append(option);
				}
			});
			entity.find("#submit").click(function (event) {
				event.preventDefault();

				if($("#routing_secondary_"+managerListId).find("#routing_rule_filter_id").val()==0){
					console.log("New Filter");
					$("#sub_form").find("input[name=routing_rule_filter_id]").each(function(){$(this).val("0")});
					$("#sub_form").find("input[name=routing_rule_id]").each(function(){$(this).val(routing_rule_id)});
					var sub_form=$("#sub_form").serialize();
					console.log($("#sub_form").serialize());

					$.ajax({
						type: "POST",
						url: "/admin/routing_rules/update_filter",
						data: $("#sub_form").serialize()
					}).done(function (data) {
						console.log(data);
//					window.location.reload();
						$(".jqx-window-close-button").trigger("click");
						$("#routes"+manager_list_id).trigger("click");
					});
				}else{
					console.log($("#sub_form").serialize());
					$.ajax({
						type: "POST",
						url: "/admin/routing_rules/update_filter",
						data: $("#sub_form").serialize()
					}).done(function (data) {
						console.log(data);
//					window.location.reload();
						$(".jqx-window-close-button").trigger("click");
						$("#routes"+manager_list_id).trigger("click");
					});
				}
			});
		});
	}
	function load_tabs(datarecord,url,tab_urls,flag){

		if(flag==0){

			$("#rowdetails_" + url).html("<iframe style='width: 100%;height: 550px;' src=''><img src='/js/jqwidgets/styles/images/loader.gif'/><iframe>");
			setTimeout(function(){
				load_tabs(datarecord,url,tab_urls,1);
			},5);
		}else{
			$("#rowdetails_" + url).find("iframe").attr("src",tab_urls[url] + datarecord['manager_list_id']);
		}

	}
	function load_legacy_queue(datarecord){
		var url = "/admin/manager_lists/legacy_queue/"+datarecord['manager_list_id'];
		$.ajax({
			url: url,
			success: function (data3) {
				$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).html($(data3).find("#main"));
				$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find("legend").remove();
				load_legacy_queue_info();
				var select=$("#rowdetails_legacy_queue_"+datarecord["manager_list_id"]).find("#manager_list_id_jqxDropDownList");
				var option=select.find("option[value='"+datarecord["manager_list_id"]+"']").text();
				$("#rowdetails_legacy_queue_"+datarecord["manager_list_id"]).find("fieldset").first().append("<span>"+option+"</span>");
				$("#rowdetails_legacy_queue_"+datarecord["manager_list_id"]).find("fieldset").first().find("#manager_list_id").hide();
				var entity=$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']);
				function load_legacy_queue_info() {
					$("#manager_list_id").jqxDropDownList({
						searchMode: 'containsignorecase',
						theme: theme,
						width: "90%",
						filterable: true
					});

					var request;

					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find("#start_date").jqxCalendar({
						width: 220,
						height: 220,
						theme: theme
					});

					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find("#end_date").jqxCalendar({
						width: 220,
						height: 220,
						theme: theme,
						enableViews: true
					});

					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find('#start_date ').jqxCalendar('setMinDate', new Date(2012, 11, 15));
					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find('#start_date ').jqxCalendar('setMaxDate', new Date());

					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find('#end_date ').jqxCalendar('setMinDate', new Date(2012, 11, 15));
					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find('#end_date ').jqxCalendar('setMaxDate', new Date());

					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find("#start_date").on('click change', function (event) {
						update_range();
					});

					$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).find("#end_date").on('click change', function (event) {
						update_range();
					});

					function update_range() {
						start_date = get_start_date();
						end_date = get_end_date();

						$("#selection").html("<div>" + start_date + " - " + end_date + "</div>");
					}

					function get_start_date() {

						start_date = new Date($("#start_date").val());
						var start_day = start_date.getDate();
						var start_month = start_date.getMonth() + 1; //Months are zero based
						var start_year = start_date.getFullYear();
						start_date = start_year + "-" + start_month + "-" + start_day;
						return start_date;
					}

					function get_end_date() {
						end_date = new Date($("#end_date").val());
						var end_day = end_date.getDate();
						var end_month = end_date.getMonth() + 1; //Months are zero based
						var end_year = end_date.getFullYear();
						end_date = end_year + "-" + end_month + "-" + end_day;
						return end_date;
					}

					$(".prev, .next").click(function () {
						update_summary_info();
					});

					function get_job_details() {
						job_details = [];

						job_details.manager_list_id = $("#manager_list_id").val();
						job_details.manager_list = $("#manager_list_id option:selected").text();
						job_details.recipient = $("#recipient").val();
						job_details.start_date = get_start_date();
						job_details.end_date = get_end_date();
						job_details.limit = $("#limit").val();
						job_details.direction = $("#direction").val();
						job_details.post_status_id = $("#post_status_id").val();
						job_details.post_status = $("#post_status_id option:selected").text();
						job_details.manager_post_status = $("#manager_post_status_id:selected").text();
						job_details.manager_post_status_id = $("#manager_post_status_id").val();
						job_details.isp_domain_group_id = $("#isp_domain_group_id").val();

						return job_details;
					}

					function update_summary_info() {

						job_details = get_job_details();

						$("#summary_manager_list").html(job_details.manager_list);
						$("#summary_recipient").html(job_details.recipient);
						$("#summary_start_date").html(job_details.start_date);
						$("#summary_end_date").html(job_details.end_date);
						$("#summary_limit").html(job_details.limit);
						$("#summary_direction").html(job_details.direction);
						$("#summary_recipient").html(job_details.recipient);
						$("#summary_post_status").html(job_details.post_status);
						$("#summary_manager_post_status").html(job_details.manager_post_status);
						console.log(job_details);

					}

					function is_array(input) {
						return typeof(input) === 'object' && (input instanceof Array);
					}

					$("#legacy_queue_job_data_button").click(function () {
						job_details = get_job_details();

						var payload = new Object();
						payload['manager_list_id'] = job_details.manager_list_id;
						payload['recipient'] = job_details.recipient;
						payload['start_date'] = job_details.start_date;
						payload['end_date'] = job_details.end_date;
						payload['limit'] = job_details.limit;
						payload['direction'] = job_details.direction;
						payload['post_status_id'] = job_details.post_status_id;
						payload['manager_post_status_id'] = job_details.manager_post_status_id;
						payload['isp_domain_group_id'] = job_details.isp_domain_group_id;

						// capture all the form fields
						var $inputs = $("#export_form").find("input, select, button, textarea");

						// let's disable the inputs for the duration of the ajax request
						$inputs.prop("disabled", true);

						// fire off the request
						$.ajax({
							url: "/admin/data/create_legacy_queue_job",
							type: "post",
							data: payload,
							success: function (response, textStatus, jqXHR) {
								console.log(response);
								$inputs.prop("disabled", false);
								$("#message").html("<b>Job created successfully</b>");
							},
							error: function (jqXHR, textStatus, errorThrown) {
								// log the error to the console
								console.error("The following error occured: " + textStatus, errorThrown);
								$inputs.prop("disabled", false);
								$("#message").html("<b>Error creating job</b>");
							}
						});
					});
				};

				$("#rowdetails_legacy_queue_"+datarecord['manager_list_id']).show();

			}
		});
	}
	function load_export_scripts(datarecord){
		$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#manager_list_id").jqxDropDownList({
			searchMode: 'containsignorecase',
			theme: theme,
			width: "100%",
			filterable: true
		});

		var request;

		$("#manager_list_id").on("select", function () {

			// abort any pending request
			if (request) {
				request.abort();
			}

			// capture all the form fields
			var $inputs = $("#export_form").find("input, select, button, textarea");

			// let's disable the inputs for the duration of the ajax request
			$inputs.prop("disabled", true);

			var request = $.ajax({
				url: "/admin/data/manager_list_fields/" + $(this).val(),
				type: "post",
				dataType: "json"
			});

			// callback handler that will be called on success
			request.done(function (response, textStatus, jqXHR) {
				// log a message to the console
				options = $("#fields");
				options.empty();

				$.each(response, function () {
					options.append($("<option />").val(this.field_id).text(this.field_name));
				});
				$("#exportable_fields").show();
			});

			// callback handler that will be called on failure
			request.fail(function (jqXHR, textStatus, errorThrown) {
				// log the error to the console
				//console.error("The following error occured: " + textStatus, errorThrown);
			});

			// callback handler that will be called regardless
			// if the request failed or succeeded
			request.always(function () {
				// reenable the inputs
				$inputs.prop("disabled", false);
			});

			// prevent default posting of form
			event.preventDefault();

		});
		$("#manager_list_id").trigger("select");

		$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#start_date").jqxCalendar({
			width: 220,
			height: 220,
			theme: theme,
			// setMinDate: new Date(2012, 11, 15),
			// setMaxDate: new Date()
			//            value: new Date(2012, 11, 15),
		});

		$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#end_date").jqxCalendar({
			width: 220,
			height: 220,
			theme: theme,
			// setMinDate: new Date(2012, 11, 15),
			// setMaxDate: new Date(),
			enableViews: true
			// value: new Date(),
		});

		$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#start_date").on('click change', function (event) {
			update_range();
		});

		$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#end_date").on('click change', function (event) {
			update_range();
		});

		function update_range() {
			start_date = get_start_date();
			end_date = get_end_date();

			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#selection").html("<div>" + start_date + " - " + end_date + "</div>");
		}

		function get_start_date() {

			start_date = new Date($("#rowdetails_export_"+datarecord["manager_list_id"]).find("#start_date").val());
			var start_day = start_date.getDate();
			var start_month = start_date.getMonth() + 1; //Months are zero based
			var start_year = start_date.getFullYear();
			start_date = start_year + "-" + start_month + "-" + start_day;
			console.log()
			return start_date;
		}

		function get_end_date() {
			end_date = new Date($("#rowdetails_export_"+datarecord["manager_list_id"]).find("#end_date").val());
			var end_day = end_date.getDate();
			var end_month = end_date.getMonth() + 1; //Months are zero based
			var end_year = end_date.getFullYear();
			end_date = end_year + "-" + end_month + "-" + end_day;
			return end_date;
		}

		function get_job_details() {
			job_details = [];

			job_details.manager_list_id = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#manager_list_id").val();
			job_details.manager_list = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#manager_list_id").text();

			job_details.field_ids = [];
			job_details.fields = [];

			$('#fields :selected').each(function (index, selected) {
				job_details.fields[index] = $(selected).text();
				job_details.field_ids[index] = $(selected).val();
			});

			job_details.isp_domain_ids = [];
			job_details.isp_domains = [];

			$('#isp_domains :selected').each(function (index, selected) {
				job_details.isp_domains[index] = $(selected).text();
				job_details.isp_domain_ids[index] = $(selected).val();
			});

			job_details.filter_type = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#filter_types").val();
			job_details.only_new_posts = (job_details.filter_type === "new_records") ? "true" : "false";

			switch (job_details.filter_type) {
				case 'date_range':
					job_details.start_date = get_start_date();
					job_details.end_date = get_end_date();
					job_details.limit = "";
					break;

				case 'limit_range':
					job_details.start_date = "";
					job_details.end_date = "";
					job_details.limit = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#limit").val() === 0 ? "All Posts" : $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#limit").val();
					break;

				case 'new_records':
					job_details.start_date = "";
					job_details.end_date = "";
					job_details.limit = "";
					break;

				default:
					job_details.start_date = "";
					job_details.end_date = "";
					job_details.limit = "";
					break;

			}
			job_details.recipient = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#recipient").val();
			job_details.save_file_path = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#sftp_users").val();
			job_details.recurring_export = $("#rowdetails_export_"+datarecord["manager_list_id"]).find("#recurring_export").val();

			console.log(job_details);

			return job_details;
		}

		function update_summary_info() {

			job_details = get_job_details();

			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_manager_list").html(job_details.manager_list);
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_fields").html(job_details.fields.join(", "));
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_isp_domains").html(job_details.isp_domains.join(", "));

			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_filter_type").html(job_details.filter_type);
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_start_date").html(job_details.start_date);
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_end_date").html(job_details.end_date);
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_limit").html(job_details.limit);
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_recurring_export").html(job_details.recurring_export);
			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_only_new_posts").html(job_details.only_new_posts);

			$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#summary_recipient").html(job_details.recipient);

			//<div id="summary">
			//    <b>Manager list: </b><span id="summary_manager_list"></span> <br/> <br/>
			//    <b>Fields:</b> <span id="summary_fields"></span> <br/> <br/>
			//    <b>ISP domains:</b> <span id="summary_isp_domains"></span> <br/> <br/>
			//
			//    <b>Filter By:</b><span id="summary_filter_type"></span><br/>
			//    <ul>
			//        <li><b>Date Range:</b> <span id="summary_start_date"></span> - <span id="summary_end_date"></span></li>
			//        <li><b>Limit:</b> <span id="summary_limit"></span></li>
			//        <li><b>Only New Posts:</b> <span id="summary_only_new_posts"></span></li>
			//    </ul>
			//    <br/> <br/>
			//    <b>Email export to:</b> <span id="summary_recipient"></span> <br/> <br/>
			//</div>
			console.log(job_details);
		}

		function is_array(input) {
			return typeof(input) === 'object' && (input instanceof Array);
		}

		$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#export_data_button").click(function () {
			job_details = get_job_details();

			var payload = new Object();
			payload['manager_list_id'] = job_details.manager_list_id;
			payload['fields[]'] = job_details.field_ids;
			payload['isp_domains[]'] = job_details.isp_domain_ids;
			payload['filter_type'] = job_details.filter_type;
			payload['start_date'] = job_details.start_date;
			payload['end_date'] = job_details.end_date;
			payload['limit'] = job_details.limit;
			payload['only_new_posts'] = job_details.only_new_posts;
			payload['recipient'] = job_details.recipient;
			payload['save_file_path'] = job_details.save_file_path;
			payload['recurring_export'] = job_details.recurring_export;

			// capture all the form fields
			var $inputs = $("#export_form").find("input, select, button, textarea");

			// let's disable the inputs for the duration of the ajax request
			$inputs.prop("disabled", true);

			// fire off the request
			$.ajax({
				url: "/admin/data/create_export_job",
				type: "post",
				data: payload,
				success: function (response, textStatus, jqXHR) {
					$inputs.prop("disabled", false);
//					$("#export_data_button").hide(100);
					$("#message").html("<b>Job created successfully</b>");
					update_summary_info();
					$("#summary").show();
				},
				error: function (jqXHR, textStatus, errorThrown) {
					// log the error to the console
					console.error("The following error occured: " + textStatus, errorThrown);
					$inputs.prop("disabled", false);
					$("#message").html("<b>Error creating job</b>");
				}
			});
		});

		$("#filter_types").change(function (event) {

			switch ($(this).val()) {
				case 'date_range':
					$("#date_range_div").show();
					$("#limit_range_div").hide();
					break;
				case 'limit_range':
					$("#date_range_div").hide();
					$("#limit_range_div").show();
					break;
				default:
					$("#date_range_div").hide();
					$("#limit_range_div").hide();
					break;
			}
		});

	}
	function load_export_data(url,datarecord){
//		tab_urls.export+datarecord["manager_list_id"]
		console.log(url);
		$.ajax({
			url: url,
			success: function (data) {
				var style=$(data).find("#content").find("style");
				$("#rowdetails_export_"+datarecord["manager_list_id"]).prepend(style);
				var email="<?php echo $this->session->userdata("email_address");?>";
				console.log(email);
				$("#rowdetails_export_"+datarecord["manager_list_id"]).find("img").remove();
				$("#rowdetails_export_"+datarecord["manager_list_id"]).append($(data).find("#main"));

				$("#rowdetails_export_"+datarecord["manager_list_id"]).find("legend").remove();
				$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#recipient").val(email);
				load_export_scripts(datarecord);
				var select=$("#rowdetails_export_"+datarecord["manager_list_id"]).find("#manager_list_id_jqxDropDownList");
				var option=select.find("option[value='"+datarecord["manager_list_id"]+"']").text();
				$("#rowdetails_export_"+datarecord["manager_list_id"]).find("fieldset").first().append("<span>"+option+"</span>");
				$("#rowdetails_export_"+datarecord["manager_list_id"]).find("fieldset").first().find("#manager_list_id").hide();

			}
		});
	}
	$(document).ready(function () {
		<?php
		$base_directory = $_SERVER['DOCUMENT_ROOT'] . "/application/views/admin/manager_lists";

		$scripts = array(
			"last_posts/script.js",
			"stats/script.js",
			"current_stats/script.js",
			"jobs/script.js",
			"logs/script.js",
			"last_errors/script.js",
		);

		foreach($scripts as $script){
			printf("\n%s\n",file_get_contents(sprintf("%s/%s",$base_directory,$script)));
		}
		?>

		var error_class = function (row, columnfield, value) {

			if (value == "") {
				return "";
			} else if (value < 10) {
				return '';
			} else if (value >= 10 && value < 40) {
				return 'yellow';
			} else {
				return 'red';
			}
		}

		var dataAdapter = new $.jqx.dataAdapter({
			url: '/admin/manager_lists/manager_lists_dataset',
			datatype: 'json',
			id: 'manager_list_id',
			sortcolumn: 'manager_list_id',
			sortdirection: 'desc',
			datafields: [
				{name: 'manager_id', type: 'number'},
				{name: 'manager', type: 'string'},
				{name: 'manager_list_id', type: 'number'},
				{name: 'manager_list', type: 'string'},
//				{name: 'list_categories', type: 'string'},
				{name: 'revenue', type: 'string'},
				{name: 'today', type: 'number'},
				{name: 'yesterday', type: 'number'},
				{name: 'daily_average', type: 'number'},
				{name: 'errors_today', type: 'number'},
				{name: 'error_rate', type: 'float'},
				{name: 'gross', type: 'number'},
				{name: 'last_report_date', type: 'date'}
			],
		});


		var greater_than_zero_filter = function () {

			var filtergroup = new $.jqx.filter();
			var filter_or_operator = 1;
			var filter = filtergroup.createfilter('numericfilter', 4.5, 'GREATER_THAN');
			filtergroup.addfilter(filter_or_operator, filter);
			return filtergroup;
		}();

		var title_cell_builder = function (row, column, value, defaulthtml, columnproperties) {
			row_data = data[row];
			manager_id = row_data["manager id"];
			manager_list_id = row_data["ID"];

			html = '<span style="width:100%;height:100%;display:inline-block;text-overflow: ellipsis; overflow: hidden; padding-bottom: 2px; text-align: left; vertical-align: middle; margin: 5px;">';

			switch (column) {

				case "manager":
					html += '<a href=/admin/managers/edit/' + manager_id + '>' + value + '</a>';
					break;

				case "manager list":
					html += '<a href=/admin/manager_lists/edit/' + manager_list_id + '>' + value + '</a>';
					break;

				default :
					console.log("No case for " + column + " in cell renderer");
					break;
			}

			html += '</span>';

			return html;
		}

		var link_cell_builder = function (row, column, value, defaulthtml, columnproperties) {
			row_data = data[row];
			manager_id = row_data["owner id"];
			manager_list_id = row_data["ID"];

			html = '<span style="width:100%;height:100%;display:inline-block;text-overflow: ellipsis; overflow: hidden; padding-bottom: 2px; text-align: center; vertical-align: middle; margin: 5px;">';

			switch (column) {

				case "content":
					html += '<a href=/admin/manager_lists/content/' + manager_list_id + '>Content</a>';
					break;

				case "routes":
					html += '<a href=/admin/manager_lists/routing_rules/' + manager_list_id + '>Routes</a>';
					break;

				case "posts":
					html += '<a href=/admin/manager_lists/last_posts/' + manager_list_id + '/100>Posts</a>';
					break;

				case "source domains":
					html += '<a href=/admin/manager_lists/source_domains/' + manager_list_id + '>Domains</a>';
					break;

				case "ISP domains":
					html += '<a href=/admin/manager_lists/isp_domains/' + manager_list_id + '>Domains</a>';
					break;

				case "stats":
					html += '<a href=/admin/manager_lists/stats/' + manager_list_id + '/>Stats</a>';
					break;

				case "current_stats":
					html += '<a href=/admin/manager_lists/current_stats/' + manager_list_id + '/>Stats</a>';
					break;

				default :
					console.log("No case for " + column + " in cell renderer");
					break;
			}

			html += '</span>';

			return html;
		}

		var tab_urls = {
			routes: "/admin/manager_lists/routing_rules/",
			posts: "/admin/manager_lists/last_posts/",
            errors: "/admin/manager_lists/last_errors/",
			stats: "/admin/manager_lists/stats/",
			current_stats: "/admin/manager_lists/current_stats/",
			manager: "/admin/manager/edit/",
			manager_list: "/admin/manager_lists/edit/",
			legacy_queue: "/admin/manager_lists/legacy_queue/",
			export: "/admin/manager_lists/export/",
			jobs: "/admin/manager_lists/jobs/",
			logs: "/admin/manager_lists/logs/",
			archive_routes: "/admin/manager_lists/archive_routes/"
		};

		var initrowdetails = function (index, parentElement, gridElement, datarecord) {
			console.log($($($(parentElement).children()[0])));
			$($($(parentElement).children()[0])).jqxTabs({theme: theme});
			$($($(parentElement).children()[0])).find("li").click(function(){
				var currentTab=$(this).find(".jqx-tabs-titleContentWrapper").text();
				var tab;
				switch(currentTab){
					case 'Routes':
						load_routing_rules(datarecord,tab_urls.routes,parentElement);
						tab='routes';
						break;
					case 'Posts':
						tab='posts';
						$("#rowdetails_posts").html('<div id="dataTable"><img src="/js/jqwidgets/styles/images/loader.gif"/></div>');
						$("#rowdetails_posts").attr("id","rowdetails_posts_"+datarecord['manager_list_id']);
						build_posts(datarecord['manager_list_id'],index, parentElement, gridElement, datarecord);
						break;
					case 'Last Errors':
						$("#rowdetails_last_errors").html('<div id="dataTable"><img src="/js/jqwidgets/styles/images/loader.gif"/></div>');
						$("#rowdetails_last_errors").attr("id","rowdetails_last_errors_"+datarecord['manager_list_id']);
						build_last_errors(datarecord['manager_list_id'],index, parentElement, gridElement, datarecord);
						break;
					case 'Stats':
						tab='stats';
						$("#rowdetails_stats").html('<div id="dataTable"></div>');
						$("#rowdetails_stats").attr("id","rowdetails_stats_"+datarecord['manager_list_id']);
						build_stats(datarecord['manager_list_id'],index, parentElement, gridElement, datarecord);
						break;
					case 'Current Stats':
						tab='current_stats';
						$("#rowdetails_current_stats").html('<div id="dataTable"></div>');
						$("#rowdetails_current_stats").attr("id","rowdetails_current_stats_"+datarecord['manager_list_id']);
						build_current_stats(datarecord['manager_list_id'],index, parentElement, gridElement, datarecord);
						break;
					case 'Edit Manager':
						tab='edit_manager';
						get_manager_info(datarecord,"/admin/manager/edit/");
						break;
					case 'Edit Manager List':
						tab='edit_manager_list';
						load_manager_list(datarecord);
						break;
					case 'Legacy Queue':
						tab='legacy_queue';
						$("#rowdetails_legacy_queue").attr("id","rowdetails_legacy_queue_"+datarecord['manager_list_id']);
						load_legacy_queue(datarecord);
						break;
					case 'Export':
						tab='export';
						$("#rowdetails_export").attr("id","rowdetails_export_"+datarecord['manager_list_id']);
						load_export_data(tab_urls.export+datarecord["manager_list_id"],datarecord);
						break;
					case 'Jobs':
						tab='jobs';
						$("#rowdetails_jobs").html('<div id="jqxWidget" style="float: left;"><div id="jqxgrid"></div></div>');
						$("#rowdetails_jobs").attr("id","rowdetails_jobs_"+datarecord['manager_list_id']);
						build_jobs_here(datarecord['manager_list_id'],index, parentElement, gridElement, datarecord);
						break;
					case 'Logs':
						tab='logs';
						$("#rowdetails_logs").html('<div id="jqxWidget" style="float: left;"><div id="jqxgrid"></div></div>');
						$("#rowdetails_logs").attr("id","rowdetails_logs_"+datarecord['manager_list_id']);
						build_logs_here(datarecord['manager_list_id'],index, parentElement, gridElement, datarecord);
						break;
					case 'Archive Routes':
						tab ='archive_routes';
						$("#rowdetails_archive_routes").attr('id',"rowdetails_archive_routes_"+datarecord['manager_list_id']);
						$("#rowdetails_archive_routes_"+datarecord['manager_list_id']).html('<img src="/js/jqwidgets/styles/images/loader.gif"/>');
						var url="/admin/manager_lists/archive_routes/" + datarecord['manager_list_id'];
						$.ajax({
							url: url,
							success: function (data) {
								$("#rowdetails_archive_routes_"+datarecord["manager_list_id"]).find("img").remove();
								$("#rowdetails_archive_routes_"+datarecord["manager_list_id"]).append($(data).find("#content"));
							}
						});
						break;
					default:
						console.log("Tab is not initialized in the switch statement.");
						break;
				}
				$(this).attr('id',tab+datarecord["manager_list_id"]);
			});
			$($($(parentElement).children()[0])).find("li").first().trigger("click");
		};

		$("#manager_lists_grid").jqxGrid({
			theme: theme,
			source: dataAdapter,
			columnsresize: true,
			autoheight: true,
			groupable: true,
			sortable: true,
			filterable: true,
			showfilterrow: true,
			altrows: true,
			height: 800,
			width: 1400,
			enablemousewheel: true,
			enablebrowserselection: true,
			rowdetails: true,
			initrowdetails: initrowdetails,
			rowdetailstemplate: {rowdetails: $("#rowdetails").html(), rowdetailsheight: 600},
			columns: [
				{text: 'manager', dataField: 'manager', cellsalign: 'left', filtercondition: 'contains', width: 200},
				{text: 'ID', dataField: 'manager_list_id', cellsalign: 'center', width: 40},
				{
					text: 'manager list',
					dataField: 'manager_list',
					filtertype: 'textbox',
					filtercondition: 'contains',
					cellsalign: 'left',
					width: 240
				},
//				{
//					text: 'list categories',
//					dataField: 'list_categories',
//					filtertype: 'textbox',
//					filtercondition: 'contains',
//					cellsalign: 'left',
//					width: 120
//				},
				{text: 'today', dataField: 'today', cellsalign: 'right', width: 90, type: 'number'},
				{text: 'yesterday', dataField: 'yesterday', cellsalign: 'right', width: 90},
				{text: 'daily average', dataField: 'daily_average', cellsalign: 'right', width: 90},
				{text: 'errors today', dataField: 'errors_today', cellsalign: 'right', width: 90},
				{
					text: 'error rate',
					dataField: 'error_rate',
					cellsalign: 'right',
					width: 90,
					cellclassname: error_class
				},
				{text: 'gross', dataField: 'gross', cellsalign: 'right', width: 90},
				{text: 'last post', dataField: 'last_report_date', cellsalign: 'right', width: 90}
			]
		});
	});



//	// display selected row index.
//	$("#manager_lists_grid").on('cellselect', function (event) {
//		var columnheader = $("#manager_lists_grid").jqxGrid('getcolumn', event.args.datafield).text;
//	});
//
//	// display unselected row index.
//	$("#manager_lists_grid").on('cellunselect', function (event) {
//		var columnheader = $("#manager_lists_grid").jqxGrid('getcolumn', event.args.datafield).text;
//	});
	function build_jobs_here(manager_list_id,index, parentElement, gridElement, datarecord) {
		var managerListID = manager_list_id;
		var url = "/admin/manager_lists/job_dataset/" + managerListID;


		// prepare the data
		var source = {
			datatype: "json",
			datafields: [
				{name: 'consumer_job_id', type: 'int'},
				{name: 'consumer_job_type', type: 'string'},
				{name: 'manager_list', type: 'string'},
				{name: 'recipient', type: 'string'},
				{name: 'processed_at', type: 'datetime'},
				{name: 'created_at', type: 'datetime'},
				{name: 'consumer_job_status', type: 'string'},
				{name: 'output', type: 'string'}
			],
			id: 'consumer_job_id',
			url: url
		};

		var dataAdapter = new $.jqx.dataAdapter(source);
		var initrowdetails = function (index, parentElement, gridElement, datarecord) {
			output = $(parentElement).find('.output');
			output.text(datarecord.output);
			console.log(output);
			console.log(datarecord);
		};

		$("#rowdetails_jobs_"+manager_list_id).find("#jqxgrid").jqxGrid({
			theme: theme,
			width: 1024,
			source: dataAdapter,
			sortable: true,
			filterable: true,
			showfilterrow: true,
			altrows: true,
			enablemousewheel: true,
			initrowdetails: initrowdetails,
			rowdetails: true,
			rowdetailstemplate: {
				rowdetails: "<div style='margin: 10px;'><pre class='output'></pre></div>",
				rowdetailsheight: 200
			},

			selectionmode: 'singlerow',
			columns: [
				{
					text: 'job_id',
					datafield: 'consumer_job_id',
					filtercondition: 'contains',
					cellsalign: 'left',
					width: 40
				},
				{
					text: 'consumer_job_type',
					datafield: 'consumer_job_type',
					filtertype: 'checkedlist',
					cellsalign: 'left',
					width: 140
				},
				{
					text: 'manager_list',
					datafield: 'manager_list',
					filtercondition: 'contains',
					cellsalign: 'left',
					width: 240
				},
				{text: 'processed_at', datafield: 'processed_at', width: 140},
				{text: 'created_at', datafield: 'created_at', width: 140},
				{text: 'status', datafield: 'consumer_job_status', filtertype: 'checkedlist', width: 80}
			]
		});
	}
	function build_logs_here(manager_list_id,index, parentElement, gridElement, datarecord){
		var managerListID = manager_list_id;
		var url = "/admin/manager_lists/log_dataset/" + managerListID;

		// prepare the data
		var source = {
			datatype: "json",
			datafields: [
				{name: 'manager_list_log_id', type: 'int'},
				{name: 'manager_list_id', type: 'int'},
				{name: 'change', type: 'string'},
				{name: 'notes', type: 'string'},
				{name: 'user_id', type: 'int'},
				{name: 'username', type: 'string'},
				{name: 'created_at', type: 'string'}
			],
			id: 'manager_list_log_id',
			url: url
		};

		var dataAdapter = new $.jqx.dataAdapter(source);

		var initrowdetails = function (index, parentElement, gridElement, datarecord) {
			var change = datarecord.change;
			var formattedChange = change.replace("\n", "<br />");
			$($($(parentElement).children()[0])).html('<strong>Changed</strong> <br />' + formattedChange + '<br /><br /><strong>Notes</strong><br />' + datarecord.notes);
		};

		$("#rowdetails_logs_"+manager_list_id).find("#jqxgrid").jqxGrid({
			theme: theme,
			width: 1100,
			source: dataAdapter,
			rowdetails: true,
			rowdetailstemplate: {rowdetails: "<div class='main'></div>", rowdetailsheight: 150},
			initrowdetails: initrowdetails,
			columns: [
				{text: 'Date', datafield: 'created_at', width: 140},
				{text: 'Who', datafield: 'username', width: 100},
				{text: 'Change', datafield: 'change', width: 400},
				{text: 'Notes', datafield: 'notes', width: 400}
			]
		});
	}

</script>
<style type="text/css">
	#main {
		float:: left;
		width: 965px;
		border: solid 1px #b2b3b5;
		-moz-border-radius: 10px;
		padding: 20px;
		background-color: #f6f6f6;
		min-height: 600px;
	}

	fieldset {
		border: none;
		width: 640px;
	}

	legend {
		font-size: 18px;
		margin: 0px;
		padding: 10px 0px;
		color: #336699;
		font-weight: bold;
	}

	label {
		display: block;
		margin: 15px 0 5px;
	}

	input[type=text], input[type=password] {
		width: 300px;
		padding: 5px;
		border: solid 1px #000;
	}

	.prev, .next {
		background-color: #336699;
		padding: 5px 10px;
		color: #fff;
		text-decoration: none;
	}

	.prev:hover, .next:hover {
		background-color: #000;
		text-decoration: none;
	}

	.prev {
		float: left;
	}

	.next {
		float: right;
	}

	#steps {
		list-style: none;
		width: 100%;
		overflow: hidden;
		margin: 0px;
		padding: 0px;
	}

	#steps li {
		font-size: 24px;
		float: left;
		padding: 10px;
		color: #b0b1b3;
	}

	#steps li span {
		font-size: 11px;
		display: block;
	}

	#steps li.current {
		color: #000;
	}

	#makeWizard {
		background-color: #336699;
		color: #fff;
		padding: 5px 10px;
		text-decoration: none;
		font-size: 18px;
	}

	#makeWizard:hover {
		background-color: #000;
	}
</style>

<div id="manager_lists_grid">
	<img src='/js/jqwidgets/styles/images/loader.gif'/>
</div>


<div id='jqxWidget' style="font-size: 13px; font-family: Verdana; float: left;">
	<div id="rowdetails" style="display: none">
		<div id='jqxTabs'>
			<ul>
				<li style="margin-left: 30px;">Routes</li>
				<li>Posts</li>
				<li>Last Errors</li>
				<li>Stats</li>
				<li>Current Stats</li>
				<li>Edit Manager</li>
				<li>Edit Manager List</li>
				<li>Legacy Queue</li>
				<li>Jobs</li>
				<li>Export</li>
				<li>Logs</li>
				<li>Archive Routes</li>
			</ul>
			<div id="rowdetails_routes" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_posts" class="rowdetails_tab" style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_last_errors" class="rowdetails_tab" style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_stats" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_current_stats" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_manager" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_manager_list" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_legacy_queue" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_jobs" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_export" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_logs" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
			<div id="rowdetails_archive_routes" class="rowdetails_tab"
			     style="display: block;width: 100%;height: 600px;overflow: scroll; ">
				<img src='/js/jqwidgets/styles/images/loader.gif'/>
			</div>
		</div>
	</div>
</div>


