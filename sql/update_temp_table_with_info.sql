
update charter inner join lbc.owner_posts_data_may_2020 using(owner_post_id) 
set first_name = content
where field_id=1 and first_name is null;

update charter inner join lbc.owner_posts_data_may_2020 using(owner_post_id) 
set last_name = content
where field_id=2 and last_name is null;

update charter inner join lbc.owner_posts_data_may_2020 using(owner_post_id) 
set email_address = content
where field_id=3 and email_address is null;

update charter inner join lbc.owner_posts_data_may_2020 using(owner_post_id) 
set generated_at = content
where field_id=13 and generated_at is null;

update charter inner join lbc.owner_posts_data_may_2020 using(owner_post_id) 
set source_domain = content
where field_id=16 and source_domain is null;

update charter inner join lbc.owner_posts_data_may_2020 using(owner_post_id) 
set ip_address = content
where field_id=12 and ip_address is null;


update LOW_PRIORITY charter inner join lbc.owner_posts_data using(owner_post_id) 
set first_name = content
where field_id=1 and first_name is null;j

update charter inner join lbc.owner_posts_data using(owner_post_id) 
set last_name = content
where field_id=2 and last_name is null;

update charter inner join lbc.owner_posts_data using(owner_post_id) 
set email_address = content
where field_id=3 and email_address is null;

update charter inner join lbc.owner_posts_data using(owner_post_id) 
set generated_at = content
where field_id=13 and generated_at is null;

update charter inner join lbc.owner_posts_data using(owner_post_id) 
set source_domain = content
where field_id=16 and source_domain is null;

update charter inner join lbc.owner_posts_data using(owner_post_id) 
set ip_address = content
where field_id=12 and ip_address is null;

