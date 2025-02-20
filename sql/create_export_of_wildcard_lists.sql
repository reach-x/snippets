insert ignore into temp.ar_export_data (owner_post_id, field_id, content)
select owner_post_id, field_id,content from lbc.owner_posts_data 
	inner join temp.ar_export using(owner_post_id)
where field_id in 
(
1, first name
2, last name
3, email
9, zip
12, IP_ADDRESS
13,GENERATED_AT
16, SOURCE_DOMAIN
)
and content <> '' and content is not null;

select * from ar_export_data where content = '' limit 1;
select * from ar_export_data where content is null limit 1;

select * 
from information_schema.processlist 
where command <> 'Sleep'
order by time desc;

update ar_export_data inner join ar_export using(owner_post_id) set ar_export_data.email_address_id = ar_export.email_address_id;
desc lbc.owner_lists;;


insert into ar_pivot (email_address_id)
select distinct email_address_id from ar_export_data ;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.zip_code = ar_export_data.content where field_id=9 ;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.ip_address = ar_export_data.content where field_id=12;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.generated_at = ar_export_data.content where field_id=13;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.source_domain = ar_export_data.content where field_id=16 ;


update ar_export inner join lbc.owner_posts using (owner_post_id) set ar_export.owner_list_id = lbc.owner_posts.owner_list_id;
update ar_export inner join lbc.owner_posts_may_2020 using (owner_post_id) set ar_export.owner_list_id = lbc.owner_posts_may_2020.wildcard_authorized;

update ar_export inner join lbc.owner_lists using (owner_list_id) set ar_export.is_wildcard_list = lbc.owner_lists.wildcard_authorized;

create table temp.ar_valids 

select email_address_id, count(*) lists
from temp.ar_export
where is_wildcard_list=1
group by 1;

use temp;

create table temp.ar_valid_export
select * from temp.ar_pivot inner join temp.ar_valids using(email_address_id)
where ip_address is not null and generated_at is not null and source_domain is not null
;
use lbc;


select * from ar_valid_export where generated_at like '%1969%' limit 10;

delete from ar_valid_export where generated_at = '1969-12-31 07:00:00';=0; is null or first_name is null or last_name is null or ip_address is null or generated_at is null or source_domain is null;




create table temp.ar_valid_export_rr
as 
select * from temp.ar_valid_export where email_address like '%twc.com' or email_address like '%roadrunner.com' or email_address like '%rr.com';

create table temp.ar_valid_export_apple
as 
select * from temp.ar_valid_export where email_address like '%icloud.com' or email_address like '%me.com' or email_address like '%mac.com';




select * from information_schema.processlist 
where command <> 'sleep'
order by time desc;	


  use temp;
  
desc update ar_export_data 
	inner join ar_export using(owner_post_id) 
set ar_export_data.email_address_id = ar_export.email_address_id;
select * from lbc.fields;

 insert ignore into temp.ar_export_data (owner_post_id, field_id, content)
select owner_post_id, field_id,content from lbc.owner_posts_data 
	inner join temp.ar_export using(owner_post_id)
where field_id in 
(
1, first name
2, last name
3, email
9, zip
12, IP_ADDRESS
13,GENERATED_AT
16, SOURCE_DOMAIN
)
and content <> '' and content is not null;

select * from ar_export_data where content = '' limit 1;
select * from ar_export_data where content is null limit 1;

select * 
from information_schema.processlist 
where command <> 'Sleep'
order by time desc;

update ar_export_data inner join ar_export using(owner_post_id) set ar_export_data.email_address_id = ar_export.email_address_id;
desc lbc.owner_lists;;


insert into ar_pivot (email_address_id)
select distinct email_address_id from ar_export_data ;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.zip_code = ar_export_data.content where field_id=9 ;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.ip_address = ar_export_data.content where field_id=12;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.generated_at = ar_export_data.content where field_id=13;

update ar_pivot inner join ar_export_data using(email_address_id) set ar_pivot.source_domain = ar_export_data.content where field_id=16 ;


update ar_export inner join lbc.owner_posts using (owner_post_id) set ar_export.owner_list_id = lbc.owner_posts.owner_list_id;
update ar_export inner join lbc.owner_posts_may_2020 using (owner_post_id) set ar_export.owner_list_id = lbc.owner_posts_may_2020.wildcard_authorized;

update ar_export inner join lbc.owner_lists using (owner_list_id) set ar_export.is_wildcard_list = lbc.owner_lists.wildcard_authorized;

select is_wildcard_list, count(*)
from temp.ar_export
group by 1;

