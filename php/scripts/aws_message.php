<?php

$json=<<<EOJ
 {"Progress":50,"AccountId":"581529152831","Description":"Launching a new EC2 instance: i-0b0ab
f05f989af044","RequestId":"0bf5c194-4b04-81d0-0eb8-0a9aebfdca21","EndTime":"2020-02-21T00:08:45.038Z","AutoS
calingGroupARN":"arn:aws:autoscaling:us-east-1:581529152831:autoScalingGroup:c71970ea-8340-45db-a0a8-d80ae3b83c8e:au
toScalingGroupName/pm-prod-lbc-outbound-asg20191220160640714700000002","ActivityId":"0bf5c194-4b04-81d0-0eb8-0a9ae
bfdca21","StartTime":"2020-02-21T00:08:12.832Z","Service":"AWS Auto Scaling","Time":"2020-02-21T00:08:45.0
38Z","EC2InstanceId":"i-0b0abf05f989af044","StatusCode":"InProgress","StatusMessage":"","Details":{"Su
bnet ID":"subnet-0f08ea90a339e3719","Availability Zone":"us-east-1b"},"AutoScalingGroupName":"pm-prod-lbc-ou
tbound-asg20191220160640714700000002","Cause":"At 2020-02-21T00:07:43Z a user request update of AutoScalingGroup c
onstraints to min: 1, max: 25, desired: 10 changing the desired capacity from 2 to 10.  At 2020-02-21T00:08:10Z an ins
tance was started in response to a difference between desired and actual capacity, increasing the capacity from 2 to 1
0.","Event":"autoscaling:EC2_INSTANCE_LAUNCH"}
EOJ;

$json=<<<EOJ

{"Progress":50,"AccountId":"581529152831","Description":"Launching a new EC2 instance: i-0b0ab
f05f989af044","RequestId":"0bf5c194-4b04-81d0-0eb8-0a9aebfdca21","EndTime":"2020-02-21T00:08:45.038Z","AutoS
calingGroupARN":"arn:aws:autoscaling:us-east-1:581529152831:autoScalingGroup:c71970ea-8340-45db-a0a8-d80ae3b83c8e:au
toScalingGroupName/pm-prod-lbc-outbound-asg20191220160640714700000002","ActivityId":"0bf5c194-4b04-81d0-0eb8-0a9ae
bfdca21","StartTime":"2020-02-21T00:08:12.832Z","Service":"AWS Auto Scaling","Time":"2020-02-21T00:08:45.0
38Z","EC2InstanceId":"i-0b0abf05f989af044","StatusCode":"InProgress","StatusMessage":"","Details":{"Su
bnet ID":"subnet-0f08ea90a339e3719","Availability Zone":"us-east-1b"},"AutoScalingGroupName":"pm-prod-lbc-ou
tbound-asg20191220160640714700000002","Cause":"At 2020-02-21T00:07:43Z a user request update of AutoScalingGroup c
onstraints to min: 1, max: 25, desired: 10 changing the desired capacity from 2 to 10.  At 2020-02-21T00:08:10Z an ins
tance was started in response to a difference between desired and actual capacity, increasing the capacity from 2 to 1
0.","Event":"autoscaling:EC2_INSTANCE_LAUNCH"}
EOJ;

$json = preg_replace('/[[:cntrl:]]/', '', $json);

print_r(array(
"json"=>$json,
"json_decoded"=>json_decode($json),
"json_last_error_msg"=>json_last_error_msg(),
));


