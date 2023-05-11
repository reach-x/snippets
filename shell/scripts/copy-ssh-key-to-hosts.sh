#!/bin/bash
set -x # enable bash debug mode

if [ -s aarons-public-key.pub]; then 
	ip_addresses = `/usr/local/bin/aws ec2 describe-instances --query "Reservations[*].Instances[*].{PublicIP:PublicIpAddress,PrivateIP:PrivateIpAddress,Name:Tags[?Key=='Name']|[0].Value,Type:InstanceType,Status:State.Name,VpcId:VpcId}" --filters "Name=instance-state-name,Values=running" --output table | grep pm-prod-isticle | awk '{ print $4 }'`

    for ip in $ip_addresses
    do # for each line from the file
        # add EOL to the end of the file (i.e., after the last line)
        # and echo it into ssh, where it is added to the authorized_keys
        # sed -e '$s/$/\n/' -s vassal-public-key.pub | ssh "$ip" 'cat >> ~/.ssh/authorized_keys'
	echo $ip
    done
else
    echo "Put new vassal public key into ./vassal-public-key.pub to add it to tc-agents-list.txt hosts"
fi
