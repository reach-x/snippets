#!/bin/bash

ssh -i ~/.ssh/2016-06-02.pem root@$1 cat /etc/haproxy/haproxy.cfg
