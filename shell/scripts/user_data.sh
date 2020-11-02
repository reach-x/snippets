#!/bin/bash
cd /home
for directory in *
do
  cd /home/$directory
  git reset --hard
  git pull
done
