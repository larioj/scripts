#!/bin/sh

name="$1"
if [ -z "$1" ]; then 
	name="master"
fi
git fetch upstream
git merge upstream/$name
git push -u origin $name
