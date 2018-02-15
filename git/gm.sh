#!/bin/sh

name="$1"
if [ -z "$1" ]; then 
	name="master"
fi
git merge $name