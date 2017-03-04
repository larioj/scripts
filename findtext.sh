#!/bin/sh

grep -rnw "$1" -e "$2"
