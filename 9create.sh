#!/bin/bash

echo -n "1,$" | 9p write acme/$winid/addr
9p read acme/$winid/body | create
