#!/bin/bash

for x in `find . | grep -v "~$" | grep "~"`
do
        #echo "mv $x ${x%~*}"
        mv $x ${x%~*} 
done
