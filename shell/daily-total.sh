#!/bin/sh
for file in $(ls ids-total-*.csv); do
    wc -l $file
done
