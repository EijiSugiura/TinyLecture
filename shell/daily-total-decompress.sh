#!/bin/sh
for file in $(ls ids-total-*.csv.xz); do
    echo -n $file,
    xz -dc $file | wc -l
done
