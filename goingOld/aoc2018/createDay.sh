#!/bin/bash

dayN=$1
if [ -z "$dayN" ] ; then
    echo "❌ Must specify day number" >&2
    exit 1
fi

set -e
set -x

template_dir=$(dirname $0)/template
day_dir=$(dirname $0)/day${dayN}

if [ -e $day_dir ] ; then
    echo "❌ ${day_dir} already exists" >&2
    exit 1
fi

cp -r $template_dir $day_dir

sed -i "s#template#day${dayN}#g" $day_dir/solution.go

echo "✅ Done!"
