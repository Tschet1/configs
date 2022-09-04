#!/bin/bash

IFS=''
branches=""
for branch in $(git branch --format='%(authoremail) %(refname)' -r --no-merged);
do
  if [[ ! $branch =~ "/release/" ]] && [[ ! $branch =~ "/prototype/" ]]; then
  branches="$branches\n$branch"
fi
done
echo -e $branches|sort
