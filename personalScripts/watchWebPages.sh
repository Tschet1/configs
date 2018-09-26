#!/bin/bash --login

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

while read url hash; do
  #echo "url=$url, hash=$hash"
  nhash=$(wget -O- $url 2> /dev/null | shasum | sed -e "s/ .*//g")
  [[ $hash == $nhash ]] ||  /usr/local/bin/terminal-notifier -title 'Website changed' -message "$url changed" -open $url
  echo "$url $nhash" >> ${DIR}/watchWebPages.conf.n

done < ${DIR}/watchWebPages.conf
mv ${DIR}/watchWebPages.conf.n ${DIR}/watchWebPages.conf
