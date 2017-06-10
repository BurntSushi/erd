#!/bin/bash
if [ $# -ne 2 ]; then
  echo "How to use it"
  echo "./generate.sh /path/to/service/sample.er /path/to/service/sample.png"
  exit
fi

process=`docker ps -a |grep erd_container|wc -l`
if [ $process -ne 0 ]; then
  docker rm -f erd_container > /dev/null
fi

erfile=$1
imgfile=$2
dir=`dirname $1`

if [ $dir = "." ]; then
  echo "Please create service directory and put erfile in it"
  exit
fi

docker run -it -d --name erd_container erd /bin/bash &> /dev/null &&
docker exec -it erd_container bash -c "mkdir -p /erd/$dir" &&
docker cp $erfile erd_container:/erd/$erfile &&
echo "$imgfile will be generated with $erfile" &&
docker exec -it erd_container bash -c "cd /erd && stack exec erd -- -i $erfile -o $imgfile" &&
docker cp erd_container:/erd/$imgfile $imgfile &&
echo  "$imgfile is generated" &&
docker rm -f erd_container &> /dev/null
