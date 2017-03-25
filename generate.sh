#!/bin/bash
if [ $# -ne 2 ]; then
  echo "How to use it"
  echo "./generate.sh sample.er sample.png"

fi
process=`docker ps -a |grep erd_container|wc -l`
if [ $process -ne 0 ]; then
  docker rm -f erd_container > /dev/null
fi

docker run -it -d --name erd_container erd /bin/bash &> /dev/null &&
docker cp $1 erd_container:/erd/$1 &&
docker exec -it erd_container bash -c "cd /erd && stack exec erd -- -i $1 -o $2" &&
docker cp erd_container:/erd/$2 $2 &&
docker rm -f erd_container &> /dev/null
