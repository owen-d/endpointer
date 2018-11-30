#!/bin/bash
set -e

docker-compose down || :
docker-compose up -d

sleep 5
./create_db.sh
