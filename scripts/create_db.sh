#!/bin/bash

set -e

DB_NAME=endpointer
TABLE_NAME=endpoints
CLI=${CLI:-"docker run --rm -i --link pg:postgres postgres:11 psql"}
CLI_OPTS=${CLI_OPTS:-"-h postgres -U postgres "}


# run "${cmd}" addl-opts input_file
run() {
    echo ------------------
    echo running:
    echo ${CLI} ${CLI_OPTS} $2 -c "$1"
    echo ------------------
    ${CLI} ${CLI_OPTS} $2 -c "$1"
}

# create db addtl_opts
create_db() {
    local cmd="CREATE DATABASE $DB_NAME;"
    run "${cmd}"
}

# drop db addtl_opts
drop_db() {
    local cmd="DROP DATABASE IF EXISTS $DB_NAME;"
    run "${cmd}"
}

# create table addtl_opts
create_table() {
    local cmd="$(cat <<EOF
CREATE TABLE IF NOT EXISTS $TABLE_NAME (
       relative_url varchar(150) NOT NULL,
       proto varchar(20) NOT NULL,
       CONSTRAINT absolute_url PRIMARY KEY(relative_url,proto)
)
EOF
)"
    run "${cmd}" "-d $DB_NAME"
}

# drop table addtl_opts
drop_table() {
    local cmd="DROP TABLE IF EXISTS $TABLE_NAME;"
    run "${cmd}" "-d $DB_NAME"
}

# seed rows is a snowflake due to trying to mess with stdin
seed_rows() {
    cat endpoints.csv | ${CLI} ${CLI_OPTS} -d $DB_NAME -c "COPY $TABLE_NAME (relative_url, proto) FROM STDIN WITH DELIMITER ',';"
}


main () {
    drop_db
    create_db
    create_table
    seed_rows
    drop_table
}

main
