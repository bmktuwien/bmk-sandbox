#!/bin/bash

DB_FILE="keystore.db"

CREATE_KEYSTORE_TABLE_QUERY="CREATE TABLE keystore(                        \
                               kpid      INTEGER PRIMARY KEY ASC NOT NULL, \
                               timeout   INTEGER NOT NULL,                 \
                               server_public_key  BLOB NOT NULL,           \
                               server_private_key BLOB NOT NULL,           \
                               client_public_key  BLOB NOT NULL);"

rm -f ${DB_FILE}

sqlite3 ${DB_FILE} "${CREATE_KEYSTORE_TABLE_QUERY}"
