#!/bin/sh
git log --pretty=format:"%ad %s" f7232cf30110f52c39598c5b1fd4c9efe37364fc..HEAD  > log.txt
tac log.txt > log_reverse.txt
rm log.txt
