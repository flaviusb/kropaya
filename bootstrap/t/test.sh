#!/bin/bash

# A super simple way to generate TAP
# Source this at the top of your bash .t files, use the funtions, then run with prove or etc
# In each tests file, start with `plan $number_of_tests`
# The for each test, start with `name  $name of the test`
# Finish the test with `expect_eq "$actual_result" "$expected_result"`
# This file is licensed under the GPLv3, the details of which are in the LICENSE file at the root of the repository

function plan() {
  echo "1..$1"
}

let test_num=0
description=""
pragma=""
result=""

function name() {
  description=$1
  pragma=""
}

function test_text() {
  test_num=$((test_num+1))
  result="$test_num $description $pragma"
}

function ok() {
  test_text
  echo "ok $result"
}

function not_ok() {
  test_text
  echo "not ok $result"
}

function to_todo() {
  pragma="# TODO $1"
}

function to_skip() {
  pragma="# SKIP $1"
}

function expect_eq() {
  if [ "$1" = "$2" ]; then
    ok
  else
    not_ok
  fi
}
