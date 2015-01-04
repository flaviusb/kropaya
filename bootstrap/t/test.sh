#!/bin/bash

# A super simple way to generate TAP
# Source this at the top of your bash .t files, use the funtions, then run with prove or etc

function plan() {
  echo "1..$1"
}

let test_num=0
description=""
pragma=""

function name() {
  description=$1
  pragma=""
}

function test_text() {
  test_num=$((test_num+1))
  return "$test_num $description $pragma"
}

function ok() {
  echo "ok ${test_text}"
}

function not_ok() {
  echo "not ok ${test_text}"
}

function to_todo() {
  pragma="# TODO $1"
}

function to_skip() {
  pragma="# SKIP $1"
}

expect_eq() {
  if [ "$1" = "$2" ]; then
    ok
  else
    not_ok
  fi
}
