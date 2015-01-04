Tests for the Kropaya Bootstrapper
==================================

This directory contains tests for the Kropaya bootstrapper.

The file `test.sh` sets up the functions that are used in the test files. The actual test files are bash scripts which source `test.sh`, and have the extension `.t`. They output TAP when run, so you can use a TAP based testrunner (like prove) to run them and get nicely formatted results. To run the test suite, I run `prove` on the directory.
