#!/bin/ksh -x
# Set up the variables the Oracle needs to run on my system (these should be
# machine specific).

ORACLE_BASE="/usr/oracle"
ORACLE_HOME="${ORACLE_BASE}/product/10g"
ORACLE_SID="temp"
export ORACLE_BASE ORACLE_HOME ORACLE_SID
