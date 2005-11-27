#!/bin/csh -X
# Set up the variables the Oracle needs to run on my system (these should be
# machine specific).

setenv ORACLE_BASE "/usr/oracle"
setenv ORACLE_HOME "${ORACLE_BASE}/product/10g"
setenv ORACLE_SID  "temp"
