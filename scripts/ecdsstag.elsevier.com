#!/bin/sh -x
ssh -l oracle -L '1522:10.10.140.104:1521' 69.84.140.104
