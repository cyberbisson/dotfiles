#!/bin/sh -x
ssh -l arcstream -L '1522:10.10.134.146:1521' 69.84.134.146
