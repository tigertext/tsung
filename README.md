# Tsung README

##  Introduction

This document gives pointers for information on this package which is
distributed under the GNU General Public License version 2 (see file
COPYING).

##  What This Package Is

Tsung is multi-protocol distributed load testing tool.

It can be used to test the scalability and performances of IP based
client/server applications (supported protocols: HTTP, WebDAV, SOAP,
PostgreSQL, MySQL, LDAP, MQTT, AMQP and Jabber/XMPP)

A User's manual is available :
          http://tsung.erlang-projects.org/user_manual/

##  Problems/Bugs

Join the mailing-list:
  https://lists.process-one.net/mailman/listinfo/tsung-users

or use the tracker https://github.com/processone/tsung/issues

## Install
### Config with prefix
In the tsung source directory, use mytsung as prefix folder, this will make a local installation of tsung 
instead of a global copy.
    ./configure --prefix=mytsung
### Compile (make clean to clean up)
    make
### Install
    make install
## Run test
    mytsung/bin/tsung -f test_config.xml start
## Account credentials and tokens
    There are two files test configure xml needs:
    /tmp/perf_test_accounts.csv - login username/password
    /tmp/perf_test_tokens.csv - list of account token used when posting messages

## Generate reports
After test run, go the the log folder, then run 
    my_tsung/lib/tsung/bin/tsung_stats.pl
open report.html to view the results.

