#!/usr/local/bin/python3
import re, os
def get_authinfo_password(machine, login, port):
    s = "machine %s login %s port %s password ([^ \n]*)" % (machine, login, port)
    p = re.compile(s)
    # authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    authinfo = open(".authinfo", "r").read()
    r = p.search(authinfo).group(1)
    return r
