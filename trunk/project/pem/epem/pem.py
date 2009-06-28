#!/usr/bin/env python
"""
    PEM - daemon control 
    
    @author: Jean-Lou Dupont
"""
import os
import sys
import subprocess
from optparse import OptionParser

usage = \
"""
        pem [options] Cmd

         Cmd="start"  : start daemon
         Cmd="stop"   : stop daemon
"""

erlcmd = "erl -noshell -pa ./ebin -s pem_admin start %s"


    
class Command(object):
    """
    """
    def erl(self, cmd):
        """Executes the Erlang administration program for PEM
        """
        proc = subprocess.Popen(erlcmd % cmd, shell=True)
        return proc.wait()
    
    def cmd_start(self):
        """
        """
        print "start!"
        return 0
        
    def cmd_stop(self):
        """
        """
        print "stop!"
        return 0
    

def main():
    """
    """
    parser=OptionParser(usage)

    parser.add_option("-v", "--verbose", action="store_true",  dest="verbose")
    parser.add_option("-q", "--quiet",   action="store_false", dest="verbose")

    (options, args) = parser.parse_args()
    
    if len(args) != 1:
        parser.error("incorrect number of arguments")
    
    fun = "cmd_%s" % args[0]
    cmd = Command()
    
    try:
        ret = getattr(cmd, fun)()
    except:
        parser.error("invalid command")
        sys.exit(1)
        
    sys.exit(ret)
    

if __name__=="__main__":
    main()
