#!/usr/bin/python
"""
    epem daemon management
    
    @author: Jean-Lou Dupont
"""
import json
import os
import sys
import subprocess
from optparse import OptionParser

bridge="erl +d -pa ebin -sname epem_py -s epem_bridge cmd %s"

usage = """epem [-v] [-q] command
"""

def exec_cmd(cmd, args):
    try:
        proc = subprocess.Popen(cmd % args, shell=True, stdout=subprocess.PIPE)
        proc.wait()
        status=proc.returncode
        out=proc.communicate()
                
    except Exception,e:
        print "Exception: "+str(e)
        status = 1 # general error
        out = "bridge command not found"
        
    return [out, status]



def main():
    """ Main entry point
    """
    parser=OptionParser(usage)

    parser.add_option("-v", "--verbose",  action="store_true",  dest="verbose")
    parser.add_option("-q", "--quiet",    action="store_false", dest="verbose")
    
    #database related
    parser.add_option("-d", "--dsn",      action="store",       dest="dsn")

    (options, args) = parser.parse_args()
    
    if len(args) < 1:
        parser.error("incorrect number of arguments")
      
    cmds=""
    for cmd in args:
        cmds=cmds + " " + cmd  
        
    try:
        [resp_tuple, status]= exec_cmd(bridge, cmds)
        (resp, rest)=resp_tuple
        print "Response <%s> Status<%s>\n" % (resp, status)
        rj=json.loads(resp)
        print "json: "+str(rj)
                
        
    except Exception, e:
        print "Exception <%s>\n" % str(e)




def erlang_to_python(str):
    """
    Parses a string representation of an Erlang term to a Python object
    """
    
    
    
# execute
main()

