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

###< CUSTOMIZE BELOW
bridge="erl +d -pa ebin -sname epem_py -s epem_bridge cmd %s"

usage = """epem [-q] command"""
###>



def exec_cmd(cmd, args):
    try:
        proc = subprocess.Popen(cmd % args, shell=True, stdout=subprocess.PIPE)
        proc.wait()
        status=proc.returncode
        out=proc.communicate()
                
    except Exception,e:
        print "Exception: "+str(e)
        status = 127 # command not found
        out = "'erl' command not found... Erlang is required."
        
    return [out, status]

def do_cmd(cmds):
    """
    Executes a command and returns the result as a Python object
    """
    [resp_tuple, status]= exec_cmd(bridge, cmds)
    (resp, rest)=resp_tuple
    
    if status==127:
        return (127, "error", "Erlang 'erl' command not found.")
    
    try:
        rj=json.loads(resp)
        if status==0:
            reply=(0, "ok", rj)
        else:
            reply=(1, "error", rj)
    except:
        rj=None
        reply=(1, "error", "error: maybe the installation is broken?")
        
    return reply


def main():
    """ Main entry point
    """
    parser=OptionParser(usage)

    parser.add_option("-q", "--quiet",  action="store_true",  dest="quiet", help="disable output to stdout")
    
    (options, args) = parser.parse_args()
    
    if len(args) < 1:
        parser.error("! incorrect number of arguments")
      
    cmds=""
    for cmd in args:
        cmds=cmds + " " + cmd  
        
    (exit_code, state, message)=do_cmd(cmds)
    if not options.quiet:
        print "> %s\n" % str( message )
    sys.exit(exit_code)
    
# execute
main()

