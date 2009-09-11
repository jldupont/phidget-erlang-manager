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
daemon="erl +d -pa ebin -sname epem -detached -run epem_app start"

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
    
    @returns (exit_code, "ok"|"error", Result)
    """
    [resp_tuple, status]= exec_cmd(bridge, cmds)
    (resp, rest)=resp_tuple
    
    if status==127:
        return (status, "error", "Erlang 'erl' command not found.")
    
    #print "resp: %s\n" % str(resp)
    
    try:
        rj=json.loads(resp)
        if status==0:
            reply=(0, "ok", rj)
        else:
            reply=(status, "error", rj)
    except Exception,e:
        print "! Exception <%s>\n" % str(e)
        reply=(1, "error", "error: maybe the installation is broken?")
        
    return reply

def extract_integer(obj):
    """
    Extracts an integer from a response
    """
    try:
        [CmdPart, ResponsePart]=obj 
        [ResponseFlag, Integer]=ResponsePart
    except:
        Integer=None
    
    return Integer

def get_daemon_pid():
    """
    Attempts to retrieve the daemon's pid
    """
    response=do_cmd("status")
    return extract_integer(response)

def do_start(quiet):
    """
    Attempts to start the daemon
    """
    Pid=get_daemon_pid()
    #print "pid: %s\n" % str(Pid)
    if Pid is None:
        proc=subprocess.Popen(daemon, shell=True)
        proc.wait()
        ret=proc.returncode
        print "start: ret: %s\n" % str(ret)
        exit_code = 0
    else:
        cond_msg(quiet, "daemon already running")
        exit_code = 1

    return exit_code


def cond_msg(quiet, msg):
    if not quiet:
        print msg
        


def main():
    """ Main entry point
    """
    parser=OptionParser(usage)

    parser.add_option("-q", "--quiet",  action="store_true",  dest="quiet", help="disable output to stdout")
    
    (options, args) = parser.parse_args()
    
    if len(args) < 1:
        parser.error("! incorrect number of arguments")
      
    cmd=args[0]
    cmds=""
    for cmd in args:
        cmds=cmds + " " + cmd  
    
    quiet=options.quiet
    
       
    if cmd=="start":
        exit_code=do_start(quiet)
    else:
        (exit_code, state, message)=do_cmd(cmds)
        if not options.quiet:
            print "> %s\n" % str( message )
            
    sys.exit(exit_code)
    
# execute
main()

