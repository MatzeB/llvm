#!/usr/bin/env python
#
# Given a previous good compile narrow down miscompiles.
# Expects two directories named "good" and "bad" each containing a set of
# assembly or object files where the "bad" version is assumed to be broken.
# You also have to provide a script called "link_test". It is called with a list
# of files which should be linked together and result tested. "link_test" should
# returns with exitcode 0 if the linking and testing succeeded.
#
# abtest.py operates by taking all files from the "good" directory and
# in each step replacing one of them with a file from the "bad" directory.
#
# Additionally you can perform the same steps with a single .s file. In this
# mode functions are identified by "# -- Begin FunctionName" and
# "# -- End FunctionName" markers. The abtest.py then takes all functions from
# the file in the "good" directory and replaces one function with the
# corresponding function from the "bad" file in each step.
#
# Example usage to identify miscompiled files:
#    1. Create a link_test script, make it executable. Simple Example:
#          clang "$@" -o /tmp/test && /tmp/test || echo "PROBLEM"
#    2. Run the script to figure out which files are miscompiled:
#       > ./abtest.py 
#       somefile.s: ok
#       someotherfile.s: skipped: same content
#       anotherfile.s: failed: './link_test' exitcode != 0
#       ...
# Example usage to identify miscompiled functions inside a file:
#    3. First you have to mark begin and end of the functions.
#       The script comes with some examples called mark_xxx.py.
#       Unfortunately this is very specific to your environment and it is likely
#       that you have to write a custom version for your environment.
#       > for i in good/*.s bad/*.s; do mark_xxx.py $i; done
#    4. Run the tests on a single (assuming good/file.s and bad/file.s exist)
#       > ./abtest.py file.s
#       funcname1 [0/XX]: ok
#       funcname2 [1/XX]: ok
#       funcname3 [2/XX]: skipped: same content
#       funcname4 [3/XX]: failed: './link_test' exitcode != 0
#       ...
from fnmatch import filter
from sys import stderr
import argparse
import filecmp
import os
import subprocess
import sys

LINKTEST="./link_test"
ESCAPE="\033[%sm"
BOLD=ESCAPE % "1"
RED=ESCAPE % "31"
NORMAL=ESCAPE % "0"
FAILED=RED+"failed"+NORMAL

def error(message):
    stderr.write("Error: %s\n" % (message,))

def warn(message):
    stderr.write("Warning: %s\n" % (message,))

def announce_test(name):
    stderr.write("%s%s%s: " % (BOLD, name, NORMAL))
    stderr.flush()

def announce_result(result, info = ""):
    stderr.write(result)
    if info != "":
        stderr.write(": %s" % info)
    stderr.write("\n")
    stderr.flush()

def find(dir, file_filter=None):
    files = [walkdir[0]+"/"+file for walkdir in os.walk(dir) for file in walkdir[2]]
    if file_filter != None:
        files = filter(files, file_filter)
    return files

def make_filelist(dir_a, dir_b, file_filter = "*"):
    files_a = find(dir_a, file_filter)
    files_b = find(dir_b, file_filter)
    # Create list of files common to both directories
    no_prefix_a = set([x[len(dir_a)+1:] for x in files_a])
    no_prefix_b = set([x[len(dir_b)+1:] for x in files_b])
    for x in (no_prefix_a - no_prefix_b):
        warn("There is '%s/%s' but no '%s/%s'" %
             (dir_a, x, dir_b, x))
    for x in (no_prefix_b - no_prefix_a):
        warn("There is '%s/%s' but no '%s/%s'" %
             (dir_b, x, dir_a, x))
    files = no_prefix_a & no_prefix_b
    # Create list of tuples with a/b filenames
    filetuples = [(x, dir_a+"/"+x, dir_b+"/"+x) for x in files]
    filetuples.sort()
    return filetuples

def prune_equal_files(filetuples):
    result = []
    for filetup in filetuples:
        name, file_a, file_b = filetup
        if filecmp.cmp(file_a, file_b):
            announce_test(name)
            announce_result("skipped", "same content")
            continue
        result.append(filetup)
    return result

def extract_functions(file):
    functions = []
    in_function = None
    for line in open(file):
        if line.startswith("# -- Begin  "):
            if in_function != None:
                warn("Missing end of function %s" % (in_function,))
            funcname = line[12:-1]
            in_function = funcname
            text = line
        elif line.startswith("# -- End  "):
            function_name = line[10:-1]
            if in_function != function_name:
                warn("End %s does not match begin %s" % (function_name, in_function))
            else:
                text += line
                functions.append( (in_function, text) )
            in_function = None
        elif in_function != None:
            text += line
    return functions

def replace_function(file, function, replacement, dest):
    out = open(dest, "w")
    skip = False
    found = False
    in_function = None
    for line in open(file):
        if line.startswith("# -- Begin  "):
            if in_function != None:
                warn("Missing end of function %s" % (in_function,))
            funcname = line[12:-1]
            in_function = funcname
            if in_function == function:
                out.write(replacement)
                skip = True
        elif line.startswith("# -- End  "):
            function_name = line[10:-1]
            if in_function != function_name:
                warn("End %s does not match begin %s" % (function_name, in_function))
            in_function = None
            if skip:
                skip = False
                continue
        if not skip:
            out.write(line)

def testrun(files):
    linkline="%s %s" % (LINKTEST, " ".join(files),)
    res = subprocess.call(linkline, shell=True)
    if res != 0:
        announce_result(FAILED, "'%s' exitcode != 0" % LINKTEST)
        return False
    else:
        announce_result("ok", "")
        return True

def take_a(filetuples):
    return [file_a for name, file_a, file_b in filetuples]

def take_b(filetuples):
    return [file_b for name, file_a, file_b in filetuples]

def take_name(filetuples):
    return [name for name, file_a, file_b in filetuples]

def bisect_files(filetuples, nobisect = False):
    known_good = []
    known_bad = []
    partitions_to_test = [ filetuples ]
    # Bisection degrades to a linear test if each file is in its own partition.
    if nobisect:
        partitions_to_test = [ [filetup] for filetup in filetuples ]

    # Partitioning loop.
    while len(partitions_to_test) > 0:
        partition = partitions_to_test.pop()
        if len(partition) == 0:
            continue

        if partition is filetuples:
            # We already sanity checked that all "b" files combines is bad.
            res = False
        else:
            testfiles = take_b(partition)
            testfiles += take_a(known_good + known_bad)
            for rest in partitions_to_test:
                testfiles += take_a(rest)
            testfiles.sort()
            ratio = float(len(known_good)+len(known_bad)) / len(filetuples)
            progress = "[%2.0f%%]" % (100. * ratio,)
            if len(partition) <= 2:
                what = " ".join(take_name(partition))
            else:
                what = "%2s files" % (len(partition),)
            announce_test(progress + " Testing " + what)
            res = testrun(testfiles)
        if res:
            known_good += partition
            announce_result("=> Good: " + " ".join(take_name(partition)))
        elif len(partition) == 1:
            known_bad += partition
            announce_result("=> Bad: " + " ".join(take_name(partition)))
        else:
            # Split partition into two
            half_index = len(partition)/2
            half0 = partition[:half_index]
            half1 = partition[half_index:]
            partitions_to_test += [half0, half1]
    assert set(known_good + known_bad) == set(filetuples)

    # Print a summary.
    sys.stderr.write("\n\n=== Result ===\n")
    for file_b in take_b(known_bad):
        announce_test(file_b)
        announce_result("bad")

def check_functions_in_file(base, goodfile, badfile):
    functions = extract_functions(goodfile)
    if len(functions) == 0:
        warn("Couldn't find any function in %s, missing annotations?" % (goodfile,))
        return
    badfunctions = dict(extract_functions(badfile))
    if len(functions) == 0:
        warn("Couldn't find any function in %s, missing annotations?" % (badfile,))
        return

    COMBINED="/tmp/combined.s"
    i = 0
    for (func,func_text) in functions:
        announce_test(func + " [%s/%s]" % (i+1, len(functions)))
        i+=1
        if func not in badfunctions:
            warn("Function '%s' missing from bad file" % func)
            continue
        if badfunctions[func] == func_text:
            announce_result("skipped", "same content")
            continue
        replace_function(goodfile, func, badfunctions[func], COMBINED)
        testfiles=[]
        for c in NO_PREFIX:
            if c == base:
                testfiles.append(COMBINED)
                continue
            testfiles.append(gooddir + "/" + c)

        testrun(testfiles)

parser = argparse.ArgumentParser()
parser.add_argument('--a', dest='dir_a', default='good')
parser.add_argument('--b', dest='dir_b', default='bad')
parser.add_argument('--no-bisect', action='store_true')
parser.add_argument('--insane', help='Skip sanity check', action='store_true')
parser.add_argument('file', metavar='file', nargs='?')
config = parser.parse_args()

dir_a = config.dir_a
dir_b = config.dir_b

filetuples = make_filelist(dir_a, dir_b)
if len(filetuples) == 0:
    error("No common files found in '%s' and '%s'" % (dir_a, dir_b))
    exit(1)

# "Checking whether build environment is sane ..."
if not config.insane:
    if not os.access(LINKTEST, os.X_OK):
        error("Expect '%s' to be present and executable" % (LINKTEST,))
        exit(1)

    # Testing all "A" files should be fine
    announce_test("sanity check '%s'" % config.dir_a)
    files_a = take_a(filetuples)
    res_a = testrun(files_a)
    if not res_a:
        # "build environment is grinning and holding a spatula. Guess not."
        linkline="%s %s" % (LINKTEST, " ".join(files_a),)
        stderr.write("\n%s\n\n" % linkline)
        stderr.write("Returned with exitcode != 0\n")
        error("Sanity test failed!");
        sys.exit(1)

    # Testing all "B" files should abort
    announce_test("sanity check '%s'" % config.dir_b)
    files_b = take_b(filetuples)
    res_b = testrun(files_b)
    if res_b:
        # "build environment is grinning and holding a spatula. Guess not."
        linkline="%s %s" % (LINKTEST, " ".join(files_b),)
        stderr.write("\n%s\n\n" % linkline)
        stderr.write("Returned with exitcode == 0\n")
        error("Sanity test failed!");
        sys.exit(1)

if config.file is not None:
    # File exchange mode
    goodfile = gooddir+"/"+config.file
    badfile = baddir+"/"+config.file
    check_functions_in_file(config.file, goodfile, badfile)
else:
    # Function exchange mode
    filetuples = prune_equal_files(filetuples)
    bisect_files(filetuples, config.no_bisect)
