#!/usr/bin/env bash
set -euo pipefail
testname="${1:-}"
[ -n "$testname" ] || { echo "Usage: $0 <A|B|C|D|UNIT>"; exit 1; }

mkdir -p io data
: > io/InCollege-Output.txt

case "$testname" in
  A)
    cat > data/users.txt << 'IN'
alice|Abcdef1!
IN
    cat > io/InCollege-Input.txt << 'IN'
1
wronguser
wrongpass
IN
    ;;
  B)
    cat > data/users.txt << 'IN'
alice|Abcdef1!
IN
    cat > io/InCollege-Input.txt << 'IN'
1
alice
Abcdef1!
IN
    ;;
  C)
    : > data/jobs.txt
    : > data/applications.txt
    cat > data/users.txt << 'IN'
alice|Abcdef1!
IN
    cat > io/InCollege-Input.txt << 'IN'
1
alice
Abcdef1!
1
1
Software Intern
Build tools for a COBOL project.
Acme Corp
Tampa, FL
NONE
3
IN
    ;;
  D)
    : > data/applications.txt
    # keep jobs from C or create a default job if empty
    if ! [ -s data/jobs.txt ]; then
      echo "1|alice|Software Intern|Build tools for a COBOL project.|Acme Corp|Tampa, FL|NONE" > data/jobs.txt
    fi
    cat > data/users.txt << 'IN'
alice|Abcdef1!
IN
    cat > io/InCollege-Input.txt << 'IN'
1
alice
Abcdef1!
1
2
1
1
2
0
3
IN
    ;;
  UNIT)
    cat > io/InCollege-Input.txt << 'IN'
TEST-JOBS
IN
    ;;
  *) echo "Unknown test"; exit 1;;
esac

./InCollege
echo "----- OUTPUT -----"
sed -n '1,999p' io/InCollege-Output.txt
