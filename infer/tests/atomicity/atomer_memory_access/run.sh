#!/usr/bin/env bash

# Author: Dominik Harmim <iharmim@fit.vut.cz>, Daniel Marek <xmarek72@vutbr.cz>

CYAN='\033[0;36m'
NC='\033[0m'

function clean {
  if [[ -f Makefile ]]; then
    make clean
  fi
  rm -rf infer-out ./*.o ./*.class ./*.ast.sh memory_accesses atomic-sets
}

function fail {
  printf "\n%b%s%b\n" "$CYAN" "$1" "$NC"
  clean
  exit 1
}

function analyse_atomic_sets {
  dir="$1"
  shift
  atomic_sets_params_str="$1"
  shift
  atomic_sets_params=("$@")

  infer analyze --atomic-sets-only "${atomic_sets_params[@]}" || \
    fail "In '$dir', 'infer analyze --atomic-sets-only\
      $atomic_sets_params_str' failed."
}

base_dir=$(pwd)

for dir in "$(dirname "$0")"/*/; do
  cd "$base_dir/$dir" || exit 1

  file=$(find . -maxdepth 1 -type f \
    \( -name '*.c' -o -name '*.cpp' -o -name '*.java' \) | head -1)
  if [[ ! "$file" && ! -f Makefile ]]; then
    continue
  fi

  printf "%b---------- Analysing '%s' ----------%b\n\n" "$CYAN" "$dir" "$NC"

  if [[ -f Makefile ]]; then
    infer capture -- make
  elif [[ "$file" == *.c || "$file" == *.cpp ]]; then
    infer capture -- gcc -c "$file"
  elif [[ "$file" == *.java ]]; then
    infer capture -- javac "$file"
  fi
  capture_rc=$?
  if [[ $capture_rc -ne 0 ]]; then
    fail "In '$dir', 'infer capture' failed."
  fi

  declare -a atomic_sets_params=()
  atomic_sets_params_str=''
  if [[ -f atomic_sets_params ]]; then
    IFS=' ' read -ra atomic_sets_params <<< "$(head -1 atomic_sets_params)"
    if [[ ${#atomic_sets_params[@]} -ne 0 ]]; then
      atomic_sets_params_str=" ${atomic_sets_params[*]}"
    fi
  fi

  analyse_atomic_sets "$dir" "$atomic_sets_params_str" "${atomic_sets_params[@]}"

  if [[ -f memory_accesses_ref ]]; then
    if [[ "$(diff -q <(sort memory_accesses) <(sort memory_accesses_ref))" ]]; then
      echo
      diff -u <(sort memory_accesses) <(sort memory_accesses_ref)
      fail "In '$dir', 'memory_accesses' and 'memory_accesses_ref' differ."
    fi
  fi

  clean
  echo
done

printf "\n%bAll tests passed!%b\n" "$CYAN" "$NC"
