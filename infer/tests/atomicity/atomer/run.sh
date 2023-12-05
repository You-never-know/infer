#!/usr/bin/env bash

# Author: Dominik Harmim <iharmim@fit.vut.cz>

CYAN='\033[0;36m'
NC='\033[0m'

function clean
{
  rm -rf infer-out ./*.o ./*.class ./*.ast.sh atomic-sets
}

function fail
{
  printf "\n%b%s%b\n" "$CYAN" "$1" "$NC"
  clean
  exit 1
}

function jsort
{
  jq -S --argfile x "$1" -n '$x | (.. | arrays) |= sort'
}

function analyse_atomic_sets
{
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
    fail "In '$dir', 'infer caputre' failed."
  fi

  declare -a atomic_sets_params=()
  atomic_sets_params_str=''
  if [[ -f atomic-sets-params ]]; then
    IFS=' ' read -ra atomic_sets_params <<< "$(head -1 atomic-sets-params)"
    if [[ ${#atomic_sets_params[@]} -ne 0 ]]; then
      atomic_sets_params_str=" ${atomic_sets_params[*]}"
    fi
  fi

  atomic_sets=false
  if [[ -f atomic-sets-exp ]]; then
    analyse_atomic_sets "$dir" "$atomic_sets_params_str" \
      "${atomic_sets_params[@]}"
    atomic_sets=true

    if [[ "$(diff -q <(sort atomic-sets) <(sort atomic-sets-exp))" ]]; then
      echo
      diff -u <(sort atomic-sets) <(sort atomic-sets-exp)
      fail "In '$dir', 'atomic-sets' and 'atomic-sets-exp' differ."
    fi
  fi

  if [[ -f report.json ]]; then
    if [[ $atomic_sets = false ]]; then
      analyse_atomic_sets "$dir" "$atomic_sets_params_str" \
        "${atomic_sets_params[@]}"
      atomic_sets=true
    fi

    declare -a atomicity_violations_params=()
    atomicity_violations_params_str=''
    if [[ -f atomicity-violations-params ]]; then
      IFS=' ' read -ra atomicity_violations_params <<< \
        "$(head -1 atomicity-violations-params)"
      if [[ ${#atomicity_violations_params[@]} -ne 0 ]]; then
        atomicity_violations_params_str=" ${atomicity_violations_params[*]}"
      fi
    fi

    infer analyze --atomicity-violations-only \
      "${atomicity_violations_params[@]}" || \
      fail "In '$dir', 'infer analyze --atomicity-violations-only\
        $atomicity_violations_params_str' failed."

    if [[ \
      "$(diff -q <(jsort infer-out/report.json) <(jsort report.json))" \
    ]]; then
      echo
      diff -u <(jsort infer-out/report.json) <(jsort report.json)
      fail "In '$dir', 'infer-out/report.json' and 'report.json' differ."
    fi
  fi

  clean
  echo
done

printf "\n%bAll tests passed!%b\n" "$CYAN" "$NC"
