#!/usr/bin/env bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 inputfile"
  exit 1
fi

if [ ! -f $1 ]; then
  echo "file does not exist"
  exit 1
fi

base_name=$(basename "$1" | sed 's/\(.*\)\..*/\1/')
outputfile1="${base_name}/Global.hs"
outputfile2="${base_name}/TaskData.hs"
outputfile3="${base_name}/Description.hs"
outputfile4="${base_name}/Parse.hs"
current_output="$outputfile1"

mkdir -p ${base_name}
> "$outputfile1"
rm -f "$outputfile2" "$outputfile3" "$outputfile4"

while IFS= read -r line || [ -n "$line" ]; do
  # Check for module separator
  if [[ "$line" =~ === ]]; then
    case $current_output in
      "$outputfile1")
        current_output="$outputfile2"
        > "$outputfile2"
        ;;
      "$outputfile2")
        current_output="$outputfile3"
        > "$outputfile3"
        ;;
      "$outputfile3")
        current_output="$outputfile4"
        > "$outputfile4"
        ;;
    esac
    # Skip writing the line with ===
    continue
  fi
  echo "${line//$'\r'/}" >> "$current_output"
done < "$1"

cd ${base_name}
hlint . --report
