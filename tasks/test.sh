#!/usr/bin/env bash

if [ $# -ne 2 ]; then
  echo "Usage: $0 inputfile pkgdb-directory"
  exit 1
fi

if [[ ! -f $1 || ! -d $2 ]]; then
  echo "file or pkgdb directory does not exist!"
  exit 1
fi

base_name=$(basename "$1" | sed 's/\(.*\)\..*/\1/')
pkg_path=$PWD/$2/pkgdb
expect_script="$PWD/runGhci.expect"
outputfile1="${base_name}/Global.hs"
outputfile2="${base_name}/TaskData.hs"
outputfile3="${base_name}/Description.hs"
outputfile4="${base_name}/Parse.hs"
current_output="$outputfile1"
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}Writing .hs files...${NC}"

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

echo -e "${CYAN}Interpreting the code files...${NC}"

export GHC_PACKAGE_PATH=$pkg_path
ghc_file=$(find $pkg_path -name "ghci*" -print -quit)
temp="${ghc_file##*/ghci-}"
ghc_version="${temp%-*.conf.copy}"

cd $base_name
expect $expect_script $ghc_version |
  sed '/GHCi, version/d;/ghci> /d;/Ok, [four,two]\+ modules loaded./d' |
    ansi2html > ghc.html

echo -e "${CYAN}writing Hlint report...${NC}"
hlint . --report -q
echo -e "${CYAN}Done! Check the reports in ghc.html and report.html.${NC}"


