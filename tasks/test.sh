#!/usr/bin/env bash

if [ $# -ne 2 ]; then
  echo "Usage: $0 input_file pkgdb_directory"
  exit 1
fi

if [[ ! -f $1 || ! -d $2 ]]; then
  echo "file or pkgdb directory does not exist!"
  exit 1
fi

base_name=$(basename "$1" | sed 's/\(.*\)\..*/\1/')
pkg_path=$PWD/$2/pkgdb
script_path="$(realpath "$(dirname "$0")")"
expect_script="${script_path}/runGhci.expect"
output_file1="${base_name}/Global.hs"
output_file2="${base_name}/TaskData.hs"
output_file3="${base_name}/Description.hs"
output_file4="${base_name}/Parse.hs"
current_output="$output_file1"
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}Writing .hs files...${NC}"

mkdir -p "${base_name}"
true >"$output_file1"
rm -f "$output_file2" "$output_file3" "$output_file4"

while IFS= read -r line || [ -n "$line" ]; do
  # Check for module separator
  if [[ "$line" =~ === ]]; then
    case $current_output in
    "$output_file1")
      current_output="$output_file2"
      true >"$output_file2"
      ;;
    "$output_file2")
      current_output="$output_file3"
      true >"$output_file3"
      ;;
    "$output_file3")
      current_output="$output_file4"
      true >"$output_file4"
      ;;
    esac
    # Skip writing the line with ===
    continue
  fi
  echo "${line//$'\r'/}" >>"$current_output"
done <"$1"

echo -e "${CYAN}Interpreting the code files...${NC}"

export GHC_PACKAGE_PATH=$pkg_path
ghc_file=$(find "$pkg_path" -name "ghci*" -print -quit)
temp="${ghc_file##*/ghci-}"
ghc_version="${temp%-*.conf*}"

cd "$base_name" || exit 1
expect "$expect_script" "$ghc_version" |
  sed -e 's/.*\*\*\*/\*\*\*/g' -e '/GHCi, version/d' -e '/ghci> /d' -e '/Ok, [four,two]\+ modules loaded./d' |
  ansi2html >ghc.html

echo -e "${CYAN}writing Hlint report...${NC}"
hlint . --report="hlint.html" -q --hint="${script_path}/.hlint.yaml"
echo -e "${CYAN}Done! Check the reports in ghc.html and hlint.html.${NC}"
