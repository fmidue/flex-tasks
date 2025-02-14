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
mapfile -t files \
  < <(awk '/^\s*module/ && !/^\s*module +Check +/ {sub(/module /,"")sub(/ (where|\().*/,".hs"); print}' "$1")
files=("${files[@]/#/$base_name/}")
CYAN='\033[0;36m'
NC='\033[0m'
current_file=0

echo -e "${CYAN}Writing .hs files...${NC}"

mkdir -p "${base_name}"
true >"${files[0]}"

while IFS= read -r line || [ -n "$line" ]; do
  # Check for module separator
  if [[ "$line" =~ === ]]; then
    ((current_file++))
    true >"${files[$current_file]}"
    continue
  fi
  echo "${line//$'\r'/}" >>"${files[$current_file]}"
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
