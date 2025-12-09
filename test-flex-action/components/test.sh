#!/usr/bin/env bash

# amount of random configs to test with
config_mutations=10

if [ "$#" -ne 3 ] && [ "$#" -ne 4 ]; then
  echo "Usage: $0 input_file pkgdb_directory settings_generator [-c]"
  exit 1
fi

if [[ ! -f $1 || ! -d $2 ]]; then
  echo "file or pkgdb directory does not exist!"
  exit 1
fi

leave_check=false

if [[ -f $3 ]]; then
  mutator="$(realpath "$3")"
else
  echo "settings generator does not exist: $3"
  echo "Usage: $0 input_file pkgdb_directory settings_generator [-c]"
  exit 1
fi

if [[ "$#" -eq 4 ]]; then
  if [[ "$4" = "-c" ]]; then
    leave_check=true
  else
    echo "flag does not exist: $4"
    echo "Usage: $0 input_file pkgdb_directory settings_generator [-c]"
    exit 1
  fi
fi

base_name=$(basename "$1" | sed 's/\(.*\)\..*/\1/')
pkg_path=$PWD/$2/pkgdb
script_path="$(realpath "$(dirname "$0")")"
expect_script="${script_path}/runGhci.expect"
mapfile -t files < <(
  awk '
    /^\s*taskName:\s*/ {
      print "TaskName.txt"
      next
    }
    /^\s*module/ && !/^\s*module +Check +/ {
      mod = $0

      while (mod !~ /where/) {
        if (getline line <= 0) break   # Stop if end of file
        mod = mod " " line
      }

      sub(/module[[:space:]]+/, "", mod)
      sub(/[[:space:]]*(where|\().*/, ".hs", mod)
      print mod
    }
  ' "$1"
)
files=("${files[@]/#/$base_name/}")
CYAN='\033[0;36m'
GREEN='\033[0;32m'
LIGHT_GREEN='\033[1;32m'
RED='\033[0;31m'
NC='\033[0m'
current_file=0
hlint_hints=false
ghc_hints=false
scan_hints=false

mkdir -p "/tmp/flex-test"

echo -e "${CYAN}Testing $base_name!\n${NC}"
echo -e "${CYAN}Writing .hs files...\n${NC}"

rm -r -f "${base_name}"
mkdir -p "${base_name}"
true >"${files[0]}"

while IFS= read -r line || [ -n "$line" ]; do
  # Check for module separator
  if [[ "$line" =~ === ]]; then
    ((current_file++))
    true >"${files[$current_file]}"
    continue
  fi
  if [[ "$line" =~ "taskName:" ]]; then
    task_name=$(awk '{$1=$1;print}' <<<"${line#"taskName:"}")
    echo "$task_name" >>"${files[$current_file]}"
  else
    echo "${line//$'\r'/}" >>"${files[$current_file]}"
  fi
done <"$1"

cp "$1" "$base_name/config.txt"

export GHC_PACKAGE_PATH=$pkg_path
ghc_file=$(find "$pkg_path" -name "ghci*" -print -quit)
temp="${ghc_file##*/ghci-}"
ghc_version="${temp%-*.conf*}"

cd "$base_name" || exit 1

echo -e "${CYAN}Writing Hlint report on static files...${NC}"
hlint . --report="hlint.html" -q --hint="${script_path}/.hlint.yaml"
if grep -q "No hints" "hlint.html"; then
  rm "hlint.html"
  echo -e "${GREEN}No Suggestions!\n${NC}"
else
  hlint_hints=true
  echo -e "${RED}Suggestions available!\n${NC}"
fi
expect "$script_path/mutator.expect" "$ghc_version" "$mutator" "$config_mutations" >/dev/null
echo "Testing a total of $(grep -c ^ settings_variants.txt) config mutations."
sed -i "s/,\ /;/g" "settings_variants.txt"
for i in $(seq 1 "$(grep -c ^ settings_variants.txt)"); do
  echo "Config $i:"
  echo "testing with these settings: "
  settings="$(head -n 1 settings_variants.txt)"
  IFS=';' read -ra pairs <<<"$settings"
  mkdir -p "$i"

  for pair in "${pairs[@]}"; do
    trimmed=$(echo "$pair" | xargs)
    echo "$trimmed"
    key=$(echo "$trimmed" | cut -d'=' -f1 | xargs)
    sed -i "s|^${key}[[:space:]]*=.*|$trimmed|" "TaskSettings.hs"
  done

  sed -i '1d' settings_variants.txt
  echo -e "${CYAN}\nInterpreting the code files...${NC}"
  expect "$expect_script" "$ghc_version" |
    sed -e 's/.*\*\*\*/\*\*\*/g' -e '/GHCi, version/d' -e '/ghci> /d' -e '/modules loaded./d' |
    ansi2html >"$i/ghc.html"
  if [ "$(grep -vw "Compiling" -c "$i/ghc.html")" -eq 51 ]; then
    rm "$i/ghc.html"
    echo -e "${GREEN}No Warnings!\n${NC}"
  else
    echo -e "${RED}GHC reported warnings!\n${NC}"
    ghc_hints=true
  fi

  echo -e "${CYAN}Writing Check.hs Hlint report...${NC}"
  hlint "Check.hs" --report="hlint2.html" -q --hint="${script_path}/.hlint.yaml"
  if grep -q "No hints" "hlint2.html"; then
    rm "hlint2.html"
    echo -e "${GREEN}No Suggestions!\n${NC}"
  else
    hlint_hints=true
    mv "hlint2.html" "$i/hlint.html"
    echo -e "${RED}Suggestions available!\n${NC}"
  fi

  echo -e "${CYAN}Running Check.hs scan...${NC}"
  bash "$script_path"/scan_check.sh "$i"
  if [ "$(grep -w "class=header" -c "$i/scan_check.html")" -eq 0 ]; then
    rm "$i/scan_check.html"
    echo -e "${GREEN}No Issues!\n${NC}"
  else
    scan_hints=true
    echo -e "${RED}Issues detected!\n${NC}"
  fi
  if [ -n "$(ls -A "$i")" ] || $leave_check; then
    cp "Check.hs" "$i/Check.hs"
    echo "$settings" >"$i/settings.txt"
  fi
done

echo -e "${CYAN}Deleting intermediate files...\n${NC}"

rm -f Check.hs
rm -f settings_variants.txt
if ! $hlint_hints && ! $ghc_hints && ! $scan_hints && ! $leave_check; then
  cd ..
  rm -f -r "$base_name"
else
  files=("${files[@]#$base_name/}")
  rm -f config.txt
  for file in "${files[@]}"; do
    rm -f "$file"
  done
  find . -type d -empty -delete
fi

echo -e "${LIGHT_GREEN}Done! Check the reports in ghc.html, hlint.html and scan_check.html.\n${NC}"
