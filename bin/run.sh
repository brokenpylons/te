
declare -a dirs=(
  json
  json_scannerless
  pascal
  pascal_scannerless
  c
  c_scannerless
  fortran
  fortran_scannerless
  cobol
  cobol_scannerless
)

for dir in "${dirs[@]}"
do
  pushd "$dir"
  dune exec "./te_$dir.exe" | tee results.txt
  popd
done
