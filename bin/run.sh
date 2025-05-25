
declare -a dirs=(
  json
  json_classic
  json_scannerless
  pascal
  pascal_classic
  pascal_scannerless
  c
  c_classic
  c_scannerless
  fortran
  fortran_classic
  fortran_scannerless
  cobol
  cobol_classic
  cobol_scannerless
)

for dir in "${dirs[@]}"
do
  pushd "$dir"
  dune exec "./te_$dir.exe" | tee results.txt
  popd
done
