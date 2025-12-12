
declare -a dirs=(
  json
  json_classic
  json_scannerless
  json_unbounded
  json_unbounded_scannerless
  pascal
  pascal_classic
  pascal_scannerless
  pascal_unbounded
  pascal_unbounded_scannerless
  c
  c_classic
  c_scannerless
  c_unbounded
  c_unbounded_scannerless
  fortran
  fortran_classic
  fortran_scannerless
  fortran_unbounded
  fortran_unbounded_scannerless
  cobol
  cobol_classic
  cobol_scannerless
  cobol_unbounded
  cobol_unbounded_scannerless
)

for i in {10..10}
do
  for dir in "${dirs[@]}"
  do
    pushd "$dir"
    dune exec "./te_$dir.exe" | tee "results${i}.txt"
    popd
  done
done
