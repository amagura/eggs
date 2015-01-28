#!/usr/bin/env bats
# vim: ft=sh:

@test "quote balancing in csi" {
  result="$(csi -n $STARTDIR/rc <<<"
  \" \"
  \"\"
  \"\"\"\"
  '(\"\")
  \"
  \"" | grep -o -- '--->' | wc -l)"
  [ "$result" -eq 1 ]
}

@test "paren balancing in csi" {
  result="$(csi -n $STARTDIR/rc <<<"
  '( )
  '()
  '(())
  \"(\"
  '((
  )
  )
  '(
  )" | grep -o -- '--->' | wc -l)"
  [ "$result" -eq 3 ]
}

@test "commented-out paren balancing in csi" {
  result="$(csi -n $STARTDIR/rc <<<"
  ;(

  #;()
  #|()|#
  " | grep -o -- '--->' | wc -l)"
  [ "$result" -eq 0 ]
}
