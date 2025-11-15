  $ INTERPETER="../bin/REPL.exe"

  $ cat << EOF | $INTERPETER -parse -patt 
  > (x, y), _
  parsed: ((x, y), _)

  $ cat << EOF | $INTERPETER -parse -patt 
  > Some (x, y, _)
  parsed: Some ((x, y, _))

  $ cat << EOF | $INTERPETER -parse -patt 
  > Just x, Some (x, y), Box _
  parsed: (Just (x), Some ((x, y)), Box (_))

  $ cat << EOF | $INTERPETER -parse -patt 
  > Just (Some None)
  parsed: Just (Some (None))

  $ cat << EOF | $INTERPETER -parse -patt 
  > x :: []
  parsed: [ x ]

  $ cat << EOF | $INTERPETER -parse -patt 
  > Some x :: Some []
  parsed: Some (x) :: Some ([])

  $ cat << EOF | $INTERPETER -parse -patt 
  > Some (x :: y)
  parsed: Some (x :: y)

  $ cat << EOF | $INTERPETER -parse -patt 
  > (x, y :: z)
  parsed: (x, y :: z)

  $ cat << EOF | $INTERPETER -parse -patt 
  > [ x; y; z ] :: []
  parsed: [ [ x; y; z ] ]

  $ cat << EOF | $INTERPETER -parse -patt 
  > Some [ x; y ]
  parsed: Some ([ x; y ])

  $ cat << EOF | $INTERPETER -parse -patt 
  > [ (x, y); (a, b) ]
  parsed: [ (x, y); (a, b) ]

  $ cat << EOF | $INTERPETER -parse -patt 
  > ( [ x; y ], [ a; b ] )
  parsed: ([ x; y ], [ a; b ])
