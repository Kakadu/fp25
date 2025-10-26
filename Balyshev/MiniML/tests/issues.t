# works slow

$ cat << EOF | ./REPL.exe -parse -expr 
> [ [ 1 ] ]

$ cat << EOF | ./REPL.exe -parse -expr 
> [ [ 1 ]; [ 2 ; 3 ] ]

$ cat << EOF | ./REPL.exe -parse -expr 
$ cat << EOF | ./REPL.exe -parse -expr 
> Some [1; 2; 3]
"Some [1; 2; 3]"
parsed: Some ((1 :: (2 :: (3 :: []))))

> Some [[a; b], [1, 2]]
"Some [[a; b], [1, 2]]"
parsed: Some ((((a :: (b :: [])), ((1, 2) :: [])) :: []))

$ cat << EOF | ./REPL.exe -parse -expr 
> Some [1 + 2 * 3; 4 / 5; 6]
"Some [1 + 2 * 3; 4 / 5; 6]"
parsed: Some (((1 + (2 * 3)) :: ((4 / 5) :: (6 :: []))))

$ cat << EOF | ./REPL.exe -parse -expr 
> Just (x, y + z, [ a; b; c ])
"Just (x, y + z, [ a; b; c ])"
parsed: Just ((x, (y + z), (a :: (b :: (c :: [])))))
