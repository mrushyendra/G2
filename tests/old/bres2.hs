fromList [("Bool",TyAlg "Bool" [("True",-5,TyConApp "Bool" [],[]),("False",-6,TyConApp "Bool" [],[])]),("Char",TyAlg "Char" [("Char!",-4,TyConApp "Char" [],[TyRawChar])]),("Double",TyAlg "Double" [("Double!",-3,TyConApp "Double" [],[TyRawDouble])]),("Float",TyAlg "Float" [("Float!",-2,TyConApp "Float" [],[TyRawFloat])]),("Int",TyAlg "Int" [("Int!",-1,TyConApp "Int" [],[TyRawInt])]),("Tree",TyAlg "Tree" [("Leaf",1,TyConApp "Tree" [],[TyRawInt]),("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])])]
fromList [("*!D",Const (COp "p_e_Mul!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("*!F",Const (COp "p_e_Mul!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("*!I",Const (COp "p_e_Mul!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("+!D",Const (COp "p_e_Add!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("+!F",Const (COp "p_e_Add!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("+!I",Const (COp "p_e_Add!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("-!D",Const (COp "p_e_Sub!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("-!F",Const (COp "p_e_Sub!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("-!I",Const (COp "p_e_Sub!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("/=!B",Const (COp "p_e_Ne!B" (TyFun (TyConApp "Bool" []) (TyFun (TyConApp "Bool" []) (TyConApp "Bool" []))))),("/=!C",Const (COp "p_e_Ne!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("/=!D",Const (COp "p_e_Ne!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("/=!F",Const (COp "p_e_Ne!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("/=!I",Const (COp "p_e_Ne!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("<!C",Const (COp "p_e_Lt!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("<!D",Const (COp "p_e_Lt!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("<!F",Const (COp "p_e_Lt!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("<!I",Const (COp "p_e_Lt!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("<=!C",Const (COp "p_e_Le!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("<=!D",Const (COp "p_e_Le!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("<=!F",Const (COp "p_e_Le!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("<=!I",Const (COp "p_e_Le!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("==!B",Const (COp "p_e_Eq!B" (TyFun (TyConApp "Bool" []) (TyFun (TyConApp "Bool" []) (TyConApp "Bool" []))))),("==!C",Const (COp "p_e_Eq!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("==!D",Const (COp "p_e_Eq!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("==!F",Const (COp "p_e_Eq!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("==!I",Const (COp "p_e_Eq!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),(">!C",Const (COp "p_e_Gt!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),(">!D",Const (COp "p_e_Gt!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),(">!F",Const (COp "p_e_Gt!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),(">!I",Const (COp "p_e_Gt!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),(">=!C",Const (COp "p_e_Ge!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),(">=!D",Const (COp "p_e_Ge!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),(">=!F",Const (COp "p_e_Ge!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),(">=!I",Const (COp "p_e_Ge!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("abstract",Case (App (App (Var "a" (TyFun TyRawInt (TyFun TyRawInt (TyConApp "Tree" [])))) (Var "b" TyRawInt)) (Var "c" TyRawInt)) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Const (CInt 123)),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Const (CInt 456))] TyRawInt),("inner",Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])),("outer",Case (Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "b" (TyConApp "Tree" []))] (TyConApp "Tree" [])),("test",App (App (App (Var "foo" (TyFun TyRawInt (TyFun (TyConApp "Tree" []) (TyFun TyRawInt TyRawInt)))) (Const (CInt 123))) (Var "outer" (TyConApp "Tree" []))) (Const (CInt 456)))]
Case (App (App (Var "a" (TyFun TyRawInt (TyFun TyRawInt (TyConApp "Tree" [])))) (Var "b" TyRawInt)) (Var "c" TyRawInt)) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Const (CInt 123)),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Const (CInt 456))] TyRawInt
[]
==============================================
fromList [("Bool",TyAlg "Bool" [("True",-5,TyConApp "Bool" [],[]),("False",-6,TyConApp "Bool" [],[])]),("Char",TyAlg "Char" [("Char!",-4,TyConApp "Char" [],[TyRawChar])]),("Double",TyAlg "Double" [("Double!",-3,TyConApp "Double" [],[TyRawDouble])]),("Float",TyAlg "Float" [("Float!",-2,TyConApp "Float" [],[TyRawFloat])]),("Int",TyAlg "Int" [("Int!",-1,TyConApp "Int" [],[TyRawInt])]),("Tree",TyAlg "Tree" [("Leaf",1,TyConApp "Tree" [],[TyRawInt]),("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])])]
fromList [("*!D",Const (COp "p_e_Mul!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("*!F",Const (COp "p_e_Mul!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("*!I",Const (COp "p_e_Mul!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("+!D",Const (COp "p_e_Add!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("+!F",Const (COp "p_e_Add!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("+!I",Const (COp "p_e_Add!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("-!D",Const (COp "p_e_Sub!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("-!F",Const (COp "p_e_Sub!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("-!I",Const (COp "p_e_Sub!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("/=!B",Const (COp "p_e_Ne!B" (TyFun (TyConApp "Bool" []) (TyFun (TyConApp "Bool" []) (TyConApp "Bool" []))))),("/=!C",Const (COp "p_e_Ne!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("/=!D",Const (COp "p_e_Ne!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("/=!F",Const (COp "p_e_Ne!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("/=!I",Const (COp "p_e_Ne!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("<!C",Const (COp "p_e_Lt!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("<!D",Const (COp "p_e_Lt!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("<!F",Const (COp "p_e_Lt!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("<!I",Const (COp "p_e_Lt!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("<=!C",Const (COp "p_e_Le!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("<=!D",Const (COp "p_e_Le!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("<=!F",Const (COp "p_e_Le!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("<=!I",Const (COp "p_e_Le!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("==!B",Const (COp "p_e_Eq!B" (TyFun (TyConApp "Bool" []) (TyFun (TyConApp "Bool" []) (TyConApp "Bool" []))))),("==!C",Const (COp "p_e_Eq!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("==!D",Const (COp "p_e_Eq!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("==!F",Const (COp "p_e_Eq!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("==!I",Const (COp "p_e_Eq!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),(">!C",Const (COp "p_e_Gt!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),(">!D",Const (COp "p_e_Gt!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),(">!F",Const (COp "p_e_Gt!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),(">!I",Const (COp "p_e_Gt!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),(">=!C",Const (COp "p_e_Ge!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),(">=!D",Const (COp "p_e_Ge!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),(">=!F",Const (COp "p_e_Ge!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),(">=!I",Const (COp "p_e_Ge!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("abstract",Case (App (App (Var "a" (TyFun TyRawInt (TyFun TyRawInt (TyConApp "Tree" [])))) (Var "b" TyRawInt)) (Var "c" TyRawInt)) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Const (CInt 123)),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Const (CInt 456))] TyRawInt),("inner",Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])),("outer",Case (Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "b" (TyConApp "Tree" []))] (TyConApp "Tree" [])),("test",App (App (App (Var "foo" (TyFun TyRawInt (TyFun (TyConApp "Tree" []) (TyFun TyRawInt TyRawInt)))) (Const (CInt 123))) (Var "outer" (TyConApp "Tree" []))) (Const (CInt 456)))]
Const (CInt 456)
[(App (App (Var "a" (TyFun TyRawInt (TyFun TyRawInt (TyConApp "Tree" [])))) (Var "b" TyRawInt)) (Var "c" TyRawInt),(("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["ba","aa"]))]
--------------
fromList [("Bool",TyAlg "Bool" [("True",-5,TyConApp "Bool" [],[]),("False",-6,TyConApp "Bool" [],[])]),("Char",TyAlg "Char" [("Char!",-4,TyConApp "Char" [],[TyRawChar])]),("Double",TyAlg "Double" [("Double!",-3,TyConApp "Double" [],[TyRawDouble])]),("Float",TyAlg "Float" [("Float!",-2,TyConApp "Float" [],[TyRawFloat])]),("Int",TyAlg "Int" [("Int!",-1,TyConApp "Int" [],[TyRawInt])]),("Tree",TyAlg "Tree" [("Leaf",1,TyConApp "Tree" [],[TyRawInt]),("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])])]
fromList [("*!D",Const (COp "p_e_Mul!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("*!F",Const (COp "p_e_Mul!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("*!I",Const (COp "p_e_Mul!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("+!D",Const (COp "p_e_Add!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("+!F",Const (COp "p_e_Add!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("+!I",Const (COp "p_e_Add!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("-!D",Const (COp "p_e_Sub!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("-!F",Const (COp "p_e_Sub!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("-!I",Const (COp "p_e_Sub!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("/=!B",Const (COp "p_e_Ne!B" (TyFun (TyConApp "Bool" []) (TyFun (TyConApp "Bool" []) (TyConApp "Bool" []))))),("/=!C",Const (COp "p_e_Ne!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("/=!D",Const (COp "p_e_Ne!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("/=!F",Const (COp "p_e_Ne!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("/=!I",Const (COp "p_e_Ne!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("<!C",Const (COp "p_e_Lt!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("<!D",Const (COp "p_e_Lt!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("<!F",Const (COp "p_e_Lt!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("<!I",Const (COp "p_e_Lt!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("<=!C",Const (COp "p_e_Le!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("<=!D",Const (COp "p_e_Le!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("<=!F",Const (COp "p_e_Le!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("<=!I",Const (COp "p_e_Le!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("==!B",Const (COp "p_e_Eq!B" (TyFun (TyConApp "Bool" []) (TyFun (TyConApp "Bool" []) (TyConApp "Bool" []))))),("==!C",Const (COp "p_e_Eq!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),("==!D",Const (COp "p_e_Eq!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),("==!F",Const (COp "p_e_Eq!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),("==!I",Const (COp "p_e_Eq!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),(">!C",Const (COp "p_e_Gt!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),(">!D",Const (COp "p_e_Gt!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),(">!F",Const (COp "p_e_Gt!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),(">!I",Const (COp "p_e_Gt!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),(">=!C",Const (COp "p_e_Ge!C" (TyFun (TyConApp "Char" []) (TyFun (TyConApp "Char" []) (TyConApp "Char" []))))),(">=!D",Const (COp "p_e_Ge!D" (TyFun (TyConApp "Double" []) (TyFun (TyConApp "Double" []) (TyConApp "Double" []))))),(">=!F",Const (COp "p_e_Ge!F" (TyFun (TyConApp "Float" []) (TyFun (TyConApp "Float" []) (TyConApp "Float" []))))),(">=!I",Const (COp "p_e_Ge!I" (TyFun (TyConApp "Int" []) (TyFun (TyConApp "Int" []) (TyConApp "Int" []))))),("abstract",Case (App (App (Var "a" (TyFun TyRawInt (TyFun TyRawInt (TyConApp "Tree" [])))) (Var "b" TyRawInt)) (Var "c" TyRawInt)) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Const (CInt 123)),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Const (CInt 456))] TyRawInt),("inner",Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])),("outer",Case (Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),Case (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))) [((("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["a"]),App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (App (DCon ("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []])) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "a" TyRawInt))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "b" TyRawInt)))) (App (DCon ("Leaf",1,TyConApp "Tree" [],[TyRawInt])) (Var "c" TyRawInt))),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "a" (TyConApp "Tree" []))] (TyConApp "Tree" [])),((("Node",2,TyConApp "Tree" [],[TyConApp "Tree" [],TyConApp "Tree" []]),["a","b"]),Var "b" (TyConApp "Tree" []))] (TyConApp "Tree" [])),("test",App (App (App (Var "foo" (TyFun TyRawInt (TyFun (TyConApp "Tree" []) (TyFun TyRawInt TyRawInt)))) (Const (CInt 123))) (Var "outer" (TyConApp "Tree" []))) (Const (CInt 456)))]
Const (CInt 123)
[(App (App (Var "a" (TyFun TyRawInt (TyFun TyRawInt (TyConApp "Tree" [])))) (Var "b" TyRawInt)) (Var "c" TyRawInt),(("Leaf",1,TyConApp "Tree" [],[TyRawInt]),["aa"]))]
Compiles!