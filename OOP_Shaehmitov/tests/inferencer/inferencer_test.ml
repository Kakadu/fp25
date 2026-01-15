[@@@ocaml.text "/*"]

(** copyright 2021-2024, kakadu and contributors *)

(** spdx-license-identifier: lgpl-3.0-or-later *)

[@@@ocaml.text "/*"]

open Inferencer

let infer_expr_str s =
  match Parser.parse s with
  | Ok expr ->
    (match TypeChecker.infer_expression expr with
     | Ok t_str -> t_str
     | Error e -> Format.asprintf "%a" TypeChecker.pp_error e)
  | Error msg -> "parse error: " ^ msg
;;

let check_prog_str s =
  match Parser.parse_structure_items s with
  | Ok prog ->
    (match TypeChecker.print_program_type prog with
     | Ok s -> s
     | Error e -> e)
  | Error msg -> "parse error: " ^ msg
;;

let%expect_test "infer_simple_types" =
  print_endline (infer_expr_str "1");
  [%expect {| int |}];
  print_endline (infer_expr_str "true");
  [%expect {| bool |}];
  print_endline (infer_expr_str "()");
  [%expect {| unit |}]
;;

let%expect_test "infer_arithmetic" =
  print_endline (infer_expr_str "1 + 2");
  [%expect {| int |}];
  print_endline (infer_expr_str "((1, 2), 3)");
  [%expect {| ((int * int) * int) |}];
  print_endline (infer_expr_str "1 + true");
  [%expect {| Unify: bool vs int |}]
;;

let%expect_test "infer_function_identity" =
  print_endline (infer_expr_str "fun x -> x");
  [%expect {| '_a1 -> '_a1 |}]
;;

let%expect_test "infer_function_application" =
  print_endline (infer_expr_str "(fun x -> x + 1) 2");
  [%expect {| int |}]
;;

let%expect_test "infer_program_let" =
  print_endline (check_prog_str "let x = 1;;");
  [%expect {|
    val x : int |}]
;;

let%expect_test "infer_program_functions" =
  print_endline (check_prog_str "let id = fun x -> x;;");
  [%expect {|
    val id : forall ... '_a1 -> '_a1 |}]
;;

let%expect_test "infer_program_let_rec" =
  print_endline (check_prog_str "let rec f = fun x -> if x then 1 else f x;;");
  [%expect {|
    val f : bool -> int |}]
;;

let%expect_test "infer_duck_typing" =
  print_endline (infer_expr_str "fun x -> x#foo()");
  [%expect {| '_a1 -> '_a2 |}]
;;

let%expect_test "check_program_class" =
  print_endline
    (check_prog_str
       "\n\
       \    class c = object\n\
       \       val x = 1\n\
       \       method get_x = x\n\
       \    end;;\n\
       \    let obj = new c;;\n\
       \  ");
  [%expect
    {|
    class c = object
        val x : int
        method get_x : int
    end
    val obj : c |}]
;;

let%expect_test "infer_duck_typing_usage" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object method foo = 1 end;;\n\
       \    class b = object method foo = 2 end;;\n\
       \    let f = fun x -> x#foo;;\n\
       \    let a = f (new a);;\n\
       \    let b = f (new b);;\n\
       \  ");
  [%expect
    {|
    class a = object
        method foo : int
    end
    class b = object
        method foo : int
    end
    val a : int
    val b : int
    val f : forall ... '_a3 -> '_a4 where '_a3 has foo : '_a4 |}]
;;

let%expect_test "infer_duck_typing_fail_missing_method" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object method foo = 1 end;;\n\
       \    let f = fun x -> x#bar;;\n\
       \    let a = f (new a);;\n\
       \  ");
  [%expect {|
    No method bar |}]
;;

let%expect_test "infer_duck_typing_fail_signature_mismatch" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object method foo = 1 end;;\n\
       \    let f = fun x -> not (x#foo);; \n\
       \    let a = f (new a);;\n\
       \  ");
  [%expect {|
    Unify: int vs bool |}]
;;

let%expect_test "class_param_inference" =
  print_endline
    (check_prog_str
       "\n\
       \    class point(x, y) = object\n\
       \      val x1 = x\n\
       \      val y1 = y\n\
       \      method get_x = x1\n\
       \      method get_y = y1\n\
       \    end;;\n\
       \    let p = new point(10, 20);;\n\
       \    let x_val = p#get_x;; \n\
       \  ");
  [%expect
    {|
    class point((int * int)) = object
        val x1 : int
        val y1 : int
        method get_x : int
        method get_y : int
    end
    val p : point
    val x_val : int |}]
;;

let%expect_test "class_param_mismatch_count" =
  print_endline
    (check_prog_str
       "\n    class point x y = object end;;\n    let p = new point(10);;\n  ");
  [%expect {| New error: Class point needs 2 args |}]
;;

let%expect_test "class_param_mismatch_type" =
  print_endline
    (check_prog_str
       "\n\
       \    class box(v) = object\n\
       \       method get = v + 1\n\
       \    end;;\n\
       \    let b = new box(true);; \n\
       \  ");
  [%expect {| Unbound: v |}]
;;

let%expect_test "class_param_usage_in_field" =
  print_endline
    (check_prog_str
       "\n\
       \    class container(init_val) = object\n\
       \       val storage = init_val\n\
       \       method get = storage\n\
       \    end;;\n\
       \    let c = new container(100);;\n\
       \    let res = c#get;;\n\
       \  ");
  [%expect
    {|
    class container(int) = object
        val storage : int
        method get : int
    end
    val c : container
    val res : int |}]
;;

let%expect_test "class_param_polymorphism_fail" =
  print_endline
    (check_prog_str
       "\n\
       \    class holder x = object\n\
       \      method get = x\n\
       \    end;;\n\
       \    let h1 = new holder 1;;\n\
       \    let h2 = new holder true;; \n\
       \  ");
  [%expect {| Unbound: x |}]
;;

let%expect_test "method_call_type_mismatch" =
  print_endline
    (check_prog_str
       "\n\
       \    class calculator = object\n\
       \      method add(x, y) = x + y\n\
       \    end;;\n\
       \    let calc = new calculator;;\n\
       \    let result = calc#add(10, true);;\n\
       \  ");
  [%expect {| Unify: int vs bool |}]
;;

let%expect_test "inheritance_method_override" =
  print_endline
    (check_prog_str
       "\n\
       \    class base = object\n\
       \      method greet = 1\n\
       \    end;;\n\
       \    class derived = object\n\
       \      inherit base\n\
       \      method greet = 2\n\
       \    end;;\n\
       \    let d = new derived;;\n\
       \    let msg = d#greet;;\n\
       \  ");
  [%expect
    {|
    class base = object
        method greet : int
    end
    class derived = object
        method greet : int
    end
    val d : derived
    val msg : int |}]
;;

let%expect_test "inheritance_method_override_type_mismatch" =
  print_endline
    (check_prog_str
       "\n\
       \    class base = object\n\
       \      method greet = 1\n\
       \    end;;\n\
       \    class derived = object\n\
       \      inherit base\n\
       \      method greet = true\n\
       \    end;;\n\
       \    let d = new derived;;\n\
       \    let msg = d#greet;;\n\
       \  ");
  [%expect {| Unify: bool vs int |}]
;;

let%expect_test "inheritance_field_access" =
  print_endline
    (check_prog_str
       "\n\
       \    class parent = object\n\
       \      val x = 10\n\
       \    end;;\n\
       \    class child = object\n\
       \      inherit parent\n\
       \      method get_x = x\n\
       \    end;;\n\
       \    let c = new child;;\n\
       \    let val_x = c#get_x;;\n\
       \  ");
  [%expect
    {|
    class child = object
        method get_x : int
    end
    class parent = object
        val x : int
    end
    val c : child
    val val_x : int |}]
;;

let%expect_test "child and parent_in_list" =
  print_endline
    (check_prog_str
       "\n\
       \    class parent = object end;;\n\
       \    class child = object inherit parent end;;\n\
       \    let f n = if n > 0 then new child else new parent;;\n\
       \  ");
  [%expect
    {|
    class child = object
    end
    class parent = object
    end
    val f : int -> parent |}]
;;

let%expect_test "inheritance_with_params" =
  print_endline
    (check_prog_str
       "\n\
       \    class animal age = object\n\
       \      method get_age = age\n\
       \    end;;\n\
       \    class dog age name = object\n\
       \      inherit animal age\n\
       \      method get_name = name\n\
       \    end;;\n\
       \    let d = new dog 5 2;;\n\
       \    let age = d#get_age;;\n\
       \    let name = d#get_name;;\n\
       \  ");
  [%expect {|
    Unbound: age |}]
;;

let%expect_test "binops" =
  print_endline (infer_expr_str "1 + 2 * 3 - 4 / 5");
  [%expect {| int |}];
  print_endline (infer_expr_str "true && false || true");
  [%expect {| bool |}];
  print_endline (infer_expr_str "1 + true");
  [%expect {| Unify: bool vs int |}];
  print_endline (infer_expr_str "true && 1");
  [%expect {| Unify: int vs bool |}];
  print_endline (infer_expr_str "1 < 2");
  [%expect {| bool |}];
  print_endline (infer_expr_str "1 < true");
  [%expect {| Unify: bool vs int |}]
;;

let%expect_test "subclass_test" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object method foo = 1 end;;\n\
       \    class b = object inherit a method bar = 2 end;;\n\
       \    let f = fun x -> x#foo + x#bar;;\n\
       \    let b = new b;;\n\
       \    let result = f b;;\n\
       \  ");
  [%expect
    {|
    class a = object
        method foo : int
    end
    class b = object
        method bar : int
    end
    val b : b
    val f : forall ... '_a3 -> int where '_a3 has bar : int, foo : int
    val result : int |}]
;;

let%expect_test "subclass_inference" =
  print_endline
    (check_prog_str
       "\n\
       \    class animal = object method speak = 1 end;;\n\
       \    class dog = object inherit animal method bark = 2 end;;\n\
       \    class cat = object inherit animal method meow = 3 end;;\n\
       \    let animal = new animal;;\n\
       \    let dog = new dog;;\n\
       \    let cat = new cat;;\n\
       \    let speak_animal = animal#speak;;\n\
       \    let speak_dog = dog#speak;;\n\
       \    let speak_cat = cat#speak;;\n\
       \    let bark_dog = dog#bark;;\n\
       \    let meow_cat = cat#meow;;\n\
       \  ");
  [%expect
    {|
    class animal = object
        method speak : int
    end
    class cat = object
        method meow : int
    end
    class dog = object
        method bark : int
    end
    val animal : animal
    val bark_dog : int
    val cat : cat
    val dog : dog
    val meow_cat : int
    val speak_animal : int
    val speak_cat : int
    val speak_dog : int |}]
;;

let%expect_test "subclass_method_override" =
  print_endline
    (check_prog_str
       "\n\
       \    class base = object method value = 10 end;;\n\
       \    class derived = object inherit base method value = 20 end;;\n\
       \    let b = new base;;\n\
       \    let d = new derived;;\n\
       \    let val_b = b#value;;\n\
       \    let val_d = d#value;;\n\
       \  ");
  [%expect
    {|
    class base = object
        method value : int
    end
    class derived = object
        method value : int
    end
    val b : base
    val d : derived
    val val_b : int
    val val_d : int |}]
;;

let%expect_test "subclass_field_inheritance" =
  print_endline
    (check_prog_str
       "\n\
       \    class parent = object val x = 100 method get = x end;;\n\
       \    class child = object inherit parent val y = 200 method get_y = y end;;\n\
       \    let p = new parent;;\n\
       \    let c = new child;;\n\
       \    let x_p = p#get;;\n\
       \    let x_c = c#get;;\n\
       \    let y_c = c#get_y;;\n\
       \  ");
  [%expect
    {|
    class child = object
        val y : int
        method get_y : int
    end
    class parent = object
        val x : int
        method get : int
    end
    val c : child
    val p : parent
    val x_c : int
    val x_p : int
    val y_c : int |}]
;;

let%expect_test "is_subclass_logic_check" =
  print_endline
    (check_prog_str
       "\n\
       \    class animal = object method make_sound = 1 end;;\n\
       \    class dog = object inherit animal method bark = 2 end;;\n\
       \    class cat = object inherit animal method meow = 3 end;;\n\
       \    \n\
       \    let get_sound a = a#make_sound;;\n\
       \    \n\
       \    let dog = new dog;;\n\
       \    let cat = new cat;;\n\
       \    \n\
       \    let s1 = get_sound dog;;\n\
       \    let s2 = get_sound cat;;\n\
       \  ");
  [%expect
    {|
    class animal = object
        method make_sound : int
    end
    class cat = object
        method meow : int
    end
    class dog = object
        method bark : int
    end
    val cat : cat
    val dog : dog
    val get_sound : forall ... '_a4 -> '_a5 where '_a4 has make_sound : '_a5
    val s1 : int
    val s2 : int |}]
;;

let%expect_test "is_subclass_deep_hierarchy" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object method a = 1 end;;\n\
       \    class b = object inherit a method b = 2 end;;\n\
       \    class c = object inherit b method c = 3 end;;\n\
       \    class d = object inherit c method d = 4 end;;\n\
       \    \n\
       \    let accept_a x = x#a;;\n\
       \    let accept_b x = x#b;;\n\
       \    \n\
       \    let d = new d;;\n\
       \    \n\
       \    let res1 = accept_a d;;\n\
       \    let res2 = accept_b d;;\n\
       \  ");
  [%expect
    {|
    class a = object
        method a : int
    end
    class b = object
        method b : int
    end
    class c = object
        method c : int
    end
    class d = object
        method d : int
    end
    val accept_a : forall ... '_a5 -> '_a6 where '_a5 has a : '_a6
    val accept_b : forall ... '_a7 -> '_a8 where '_a7 has b : '_a8
    val d : d
    val res1 : int
    val res2 : int |}]
;;

let%expect_test "unify_subclass_success" =
  print_endline
    (check_prog_str
       "\n\
       \    class parent = object method p = 1 end;;\n\
       \    class child = object inherit parent method c = 2 end;;\n\
       \    \n\
       \    let force_parent x = \n\
       \       let p = new parent in \n\
       \       if true then x else p;;\n\
       \    \n\
       \    let c = new child;;\n\
       \    \n\
       \    let res = force_parent c;;\n\
       \  ");
  [%expect
    {|
    class child = object
        method c : int
    end
    class parent = object
        method p : int
    end
    val c : child
    val force_parent : parent -> parent
    val res : parent |}]
;;

let%expect_test "unify_class_fail" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object end;;\n\
       \    class b = object end;;\n\
       \    let force_a x = \n\
       \       let a = new a in \n\
       \       if true then x else a;;\n\
       \    \n\
       \    let b = new b;;\n\
       \    let res = force_a b;;\n\
       \  ");
  [%expect {|
    Unify: a vs b |}]
;;

let%expect_test "parent with params" =
  print_endline
    (check_prog_str
       "\n\
       \    class parent x y = object val x = x val y = y method get_x = x end;;\n\
       \    class child (x, y) = object inherit parent x y method get_y = y end;;\n\
       \    \n\
       \    let c = new child (10, 20);;\n\
       \    let x_val = c#get_x;;\n\
       \    let y_val = c#get_y;;\n\
       \  ");
  [%expect
    {|
    class child((int * int)) = object
        method get_y : int
    end
    class parent(int, int) = object
        val x : int
        val y : int
        method get_x : int
    end
    val c : child
    val x_val : int
    val y_val : int |}]
;;

let%expect_test "class not exist" =
  print_endline
    (check_prog_str "\n    let f = fun x -> x#bar;;\n    let a = f (new a);;\n  ");
  [%expect {|
    No class a |}]
;;

let%expect_test "subclass_bad_override" =
  print_endline
    (check_prog_str
       "\n\
       \    class base = object method greet = 1 end;;\n\
       \    class derived = object inherit base method greet = true end;;\n\
       \    let d = new derived;;\n\
       \    let msg = d#greet;;\n\
       \  ");
  [%expect {| Unify: bool vs int |}]
;;

let%expect_test "occurs check fail" =
  print_endline (check_prog_str "\n    let f = fun x -> x x ;;\n  ");
  [%expect {|
    Occurs check failed for type variable |}]
;;

let%expect_test "test monomorphic method" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object\n\
       \      method id x = x\n\
       \    end;;\n\
       \    let a = new a;;\n\
       \    let res1 = a#id 10;;\n\
       \    let res2 = a#id true;;\n\
       \  ");
  [%expect {| Unify: int vs bool |}]
;;

let%expect_test "test monomorphic arg of class" =
  print_endline
    (check_prog_str
       "\n\
       \    class box x = object\n\
       \      val x1 = x\n\
       \      method get = x1\n\
       \    end;;\n\
       \    let b1 = new box 10 ;;\n\
       \    let b2 = new box true;;\n\
       \  ");
  [%expect {| Unify: bool vs int |}]
;;

let%expect_test "object self test" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object(self)\n\
       \      method get_five = 5\n\
       \      method call_get_five = self#get_five\n\
       \    end;;\n\
       \    let a = new a;;\n\
       \    let res = a#call_get_five;;\n\
       \  ");
  [%expect
    {|
    class a = object
        method call_get_five : int
        method get_five : int
    end
    val a : a
    val res : int |}]
;;

let%expect_test "overriding of id in inheritance" =
  print_endline
    (check_prog_str
       "\n\
       \    class base = object\n\
       \      method id x = x\n\
       \    end;;\n\
       \    class derived = object\n\
       \      inherit base\n\
       \      method id x = x + 1\n\
       \    end;;\n\
       \    let b = new base;;\n\
       \    let d = new derived;;\n\
       \    let res1 = b#id 10;;\n\
       \    let res2 = d#id true;;\n\
       \  ");
  [%expect {| Unify: int vs bool |}]
;;

let%expect_test "field overriding in inheritance" =
  print_endline
    (check_prog_str
       "\n\
       \    class parent = object\n\
       \      val x = 10\n\
       \    method get_x = x\n\
       \    end;;\n\
       \    class child = object\n\
       \      inherit parent\n\
       \      val x = true\n\
       \    end;;\n\
       \    let p = new parent;;\n\
       \    let c = new child;;\n\
       \    let x_p = p#get_x;;\n\
       \    let x_c = c#get_x;;\n\
       \  ");
  [%expect {|
    Unify: bool vs int |}]
;;

let%expect_test "parent class not exist" =
  print_endline
    (check_prog_str
       "\n\
       \    class child = object\n\
       \      inherit nonexistentparent\n\
       \      method foo = 1\n\
       \    end;;\n\
       \    let c = new child;;\n\
       \  ");
  [%expect {| No class nonexistentparent |}]
;;

let%expect_test "self return" =
  print_endline
    (check_prog_str
       "\n\
       \    class chain = \n\
       \      object(self)\n\
       \        method get_self = self\n\
       \        method get_value = 42\n\
       \      end;;\n\
       \    let c = new chain;;\n\
       \    let c2 = c#get_self#get_self#get_self ;;\n\
       \    let v = c2#get_value ;;\n\
       \    let () = print_int v;;\n\
       \  ");
  [%expect
    {|
    class chain = object
        method get_self : chain
        method get_value : int                
    end
    val c : chain
    val c2 : chain
    val v : int |}]
;;

let%expect_test "duck typing with arguments" =
  print_endline
    (check_prog_str
       "\n\
       \    let f = fun x -> x#compute(10, 20);;\n\
       \    class calca = object method compute(a, b) = a + b end;;\n\
       \    class calcb = object method compute(a, b) = a * b end;;\n\
       \    let a = new calca;;\n\
       \    let b = new calcb;;\n\
       \    let res1 = f a;;\n\
       \    let res2 = f b;;\n\
       \  ");
  [%expect
    {|
    class calca = object
        method compute : (int * int) -> int
    end
    class calcb = object
        method compute : (int * int) -> int
    end
    val a : calca
    val b : calcb
    val f : forall ... '_a3 -> '_a4 where '_a3 has compute (int * int): '_a4
    val res1 : int
    val res2 : int |}]
;;

let%expect_test "field monomorphism" =
  print_endline
    (check_prog_str
       "\n\
       \    class box x = object\n\
       \      val content = x\n\
       \      method get_content = content\n\
       \    end;;\n\
       \    let box1 = new box 10;;\n\
       \    let box2 = new box true;;\n\
       \    let val1 = box1#get_content;;\n\
       \    let val2 = box2#get_content;;\n\
       \  ");
  [%expect {| Unify: bool vs int |}]
;;

let%expect_test "val get method" =
  print_endline
    (check_prog_str
       "\n\
       \    class a = object(self)\n\
       \      val x = self#get \n\
       \      method get = x\n\
       \    end;;\n\
       \    let a = new a;;\n\
       \    let v = a#get;;\n\
       \  ");
  [%expect {| Unbound: self |}]
;;

let%expect_test "functional type in method" =
  print_endline
    (check_prog_str
       "\n\
       \    class funcholder = object\n\
       \      method get_func = fun x -> x + 1\n\
       \    end;;\n\
       \    let fh = new funcholder;;\n\
       \    let f = fh#get_func;;\n\
       \    let res = f 10;;\n\
       \  ");
  [%expect
    {|
    class funcholder = object
        method get_func : int -> int
    end
    val f : int -> int
    val fh : funcholder
    val res : int |}]
;;

let%expect_test "function in arg" =
  print_endline
    (check_prog_str
       "\n\
       \    class funcholder f = object\n\
       \       val f = f\n\
       \      method call_func x = f x\n\
       \    end;;\n\
       \    let fh = new funcholder (fun z -> z + 3);;\n\
       \    let () = print_int (fh#call_func 39);;\n\
       \  ");
  [%expect
    {|
    class funcholder(int -> int) = object
        val f : int -> int
        method call_func : int -> int
    end
    val fh : funcholder |}]
;;

let%expect_test "method returning function" =
  print_endline
    (check_prog_str
       "\n\
       \    class funcprovider = object\n\
       \      method get_adder n = fun x -> x + n\n\
       \    end;;\n\
       \    let fp = new funcprovider;;\n\
       \    let add_five = fp#get_adder 5;;\n\
       \    let result = add_five 10;;\n\
       \  ");
  [%expect
    {|
    class funcprovider = object
        method get_adder : int -> int -> int
    end
    val add_five : int -> int
    val fp : funcprovider
    val result : int |}]
;;

let%expect_test "function in arg of method" =
  print_endline
    (check_prog_str
       "\n\
       \    class funcuser = object\n\
       \      method apply_func f x = f x\n\
       \    end;;\n\
       \    let fu = new funcuser;;\n\
       \    let  x = fu#apply_func (fun y -> y * 2) 21;;\n\
       \  ");
  [%expect
    {|  
    class funcuser = object
        method apply_func : (int -> int) -> int -> int
    end
    val fu : funcuser
    val x : int |}]
;;

let%expect_test "functional type in functional type" =
  print_endline (check_prog_str "\n    let f g m x = (g m) x ;;\n  ");
  [%expect {|
    val f : forall ... ('_a2 -> '_a3 -> '_a5) -> '_a2 -> '_a3 -> '_a5 |}]
;;

let%expect_test "fix point" =
  print_endline
    (check_prog_str
       "\n\
       \    let rec fix f x = f (fix f) x;;\n\
       \    let fact = fix (fun f n -> if n = 0 then 1 else n * f (n - 1));;\n\
       \    let result = fact 5;;\n\
       \  ");
  [%expect
    {|
    val fact : int -> int
    val fix : forall ... (('_a3 -> '_a6) -> '_a3 -> '_a6) -> '_a3 -> '_a6
    val result : int |}]
;;
