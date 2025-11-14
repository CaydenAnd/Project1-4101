open OUnit2
open Interp
open Ast
open Main

let make_i n i s =
  n >:: (fun _ -> assert_equal (Int i) (interp s))

let make_b n b s =
  n >:: (fun _ -> assert_equal (Bool b) (interp s))

let make_f n f s =
  n >:: (fun _ -> assert_equal (Float f) (interp s))

let make_t n s' s =
  n >:: (fun _ -> assert_raises (Failure s') (fun () -> interp s))

let tests = [
  make_f "custom_mul_add_f" 11.5 "1.5+.2.5*.4.0";
  make_f "custom_paren_mul_f" 16.0 "(1.5+.2.5)*.4.0";
  make_b "custom_leq_expr_f" true "2.5+.2.5<=5.0";
  make_t "custom_let_int_float_expr_err" annotation_err "let x : int = 1.0+.1.0 in x";
  make_t "invalid_geq_float_bool" bop_err "1.0>=true";
]
