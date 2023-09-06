(* Author: Harshil Vagadia *)
(* Script for evaluating ExpOp package *)

use "rational.sml"; (* load ExpOp package *)

(* Create test cases *)

val list_of_tests = [
    (ExpOp.showDecimal(ExpOp.fromDecimal("1.2(0)")), "1.2(0)"),
    (ExpOp.showDecimal(ExpOp.add(ExpOp.fromDecimal("+100.(0)"), ExpOp.fromDecimal("~200.(0)"))), "~100.(0)"),
    (ExpOp.showDecimal(ExpOp.subtract(ExpOp.fromDecimal("1000.(0)"), ExpOp.fromDecimal("0.00001(0)"))), "999.99999(0)"),
    (ExpOp.showDecimal(ExpOp.multiply(ExpOp.fromDecimal("1000000.(0)"), ExpOp.fromDecimal("1000000.(0)"))), "1000000000000.(0)"),
    (ExpOp.showRat(valOf(ExpOp.divide(ExpOp.fromDecimal("0.000001(0)"), ExpOp.fromDecimal("1000000.(0)")))), "1/1000000000000"),
    (if ExpOp.equal(valOf(ExpOp.inverse(ExpOp.fromDecimal("0.4(3)"))), ExpOp.fromDecimal("3.(0)")) then "1" else "0", "0"),
    (if ExpOp.equal(valOf(ExpOp.inverse(ExpOp.fromDecimal("0.333333(3)"))), ExpOp.fromDecimal("3.00(0000)")) then "1" else "0", "1"),
    (if ExpOp.less(ExpOp.neg(ExpOp.fromDecimal("+999999999999999999.(9)")), ExpOp.fromDecimal("~0.0000000000000000001(0)")) then "1" else "0", "1")
]

fun check_test(test) =
    let
        val (x, y) = test
    in
        if x = y then
            (print("Test passed: " ^ x ^ " = " ^ y ^ "\n"); 1)
        else
            (print("Test failed: " ^ x ^ " != " ^ y ^ "\n"); 0)
    end

(* Run tests *)

val score = foldl (fn (x, y) => x + y) 0 (map check_test list_of_tests)