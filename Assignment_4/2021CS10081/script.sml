fun filesAreIdentical(file1: string, file2: string): bool =
    let
        val f1 = TextIO.openIn file1
        val f2 = TextIO.openIn file2

        fun readContents(stream: TextIO.instream, accum: string) =
            case TextIO.inputLine stream of
                NONE => accum
              | SOME line => readContents(stream, accum ^ line)

        val contents1 = readContents(f1, "")
        val contents2 = readContents(f2, "")

        val identical = (contents1 = contents2)

        val _ = TextIO.closeIn f1
        val _ = TextIO.closeIn f2

    in
        identical
    end;

(* list of (input_file,output_file,correct_output) *)
val tests=[
    ("tests/check.rat","tests/check.output","tests/check_correct.output"),
    ("tests/example.rat","tests/example.output","tests/example_correct.output"),
    ("tests/static_scope.rat","tests/static_scope.output","tests/static_scope_correct.output"),
    ("tests/scope.rat","tests/scope.output","tests/scope_correct.output"),
    ("tests/nested_if.rat","tests/nested_if.output","tests/nested_if_correct.output"),
    ("tests/fibonacci.rat","tests/fibonacci.output","tests/fibonacci_correct.output"),
    ("tests/recursive.rat","tests/recursive.output","tests/recursive_correct.output"),
    ("tests/exp2.rat","tests/exp2.output","tests/exp2_correct.output"),
    ("tests/exp1.rat","tests/exp1.output","tests/exp1_correct.output"),
    ("tests/scope2.rat","tests/scope2.output","tests/scope2_correct.output")
];

fun run_tests(tests) =
    case tests of
        [] => ()
        | (input,output,correct)::rest =>
            let
                val _ = Calc.interpret(input,output)
            in
                if filesAreIdentical(output,correct) then
                    (print("Test " ^ input ^ " passed\n"))
                else
                    (print("Test " ^ input ^ " failed\n"));
                run_tests(rest)
            end;

val _ = run_tests(tests)
(* val _=interpret("tests/exp1.rat","tests/exp1.output") *)
