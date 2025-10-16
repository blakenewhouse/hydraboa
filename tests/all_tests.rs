mod infra;

// Your tests go here!
success_tests! {
    T12: "12",
    T37: "37",
    Tadd: "72",
    B_0: "0",
    B_1: "1",
    B_2: "2",
    B_3: "3",
    B_4: "4",
    B_5: "5",
}

failure_tests! {
    B_InvalidLet: "Invalid: Need to have format:",
    B_BadGrammar: "Invalid: Must provide an expression",
    B_InvalidPlus: "Invalid number of arguments for +",
    B_InvalidLet1: "Unbound variable indentifier x",
    B_InvalidLet2: "Invalid: No binding provided",
    B_InvalidLet3: "Invalid binding provided: Binding must be a ((String i32))",
    B_InvalidLet4: "Invalid binding: name cannot contain add1",
    B_InvalidLet5: "Unbound variable indentifier y",
}


// You don't need to worry about the REPL tests for now, so I commented them out.
// repl_tests! {
//     simple_numbers: ["42", "0", "-17"] => ["42", "0", "-17"],
// }