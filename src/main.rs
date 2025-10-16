use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::mem;
use im::HashMap;

use sexp::*;
use sexp::Atom::*;

use dynasmrt::{dynasm, DynasmApi};
/*
/// Compile a source program into a string of x86-64 assembly
fn compile(program: String) -> String {
    let num = program.trim().parse::<i32>().unwrap();
    return format!("mov rax, {}", num);
}*/



enum Reg {
    Rax,
    //Rbx,
    Rcx,
    /*Rdx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,*/
}

enum Val {
    Reg(Reg),
    I32(i32),
}

enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    MovFromStack(Val, i32),
    MovToStack(Val, i32)
}

enum Op1 {
    Add1,
    Sub1
}

enum Op2 {
    Plus,
    Minus,
    Times,
}

enum Expr {
    Num(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

enum ReplEntry {
    Define(String, Box<Expr>),
    Expr(Box<Expr>),
    Exit(),
}

fn parse_expr(s: &Sexp) -> Expr {
    let reserved = ["let", "add1", "sub1", "neg", "+", "-", "*"];

    match s {
        Sexp::Atom(atom) => match atom {
            S(sym) => {
                if reserved.contains(&sym.as_str()) {
                    panic!("Invalid binding: name cannot contain {}", &sym.as_str());
                }
                Expr::Id(sym.clone())
            }

            I(n) => {
                let v = i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"));
                Expr::Num(v)
            }
            F(_) => panic!("Invalid: No Floats"),
        },

        Sexp::List(items) => {
            if items.is_empty() {
                panic!("Invalid: Must provide an expression");
            }

            match &items[0] {
                Sexp::Atom(S(op)) => match op.as_str() {

                    "add1" | "sub1" => {
                        if items.len() != 2 { panic!("Invalid number of arguments for {}", op.as_str()); }
                        let op1 = match op.as_str() {
                            "add1" => Op1::Add1,
                            "sub1" => Op1::Sub1,
                            _ => unreachable!(),
                        };
                        Expr::UnOp(op1, Box::new(parse_expr(&items[1])))
                    }

                    "+" | "-" | "*" => {
                        if items.len() != 3 { panic!("Invalid number of arguments for {}", op.as_str()); }
                        let op2 = match op.as_str() {
                            "+" => Op2::Plus,
                            "-" => Op2::Minus,
                            "*" => Op2::Times,
                            _ => unreachable!(),
                        };
                        Expr::BinOp(op2, Box::new(parse_expr(&items[1])), Box::new(parse_expr(&items[2])))
                    }

                    "let" => {
                        if items.len() != 3 { panic!("Invalid number of arguments for {}", op.as_str()); }
                        match &items[1] {
                            Sexp::List(bindings) => {
                                let mut binds: Vec<(String, Expr)> = Vec::new();
                                for b in bindings {
                                    match b {
                                        Sexp::List(pair) => {
                                            if pair.len() != 2 { panic!("Invalid: No binding provided"); }
                                            match &pair[0] {
                                                Sexp::Atom(S(id)) => {
                                                    if reserved.contains(&id.as_str()) { panic!("Invalid binding: name cannot contain {}", &id.as_str()); }
                                                    binds.push((id.clone(), parse_expr(&pair[1])));
                                                }
                                                _ => panic!("Invalid binding provided: Binding must be a ((String i32))"),
                                            }
                                        }
                                        _ => panic!("Invalid: Need to have format: ((bind_name value) (bind_name value) ...)"),
                                    }
                                }
                                Expr::Let(binds, Box::new(parse_expr(&items[2])))
                            }
                            _ => panic!("Invalid: No bindings provided"),
                        }
                    }

                    _ => panic!("Invalid: No expression provided"),
                },

                _ => panic!("Invalid"),
            }
        }
    }
}

fn parse_repl_entry(s: &Sexp) -> ReplEntry {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), Sexp::Atom(S(var)), expr] if op == "define" => {
                    ReplEntry::Define(var.to_string(), Box::new(parse_expr(expr)))
                }
                [Sexp::Atom(S(op))] if (op == "exit" || op == "quit") => ReplEntry::Exit(),
                _ => ReplEntry::Expr(Box::new(parse_expr(s))),
            }
        }
        Sexp::Atom(S(op)) if (op == "exit" || op == "quit") => ReplEntry::Exit(),
        _ => ReplEntry::Expr(Box::new(parse_expr(s)))
    }
}

fn val_to_str(val: &Val) -> String {
    match val {
        Val::Reg(r) => match r {
            Reg::Rax => "rax".to_string(),
            //Reg::Rbx => "rbx".to_string(),
            Reg::Rcx => "rcx".to_string(),
            /*Reg::Rdx => "rdx".to_string(),
            Reg::Rsi => "rsi".to_string(),
            Reg::Rdi => "rdi".to_string(),
            Reg::Rsp => "rsp".to_string(),
            Reg::Rbp => "rbp".to_string(),*/
        }
        Val::I32(n) => n.to_string(),
  }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(reg, val) => format!("mov {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::IAdd(reg, val) => format!("add {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::ISub(reg, val) => format!("sub {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::IMul(reg, val) => format!("imul {}, {}", val_to_str(reg), val_to_str(val)),
        Instr::MovFromStack(reg, offset ) => format!("mov {}, [rsp - {}]", val_to_str(reg), offset),
        Instr::MovToStack(reg, offset ) => format!("mov [rsp - {}], {}", offset, val_to_str(reg))
    }
}

fn instr_to_asm(i: &Instr, ops: &mut dynasmrt::x64::Assembler) {
    match i {
        Instr::IMov(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rax, rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; mov rcx, rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; mov rcx, rcx),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; mov rax, *n),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; mov rcx, *n),
            _ => panic!("Unsupported IMov combination"),
        },

        Instr::IAdd(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; add rax, rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; add rax, rcx),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; add rax, *n),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; add rcx, rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; add rcx, rcx),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; add rcx, *n),
            _ => panic!("Unsupported IAdd combination"),
        },

        Instr::ISub(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; sub rax, rax),
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; sub rax, rcx),
            (Val::Reg(Reg::Rax), Val::I32(n)) => dynasm!(ops ; .arch x64 ; sub rax, *n),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; sub rcx, rax),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; sub rcx, rcx),
            (Val::Reg(Reg::Rcx), Val::I32(n)) => dynasm!(ops ; .arch x64 ; sub rcx, *n),
            _ => panic!("Unsupported ISub combination"),
        },

        Instr::IMul(dst, src) => match (dst, src) {
            (Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)) => dynasm!(ops ; .arch x64 ; imul rax, rcx),
            (Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)) => dynasm!(ops ; .arch x64 ; imul rcx, rax),
            (Val::Reg(Reg::Rax), Val::I32(n)) => {
                // move immediate into RCX then imul RAX, RCX
                dynasm!(ops ; .arch x64 ; mov rcx, *n ; imul rax, rcx);
            }
            (Val::Reg(Reg::Rcx), Val::I32(n)) => {
                // move immediate into RAX then imul RCX, RAX
                dynasm!(ops ; .arch x64 ; mov rax, *n ; imul rcx, rax);
            }
            _ => panic!("Unsupported IMul combination"),
        },

        Instr::MovFromStack(dst, offset) => match dst {
            Val::Reg(Reg::Rax) => dynasm!(ops ; .arch x64 ; mov rax, [rsp - *offset]),
            Val::Reg(Reg::Rcx) => dynasm!(ops ; .arch x64 ; mov rcx, [rsp - *offset]),
            _ => panic!("Unsupported MovFromStack destination"),
        },

        Instr::MovToStack(src, offset) => match src {
            Val::Reg(Reg::Rax) => dynasm!(ops ; .arch x64 ; mov [rsp - *offset], rax),
            Val::Reg(Reg::Rcx) => dynasm!(ops ; .arch x64 ; mov [rsp - *offset], rcx),
            _ => panic!("Unsupported MovToStack source"),
        },
    }
}

fn compile_to_instrs(e: &Expr, stack_buff: i32, env: &im::HashMap<String, i32>, define_env: &im::HashMap<String, i32>) -> Vec<Instr> {
    match e {
        Expr::Num(n) => vec![Instr::IMov(Val::Reg(Reg::Rax), Val::I32(*n))],
        Expr::Id(s) => {
            match env.get(s) {
                Some(offset) => vec![Instr::MovFromStack(Val::Reg(Reg::Rax), *offset)],
                _ => {
                    match define_env.get(s) {
                        Some(value) => vec![Instr::IMov(Val::Reg(Reg::Rax), Val::I32(*value))],
                        _ => panic!("Unbound variable indentifier {}", s),
                    }
                }
            }
        },
        Expr::Let(bindings, body_expr) => {
            let mut env2 = env.clone();
            let mut instr_vec:Vec<Instr> = Vec::new();
            let mut dup_vec:Vec<String> = Vec::new();
            let mut i = 0;
            for (name, bind_expr) in bindings {
                if dup_vec.contains(name) {
                    println!("ERROR | Duplicate binding: {}", name);
                    break;
                }
                instr_vec.extend(compile_to_instrs(bind_expr, stack_buff + i, &env2, &define_env));
                instr_vec.push(Instr::MovToStack(Val::Reg(Reg::Rax), stack_buff + i)); 
                env2.insert(name.clone(), stack_buff + i);
                dup_vec.push(name.clone());
                i += 8;
            }
            instr_vec.extend(compile_to_instrs(body_expr, stack_buff + i, &env2, &define_env));
            instr_vec
            
        },
        Expr::UnOp(op, subexpr) => {
            let mut instr_vec: Vec<Instr> = compile_to_instrs(subexpr, stack_buff, env, &define_env);
            match op {
                Op1::Add1 => instr_vec.push(Instr::IAdd(Val::Reg(Reg::Rax), Val::I32(1))),
                Op1::Sub1 => instr_vec.push(Instr::ISub(Val::Reg(Reg::Rax), Val::I32(1))),
            }
            instr_vec
        }
        Expr::BinOp(op,left ,right ) => {
            let mut instr_vec: Vec<Instr> = compile_to_instrs(left, stack_buff, env, &define_env);
            instr_vec.push(Instr::MovToStack(Val::Reg(Reg::Rax), stack_buff));
            instr_vec.extend(compile_to_instrs(right, stack_buff + 8, env, &define_env));
            instr_vec.push(Instr::MovFromStack(Val::Reg(Reg::Rcx), stack_buff));
            match op {
                Op2::Plus => instr_vec.push(Instr::IAdd(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx))),
                Op2::Minus => {
                    instr_vec.push(Instr::ISub(Val::Reg(Reg::Rcx), Val::Reg(Reg::Rax)));
                    instr_vec.push(Instr::IMov(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx)));
                }
                Op2::Times => instr_vec.push(Instr::IMul(Val::Reg(Reg::Rax), Val::Reg(Reg::Rcx))),
            }
            instr_vec
        }
    }
}

fn compile(e: &Expr) -> String {
    let hash:HashMap<String, i32> = HashMap::new();
    let define_hash:HashMap<String, i32> = HashMap::new();
    let instr_vec = compile_to_instrs(e, 16, &hash, &define_hash);
    let mut res = String::new();
    for instr in instr_vec {
        res.push_str(&instr_to_str(&instr));
        res.push_str("\n");
    }
    res
}


/* 
fn compile_expr(e: &Expr) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n),
        Expr::Add1(subexpr) => compile_expr(subexpr) + "\nadd rax, 1",
        Expr::Sub1(subexpr) => compile_expr(subexpr) + "\nsub rax, 1",
        Expr::Neg(subexpr) => compile_expr(subexpr) + "\nneg rax",
    }
}
*/

fn compile_ops(e : &Expr, ops : &mut dynasmrt::x64::Assembler, define_env: &HashMap<String, i32>) {
    let env: HashMap<String, i32> = HashMap::new();
    let instrs = compile_to_instrs(e, 16, &env, define_env);

    // Emit the compiled instructions
    for instr in instrs {
        instr_to_asm(&instr, ops);
    }
}

fn eval_snek(source: &str) -> Result<i32, String> {  
    eval_snek_with_define(source, &HashMap::new())
}

fn eval_snek_with_define(source: &str, define_env: &HashMap<String, i32>) -> Result<i32, String> {
    // Parse the source code
    let sexp = parse(source).map_err(|e| format!("Parse error: {}", e))?;
    let expr = parse_expr(&sexp);
    eval_jit_with_define(&expr, define_env)
}


fn eval_jit_with_define(expr: &Expr, define_env: &HashMap<String, i32>) -> Result<i32, String> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();

    compile_ops(&expr, &mut ops, define_env);

    dynasm!(ops ; .arch x64 ; ret);
    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i32 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn();
    
    Ok(result)
}

fn interactive_env() -> std::io::Result<()> {
    println!("\nWelcome to the snek REPL! Type \"exit\" to quit.\n");

    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut define_env: HashMap<String, i32> = HashMap::new();

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        match reader.read_line(&mut input) {
            Ok(0) => {
                println!("\nSee you next time!");
                std::process::exit(1);
            }
            Ok(_) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }

                match parse(input) {
                    Ok(sexp) => {
                        match std::panic::catch_unwind(|| parse_repl_entry(&sexp)) {
                            Ok(ReplEntry::Define(var, expr)) => {
                                match std::panic::catch_unwind(|| eval_jit_with_define(&expr, &define_env)) {
                                    Ok(Ok(value)) => {
                                        if define_env.contains_key(&var) {
                                            println!("ERROR | Duplicate define binding: {}", var)
                                        }
                                        else {
                                            define_env.insert(var.clone(), value);
                                            println!("Defined {} = {}", var, value);
                                        }
                                    },
                                    Ok(Err(e)) => println!("{}", e),
                                    Err(_) => println!("Invalid define")
                                }
                            },
                            Ok(ReplEntry::Exit()) => {
                                println!("\nSee you next time!");
                                std::process::exit(0);
                            }
                            Ok(ReplEntry::Expr(expr)) => {
                                // For expressions, evaluate with current define environment
                                match std::panic::catch_unwind(|| eval_jit_with_define(&expr, &define_env)) {
                                    Ok(Ok(result)) => println!("{}", result),
                                    Ok(Err(e)) => println!("{}", e),
                                    Err(_) => println!("Invalid Expression: could not evaluate")
                                }
                            }
                            Err(e) => {
                                println!("Compile error: {:?}", e);
                            }
                        }
                    }
                    Err(_) => {
                        println!("Invalid: Parse error");
                    }
                }
            }
            Err(e) => {
                println!("Error reading input: {}", e);
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        panic!("Invalid Flag - Usage: program (-c|-e|-g|-i) <in.snek> <out.asm>");
    }

    let flag = &args[1];

    if flag == "-i" {
        if args.len() > 2 {
            panic!("Invalid arguments for \"-i\" - The interactive environment flag takes no arguments");
        }
        match interactive_env() {
            Err(e) => eprintln!("Error: {}", e),
            _ => return Ok(())
        }
    }

    let in_name = &args[2];
    
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let result= compile(&expr);

    let asm_program = format!("
section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
", result);

    match flag.as_str() {
        "-c" => {
            if args.len() != 4 {
                panic!("Invalid arguments for \"-c\" - The AOT compiler flag requires 4 arguments: program -c <in.snek> <out.asm>");
            }
            // AOT: write assembly and exit
            let out_name = &args[3];
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
            println!("Wrote assembly to {}", out_name);
            return Ok(());
        }
        "-g" => {
            if args.len() != 4 {
                panic!("Invalid arguments for \"-g\" - The combined compiler flag requires 4 arguments: program -g <in.snek> <out.asm>");
            }
            let out_name = &args[3];
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
        }
        "-e" => {
            if args.len() != 3 {
                panic!("Invalid arguments for \"-e\" - The JIT compiler flag requires 3 arguments: program -e <in.snek>");
            }
            match eval_snek(&in_contents) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        _ => panic!("Invalid Flag - Usage: program (-c|-e|-g|-i) <in.snek> <out.asm>"),
    }
    
    
    
    Ok(())
}

