use std::string::*;
use std::io::*;
use std::collections::HashMap;
use std::result::Result;


/*******************************************************************/
/**********************  data structures ***************************/
/*******************************************************************/


enum PrimOp {
    Right,
    Left,
    Inc,
    Dec,
    Inp,
    Print,
    Nothing,
}

struct BTape {
    pos: u32,
    tape: HashMap<u32,i32>
}


/*******************************************************************/
/*****************************   main    ***************************/
/*******************************************************************/




fn main(){
    print_greeting();
    let mut btape: BTape = BTape {pos: 0_u32, tape:HashMap::new()};
    loop {
        let mut stack = Vec::new();
        let curr_line: String = get_line();
        //curr_line = get_line();
        bf_engine(&curr_line, &mut btape, &mut stack);
        //stack.clear();
    }
}




/*******************************************************************/
/*********************   core functions        *********************/
/*******************************************************************/


fn bf_engine<'a>( cmd: &'a str
                 , btape: &mut BTape
                 , stack: & mut Vec<&'a str>) -> &'a str {
    if cmd.len() == 0 {
        return cmd;
    } else {
        use std::usize;
        let curr_char = cmd.chars().next().unwrap();
        match curr_char {
            '+' | '-' | '.' | ',' | '>' | '<' => {
                let op = to_primop(curr_char);
                do_primop(btape, op);
                bf_engine(&cmd[1..], btape, stack);
            },
            '[' => {
                stack.push(&cmd[1..]);
                bf_engine(&cmd[1..], btape, stack);
            },
            ']' => {
                if is_curr_zero(btape) {
                    stack.pop();
                    bf_engine(&cmd[1..], btape, stack);
                    return cmd;
                } else {
                    if (stack.len() == 0){
                        panic!("Unbalanced loops.");
                    } else {
                        bf_engine(stack[0], btape, stack);
                    }
                };
            },
            _ => (),
        };
        return cmd;
    }
}


fn is_curr_zero(btape: &BTape) -> bool {
    match btape.tape.get(&btape.pos) {
        Some(i) => *i == 0_i32,
        None => true,
    }
}


fn print_greeting() {
    println!("Hello! Welcome to brainf.\n")
}


fn get_line() -> String {
    let mut a_str: String = String::new();
    print!("brain$ ");
    stdout().flush();
    stdin().read_line(&mut a_str);
    return a_str;
}


fn to_primop(c:char) -> PrimOp {
    use crate::PrimOp::*;
    match c {
        '+' => Inc,
        '-' => Dec,
        '>' => Right,
        '<' => Left,
        '.' => Print,
        ',' => Inp,
        _   => Nothing,
    }
}


fn do_primop(btape: &mut BTape, op: PrimOp){
    use crate::PrimOp::*;
    match op {
        Right => btape.pos = btape.pos + 1,
        Left =>
            if (btape.pos == 0) {
                panic!("Running off tape start.");
            } else {
                btape.pos = btape.pos -1;
            },
        Inc =>
            match btape.tape.get(&btape.pos) {
                Some(val) => {btape.tape.insert(btape.pos, val + 1);},
                None => {btape.tape.insert(btape.pos, 1);},
            },
        Dec =>
            match btape.tape.get(&btape.pos) {
                Some(val) => {btape.tape.insert(btape.pos, val - 1);} ,
                None => {btape.tape.insert(btape.pos, -1);} ,
            },
        Inp => get_input(btape),
        Print =>
            match btape.tape.get(&btape.pos) {
                Some(val) => println!("{}",val),
                None => println!("{}",0_i32),
            },
        Nothing => (),
    }
}


fn get_input(btape: &mut BTape) {
    print!("Enter Int: ");
    stdout().flush();
    let mut a_str: String = String::new();
    stdin().read_line(&mut a_str);
    match a_str.trim().parse::<i32>() {
        Ok(i) => {btape.tape.insert(btape.pos, i);},
        Err(..) => get_input(btape),
    }
}

