

fn fib(n: u32) -> u32 {
    if (n < 2) {
        return 1;
    } else {
        return fib(n-1) + fib(n-2);
    }
}


fn main() {
    println!("fib(10) is {}", fib(10_u32));
}








