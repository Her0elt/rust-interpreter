use std::io::BufRead;

use anyhow::Result;
use rust_interpreter::repl::Repl;

const PROMPT: &'static str = ">>";

fn main() -> Result<()> {
    let repl = Repl::new();

    let stdin = std::io::stdin();

    loop {
        println!("{}", PROMPT);
        if let Some(Ok(ref line)) = stdin.lock().lines().next() {
            println!("{:?}", repl.line(line));
        }
    }
}
