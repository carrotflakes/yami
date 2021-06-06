use crate::core::VM;

pub mod core;
mod pool;
pub mod q;
pub mod script;
pub mod serialize;
pub mod store;

pub fn interact(store: &mut store::Store) {
    let mut reader = script::make_reader();
    let mut buf = String::new();
    while let Ok(_) = std::io::stdin().read_line(&mut buf) {
        if buf.trim() == "exit" {
            break;
        }
        let ast = reader.parse(&buf).unwrap();
        let inst = script::instize(&mut Default::default(), ast);

        VM::new(store, &mut |n| println!("{}", n)).run(&inst);

        buf.clear();
    }
}
