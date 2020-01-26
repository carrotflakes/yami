use std::collections::HashMap;
use crate::core::{
    Node,
    Store,
    VM,
    QNode,
    Inst
};
pub use gluten::{
    R, Val, r, Symbol, MyFn,
    Reader,
    reader::default_atom_reader,
    eval, Env,
    StringPool
};

fn atom_reader(sp: &mut StringPool, s: &str) -> Result<Val, String> {
    if let Ok(v) = s.parse::<i32>() {
        return Ok(r(v));
    }
    if let Ok(v) = s.parse::<f64>() {
        return Ok(r(v));
    }
    if s == "?" {
        return Ok(r(QNode::Variable));
    }
    if s.starts_with("?") {
        return Ok(r(QNode::BoundVariable(s[1..].parse().unwrap())));
    }
    if s.starts_with(":") {
        return Ok(r(QNode::Node(Node::Symbol(s[1..].parse().unwrap()))));
    }
    default_atom_reader(sp, s)
}

pub fn make_reader() -> Reader {
    Reader::new(Box::new(atom_reader))
}

// pub fn make_env<'a>(reader: &mut Reader, store: &'a mut Store) -> Env {
//     let mut env = Env::new();
//     fn qn(val: &Val) -> QNode {
//         let val = val.borrow();
//         if let Some(s) = val.downcast_ref::<String>() {
//             QNode::UninternedString(s.clone())
//         } else if let Some(qn) = val.downcast_ref::<QNode>() {
//             qn.clone()
//         } else {
//             panic!();
//         }
//     }
//     env.insert(reader.intern("find"), r(Box::new(|vec: Vec<Val>| {
//         r(Inst::Find(qn(&vec[0]), qn(&vec[1]), qn(&vec[2]),
//             Box::new(clone_as::<Inst>(&vec[3]).unwrap())))
//     }) as MyFn));
//     env.insert(reader.intern("add"), r(Box::new(|vec: Vec<Val>| {
//         r(Inst::Add(qn(&vec[0]), qn(&vec[1]), qn(&vec[2])))
//     }) as MyFn));
//     env.insert(reader.intern("rm"), r(Box::new(|vec: Vec<Val>| {
//         r(Inst::Rm(qn(&vec[0]), qn(&vec[1]), qn(&vec[2])))
//     }) as MyFn));
//     env.insert(reader.intern("rm"), r(Box::new(|vec: Vec<Val>| {
//         r(Inst::Rm(qn(&vec[0]), qn(&vec[1]), qn(&vec[2])))
//     }) as MyFn));
//     // sym and
//     env
// }
//
// pub fn clone_as<T: 'static + Clone>(val: &Val) -> Option<T> {
//     Some(val.borrow().downcast_ref::<T>()?.clone())
// }

pub fn instize(store: &mut Store, bindings: &mut (Vec<Symbol>, HashMap<Symbol, QNode>), ast: Val) -> Inst {
    if let Some(vec) = ast.borrow().downcast_ref::<Vec<Val>>() {
        if let Some(sym) = vec[0].borrow().downcast_ref::<Symbol>() {
            return match sym.0.as_str() {
                "find" => {
                    let mut bindings = (bindings.0.clone(), bindings.1.clone());
                    Inst::Find(
                        qn(store, &mut bindings, vec[1].clone()), 
                        qn(store, &mut bindings, vec[2].clone()), 
                        qn(store, &mut bindings, vec[3].clone()),
                        Box::new(instize(store, &mut bindings, vec[4].clone())))
                }
                "add" => {
                    Inst::Add(
                        qn(store, bindings, vec[1].clone()), 
                        qn(store, bindings, vec[2].clone()), 
                        qn(store, bindings, vec[3].clone()))
                }
                "rm" => {
                    Inst::Rm(
                        qn(store, bindings, vec[1].clone()), 
                        qn(store, bindings, vec[2].clone()), 
                        qn(store, bindings, vec[3].clone()))
                }
                "sym" => {
                    let mut bindings = (bindings.0.clone(), bindings.1.clone());
                    let symbol = vec[1].borrow().downcast_ref::<Symbol>().unwrap().clone();
                    // TODO: shdowing
                    bindings.1.insert(symbol, QNode::Node(store.new_symbol()));
                    Inst::Sym(Box::new(instize(store, &mut bindings, vec[2].clone())))
                }
                "and" => {
                    let mut inst = instize(store, bindings, vec[1].clone());
                    for i in 2..vec.len() {
                        inst = Inst::And(Box::new(inst), Box::new(instize(store, bindings, vec[i].clone())));
                    }
                    inst
                }
                "print" => {
                    if let QNode::BoundVariable(i) = qn(store, bindings, vec[1].clone()) {
                        Inst::Print(i)
                    } else {
                        panic!();
                    }
                }
                _ => {
                    panic!("what: {:?}", sym);
                }
            }
        }
    }
    panic!("invalid ast!");
}

fn qn(store: &mut Store, bindings: &mut (Vec<Symbol>, HashMap<Symbol, QNode>), val: Val) -> QNode {
    if let Some(s) = val.borrow().downcast_ref::<String>() {
        QNode::UninternedString(s.clone())
    } else if let Some(s) = val.borrow().downcast_ref::<Symbol>() {
        if let Some(qn) = bindings.1.get(&s) {
            qn.clone()
        } else {
            bindings.1.insert(s.clone(), QNode::BoundVariable(bindings.0.len()));
            bindings.0.push(s.clone());
            QNode::Variable
        }
    } else if let Some(qn) = val.borrow().downcast_ref::<QNode>() {
        qn.clone()
    } else {
        panic!();
    }
}
