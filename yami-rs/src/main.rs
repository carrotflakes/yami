mod core;
use crate::core::{
    Node,
    Store,
    Edge,
    VM,
    QNode,
    Inst
};

fn qn(str: &str) -> QNode {
    if str == "?" {
        QNode::Variable
    } else if str.starts_with("?") {
        QNode::BoundVariable(str[1..].parse().unwrap())
    } else if str.starts_with(":") {
        QNode::Node(Node::Symbol(str[1..].parse().unwrap()))
    } else {
        QNode::Node(Node::String(str.to_owned()))
    }
}

fn q_find(str: &str, then: Inst) -> Inst {
    let v: Vec<&str> = str.split(" ").collect();
    Inst::Find(qn(v[0]), qn(v[1]), qn(v[2]), Box::new(then))
}

fn q_add(str: &str) -> Inst {
    let v: Vec<&str> = str.split(" ").collect();
    Inst::Add(qn(v[0]), qn(v[1]), qn(v[2]))
}

fn q_rm(str: &str) -> Inst {
    let v: Vec<&str> = str.split(" ").collect();
    Inst::Rm(qn(v[0]), qn(v[1]), qn(v[2]))
}

fn q_and(is: &[Inst]) -> Inst {
    let mut inst = is[0].clone();
    for i in 1..is.len() {
        inst = Inst::And(Box::new(inst), Box::new(is[i].clone()));
    }
    inst
}

fn q_print(i: usize) -> Inst {
    Inst::Print(i)
}

fn main() {
    let mut store = Store::new();

    // store.push(Edge(Node::String("a".to_owned()), Node::String("b".to_owned()), Node::String("c".to_owned())));
    // let s = store.new_symbol();
    // store.push(Edge(Node::String("a".to_owned()), Node::String("b".to_owned()), s));
    // store.print();

    let mut vm = VM::new(store);
    vm.run(
        &q_and(&[
            q_add("a b c"),
            q_add("a b :1"),
            q_add("x x d"),
            q_find(
                "a ? ?",
                q_and(&[
                    q_print(0),
                    q_print(1)]
                )
            ),
            q_find(
                "? ?0 ?",
                q_print(1)
            )
        ])
    );
}
