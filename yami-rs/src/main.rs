mod pool;
mod core;
use crate::core::{
    Node,
    Store,
    VM,
    QNode,
    Inst
};

struct Q<'a>(&'a mut Store);

impl<'a> Q<'a> {
    fn new(store: &'a mut Store) -> Q {
        Q(store)
    }

    fn n(&mut self, str: &str) -> QNode {
        if str == "?" {
            QNode::Variable
        } else if str.starts_with("?") {
            QNode::BoundVariable(str[1..].parse().unwrap())
        } else if str.starts_with(":") {
            QNode::Node(Node::Symbol(str[1..].parse().unwrap()))
        } else {
            QNode::Node(self.0.new_string(str))
        }
    }

    fn find(&mut self, str: &str, then: Inst) -> Inst {
        let v: Vec<&str> = str.split(" ").collect();
        Inst::Find(self.n(v[0]), self.n(v[1]), self.n(v[2]), Box::new(then))
    }
    
    fn add(&mut self, str: &str) -> Inst {
        let v: Vec<&str> = str.split(" ").collect();
        Inst::Add(self.n(v[0]), self.n(v[1]), self.n(v[2]))
    }
    
    fn rm(&mut self, str: &str) -> Inst {
        let v: Vec<&str> = str.split(" ").collect();
        Inst::Rm(self.n(v[0]), self.n(v[1]), self.n(v[2]))
    }
    
    fn sym(then: Inst) -> Inst {
        Inst::Sym(Box::new(then))
    }

    fn and(is: &[Inst]) -> Inst {
        let mut inst = is[0].clone();
        for i in 1..is.len() {
            inst = Inst::And(Box::new(inst), Box::new(is[i].clone()));
        }
        inst
    }
    
    fn print(i: usize) -> Inst {
        Inst::Print(i)
    }
}



fn main() {
    let mut store = Store::new();

    // store.push(Edge(Node::String("a".to_owned()), Node::String("b".to_owned()), Node::String("c".to_owned())));
    // let s = store.new_symbol();
    // store.push(Edge(Node::String("a".to_owned()), Node::String("b".to_owned()), s));
    // store.print();

    let mut q = Q::new(&mut store);
    let inst = Q::and(&[
        q.add("a b c"),
        Q::sym(q.add("a b ?0")),
        q.add("x x d"),
        q.find(
            "a ? ?",
            Q::and(&[
                Q::print(0),
                Q::print(1)]
            )
        ),
        q.find(
            "? ?0 ?",
            Q::print(1)
        )
    ]);
    let mut vm = VM::new(store);
    vm.run(&inst);
}
