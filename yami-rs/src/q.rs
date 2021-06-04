use crate::core::{Inst, Node, QNode, Store};
pub struct Q<'a>(&'a mut Store);

impl<'a> Q<'a> {
    pub fn new(store: &'a mut Store) -> Q {
        Q(store)
    }

    pub fn n(&mut self, str: &str) -> QNode {
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

    pub fn find(&mut self, str: &str, then: Inst) -> Inst {
        let v: Vec<&str> = str.split(" ").collect();
        Inst::Find(self.n(v[0]), self.n(v[1]), self.n(v[2]), Box::new(then))
    }

    pub fn add(&mut self, str: &str) -> Inst {
        let v: Vec<&str> = str.split(" ").collect();
        Inst::Add(self.n(v[0]), self.n(v[1]), self.n(v[2]))
    }

    pub fn rm(&mut self, str: &str) -> Inst {
        let v: Vec<&str> = str.split(" ").collect();
        Inst::Rm(self.n(v[0]), self.n(v[1]), self.n(v[2]))
    }

    pub fn sym(then: Inst) -> Inst {
        Inst::Sym(Box::new(then))
    }

    pub fn and(is: &[Inst]) -> Inst {
        let mut inst = is[0].clone();
        for i in 1..is.len() {
            inst = Inst::And(Box::new(inst), Box::new(is[i].clone()));
        }
        inst
    }

    pub fn print(i: usize) -> Inst {
        Inst::Print(QNode::BoundVariable(i))
    }
}
