use crate::pool::InternedString;
pub use crate::store::Store;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Symbol(u64),
    String(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Edge(pub Node, pub Node, pub Node);

#[derive(Debug, Clone)]
pub enum QNode {
    Node(Node),
    UninternedString(String),
    Variable,
    BoundVariable(usize),
}

#[derive(Debug, Clone)]
pub enum Inst {
    Find(QNode, QNode, QNode, Box<Inst>),
    Add(QNode, QNode, QNode),
    Rm(QNode, QNode, QNode),
    Sym(Box<Inst>),
    // GuardEq GuardNe
    And(Box<Inst>, Box<Inst>),
    Print(QNode),
}

pub struct VM<'a> {
    store: &'a mut Store,
    bindings: Vec<Node>,
}

impl<'a> VM<'a> {
    pub fn new(store: &'a mut Store) -> Self {
        VM {
            store,
            bindings: Vec::new(),
        }
    }

    pub fn run(&mut self, inst: &Inst) {
        match inst.clone() {
            Inst::Find(qn0, qn1, qn2, then) => {
                let qn0 = self.ensure_intern(qn0);
                let qn1 = self.ensure_intern(qn1);
                let qn2 = self.ensure_intern(qn2);
                let len = self.bindings.len();
                let edges: Vec<Edge> = match (&qn0, &qn1, &qn2) {
                    (QNode::Node(n0), QNode::Node(n1), QNode::Node(n2)) => self
                        .store
                        .find_if(|edge| &edge.0 == n0 && &edge.1 == n1 && &edge.2 == n2)
                        .map(|edge| edge.clone())
                        .collect(),
                    (QNode::Node(n0), QNode::Node(n1), _) => self
                        .store
                        .find_if(|edge| &edge.0 == n0 && &edge.1 == n1)
                        .map(|edge| edge.clone())
                        .collect(),
                    (QNode::Node(n0), _, QNode::Node(n2)) => self
                        .store
                        .find_if(|edge| &edge.0 == n0 && &edge.2 == n2)
                        .map(|edge| edge.clone())
                        .collect(),
                    (QNode::Node(n0), _, _) => self
                        .store
                        .find_if(|edge| &edge.0 == n0)
                        .map(|edge| edge.clone())
                        .collect(),
                    (_, QNode::Node(n1), QNode::Node(n2)) => self
                        .store
                        .find_if(|edge| &edge.1 == n1 && &edge.2 == n2)
                        .map(|edge| edge.clone())
                        .collect(),
                    (_, QNode::Node(n1), _) => self
                        .store
                        .find_if(|edge| &edge.1 == n1)
                        .map(|edge| edge.clone())
                        .collect(),
                    (_, _, QNode::Node(n2)) => self
                        .store
                        .find_if(|edge| &edge.2 == n2)
                        .map(|edge| edge.clone())
                        .collect(),
                    (_, _, _) => self
                        .store
                        .find_if(|_| true)
                        .map(|edge| edge.clone())
                        .collect(),
                };
                for edge in edges {
                    match qn0 {
                        QNode::Variable => self.bindings.push(edge.0.clone()),
                        QNode::BoundVariable(i) if self.bindings[i as usize] != edge.0 => {
                            self.bindings.resize(len, Node::Symbol(0));
                            continue;
                        }
                        _ => {}
                    }
                    match qn1 {
                        QNode::Variable => self.bindings.push(edge.1.clone()),
                        QNode::BoundVariable(i) if self.bindings[i as usize] != edge.1 => {
                            self.bindings.resize(len, Node::Symbol(0));
                            continue;
                        }
                        _ => {}
                    }
                    match qn2 {
                        QNode::Variable => self.bindings.push(edge.2.clone()),
                        QNode::BoundVariable(i) if self.bindings[i as usize] != edge.2 => {
                            self.bindings.resize(len, Node::Symbol(0));
                            continue;
                        }
                        _ => {}
                    }
                    self.run(then.as_ref());
                    self.bindings.resize(len, Node::Symbol(0));
                }
            }
            Inst::Add(qn0, qn1, qn2) => {
                let n0 = self.resolve(qn0);
                let n1 = self.resolve(qn1);
                let n2 = self.resolve(qn2);
                self.store.push(Edge(n0, n1, n2));
            }
            Inst::Rm(qn0, qn1, qn2) => {
                // TODO: enable variable?
                let n0 = self.resolve(qn0);
                let n1 = self.resolve(qn1);
                let n2 = self.resolve(qn2);
                self.store.remove(&Edge(n0, n1, n2));
            }
            Inst::Sym(then) => {
                self.bindings.push(self.store.new_symbol());
                self.run(then.as_ref());
                self.bindings.pop();
            }
            Inst::And(left, right) => {
                self.run(left.as_ref());
                self.run(right.as_ref());
            }
            Inst::Print(qn) => {
                println!("{:?}", self.resolve(qn));
            }
        }
    }

    fn ensure_intern(&mut self, qnode: QNode) -> QNode {
        if let QNode::UninternedString(ref s) = qnode {
            QNode::Node(self.store.new_string(s))
        } else {
            qnode
        }
    }

    fn resolve(&mut self, qnode: QNode) -> Node {
        match self.ensure_intern(qnode) {
            QNode::Node(n) => n,
            QNode::UninternedString(_) => unreachable!(),
            QNode::Variable => panic!("cannot resolve for QNode::Variable"),
            QNode::BoundVariable(i) => self.bindings[i].clone(),
        }
    }
}
