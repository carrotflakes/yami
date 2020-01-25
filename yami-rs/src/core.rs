use crate::pool::{StringPool, InternedString};

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Symbol(u64),
    String(InternedString)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Edge(pub Node, pub Node, pub Node);

pub struct Store {
    edges: Vec<Edge>,
    symbol_id: u64,
    string_pool: StringPool
}

impl Store {
    pub fn new() -> Store {
        Store {
            edges: Vec::new(),
            symbol_id: 0,
            string_pool: StringPool::new()
        }
    }

    pub fn new_symbol(&mut self) -> Node {
        self.symbol_id += 1;
        Node::Symbol(self.symbol_id)
    }

    pub fn new_string(&mut self, str: &str) -> Node {
        Node::String(self.string_pool.intern(str))
    }

    pub fn push(&mut self, edge: Edge) {
        if !self.contains(&edge) { 
            self.edges.push(edge);
        }
    }

    pub fn remove(&mut self, edge: &Edge) -> bool {
        if let Some(idx) = self.edges.iter().position(|e| e == edge) {
            self.edges.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn print(&self) {
        for edge in self.edges.iter() {
            println!("{:?}", edge);
        }
    }
    
    pub fn contains(&self, edge: &Edge) -> bool {
        self.edges.contains(edge)
    }

    pub fn find_if(&self, test: impl Fn(&&Edge) -> bool) -> impl Iterator<Item = &Edge> {
        self.edges.iter().filter(test)
    }
}

#[derive(Debug, Clone)]
pub enum QNode {
    Node(Node),
    Variable,
    BoundVariable(usize)
}

#[derive(Debug, Clone)]
pub enum Inst {
    Find(QNode, QNode, QNode, Box<Inst>),
    Add(QNode, QNode, QNode),
    Rm(QNode, QNode, QNode),
    Sym(Box<Inst>),
    // GuardEq GuardNeq
    And(Box<Inst>, Box<Inst>),
    Print(usize)
}

pub struct VM {
    store: Store,
    bindings: Vec<Node>
}

impl VM {
    pub fn new(store: Store) -> VM {
        VM {
            store,
            bindings: Vec::new()
        }
    }

    pub fn run(&mut self, inst: &Inst) {
        match inst.clone() {
            Inst::Find(qn0, qn1, qn2, then) => {
                let len = self.bindings.len();
                let edges: Vec<Edge> = match (&qn0, &qn1, &qn2) {
                    (QNode::Node(n0), QNode::Node(n1), QNode::Node(n2)) => self.store.find_if(|edge| {
                        &edge.0 == n0 && &edge.1 == n1 && &edge.2 == n2
                    }).map(|edge| edge.clone()).collect(),
                    (QNode::Node(n0), QNode::Node(n1), _) => self.store.find_if(|edge| {
                        &edge.0 == n0 && &edge.1 == n1
                    }).map(|edge| edge.clone()).collect(),
                    (QNode::Node(n0), _, QNode::Node(n2)) => self.store.find_if(|edge| {
                        &edge.0 == n0 && &edge.2 == n2
                    }).map(|edge| edge.clone()).collect(),
                    (QNode::Node(n0), _, _) => self.store.find_if(|edge| {
                        &edge.0 == n0
                    }).map(|edge| edge.clone()).collect(),
                    (_, QNode::Node(n1), QNode::Node(n2)) => self.store.find_if(|edge| {
                        &edge.1 == n1 && &edge.2 == n2
                    }).map(|edge| edge.clone()).collect(),
                    (_, QNode::Node(n1), _) => self.store.find_if(|edge| {
                        &edge.1 == n1
                    }).map(|edge| edge.clone()).collect(),
                    (_, _, QNode::Node(n2)) => self.store.find_if(|edge| {
                        &edge.2 == n2
                    }).map(|edge| edge.clone()).collect(),
                    (_, _, _) => self.store.find_if(|_| {
                        true
                    }).map(|edge| edge.clone()).collect(),
                };
                for edge in edges {
                    match qn0 {
                        QNode::Variable => {
                            self.bindings.push(edge.0.clone())
                        }
                        QNode::BoundVariable(i) if self.bindings[i as usize] != edge.0 => {
                            self.bindings.resize(len, Node::Symbol(0));
                            continue;
                        }
                        _ => {}
                    }
                    match qn1 {
                        QNode::Variable => {
                            self.bindings.push(edge.1.clone())
                        }
                        QNode::BoundVariable(i) if self.bindings[i as usize] != edge.1 => {
                            self.bindings.resize(len, Node::Symbol(0));
                            continue;
                        }
                        _ => {}
                    }
                    match qn2 {
                        QNode::Variable => {
                            self.bindings.push(edge.2.clone())
                        }
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
                self.store.push(Edge(self.resolve(qn0), self.resolve(qn1), self.resolve(qn2)));
            }
            Inst::Rm(qn0, qn1, qn2) => {
                // TODO: enable variable?
                self.store.remove(&Edge(self.resolve(qn0), self.resolve(qn1), self.resolve(qn2)));
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
            Inst::Print(i) => {
                println!("{:?}", self.bindings[i]);
            }
        }
    }

    fn resolve(&self, qnode: QNode) -> Node {
        match qnode {
            QNode::Node(n) => n,
            QNode::Variable => panic!("cannot resolve for Inst::Variable"),
            QNode::BoundVariable(i) => self.bindings[i].clone()
        }
    }
}
