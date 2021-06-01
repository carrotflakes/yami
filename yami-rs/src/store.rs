use crate::{core::{Edge, Node}, pool::StringPool};

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

    pub(crate) fn symbol_from_id(&mut self, id: u64) -> Node {
        if self.symbol_id < id {
            self.symbol_id = id;
        }
        Node::Symbol(id)
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

    pub fn edges_ref(&self) -> &[Edge] {
        &self.edges
    }
}
