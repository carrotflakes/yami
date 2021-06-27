use std::io::{BufRead, BufReader, Read, Write};

use crate::core::{Edge, Node, Store};

pub fn write(store: &Store, writer: &mut impl Write) -> Result<(), std::io::Error> {
    let mut strings = Vec::new();
    for Edge(a, b, c) in store.edges_ref() {
        for node in &[a, b, c] {
            match node {
                Node::Symbol(_) => {}
                Node::String(s) => {
                    if !strings.contains(s) {
                        strings.push(s.clone());
                    }
                }
            }
        }
    }

    writeln!(writer, "{:?}", strings.len())?;
    for string in &strings {
        writeln!(writer, "{}", serde_json::Value::String(store.get_string(*string).to_owned()))?;
    }

    let offset = strings.len() as u64;
    for Edge(a, b, c) in store.edges_ref() {
        for node in &[a, b, c] {
            match node {
                Node::Symbol(i) => {
                    write!(writer, "{} ", offset + i)?;
                }
                Node::String(s) => {
                    write!(writer, "{} ", strings.iter().position(|x| x == s).unwrap())?;
                }
            }
        }
        writeln!(writer)?;
    }

    Ok(())
}

pub fn read(reader: &mut impl Read) -> Result<Store, std::io::Error> {
    let reader = BufReader::new(reader);
    let mut lines = reader.lines();
    let mut store = Store::new();

    let string_num: usize = lines.next().unwrap()?.trim_end().parse().unwrap();
    let mut strings = Vec::with_capacity(string_num);
    for _ in 0..string_num {
        let str: String = lines.next().unwrap()?;
        let value = serde_json::from_str::<serde_json::Value>(&str).unwrap();
        strings.push(store.new_string(value.as_str().unwrap()));
    }

    for line in lines {
        let nodes = line?
            .split(' ')
            .take(3)
            .map(|x| {
                let i: usize = x.trim_end().parse().unwrap();
                if i < string_num {
                    strings[i].clone()
                } else {
                    store.symbol_from_id((i - string_num) as u64)
                }
            })
            .collect::<Vec<_>>();
        store.push(Edge(nodes[0].clone(), nodes[1].clone(), nodes[2].clone()));
    }

    Ok(store)
}

#[test]
fn test() {
    let mut store = Store::new();
    
    let e = Edge(
        store.new_string("hello"),
        store.new_string("world"),
        store.new_symbol(),
    );
    store.push(e);

    let e = Edge(
        store.new_string("hello"),
        store.new_symbol(),
        store.new_symbol(),
    );
    store.push(e);

    write(&store, &mut std::fs::File::create("hoge").unwrap()).unwrap();

    let store = read(&mut std::fs::File::open("hoge").unwrap()).unwrap();
    store.print();
    write(&store, &mut std::fs::File::create("hoge2").unwrap()).unwrap();
}
