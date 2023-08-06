use std::collections::HashMap;

use crate::core::{Inst, Node, QNode};

pub fn json_to_inst(src: &str) -> Result<Inst, String> {
    let json = serde_json::from_str(src).map_err(|x| x.to_string())?;
    json_to_inst_(&mut Default::default(), json)
}

fn json_to_inst_(
    bindings: &mut (Vec<String>, HashMap<String, QNode>),
    json: serde_json::Value,
) -> Result<Inst, String> {
    if let Some(a) = json.as_array() {
        if let Some(s) = a.get(0).to_owned().unwrap().as_str() {
            return match s {
                "find" => {
                    let mut bindings = bindings.clone();
                    Ok(Inst::Find(
                        qn(&mut bindings, a[1].clone()),
                        qn(&mut bindings, a[2].clone()),
                        qn(&mut bindings, a[3].clone()),
                        Box::new(json_to_inst_(&mut bindings, a[4].clone())?),
                    ))
                }
                "add" => Ok(Inst::Add(
                    qn(bindings, a[1].clone()),
                    qn(bindings, a[2].clone()),
                    qn(bindings, a[3].clone()),
                )),
                "rm" => Ok(Inst::Rm(
                    qn(bindings, a[1].clone()),
                    qn(bindings, a[2].clone()),
                    qn(bindings, a[3].clone()),
                )),
                "sym" => {
                    // TODO: shdowing
                    let mut bindings = bindings.clone();
                    let symbols = a[1].as_array().unwrap();
                    let symbols = symbols
                        .iter()
                        .map(|x| x.as_str().unwrap().to_owned())
                        .collect::<Vec<String>>();

                    let n = symbols.len();
                    for symbol in symbols.into_iter() {
                        bindings
                            .1
                            .insert(symbol.clone(), QNode::BoundVariable(bindings.0.len()));
                        bindings.0.push(symbol);
                    }
                    Ok(Inst::Sym(
                        n,
                        Box::new(json_to_inst_(&mut bindings, a[2].clone())?),
                    ))
                }
                "and" => {
                    let mut inst = json_to_inst_(bindings, a[1].clone())?;
                    for i in 2..a.len() {
                        inst = Inst::And(
                            Box::new(inst),
                            Box::new(json_to_inst_(bindings, a[i].clone())?),
                        );
                    }
                    Ok(inst)
                }
                "print" => Ok(Inst::Print(qn(bindings, a[1].clone()))),
                _ => {
                    panic!("what: {:?}", s);
                }
            };
        }
    }
    return Err("Unexpected json".to_owned());
}

fn qn(bindings: &mut (Vec<String>, HashMap<String, QNode>), val: serde_json::Value) -> QNode {
    match val {
        serde_json::Value::Array(a) => match a.get(0).to_owned() {
            Some(serde_json::Value::String(s)) => match s.as_str() {
                "str" => QNode::UninternedString(a[1].as_str().unwrap().to_owned()),
                "var" => {
                    let s = a[1].as_str().unwrap().to_owned();
                    if let Some(qn) = bindings.1.get(&s) {
                        qn.clone()
                    } else {
                        bindings
                            .1
                            .insert(s.clone(), QNode::BoundVariable(bindings.0.len()));
                        bindings.0.push(s.clone());
                        QNode::Variable
                    }
                }
                "sym" => QNode::Node(Node::Symbol(a[1].as_u64().unwrap())),
                _ => todo!(),
            },
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[test]
fn test() {
    dbg!(json_to_inst(
        r#"["find", ["var", "a"], ["var", "b"], ["var", "b"], ["print", ["str", "hello"]]]"#
    )
    .unwrap());
}
