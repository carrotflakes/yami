use yami::{
    core::{Store, VM},
    q::Q,
    script,
};

fn main() {
    {
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
            q.find("a ? ?", Q::and(&[Q::print(0), Q::print(1)])),
            q.find("? ?0 ?", Q::print(1)),
        ]);
        let mut vm = VM::new(&mut store);
        vm.run(&inst);
    }
    println!("===");
    {
        let mut store = Store::new();
        let mut reader = script::make_reader();
        let ast = reader
            .parse(
                r#"
            (and
                (add "a" "b" "c")
                (sym signify
                    (add signify "signify" signify))
                (sym x
                    (add "a" "b" x))
                (add "x" "x" "d")
                (find "a" x y
                    (and
                        (print x)
                        (print y)))
                (find x x y
                    (print y))
            )"#,
            )
            .unwrap();
        let inst = script::instize(&mut Default::default(), ast);
        println!("{:?}", inst);
        VM::new(&mut store).run(&inst);
    }
    println!("===");
    {
        let code = stringify! {
        (and
            (sym signify
                (and
                    (add signify "signify" signify)
                    (sym hoge
                        (add signify "hoge" hoge))
                    (find signify x y
                        (and (print x) (print y)))
            ))
            (add "a" "b" "c")
            (sym x
                (add "a" "b" x))
            (add "x" "x" "d")
            (find "a" x y
                (and
                    (print x)
                    (print y)))
            (find x x y
                (print y))
        )};
        let ast = script::make_reader().parse(code).unwrap();
        let mut store = Store::new();
        let inst = script::instize(&mut Default::default(), ast);
        VM::new(&mut store).run(&inst);
    }
}
