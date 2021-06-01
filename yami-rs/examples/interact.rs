fn main() {
    let filename = "interact.yami";
    let mut store = if let Ok(mut f) = std::fs::File::open(filename) {
        yami::serialize::read(&mut f).unwrap()
    } else {
        yami::store::Store::new()
    };
    yami::interact(&mut store);
    yami::serialize::write(&store, &mut std::fs::File::create(filename).unwrap()).unwrap();
}
