use std::sync::{Arc, Mutex};

use actix_cors::Cors;
use actix_web::{
    guard, http,
    web::{self, Bytes},
    App, HttpResponse, HttpServer,
};
use yami::store::Store;

async fn index(store: web::Data<Arc<Mutex<Store>>>, bytes: Bytes) -> HttpResponse {
    let code = String::from_utf8(bytes.to_vec()).unwrap();

    let mut reader = yami::script::make_reader();
    let ast = match reader.parse(&code) {
        Ok(ast) => ast,
        Err(e) => return HttpResponse::BadRequest().body(e.to_string()),
    };
    let inst = yami::script::instize(&mut Default::default(), ast);
    let mut buf = String::new();
    use std::fmt::Write as FmtWrite;
    yami::core::VM::new(&mut store.lock().unwrap(), &mut |n| {
        writeln!(&mut buf, "{}", n).unwrap();
    })
    .run(&inst);
    HttpResponse::Ok().body(buf)
}

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    let filename = "interact.yami";
    let store = if let Ok(mut f) = std::fs::File::open(filename) {
        yami::serialize::read(&mut f).unwrap()
    } else {
        yami::store::Store::new()
    };
    let store = Arc::new(Mutex::new(store));

    println!("Playground: http://localhost:5000");

    HttpServer::new(move || {
        let cors = Cors::default()
            .allow_any_origin()
            .allowed_methods(vec!["GET", "POST"])
            .allowed_headers(vec![http::header::ACCEPT])
            .allowed_header(http::header::CONTENT_TYPE)
            .max_age(3600);

        App::new()
            .data(store.clone())
            .wrap(cors)
            .service(web::resource("/").guard(guard::Post()).to(index))
    })
    .bind("0.0.0.0:5000")?
    .run()
    .await
}
