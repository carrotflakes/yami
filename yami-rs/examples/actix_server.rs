use std::sync::{Arc, Mutex};

use actix_cors::Cors;
use actix_web::{
    guard, http,
    web::{self, Bytes},
    App, HttpMessage, HttpRequest, HttpResponse, HttpServer,
};
use yami::store::Store;

const FILENAME: &str = "interact.yami";

async fn index(
    store: web::Data<Arc<Mutex<Store>>>,
    req: HttpRequest,
    bytes: Bytes,
) -> HttpResponse {
    let code = String::from_utf8(bytes.to_vec()).unwrap();

    let inst = match req.content_type() {
        "application/json" => yami::json::json_to_inst(&code).unwrap(),
        "text/plain" | _ => {
            let mut reader = yami::script::make_reader();
            let ast = match reader.parse(&code) {
                Ok(ast) => ast,
                Err(e) => return HttpResponse::BadRequest().body(e.to_string()),
            };
            yami::script::instize(&mut Default::default(), ast)
        }
    };
    let mut buf = String::new();
    use std::fmt::Write as FmtWrite;
    yami::core::VM::new(&mut store.lock().unwrap(), &mut |n| {
        writeln!(&mut buf, "{}", n).unwrap();
    })
    .run(&inst);
    HttpResponse::Ok().body(buf)
}

async fn save(store: web::Data<Arc<Mutex<Store>>>) -> HttpResponse {
    yami::serialize::write(
        &store.lock().unwrap(),
        &mut std::fs::File::create(FILENAME).unwrap(),
    )
    .unwrap();
    HttpResponse::Ok().body("ok")
}

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    let store = if let Ok(mut f) = std::fs::File::open(FILENAME) {
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
            .service(web::resource("/save").guard(guard::Get()).to(save))
    })
    .bind("0.0.0.0:5000")?
    .run()
    .await
}
