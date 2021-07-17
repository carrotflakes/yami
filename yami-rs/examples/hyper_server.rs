use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server, StatusCode};
use std::sync::{Arc, Mutex};
use std::{convert::Infallible, net::SocketAddr};

async fn handle(req: Request<Body>) -> Result<Response<Body>, Infallible> {
    let full_body = hyper::body::to_bytes(req.into_body()).await.unwrap();
    let res: String = String::from_utf8(full_body.to_vec()).unwrap();

    Ok(Response::new(res.into()))
}

#[tokio::main]
async fn main() {
    let addr = SocketAddr::from(([127, 0, 0, 1], 5000));
    let store = Arc::new(Mutex::new(yami::store::Store::new()));

    let make_svc = make_service_fn(move |_conn| {
        let store = store.clone();
        async move {
            let f = move |req: Request<Body>| {
                let store = store.clone();
                async move {
                    let full_body = hyper::body::to_bytes(req.into_body()).await.unwrap();
                    let code: String = String::from_utf8(full_body.to_vec()).unwrap();

                    let mut reader = yami::script::make_reader();
                    let ast = match reader.parse(&code) {
                        Ok(ast) => ast,
                        Err(e) => {
                            let mut res = Response::new(e.into());
                            *res.status_mut() = StatusCode::BAD_REQUEST;
                            return Ok::<Response<Body>, Infallible>(res)
                        },
                    };
                    let inst = yami::script::instize(&mut Default::default(), ast);
                    let mut buf = String::new();
                    use std::fmt::Write as FmtWrite;
                    yami::core::VM::new(&mut store.lock().unwrap(), &mut |n| {
                        writeln!(&mut buf, "{}", n).unwrap();
                    })
                    .run(&inst);

                    Ok::<Response<Body>, Infallible>(Response::new(buf.into()))
                }
            };

            Ok::<_, Infallible>(service_fn(f))
        }
    });

    let server = Server::bind(&addr).serve(make_svc);

    if let Err(e) = server.await {
        eprintln!("server error: {}", e);
    }
}
