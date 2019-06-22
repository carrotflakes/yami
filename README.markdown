# Yami

Yami is a system for managing large-scale labeled directed graph.
Yami aims to be a single huge knowledge base.

## Usage

Start server:

``` sh
$ ros run -e "(ql:quickload :yami-server)"
```

Then you can access to localhost:3000 .

### Web client

After server start, you can use the built-in web client that enables you GUI operation.

``` sh
$ cd web-client
$ npm i
$ npm run serve
```

And open http://localhost:8080/ .

## Installation

``` sh
$ ros install carrotflakes/snaky
$ ros install carrotflakes/yami
```

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2019 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
