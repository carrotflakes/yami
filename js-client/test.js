const {YamiClient} = require('./dist/yami-client.js');
const yami = new YamiClient({url: 'http://localhost:3000'});

async function f() {
  let edge = null;

  await yami.query(s => {
    const {x, y, z} = s.var;
    s.findAll(x, y, z);
    s.collect((x, y, z) => console.log(edge = [x, y, z]));
  });

  await yami.query(s => {
    s.findAll(...edge);
    s.collect(() => console.log('ok'));
  });
}

f();
