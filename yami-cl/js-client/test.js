const {YamiClient} = require('./dist/yami-client.js');
const yami = new YamiClient({url: 'http://localhost:3000'});

async function test(){

  console.log('raw query ====');

  console.log(await yami.sendRawQuery('findAll x y z; collect x y z;'));

  console.log('query ====');

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

  await yami.query(s => {
    s.add('a', 'b', 'c');
    s.rm1('a', 'b', 'c');
    s.collect(() => console.log('ok'));
  });

  console.log('fetchAsVertex ====');

  const node = await yami.fetchAsVertex('carrotflakes').catch(e => console.log(e));

  console.log(`node ${node.name}:`);
  console.log(node.edgesFrom);
  console.log(node.edgesTo);

  console.log('addEdge, removeEdge ====');

  console.log(await yami.addEdge('aa', 'bb', 'cc'));
  console.log(await yami.removeEdge('aa', 'bb', 'cc'));

}

test();
