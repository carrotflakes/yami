const {YamiClient} = require('./dist/yami-client.js');
const yami = new YamiClient({url: 'http://localhost:3000'});

yami.query(s => {
  const {x, y, z} = s.var;
  s.findAll(x, y, z);
  s.collect((x, y, z) => console.log([x, y, z]));
});
